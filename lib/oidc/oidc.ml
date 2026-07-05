type provider_config = {
  issuer : string;
  authorization_endpoint : string;
  token_endpoint : string;
  jwks_uri : string;
  end_session_endpoint : string option;
}

type claims = Yojson.Safe.t
type jwk = { kid : string; n : string; e : string }
type token_response = { id_token : string; access_token : string option }
type error = Http_error of string | Json_error of string | Jwt_error of string

let pp_error fmt = function
  | Http_error s -> Format.fprintf fmt "HTTP error: %s" s
  | Json_error s -> Format.fprintf fmt "JSON error: %s" s
  | Jwt_error s -> Format.fprintf fmt "JWT error: %s" s

let curl_get url =
  let cmd = Printf.sprintf "curl -sfS --max-time 10 %s" (Filename.quote url) in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_char buf (input_char ic)
     done
   with End_of_file -> ());
  let status = Unix.close_process_in ic in
  match status with
  | Unix.WEXITED 0 -> Ok (Buffer.contents buf)
  | Unix.WEXITED code ->
      Error
        (Http_error (Printf.sprintf "curl GET %s failed (exit %d)" url code))
  | _ -> Error (Http_error (Printf.sprintf "curl GET %s killed" url))

let curl_post url form_data =
  let body =
    List.map
      (fun (k, v) ->
        Uri.pct_encode ~component:`Query_value k
        ^ "="
        ^ Uri.pct_encode ~component:`Query_value v)
      form_data
    |> String.concat "&"
  in
  let cmd =
    Printf.sprintf "curl -sfS --max-time 10 -X POST --data-binary @- %s"
      (Filename.quote url)
  in
  let ic, oc = Unix.open_process cmd in
  output_string oc body;
  close_out oc;
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_char buf (input_char ic)
     done
   with End_of_file -> ());
  let status = Unix.close_process (ic, oc) in
  match status with
  | Unix.WEXITED 0 -> Ok (Buffer.contents buf)
  | Unix.WEXITED code ->
      Error
        (Http_error (Printf.sprintf "curl POST %s failed (exit %d)" url code))
  | _ -> Error (Http_error (Printf.sprintf "curl POST %s killed" url))

let json_string_field key json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | Some _ ->
          Error (Json_error (Printf.sprintf "field %s is not a string" key))
      | None -> Error (Json_error (Printf.sprintf "missing field %s" key)))
  | _ -> Error (Json_error "expected JSON object")

let json_string_field_opt key json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with
      | Some (`String s) -> Some s
      | _ -> None)
  | _ -> None

let base64url_decode s =
  let s = String.map (function '-' -> '+' | '_' -> '/' | c -> c) s in

  let padding = (4 - (String.length s mod 4)) mod 4 in
  let s = s ^ String.make padding '=' in
  match Base64.decode s with
  | Ok decoded -> Ok decoded
  | Error (`Msg msg) ->
      Error (Jwt_error (Printf.sprintf "base64url decode: %s" msg))

let discovery_cache : (string, provider_config) Hashtbl.t = Hashtbl.create 4

let discover issuer_url =
  match Hashtbl.find_opt discovery_cache issuer_url with
  | Some config -> Ok config
  | None ->
      let base =
        if
          String.length issuer_url > 0
          && issuer_url.[String.length issuer_url - 1] = '/'
        then String.sub issuer_url 0 (String.length issuer_url - 1)
        else issuer_url
      in
      let url = base ^ "/.well-known/openid-configuration" in
      Result.bind (curl_get url) (fun body ->
          try
            let json = Yojson.Safe.from_string body in
            Result.bind (json_string_field "issuer" json) (fun issuer ->
                Result.bind (json_string_field "authorization_endpoint" json)
                  (fun authorization_endpoint ->
                    Result.bind (json_string_field "token_endpoint" json)
                      (fun token_endpoint ->
                        Result.bind (json_string_field "jwks_uri" json)
                          (fun jwks_uri ->
                            let end_session_endpoint =
                              json_string_field_opt "end_session_endpoint" json
                            in
                            let config =
                              {
                                issuer;
                                authorization_endpoint;
                                token_endpoint;
                                jwks_uri;
                                end_session_endpoint;
                              }
                            in
                            Hashtbl.replace discovery_cache issuer_url config;
                            Ok config))))
          with Yojson.Json_error msg -> Error (Json_error msg))

let authorization_url provider ~client_id ~redirect_uri ~state ~nonce =
  let params =
    [
      ("response_type", "code");
      ("scope", "openid email profile");
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("state", state);
      ("nonce", nonce);
    ]
  in
  let base = Uri.of_string provider.authorization_endpoint in
  let uri = Uri.add_query_params' base params in
  Uri.to_string uri

let exchange_code provider ~client_id ~client_secret ~redirect_uri ~code =
  let form_data =
    [
      ("grant_type", "authorization_code");
      ("code", code);
      ("redirect_uri", redirect_uri);
      (* client_secret_post: client credentials in POST body *)
      ("client_id", client_id);
      ("client_secret", client_secret);
    ]
  in
  Result.bind (curl_post provider.token_endpoint form_data) (fun body ->
      try
        let json = Yojson.Safe.from_string body in
        Result.bind (json_string_field "id_token" json) (fun id_token ->
            let access_token = json_string_field_opt "access_token" json in
            Ok { id_token; access_token })
      with Yojson.Json_error msg -> Error (Json_error msg))

let jwks_cache : (string, jwk list) Hashtbl.t = Hashtbl.create 4

let parse_jwks_json body =
  try
    let json = Yojson.Safe.from_string body in
    match json with
    | `Assoc fields -> (
        match List.assoc_opt "keys" fields with
        | Some (`List keys) ->
            let parse_key json =
              match
                ( json_string_field_opt "kty" json,
                  json_string_field_opt "kid" json,
                  json_string_field_opt "n" json,
                  json_string_field_opt "e" json )
              with
              | Some "RSA", Some kid, Some n, Some e -> Some { kid; n; e }
              | _ -> None
            in
            Ok (List.filter_map parse_key keys)
        | _ -> Error (Json_error "JWKS missing 'keys' array"))
    | _ -> Error (Json_error "JWKS expected JSON object")
  with Yojson.Json_error msg -> Error (Json_error msg)

let fetch_jwks_internal ~force_refresh jwks_uri =
  match
    if force_refresh then None else Hashtbl.find_opt jwks_cache jwks_uri
  with
  | Some keys -> Ok keys
  | None ->
      Result.bind (curl_get jwks_uri) (fun body ->
          Result.bind (parse_jwks_json body) (fun keys ->
              Hashtbl.replace jwks_cache jwks_uri keys;
              Ok keys))

let fetch_jwks jwks_uri = fetch_jwks_internal ~force_refresh:false jwks_uri

let decode_bigint_b64url s =
  Result.bind (base64url_decode s) (fun bytes ->
      Ok (Mirage_crypto_pk.Z_extra.of_octets_be bytes))

let rsa_pub_of_jwk jwk =
  Result.bind (decode_bigint_b64url jwk.e) (fun e ->
      Result.bind (decode_bigint_b64url jwk.n) (fun n ->
          match Mirage_crypto_pk.Rsa.pub ~e ~n with
          | Ok pub -> Ok pub
          | Error (`Msg msg) ->
              Error (Jwt_error (Printf.sprintf "RSA key error: %s" msg))))

let split_jwt token =
  match String.split_on_char '.' token with
  | [ header; payload; signature ] -> Ok (header, payload, signature)
  | _ -> Error (Jwt_error "JWT must have exactly 3 parts")

let verify_rs256 ~pub_key ~header_b64 ~payload_b64 ~signature_b64 =
  Result.bind (base64url_decode signature_b64) (fun signature ->
      let message = header_b64 ^ "." ^ payload_b64 in
      if
        Mirage_crypto_pk.Rsa.PKCS1.verify
          ~hashp:(fun h -> h = `SHA256)
          ~key:pub_key ~signature (`Message message)
      then Ok ()
      else Error (Jwt_error "RS256 signature verification failed"))

let decode_payload payload_b64 =
  Result.bind (base64url_decode payload_b64) (fun payload_json ->
      try
        let json = Yojson.Safe.from_string payload_json in
        match json with
        | `Assoc _ -> Ok json
        | _ -> Error (Jwt_error "JWT payload is not a JSON object")
      with Yojson.Json_error msg ->
        Error (Jwt_error (Printf.sprintf "JWT payload JSON error: %s" msg)))

let claims_of_json_string s =
  try
    match Yojson.Safe.from_string s with
    | `Assoc _ as json -> Ok json
    | _ -> Error (Json_error "claims JSON is not an object")
  with Yojson.Json_error msg -> Error (Json_error msg)

let scalar_to_string = function
  | `String s -> Some s
  | `Int i -> Some (string_of_int i)
  | `Intlit s -> Some s
  | `Float f -> Some (Int64.to_string (Int64.of_float f))
  | `Bool b -> Some (string_of_bool b)
  | _ -> None

let json_at_path (claims : claims) dotted =
  let rec navigate json = function
    | [] -> Some json
    | key :: rest -> (
        match json with
        | `Assoc fields -> (
            match List.assoc_opt key fields with
            | Some v -> navigate v rest
            | None -> None)
        | _ -> None)
  in
  navigate claims (String.split_on_char '.' dotted)

let claim_string (claims : claims) path =
  match json_at_path claims path with
  | Some j -> scalar_to_string j
  | None -> None

let claim_has_value (claims : claims) ~path ~value =
  match json_at_path claims path with
  | Some (`List items) ->
      List.exists
        (fun it ->
          match scalar_to_string it with Some s -> s = value | None -> false)
        items
  | Some j -> (
      match scalar_to_string j with Some s -> s = value | None -> false)
  | None -> false

let validate_claims ~client_id ~issuer ~nonce claims =
  let ( let* ) = Result.bind in

  let* () =
    match claim_string claims "iss" with
    | Some iss when iss = issuer -> Ok ()
    | Some iss ->
        Error
          (Jwt_error
             (Printf.sprintf "iss mismatch: expected %s, got %s" issuer iss))
    | None -> Error (Jwt_error "missing iss claim")
  in

  let* () =
    match json_at_path claims "aud" with
    | None -> Error (Jwt_error "missing aud claim")
    | Some _ ->
        if claim_has_value claims ~path:"aud" ~value:client_id then Ok ()
        else
          Error
            (Jwt_error
               (Printf.sprintf "aud does not contain client_id %s" client_id))
  in

  let* () =
    match claim_string claims "exp" with
    | Some exp_s -> (
        try
          let exp = Int64.of_string exp_s in
          let now = Int64.of_float (Unix.time ()) in
          if Int64.sub exp now >= -60L then Ok ()
          else Error (Jwt_error "id_token has expired")
        with Failure _ -> Error (Jwt_error "exp claim is not a valid number"))
    | None -> Error (Jwt_error "missing exp claim")
  in

  match claim_string claims "nonce" with
  | Some n when n = nonce -> Ok ()
  | Some n ->
      Error
        (Jwt_error
           (Printf.sprintf "nonce mismatch: expected %s, got %s" nonce n))
  | None -> Error (Jwt_error "missing nonce claim")

let verify_and_decode_id_token ~jwks_uri ~client_id ~issuer ~nonce token =
  let ( let* ) = Result.bind in
  let* header_b64, payload_b64, signature_b64 = split_jwt token in
  let* header_json = base64url_decode header_b64 in
  let header =
    try Ok (Yojson.Safe.from_string header_json)
    with Yojson.Json_error msg ->
      Error (Jwt_error (Printf.sprintf "JWT header JSON error: %s" msg))
  in
  let* header = header in
  let alg = json_string_field_opt "alg" header in
  let kid = json_string_field_opt "kid" header in
  let* () =
    match alg with
    | Some "RS256" -> Ok ()
    | Some other ->
        Error
          (Jwt_error
             (Printf.sprintf "unsupported JWT alg: %s (only RS256)" other))
    | None -> Error (Jwt_error "missing alg in JWT header")
  in
  let find_key keys =
    match kid with
    | Some kid_val -> List.find_opt (fun k -> k.kid = kid_val) keys
    | None -> ( match keys with k :: _ -> Some k | [] -> None)
  in
  let verify_with_key key =
    let* pub_key = rsa_pub_of_jwk key in
    let* () = verify_rs256 ~pub_key ~header_b64 ~payload_b64 ~signature_b64 in
    let* claims = decode_payload payload_b64 in
    let* () = validate_claims ~client_id ~issuer ~nonce claims in
    Ok claims
  in
  let* keys = fetch_jwks_internal ~force_refresh:false jwks_uri in
  match find_key keys with
  | Some key -> verify_with_key key
  | None -> (
      let* keys = fetch_jwks_internal ~force_refresh:true jwks_uri in
      match find_key keys with
      | Some key -> verify_with_key key
      | None ->
          Error
            (Jwt_error
               (Printf.sprintf "no matching JWK found for kid=%s"
                  (Option.value ~default:"(none)" kid))))

let logout_url provider ~id_token_hint ~post_logout_redirect_uri =
  match provider.end_session_endpoint with
  | None -> None
  | Some endpoint ->
      let params =
        [
          ("id_token_hint", id_token_hint);
          ("post_logout_redirect_uri", post_logout_redirect_uri);
        ]
      in
      let base = Uri.of_string endpoint in
      let uri = Uri.add_query_params' base params in
      Some (Uri.to_string uri)

let generate_random_hex len =
  let bytes = Mirage_crypto_rng_unix.getrandom len in
  let hex = Buffer.create (len * 2) in
  String.iter
    (fun c -> Buffer.add_string hex (Printf.sprintf "%02x" (Char.code c)))
    bytes;
  Buffer.contents hex

let generate_state () = generate_random_hex 16
let generate_nonce () = generate_random_hex 16
let generate_token () = generate_random_hex 32
