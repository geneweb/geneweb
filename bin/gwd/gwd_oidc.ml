open Geneweb
open Config
module Server = Geneweb_http.Server
module Code = Geneweb_http.Code
open Cmd_legacy

let src = Logs.Src.create ~doc:"OIDC" "OIDC"

module Log = (val Logs.src_log src : Logs.LOG)

let oidc_sessions_file () = !GWPARAM.adm_file "oidc_sessions"

(* The login flow (state, nonce, PKCE verifier) is bound to the initiating
   browser through a signed cookie instead of a shared server-side store. *)

let login_cookie_name base_file = "gw_oidc_login_" ^ base_file

let login_cookie_sig secret ~base_file ~state ~nonce ~verifier ~exp =
  let msg =
    String.concat "\000"
      [ base_file; state; nonce; verifier; string_of_int exp ]
  in
  Digestif.SHA256.(to_hex (hmac_string ~key:secret msg))

let make_login_cookie secret ~base_file ~state ~nonce ~verifier ~exp =
  let s = login_cookie_sig secret ~base_file ~state ~nonce ~verifier ~exp in
  String.concat "." [ state; nonce; verifier; string_of_int exp; s ]

let parse_login_cookie secret ~base_file value =
  match String.split_on_char '.' value with
  | [ state; nonce; verifier; exp_s; s ] -> (
      match int_of_string_opt exp_s with
      | Some exp
        when String.equal s
               (login_cookie_sig secret ~base_file ~state ~nonce ~verifier ~exp)
             && float_of_int exp >= Unix.time () ->
          Some (state, nonce, verifier)
      | _ -> None)
  | _ -> None

let read_oidc_sessions () =
  let fname = oidc_sessions_file () in
  if not (Sys.file_exists fname) then []
  else
    try
      let ic = Secure.open_in fname in
      let tmout = float_of_int !login_timeout in
      let now = Unix.time () in
      let rec loop acc =
        match input_line ic with
        | line -> (
            match String.split_on_char ' ' line with
            | cookie :: ts_str :: from :: base :: acc_str :: id_tok :: user
              :: rest
              when String.length acc_str = 1 -> (
                match float_of_string_opt ts_str with
                | Some ts when now -. ts < tmout ->
                    let username = String.concat " " rest in
                    loop
                      (( cookie,
                         ts,
                         from,
                         base,
                         acc_str.[0],
                         id_tok,
                         user,
                         username )
                      :: acc)
                | _ -> loop acc)
            | _ -> loop acc)
        | exception End_of_file ->
            close_in ic;
            List.rev acc
      in
      loop []
    with Sys_error _ -> []

let write_oidc_sessions entries =
  let fname = oidc_sessions_file () in
  try
    let oc = Secure.open_out fname in
    List.iter
      (fun (cookie, ts, from, base, acc, id_tok, user, username) ->
        Printf.fprintf oc "%s %.0f %s %s %c %s %s%s\n" cookie ts from base acc
          id_tok user
          (if username = "" then "" else " " ^ username))
      entries;
    close_out oc
  with Sys_error _ -> ()

let add_oidc_session cookie_token from_addr base_name access_char id_token user
    username =
  let lock_file = !GWPARAM.adm_file "gwd.lck" in
  let on_exn _exn _bt = () in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  let entries = read_oidc_sessions () in
  let now = Unix.time () in
  let entries =
    ( cookie_token,
      now,
      from_addr,
      base_name,
      access_char,
      id_token,
      user,
      username )
    :: entries
  in
  write_oidc_sessions entries

let lookup_oidc_session cookie_token base_name =
  let lock_file = !GWPARAM.adm_file "gwd.lck" in
  let on_exn _exn _bt = None in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  let entries = read_oidc_sessions () in
  let found = ref None in
  let touched = ref false in
  let now = Unix.time () in
  let entries =
    List.map
      (fun ((cookie, ts, from, base, acc, id_tok, user, username) as entry) ->
        if cookie = cookie_token && base = base_name then begin
          found := Some (acc, user, username);
          (* slide the expiry at most once a minute to avoid a rewrite per request *)
          if now -. ts > 60. then begin
            touched := true;
            (cookie, now, from, base, acc, id_tok, user, username)
          end
          else entry
        end
        else entry)
      entries
  in
  if !touched then write_oidc_sessions entries;
  !found

let remove_oidc_session cookie_token base_name =
  let lock_file = !GWPARAM.adm_file "gwd.lck" in
  let on_exn _exn _bt = None in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  let entries = read_oidc_sessions () in
  let id_token_ref = ref None in
  let entries =
    List.filter
      (fun (cookie, _ts, _from, base, _acc, id_tok, _user, _username) ->
        if cookie = cookie_token && base = base_name then begin
          id_token_ref := Some id_tok;
          false
        end
        else true)
      entries
  in
  write_oidc_sessions entries;
  !id_token_ref

let extract_oidc_cookie request cookie_name =
  let cookie_hdr = Mutil.extract_param "cookie: " '\n' request in
  if cookie_hdr = "" then None
  else
    let prefix = cookie_name ^ "=" in
    let prefix_len = String.length prefix in
    let pairs = String.split_on_char ';' cookie_hdr in
    let rec find = function
      | [] -> None
      | pair :: rest ->
          let pair = String.trim pair in
          if
            String.length pair > prefix_len
            && String.sub pair 0 prefix_len = prefix
          then
            Some (String.sub pair prefix_len (String.length pair - prefix_len))
          else find rest
    in
    find pairs

let cookie_access request base_name =
  let cookie_name = "gw_oidc_" ^ base_name in
  match extract_oidc_cookie request cookie_name with
  | None -> None
  | Some cookie_token -> lookup_oidc_session cookie_token base_name

type oidc_config = {
  provider_url : string;
  client_id : string;
  client_secret : string;
  redirect_uri : string;
  user_claim : string;
  role_claim : string;
  wizard_role : string;
  friend_role : string;
  person_key_claim : string;
}

let read_oidc_config base_env =
  match List.assoc_opt "oidc_provider_url" base_env with
  | None | Some "" -> None
  | Some provider_url ->
      let get key default =
        try List.assoc key base_env with Not_found -> default
      in
      let client_id = get "oidc_client_id" "" in
      let client_secret =
        try
          let file = List.assoc "oidc_client_secret_file" base_env in
          if file <> "" then (
            let ic = open_in file in
            let s = String.trim (input_line ic) in
            close_in ic;
            s)
          else raise Not_found
        with Not_found | Sys_error _ | End_of_file ->
          get "oidc_client_secret" ""
      in
      let redirect_uri = get "oidc_redirect_uri" "" in
      if client_id = "" || client_secret = "" || redirect_uri = "" then None
      else
        Some
          {
            provider_url;
            client_id;
            client_secret;
            redirect_uri;
            user_claim = get "oidc_user_claim" "email";
            role_claim = get "oidc_role_claim" "";
            wizard_role = get "oidc_wizard_role" "";
            friend_role = get "oidc_friend_role" "";
            person_key_claim = get "oidc_person_key_claim" "";
          }

let oidc_error_page conf msg =
  Log.warn (fun k -> k "authentication failed: %s" msg);
  Output.status conf Code.Bad_Request;
  Output.header conf "Content-type: text/html; charset=utf-8";
  Output.print_sstring conf "<html><head><title>OIDC Error</title></head><body>";
  Output.print_sstring conf "<h1>Authentication Error</h1><p>";
  Output.print_sstring conf
    "Authentication failed. Please try again, or contact the administrator.";
  Output.print_sstring conf "</p><p><a href=\"";
  Output.print_sstring conf conf.command;
  Output.print_sstring conf "\">Back</a></p></body></html>";
  Output.flush conf

let conf_secret conf = match conf.secret_salt with Some s -> s | None -> ""

let set_login_cookie conf base_file value =
  Output.header conf
    "Set-Cookie: %s=%s; Path=/; HttpOnly; Secure; SameSite=Lax; Max-Age=600"
    (login_cookie_name base_file)
    value

let clear_login_cookie conf base_file =
  Output.header conf
    "Set-Cookie: %s=; Path=/; HttpOnly; Secure; SameSite=Lax; Max-Age=0"
    (login_cookie_name base_file)

let handle_oidc_login conf base_env base_file =
  match read_oidc_config base_env with
  | None -> oidc_error_page conf "OIDC not configured for this base"
  | Some cfg -> (
      match Geneweb_oidc.Oidc.discover cfg.provider_url with
      | Error e ->
          oidc_error_page conf
            (Format.asprintf "%a" Geneweb_oidc.Oidc.pp_error e)
      | Ok provider ->
          let state = Geneweb_oidc.Oidc.generate_state () in
          let nonce = Geneweb_oidc.Oidc.generate_nonce () in
          let verifier = Geneweb_oidc.Oidc.generate_code_verifier () in
          let exp = int_of_float (Unix.time ()) + 600 in
          let cookie =
            make_login_cookie (conf_secret conf) ~base_file ~state ~nonce
              ~verifier ~exp
          in
          let url =
            Geneweb_oidc.Oidc.authorization_url provider
              ~client_id:cfg.client_id ~redirect_uri:cfg.redirect_uri ~state
              ~nonce
              ~code_challenge:(Geneweb_oidc.Oidc.code_challenge verifier)
          in
          Log.info (fun k -> k "login initiated: base=%s" base_file);
          Output.status conf Code.Moved_Temporarily;
          set_login_cookie conf base_file cookie;
          Output.header conf "Location: %s" url;
          Output.flush conf)

let handle_oidc_callback conf base_env from_addr base_file _utm =
  let ( let* ) = Result.bind in
  let err_str e = Format.asprintf "%a" Geneweb_oidc.Oidc.pp_error e in
  let result =
    let* () =
      match Util.p_getenv conf.env "error" with
      | None -> Ok ()
      | Some err ->
          let desc =
            match Util.p_getenv conf.env "error_description" with
            | Some d -> err ^ ": " ^ d
            | None -> err
          in
          Error ("IdP returned error: " ^ desc)
    in
    let* code =
      match Util.p_getenv conf.env "code" with
      | Some c -> Ok c
      | None -> Error "Missing 'code' parameter in callback"
    in
    let* url_state =
      match Util.p_getenv conf.env "state" with
      | Some s -> Ok s
      | None -> Error "Missing 'state' parameter in callback"
    in
    (* matching the URL state against the login cookie is what stops login CSRF *)
    let* nonce, verifier =
      match extract_oidc_cookie conf.request (login_cookie_name base_file) with
      | None -> Error "No login in progress"
      | Some v -> (
          match parse_login_cookie (conf_secret conf) ~base_file v with
          | None -> Error "Invalid or expired login state"
          | Some (cookie_state, nonce, verifier) ->
              if String.equal url_state cookie_state then Ok (nonce, verifier)
              else Error "State does not match")
    in
    let* cfg =
      match read_oidc_config base_env with
      | Some cfg -> Ok cfg
      | None -> Error "OIDC not configured"
    in
    let* provider =
      Result.map_error err_str (Geneweb_oidc.Oidc.discover cfg.provider_url)
    in
    let* token_resp =
      Result.map_error err_str
        (Geneweb_oidc.Oidc.exchange_code provider ~client_id:cfg.client_id
           ~client_secret:cfg.client_secret ~redirect_uri:cfg.redirect_uri ~code
           ~code_verifier:verifier)
    in
    let* claims =
      Result.map_error err_str
        (Geneweb_oidc.Oidc.verify_and_decode_id_token
           ~jwks_uri:provider.jwks_uri ~client_id:cfg.client_id
           ~issuer:provider.issuer ~nonce token_resp.id_token)
    in
    let claim_string path = Geneweb_oidc.Oidc.claim_string claims path in
    let claim_value =
      match claim_string cfg.user_claim with
      | Some v -> v
      | None -> ( match claim_string "sub" with Some v -> v | None -> "")
    in
    let* () =
      if claim_value = "" then
        Error ("No '" ^ cfg.user_claim ^ "' claim found in id_token")
      else Ok ()
    in
    let has_role role =
      role <> "" && cfg.role_claim <> ""
      && Geneweb_oidc.Oidc.claim_has_value claims ~path:cfg.role_claim
           ~value:role
    in
    let acc =
      if has_role cfg.wizard_role then 'w'
      else if has_role cfg.friend_role then 'f'
      else 'v'
    in
    let person_key =
      if cfg.person_key_claim = "" then ""
      else
        match claim_string cfg.person_key_claim with Some v -> v | None -> ""
    in
    let display_name =
      match claim_string "name" with
      | Some v when v <> "" -> v
      | _ -> claim_value
    in
    let username =
      if person_key <> "" then display_name ^ "|" ^ person_key else display_name
    in
    Ok (acc, claim_value, username, token_resp.id_token)
  in
  let base_url =
    if !Server.cgi then conf.command ^ "?b=" ^ base_file else base_file
  in
  match result with
  | Error msg -> oidc_error_page conf msg
  | Ok ('v', user, _, _) ->
      Log.info (fun k ->
          k "login as visitor (no role): base=%s user=%s from=%s" base_file user
            from_addr);
      Output.status conf Code.Moved_Temporarily;
      clear_login_cookie conf base_file;
      Output.header conf "Location: %s" base_url;
      Output.flush conf
  | Ok (acc, claim_value, username, id_token) ->
      Log.info (fun k ->
          k "login: base=%s user=%s access=%c from=%s" base_file claim_value acc
            from_addr);
      let cookie_token = Geneweb_oidc.Oidc.generate_token () in
      add_oidc_session cookie_token from_addr base_file acc id_token claim_value
        username;
      let cookie_name = "gw_oidc_" ^ base_file in
      Output.status conf Code.Moved_Temporarily;
      clear_login_cookie conf base_file;
      Output.header conf
        "Set-Cookie: %s=%s; Path=/; HttpOnly; Secure; SameSite=Lax" cookie_name
        cookie_token;
      Output.header conf "Location: %s" base_url;
      Output.flush conf

let handle_oidc_logout conf base_env _from_addr base_file =
  Log.info (fun k -> k "logout: base=%s" base_file);
  let cookie_name = "gw_oidc_" ^ base_file in
  let id_token_opt =
    match extract_oidc_cookie conf.request cookie_name with
    | Some cookie_token -> remove_oidc_session cookie_token base_file
    | None -> None
  in
  let base_url =
    if !Server.cgi then conf.command ^ "?b=" ^ base_file else base_file
  in

  let clear_cookie conf =
    Output.header conf
      "Set-Cookie: %s=; Path=/; HttpOnly; Secure; SameSite=Lax; Max-Age=0"
      cookie_name
  in
  match read_oidc_config base_env with
  | None ->
      Output.status conf Code.Moved_Temporarily;
      clear_cookie conf;
      Output.header conf "Location: %s" base_url;
      Output.flush conf
  | Some cfg ->
      let post_logout_uri = cfg.redirect_uri in
      let logout_target =
        match Geneweb_oidc.Oidc.discover cfg.provider_url with
        | Error _ -> base_url
        | Ok provider -> (
            match id_token_opt with
            | None -> base_url
            | Some id_token -> (
                match
                  Geneweb_oidc.Oidc.logout_url provider ~id_token_hint:id_token
                    ~post_logout_redirect_uri:post_logout_uri
                with
                | None -> base_url
                | Some url -> url))
      in
      Output.status conf Code.Moved_Temporarily;
      clear_cookie conf;
      Output.header conf "Location: %s" logout_target;
      Output.flush conf

let handle_mode conf base_env from_addr base_file utm mode =
  match mode with
  | Some "OIDC_LOGIN" ->
      handle_oidc_login conf base_env base_file;
      true
  | Some "OIDC_CALLBACK" ->
      handle_oidc_callback conf base_env from_addr base_file utm;
      true
  | Some "OIDC_LOGOUT" ->
      handle_oidc_logout conf base_env from_addr base_file;
      true
  | None ->
      (* auto-detect a callback only when no explicit mode is set *)
      let has_code = Util.p_getenv conf.env "code" <> None in
      let has_state = Util.p_getenv conf.env "state" <> None in
      let has_error = Util.p_getenv conf.env "error" <> None in
      if
        ((has_code && has_state) || (has_error && has_state))
        && read_oidc_config base_env <> None
      then begin
        handle_oidc_callback conf base_env from_addr base_file utm;
        true
      end
      else false
  | Some _ -> false
