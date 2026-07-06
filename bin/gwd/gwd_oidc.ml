open Geneweb
open Config
module Server = Geneweb_http.Server
module Code = Geneweb_http.Code

let src = Logs.Src.create ~doc:"OIDC" "OIDC"

module Log = (val Logs.src_log src : Logs.LOG)

let oidc_sessions_file () = !GWPARAM.adm_file "oidc_sessions"
let oidc_lock_file () = !GWPARAM.adm_file "oidc.lck"

(* login state is bound to the browser via a signed cookie, not a server store *)

let session_cookie_name base_file = "__Host-gw_oidc_" ^ base_file
let login_cookie_name base_file = "__Host-gw_oidc_login_" ^ base_file

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
      let tmout = float_of_int !Cmd_legacy.login_timeout in
      let now = Unix.time () in
      let rec loop acc =
        match input_line ic with
        | line -> (
            match String.split_on_char ' ' line with
            | [ cookie; ts_str; base; acc_str; id_tok; user; username ]
              when String.length acc_str = 1 -> (
                match float_of_string_opt ts_str with
                | Some ts when now -. ts < tmout ->
                    loop
                      (( cookie,
                         ts,
                         base,
                         acc_str.[0],
                         id_tok,
                         Uri.pct_decode user,
                         Uri.pct_decode username )
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
  (* write to a temp file then rename, so lockless readers never see a partial
     file (0600: the store holds bearer id_tokens) *)
  let tmp = fname ^ ".tmp" in
  try
    let oc =
      Secure.open_out_gen [ Open_wronly; Open_creat; Open_trunc ] 0o600 tmp
    in
    List.iter
      (fun (cookie, ts, base, acc, id_tok, user, username) ->
        Printf.fprintf oc "%s %.0f %s %c %s %s %s\n" cookie ts base acc id_tok
          (Uri.pct_encode user) (Uri.pct_encode username))
      entries;
    close_out oc;
    Sys.rename tmp fname
  with Sys_error _ -> ()

let add_oidc_session cookie_token base_name access_char id_token user username =
  let lock_file = oidc_lock_file () in
  let on_exn _exn _bt = () in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  let entries = read_oidc_sessions () in
  let now = Unix.time () in
  let entries =
    (cookie_token, now, base_name, access_char, id_token, user, username)
    :: entries
  in
  write_oidc_sessions entries

let lookup_oidc_session cookie_token base_name =
  let now = Unix.time () in
  let matches (cookie, _, base, _, _, _, _) =
    cookie = cookie_token && base = base_name
  in
  (* read without the lock; writes are atomic so the file is always consistent *)
  match List.find_opt matches (read_oidc_sessions ()) with
  | None -> None
  | Some (_, ts, _, acc, _, user, username) ->
      (* slide the expiry at most once a minute, under the lock, best effort *)
      if now -. ts > 60. then begin
        let lock_file = oidc_lock_file () in
        Lock.control ~on_exn:(fun _ _ -> ()) ~wait:true ~lock_file @@ fun () ->
        read_oidc_sessions ()
        |> List.map
             (fun ((cookie, _, base, acc, id_tok, user, username) as e) ->
               if cookie = cookie_token && base = base_name then
                 (cookie, now, base, acc, id_tok, user, username)
               else e)
        |> write_oidc_sessions
      end;
      Some (acc, user, username)

let remove_oidc_session cookie_token base_name =
  let lock_file = oidc_lock_file () in
  let on_exn _exn _bt = None in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  let entries = read_oidc_sessions () in
  let id_token_ref = ref None in
  let entries =
    List.filter
      (fun (cookie, _ts, base, _acc, id_tok, _user, _username) ->
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
  match extract_oidc_cookie request (session_cookie_name base_name) with
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

let curl_available () =
  match Sys.getenv_opt "PATH" with
  | None -> false
  | Some path ->
      let sep = if Sys.win32 then ';' else ':' in
      let exe = if Sys.win32 then "curl.exe" else "curl" in
      String.split_on_char sep path
      |> List.exists (fun dir ->
          dir <> "" && Sys.file_exists (Filename.concat dir exe))

let read_oidc_config base_env =
  match List.assoc_opt "oidc_provider_url" base_env with
  | None | Some "" -> None
  | Some provider_url ->
      let get key default =
        try List.assoc key base_env with Not_found -> default
      in
      let client_id = get "oidc_client_id" "" in
      let client_secret =
        match List.assoc_opt "oidc_client_secret_file" base_env with
        | Some file when file <> "" -> (
            try
              let ic = open_in file in
              let s = String.trim (input_line ic) in
              close_in ic;
              s
            with Sys_error _ | End_of_file ->
              Log.warn (fun k ->
                  k "OIDC: cannot read oidc_client_secret_file %s" file);
              get "oidc_client_secret" "")
        | _ -> get "oidc_client_secret" ""
      in
      let redirect_uri = get "oidc_redirect_uri" "" in
      if client_id = "" || client_secret = "" || redirect_uri = "" then None
      else begin
        if not (curl_available ()) then
          Log.err (fun k ->
              k
                "OIDC is configured for this base but the curl binary was not \
                 found in PATH");
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
      end

let oidc_error_page conf msg =
  Log.warn (fun k -> k "authentication failed: %s" msg);
  let title = Util.transl conf "authentication error" |> Utf8.capitalize_fst in
  let body =
    Util.transl conf "authentication failed, retry or contact administrator"
    |> Utf8.capitalize_fst
  in
  let back = Util.transl conf "back" |> Utf8.capitalize_fst in
  Output.status conf Code.Bad_Request;
  Output.header conf "Content-type: text/html; charset=utf-8";
  Output.print_sstring conf "<html><head><title>";
  Output.print_sstring conf title;
  Output.print_sstring conf "</title></head><body><h1>";
  Output.print_sstring conf title;
  Output.print_sstring conf "</h1><p>";
  Output.print_sstring conf body;
  Output.print_sstring conf "</p><p><a href=\"";
  Output.print_sstring conf (Util.escape_html conf.command :> string);
  Output.print_sstring conf "\">";
  Output.print_sstring conf back;
  Output.print_sstring conf "</a></p></body></html>";
  Output.flush conf

let conf_secret conf = match conf.secret_salt with Some s -> s | None -> ""

let set_cookie conf ~name ~value ~max_age =
  let max_age =
    match max_age with Some n -> Printf.sprintf "; Max-Age=%d" n | None -> ""
  in
  Output.header conf
    "Set-Cookie: %s=%s; Path=/; HttpOnly; Secure; SameSite=Lax%s" name value
    max_age

let set_login_cookie conf base_file value =
  set_cookie conf ~name:(login_cookie_name base_file) ~value ~max_age:(Some 600)

let clear_login_cookie conf base_file =
  set_cookie conf
    ~name:(login_cookie_name base_file)
    ~value:"" ~max_age:(Some 0)

let handle_oidc_login conf base_env base_file =
  match (conf_secret conf, read_oidc_config base_env) with
  | "", _ -> oidc_error_page conf "OIDC unavailable: no secret salt configured"
  | _, None -> oidc_error_page conf "OIDC not configured for this base"
  | _, Some cfg -> (
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

let handle_oidc_callback conf base_env from_addr base_file =
  let ( let* ) = Result.bind in
  let err_str e = Format.asprintf "%a" Geneweb_oidc.Oidc.pp_error e in
  let result =
    let* () =
      if conf_secret conf = "" then
        Error "OIDC unavailable: no secret salt configured"
      else Ok ()
    in
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
        (Geneweb_oidc.Oidc.decode_and_validate_id_token ~client_id:cfg.client_id
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
      add_oidc_session cookie_token base_file acc id_token claim_value username;
      Output.status conf Code.Moved_Temporarily;
      clear_login_cookie conf base_file;
      set_cookie conf
        ~name:(session_cookie_name base_file)
        ~value:cookie_token ~max_age:None;
      Output.header conf "Location: %s" base_url;
      Output.flush conf

let request_is_post request = Mutil.extract_param "GET " ' ' request = ""

let handle_oidc_logout conf base_env _from_addr base_file =
  let base_url =
    if !Server.cgi then conf.command ^ "?b=" ^ base_file else base_file
  in
  (* logout must be POST so a cross-site GET cannot trigger it (CSRF) *)
  if not (request_is_post conf.request) then begin
    Output.status conf Code.Moved_Temporarily;
    Output.header conf "Location: %s" base_url;
    Output.flush conf
  end
  else begin
    Log.info (fun k -> k "logout: base=%s" base_file);
    let id_token_opt =
      match
        extract_oidc_cookie conf.request (session_cookie_name base_file)
      with
      | Some cookie_token -> remove_oidc_session cookie_token base_file
      | None -> None
    in
    let clear_cookie conf =
      set_cookie conf
        ~name:(session_cookie_name base_file)
        ~value:"" ~max_age:(Some 0)
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
                    Geneweb_oidc.Oidc.logout_url provider
                      ~id_token_hint:id_token
                      ~post_logout_redirect_uri:post_logout_uri
                  with
                  | None -> base_url
                  | Some url -> url))
        in
        Output.status conf Code.Moved_Temporarily;
        clear_cookie conf;
        Output.header conf "Location: %s" logout_target;
        Output.flush conf
  end

let handle_mode conf mode =
  let base_env = conf.base_env
  and from_addr = conf.from
  and base_file = conf.bname in
  match mode with
  | Some "OIDC_LOGIN" ->
      handle_oidc_login conf base_env base_file;
      true
  | Some "OIDC_CALLBACK" ->
      handle_oidc_callback conf base_env from_addr base_file;
      true
  | Some "OIDC_LOGOUT" ->
      handle_oidc_logout conf base_env from_addr base_file;
      true
  | None ->
      (* only treat code+state as a callback for a login this browser started *)
      let has_state = Util.p_getenv conf.env "state" <> None in
      let has_code = Util.p_getenv conf.env "code" <> None in
      let has_error = Util.p_getenv conf.env "error" <> None in
      let in_login =
        extract_oidc_cookie conf.request (login_cookie_name base_file) <> None
      in
      if
        has_state && (has_code || has_error) && in_login
        && Option.is_some (read_oidc_config base_env)
      then begin
        handle_oidc_callback conf base_env from_addr base_file;
        true
      end
      else false
  | Some _ -> false
