(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Config
module Server = Geneweb_http.Server
module Code = Geneweb_http.Code
open Cmd_legacy

let oidc_states_file () = !GWPARAM.adm_file "oidc_states"
let oidc_sessions_file () = !GWPARAM.adm_file "oidc_sessions"

let read_oidc_states () =
  let fname = oidc_states_file () in
  if not (Sys.file_exists fname) then []
  else
    try
      let ic = Secure.open_in fname in
      let now = Unix.time () in
      let rec loop acc =
        match input_line ic with
        | line -> (
            match String.split_on_char ' ' line with
            | [ state; nonce; base_name; ts_str ] ->
                let ts = float_of_string ts_str in
                if now -. ts < 300.0 then
                  loop ((state, (nonce, base_name, ts)) :: acc)
                else loop acc
            | _ -> loop acc)
        | exception End_of_file ->
            close_in ic;
            List.rev acc
      in
      loop []
    with Sys_error _ -> []

let write_oidc_states entries =
  let fname = oidc_states_file () in
  try
    let oc = Secure.open_out fname in
    List.iter
      (fun (state, (nonce, base_name, ts)) ->
        Printf.fprintf oc "%s %s %s %.0f\n" state nonce base_name ts)
      entries;
    close_out oc
  with Sys_error _ -> ()

let add_oidc_state state nonce base_name ts =
  let lock_file = !GWPARAM.adm_file "gwd.lck" in
  let on_exn _exn _bt = () in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  let entries = read_oidc_states () in
  let entries = (state, (nonce, base_name, ts)) :: entries in
  write_oidc_states entries

let take_oidc_state state =
  let lock_file = !GWPARAM.adm_file "gwd.lck" in
  let on_exn _exn _bt = None in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  let entries = read_oidc_states () in
  match List.assoc_opt state entries with
  | None -> None
  | Some v ->
      let entries = List.filter (fun (k, _) -> k <> state) entries in
      write_oidc_states entries;
      Some v

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
              when String.length acc_str = 1 ->
                let ts = float_of_string ts_str in
                let username = String.concat " " rest in
                if now -. ts < tmout then
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
                else loop acc
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
  let now = Unix.time () in
  let entries =
    List.map
      (fun ((cookie, _ts, from, base, acc, id_tok, user, username) as entry) ->
        if cookie = cookie_token && base = base_name then begin
          found := Some (acc, user, username);

          (cookie, now, from, base, acc, id_tok, user, username)
        end
        else entry)
      entries
  in
  if !found <> None then write_oidc_sessions entries;
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

let random_self_init () =
  let seed = int_of_float (mod_float (Unix.time ()) (float max_int)) in
  Random.init seed

let mk_oidc_cookie_token () =
  random_self_init ();
  let rec loop len =
    if len = 32 then Buff.get len
    else
      let v = Char.code 'a' + Random.int 26 in
      loop (Buff.store len (Char.chr v))
  in
  loop 0

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

let oidc_redirect conf url =
  Output.status conf Code.Moved_Temporarily;
  Output.header conf "Location: %s" url;
  Output.flush conf

let oidc_error_page conf msg =
  Output.status conf Code.Bad_Request;
  Output.header conf "Content-type: text/html; charset=utf-8";
  Output.print_sstring conf "<html><head><title>OIDC Error</title></head><body>";
  Output.print_sstring conf "<h1>Authentication Error</h1><p>";
  Output.print_sstring conf (Util.escape_html msg :> string);
  Output.print_sstring conf "</p><p><a href=\"";
  Output.print_sstring conf conf.command;
  Output.print_sstring conf "\">Back</a></p></body></html>";
  Output.flush conf

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
          add_oidc_state state nonce base_file (Unix.time ());
          let url =
            Geneweb_oidc.Oidc.authorization_url provider
              ~client_id:cfg.client_id ~redirect_uri:cfg.redirect_uri ~state
              ~nonce
          in
          oidc_redirect conf url)

let handle_oidc_callback conf base_env from_addr base_file _utm =
  let code = Util.p_getenv conf.env "code" in
  let state = Util.p_getenv conf.env "state" in
  let error = Util.p_getenv conf.env "error" in
  match error with
  | Some err ->
      let desc =
        match Util.p_getenv conf.env "error_description" with
        | Some d -> err ^ ": " ^ d
        | None -> err
      in
      oidc_error_page conf ("IdP returned error: " ^ desc)
  | None -> (
      match (code, state) with
      | None, _ -> oidc_error_page conf "Missing 'code' parameter in callback"
      | _, None -> oidc_error_page conf "Missing 'state' parameter in callback"
      | Some code, Some state -> (
          match take_oidc_state state with
          | None -> oidc_error_page conf "Invalid or expired state parameter"
          | Some (nonce, expected_base, _ts) -> (
              if expected_base <> base_file then
                oidc_error_page conf "State does not match this base"
              else
                match read_oidc_config base_env with
                | None -> oidc_error_page conf "OIDC not configured"
                | Some cfg -> (
                    match Geneweb_oidc.Oidc.discover cfg.provider_url with
                    | Error e ->
                        oidc_error_page conf
                          (Format.asprintf "%a" Geneweb_oidc.Oidc.pp_error e)
                    | Ok provider -> (
                        match
                          Geneweb_oidc.Oidc.exchange_code provider
                            ~client_id:cfg.client_id
                            ~client_secret:cfg.client_secret
                            ~redirect_uri:cfg.redirect_uri ~code
                        with
                        | Error e ->
                            oidc_error_page conf
                              (Format.asprintf "%a" Geneweb_oidc.Oidc.pp_error e)
                        | Ok token_resp -> (
                            match
                              Geneweb_oidc.Oidc.verify_and_decode_id_token
                                ~jwks_uri:provider.jwks_uri
                                ~client_id:cfg.client_id ~issuer:provider.issuer
                                ~nonce token_resp.id_token
                            with
                            | Error e ->
                                oidc_error_page conf
                                  (Format.asprintf "%a"
                                     Geneweb_oidc.Oidc.pp_error e)
                            | Ok claims ->
                                let claim_value =
                                  match
                                    Geneweb_oidc.Oidc.claim_string claims
                                      cfg.user_claim
                                  with
                                  | Some v -> v
                                  | None -> (
                                      match
                                        Geneweb_oidc.Oidc.claim_string claims
                                          "sub"
                                      with
                                      | Some v -> v
                                      | None -> "")
                                in
                                if claim_value = "" then
                                  oidc_error_page conf
                                    ("No '" ^ cfg.user_claim
                                   ^ "' claim found in id_token")
                                else
                                  let has_role role =
                                    role <> "" && cfg.role_claim <> ""
                                    && Geneweb_oidc.Oidc.claim_has_value claims
                                         ~path:cfg.role_claim ~value:role
                                  in
                                  let acc =
                                    if has_role cfg.wizard_role then 'w'
                                    else if has_role cfg.friend_role then 'f'
                                    else 'v'
                                  in

                                  let person_key =
                                    if cfg.person_key_claim = "" then ""
                                    else
                                      match
                                        Geneweb_oidc.Oidc.claim_string claims
                                          cfg.person_key_claim
                                      with
                                      | Some v -> v
                                      | None -> ""
                                  in
                                  let display_name =
                                    match
                                      Geneweb_oidc.Oidc.claim_string claims
                                        "name"
                                    with
                                    | Some v when v <> "" -> v
                                    | _ -> claim_value
                                  in
                                  if acc = 'v' then begin
                                    let url =
                                      if !Server.cgi then
                                        conf.command ^ "?b=" ^ base_file
                                      else base_file
                                    in
                                    oidc_redirect conf url
                                  end
                                  else begin
                                    let username =
                                      if person_key <> "" then
                                        display_name ^ "|" ^ person_key
                                      else display_name
                                    in
                                    let cookie_token =
                                      mk_oidc_cookie_token ()
                                    in
                                    add_oidc_session cookie_token from_addr
                                      base_file acc token_resp.id_token
                                      claim_value username;

                                    let cookie_name = "gw_oidc_" ^ base_file in
                                    let url =
                                      if !Server.cgi then
                                        conf.command ^ "?b=" ^ base_file
                                      else base_file
                                    in
                                    Output.status conf Code.Moved_Temporarily;
                                    Output.header conf
                                      "Set-Cookie: %s=%s; Path=/; HttpOnly; \
                                       SameSite=Lax"
                                      cookie_name cookie_token;
                                    Output.header conf "Location: %s" url;
                                    Output.flush conf
                                  end))))))

let handle_oidc_logout conf base_env _from_addr base_file =
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
      "Set-Cookie: %s=; Path=/; HttpOnly; SameSite=Lax; Max-Age=0" cookie_name
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
  | _ ->
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
