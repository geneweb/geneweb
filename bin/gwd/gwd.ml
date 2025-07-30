(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Config
open Def
open Util
open Gwd_lib
module Logs = Geneweb_logs.Logs
module StrSet = Mutil.StrSet
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let output_conf =
  {
    status = Wserver.http;
    header = Wserver.header;
    body = Wserver.print_string;
    flush = Wserver.wflush;
  }

let printer_conf = { Config.empty with output_conf }
let auth_file = ref ""
let cache_langs = ref []
let cache_databases = ref []
let choose_browser_lang = ref false
let conn_timeout = ref 120
let daemon = ref false
let default_lang = ref "fr"
let friend_passwd = ref ""
let green_color = "#2f6400"
let images_dir = ref ""
let gw_prefix = ref ""
let images_prefix = ref ""
let etc_prefix = ref ""
let lexicon_list = ref [ Filename.concat "lang" "lexicon.txt" ]
let login_timeout = ref 1800
let default_n_workers = 20
let n_workers = ref default_n_workers
let default_max_pending_requests = 150
let max_pending_requests = ref default_max_pending_requests
let no_host_address = ref false
let only_addresses = ref []
let plugins = ref []
let forced_plugins = ref []
let unsafe_plugins = ref []
let redirected_addr = ref None
let robot_xcl = ref None
let selected_addr = ref None
let selected_port = ref 2317
let setup_link = ref false
let trace_failed_passwd = ref false
let debug = ref false
let use_auth_digest_scheme = ref false
let wizard_just_friend = ref false
let wizard_passwd = ref ""
let predictable_mode = ref false

let is_multipart_form =
  let s = "multipart/form-data" in
  fun content_type ->
    let rec loop i =
      if i >= String.length content_type then false
      else if i >= String.length s then true
      else if content_type.[i] = Char.lowercase_ascii s.[i] then loop (i + 1)
      else false
    in
    loop 0

let extract_boundary content_type =
  List.assoc "boundary" (Util.create_env content_type)

let deprecated_warning_max_clients () =
  Logs.warn (fun k ->
      k
        "The `-max_clients` option is deprecated and may be removed in a \
         future release.@ It has no effect.@ Use `-n_workers` and\n\
        \    `-max_pending_requests` instead.@.")

let deprecated_warning_no_fork () =
  Logs.warn (fun k ->
      k
        "The `-no-fork` option is deprecated and may be removed in a future \
         release.@ To achieve the same behavior, use `-n_workers 0` instead.@.")

type auth_report = {
  ar_ok : bool;
  ar_command : string;
  ar_passwd : string;
  ar_scheme : auth_scheme_kind;
  ar_user : string;
  ar_name : string;
  ar_wizard : bool;
  ar_friend : bool;
  ar_uauth : string;
  ar_can_stale : bool;
}

let split_username username =
  let l1 = String.split_on_char '|' username in
  match List.length l1 with
  | 1 -> (username, "")
  | 2 -> (List.nth l1 0, List.nth l1 1)
  | _ ->
      Logs.syslog `LOG_CRIT "Bad .auth key or sosa encoding";
      (username, "")

let log_passwd_failed ar tm from request base_file =
  let referer = Mutil.extract_param "referer: " '\n' request in
  let user_agent = Mutil.extract_param "user-agent: " '\n' request in
  let tm = Unix.localtime tm in
  Logs.info (fun k ->
      k "%s (%d) %s_%s => failed (%s)"
        (Mutil.sprintf_date tm :> string)
        (Unix.getpid ()) base_file ar.ar_passwd ar.ar_user);
  if !trace_failed_passwd then
    Logs.info (fun k -> k " (%s)" (String.escaped ar.ar_uauth));
  Logs.info (fun k -> k "\n  From: %s\n  Agent: %s" from user_agent);
  if referer <> "" then Logs.info (fun k -> k "  Referer: %s" referer)

let copy_file conf fname =
  match Util.open_etc_file conf fname with
  | Some (ic, _fname) ->
      (try
         while true do
           let c = input_char ic in
           Output.printf conf "%c" c
         done
       with _ -> ());
      close_in ic;
      true
  | None -> false

let http conf status =
  Output.status conf status;
  Output.header conf "Content-type: text/html; charset=iso-8859-1"

let robots_txt conf =
  Logs.syslog `LOG_NOTICE "Robot request";
  Output.status conf Def.OK;
  Output.header conf "Content-type: text/plain";
  if copy_file conf "robots" then ()
  else (
    Output.print_sstring conf "User-Agent: *\n";
    Output.print_sstring conf "Disallow: /\n")

let refuse_log conf from =
  Logs.syslog `LOG_NOTICE @@ "Excluded: " ^ from;
  http conf Def.Forbidden;
  Output.header conf "Content-type: text/html";
  Output.print_sstring conf
    "Your access has been disconnected by administrator.\n";
  let _ = (copy_file conf "refuse" : bool) in
  ()

let only_log conf from =
  Logs.syslog `LOG_NOTICE @@ "Connection refused from " ^ from;
  http conf Def.OK;
  Output.header conf "Content-type: text/html; charset=iso-8859-1";
  Output.print_sstring conf "<head><title>Invalid access</title></head>\n";
  Output.print_sstring conf "<body><h1>Invalid access</h1></body>\n"

let refuse_auth conf from auth auth_type =
  Logs.syslog `LOG_NOTICE
  @@ Printf.sprintf
       "Access failed --- From: %s --- Basic realm: %s --- Response: %s" from
       auth_type auth;
  Util.unauthorized conf auth_type

let index_from s o c =
  match String.index_from_opt s o c with Some i -> i | None -> String.length s

let index s c = index_from s 0 c

let rec extract_assoc key = function
  | [] -> ("", [])
  | ((k, v) as kv) :: kvl ->
      if k = key then (Mutil.decode v, kvl)
      else
        let v, kvl = extract_assoc key kvl in
        (v, kv :: kvl)

let tmp = Filename.get_temp_dir_name ()
let lexicon_fname = ref (Filename.concat tmp "lexicon.bin.")

(* NB: Lexicon will be impacted by plugins even if the base
   does not activate this plugin in .gwf file. *)
let load_lexicon =
  let lexicon_cache = Hashtbl.create 0 in
  fun lang ->
    let fname = !lexicon_fname ^ lang in
    match Hashtbl.find_opt lexicon_cache fname with
    | Some lex -> lex
    | None ->
        let lex =
          Mutil.read_or_create_value ~wait:true ~magic:Mutil.random_magic fname
            (fun () ->
              let ht = Hashtbl.create 0 in
              let rec rev_iter fn = function
                | [] -> ()
                | hd :: tl ->
                    rev_iter fn tl;
                    fn hd
              in
              rev_iter
                (fun fname ->
                  let fname = Util.search_in_assets fname in
                  if Sys.file_exists fname then
                    Mutil.input_lexicon lang ht (fun () -> Secure.open_in fname)
                  else
                    Logs.syslog `LOG_WARNING
                      (Format.sprintf "File %s unavailable\n" fname))
                !lexicon_list;
              ht)
        in
        Hashtbl.add lexicon_cache fname lex;
        lex

let cache_lexicon () =
  List.iter (fun x -> ignore @@ load_lexicon x) !cache_langs

exception
  Register_plugin_failure of
    string * [ `dynlink_error of Dynlink.error | `string of string ]

let register_plugin dir =
  if !debug then print_endline (__LOC__ ^ ": " ^ dir);
  if not (List.mem dir !unsafe_plugins || GwdPluginMD5.allowed dir) then
    failwith dir;
  let pname = Filename.basename dir in
  let plugin = Filename.concat dir @@ "plugin_" ^ pname ^ ".cmxs" in
  lexicon_fname := !lexicon_fname ^ pname ^ ".";
  let lex_dir = Filename.concat (Filename.concat dir "assets") "lex" in
  if Sys.file_exists lex_dir then (
    let lex = Sys.readdir lex_dir in
    Array.sort compare lex;
    Array.iter
      (fun f ->
        let f = Filename.concat lex_dir f in
        if not (Sys.is_directory f) then lexicon_list := f :: !lexicon_list)
      lex);
  let assets = Filename.concat dir "assets" in
  GwdPlugin.assets := assets;
  (try Dynlink.loadfile plugin
   with Dynlink.Error e ->
     raise (Register_plugin_failure (plugin, `dynlink_error e)));
  GwdPlugin.assets := ""

let alias_lang lang =
  if String.length lang < 2 then lang
  else
    let fname = Util.search_in_assets (Filename.concat "lang" "alias_lg.txt") in
    try
      let ic = Secure.open_in fname in
      let lang =
        let rec loop () =
          match input_line ic with
          | line -> (
              match String.index_opt line '=' with
              | Some i ->
                  if lang = String.sub line 0 i then
                    String.sub line (i + 1) (String.length line - i - 1)
                  else loop ()
              | None -> loop ())
          | exception End_of_file -> lang
        in
        loop ()
      in
      close_in ic;
      lang
    with Sys_error _ -> lang

let print_renamed conf new_n =
  let link =
    let req = Util.get_request_string conf in
    let new_req =
      let len = String.length conf.bname in
      let rec loop i =
        if i > String.length req then ""
        else if i >= len && String.sub req (i - len) len = conf.bname then
          String.sub req 0 (i - len)
          ^ new_n
          ^ String.sub req i (String.length req - i)
        else loop (i + 1)
      in
      loop 0
    in
    "http://" ^ Util.get_server_string conf ^ new_req
  in
  let env =
    Templ.Env.(
      empty
      |> add "old" (Templ.Vstring (Mutil.encode conf.bname))
      |> add "new" (Templ.Vstring (Mutil.encode new_n))
      |> add "link" (Templ.Vstring (Mutil.encode link)))
  in
  try Templ.output_simple conf env "renamed"
  with _ ->
    let title _ = Output.printf conf "%s -&gt; %s" conf.bname new_n in
    Hutil.header conf title;
    Output.printf conf "<ul><li><a href=\"%s\">%s</a></li></ul>" link link;
    Hutil.trailer conf

let log_redirect from request req =
  let lock_file = !GWPARAM.adm_file "gwd.lck" in
  let on_exn exn bt =
    Logs.syslog `LOG_NOTICE @@ Format.asprintf "%a\n" Lock.pp_exception (exn, bt)
  in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  let referer = Mutil.extract_param "referer: " '\n' request in
  Logs.syslog `LOG_NOTICE
  @@ Printf.sprintf "%s --- From: %s --- Referer: %s" req from referer

let print_redirected conf from request new_addr =
  let req = Util.get_request_string conf in
  let link = "http://" ^ new_addr ^ req in
  let env = Templ.Env.(add "link" (Templ.Vstring (Mutil.encode link)) empty) in
  log_redirect from request req;
  try Templ.output_simple conf env "redirect"
  with _ ->
    let title _ = Output.print_sstring conf "Address changed" in
    Hutil.header conf title;
    Output.print_sstring conf "Use the following address:\n<p>\n";
    Output.printf conf "<ul><li><a href=\"%s\">%s</a></li></ul>" link link;
    Hutil.trailer conf

let nonce_private_key =
  Lazy.from_fun (fun () ->
      let fname = Filename.concat !GWPARAM.cnt_dir "gwd_private.txt" in
      let k =
        try
          let ic = open_in fname in
          let s =
            let rec loop () =
              match input_line ic with
              | s when s = "" || s.[0] = '#' -> loop ()
              | s -> s
              | exception End_of_file -> ""
            in
            loop ()
          in
          close_in ic;
          s
        with Sys_error _ -> ""
      in
      if k = "" then (
        Random.self_init ();
        let k = Random.bits () in
        let oc = open_out fname in
        Printf.fprintf oc
          "# Gwd key for better password protection in communication.\n\
           # Changing it makes all users receive their login window again.\n\
           # Generated by program but can be modified by hand to any value.\n";
        Printf.fprintf oc "\n%d\n" k;
        close_out oc;
        string_of_int k)
      else k)

let digest_nonce _ = Lazy.force nonce_private_key

let trace_auth base_env f =
  if List.mem_assoc "trace_auth" base_env then (
    let oc =
      open_out_gen
        [ Open_wronly; Open_append; Open_creat ]
        0o777 "trace_auth.txt"
    in
    f oc;
    close_out oc)

let unauth_server conf ar =
  let typ = if ar.ar_passwd = "w" then "Wizard" else "Friend" in
  Output.status conf Def.Unauthorized;
  if !use_auth_digest_scheme then
    let nonce = digest_nonce conf.ctime in
    let _ =
      let tm = Unix.localtime (Unix.time ()) in
      trace_auth conf.base_env (fun oc ->
          Printf.fprintf oc
            "\n\
             401 unauthorized\n\
             - date: %s\n\
             - request:\n\
             %t- passwd: %s\n\
             - nonce: \"%s\"\n\
             - can_stale: %b\n"
            (Mutil.sprintf_date tm :> string)
            (fun oc ->
              List.iter (fun s -> Printf.fprintf oc "  * %s\n" s) conf.request)
            ar.ar_passwd nonce ar.ar_can_stale)
    in
    Output.header conf
      "WWW-Authenticate: Digest realm=\"%s %s\"%s%s,qop=\"auth\"" typ conf.bname
      (if nonce = "" then "" else Printf.sprintf ",nonce=\"%s\"" nonce)
      (if ar.ar_can_stale then ",stale=true" else "")
  else
    Output.header conf "WWW-Authenticate: Basic realm=\"%s %s\"" typ conf.bname;
  let env =
    List.fold_left
      (fun l (k, v) ->
        if k = "" || (k = "oc" && int_of_string (Mutil.decode v) = 0) then l
        else (k ^ "=" ^ Mutil.decode v) :: l)
      []
      (conf.henv @ conf.senv @ conf.env)
  in
  let env = String.concat "&" env in
  let txt i = transl_nth conf "wizard/wizards/friend/friends/exterior" i in
  let typ = txt (if ar.ar_passwd = "w" then 0 else 2) in
  let title h =
    Output.printf conf
      (fcapitale (ftransl conf "%s access cancelled for that page"))
      (if not h then "<em>" ^ typ ^ "</em>" else typ)
  in
  Hutil.header_without_http_nor_home conf title;
  Output.print_sstring conf "<h1>\n";
  title false;
  Output.print_sstring conf "</h1>\n";
  Output.print_sstring conf "<dl>\n";
  (let alt_bind, alt_access =
     if ar.ar_passwd = "w" then ("w=f", txt 2) else ("w=w", txt 0)
   in
   Output.print_sstring conf "<dd>\n";
   Output.print_sstring conf "<ul>\n";
   Output.print_sstring conf "<li>\n";
   Output.printf conf {|%s : <a href="%s?%s%s%s">%s</a>|} (transl conf "access")
     conf.bname env
     (if env = "" then "" else "&")
     alt_bind alt_access;
   Output.print_sstring conf "</li>\n";
   Output.print_sstring conf "<li>\n";
   Output.printf conf {|%s : <a href="%s?%s">%s</a>|} (transl conf "access")
     conf.bname env (txt 4);
   Output.print_sstring conf "</li>\n";
   Output.print_sstring conf "</ul>\n";
   Output.print_sstring conf "</dd>\n");
  Output.print_sstring conf "</dl>\n";
  Hutil.trailer conf

let gen_match_auth_file test_user_and_password auth_file base_file =
  if auth_file = "" then None
  else
    let aul = read_gen_auth_file auth_file base_file in
    let rec loop = function
      | au :: aul ->
          if test_user_and_password au then
            let s =
              try
                let i = String.index au.au_info ':' in
                String.sub au.au_info 0 i
              with Not_found -> au.au_info
            in
            let username =
              (* clean the / needed for sorting *)
              try
                let i = String.index s '/' in
                let len = String.length s in
                String.sub s 0 i ^ String.sub s (i + 1) (len - i - 1)
              with Not_found -> s
            in
            Some username
          else loop aul
      | [] -> None
    in
    loop aul

let basic_match_auth_file uauth base_file =
  gen_match_auth_file
    (fun au -> au.au_user ^ ":" ^ au.au_passwd = uauth)
    base_file

let digest_match_auth_file asch base_file =
  gen_match_auth_file
    (fun au -> is_that_user_and_password asch au.au_user au.au_passwd)
    base_file

let match_simple_passwd sauth uauth =
  match String.index_opt sauth ':' with
  | Some _ -> sauth = uauth
  | None -> (
      match String.index_opt uauth ':' with
      | Some i -> sauth = String.sub uauth (i + 1) (String.length uauth - i - 1)
      | None -> sauth = uauth)

let basic_match_auth passwd auth_file uauth base_file =
  if passwd <> "" && match_simple_passwd passwd uauth then Some ""
  else basic_match_auth_file uauth auth_file base_file

type access_type =
  | ATwizard of string * string
  | ATfriend of string * string
  | ATnormal
  | ATnone
  | ATset

let compatible_tokens check_from (addr1, base1_pw1) (addr2, base2_pw2) =
  ((not check_from) || addr1 = addr2) && base1_pw1 = base2_pw2

let get_actlog check_from utm from_addr base_password =
  let fname = !GWPARAM.adm_file "actlog" in
  (if not (Sys.file_exists fname) then
     let oc = Secure.open_out fname in
     close_out oc);
  try
    let ic = Secure.open_in fname in
    let tmout = float_of_int !login_timeout in
    let rec loop changed r list =
      match input_line ic with
      | line ->
          let i = index line ' ' in
          let tm = float_of_string (String.sub line 0 i) in
          let islash = index_from line (i + 1) '/' in
          let ispace = index_from line (islash + 1) ' ' in
          let addr = String.sub line (i + 1) (islash - i - 1) in
          let db_pwd = String.sub line (islash + 1) (ispace - islash - 1) in
          let c = line.[ispace + 1] in
          let user =
            let k = ispace + 3 in
            if k >= String.length line then ""
            else String.sub line k (String.length line - k)
          in
          let len = String.length user in
          let user, username =
            match String.index_opt user ' ' with
            | Some i ->
                (String.sub user 0 i, String.sub user (i + 1) (len - i - 1))
            | None -> (user, "")
          in
          let list, r, changed =
            if utm -. tm >= tmout then (list, r, true)
            else if
              compatible_tokens check_from (addr, db_pwd)
                (from_addr, base_password)
            then
              let r =
                if c = 'w' then ATwizard (user, username)
                else ATfriend (user, username)
              in
              (((from_addr, db_pwd), (utm, c, user, username)) :: list, r, true)
            else (((addr, db_pwd), (tm, c, user, username)) :: list, r, changed)
          in
          loop changed r list
      | exception End_of_file ->
          close_in ic;
          let list =
            List.sort
              (fun (_, (t1, _, _, _)) (_, (t2, _, _, _)) -> compare t2 t1)
              list
          in
          (list, r, changed)
    in
    loop false ATnormal []
  with Sys_error e ->
    Logs.syslog `LOG_WARNING ("Error opening (get) actlog: " ^ e);
    ([], ATnormal, false)

let set_actlog list =
  let fname = !GWPARAM.adm_file "actlog" in
  try
    let oc = Secure.open_out fname in
    List.iter
      (fun ((from, base_pw), (a, c, d, e)) ->
        Printf.fprintf oc "%.0f %s/%s %c%s%s\n" a from base_pw c
          (if d = "" then "" else " " ^ d)
          (if e = "" then "" else " " ^ e))
      list;
    close_out oc
  with Sys_error e ->
    Logs.syslog `LOG_WARNING ("Error opening actlog: " ^ e);
    ()

let get_token check_from utm from_addr base_password =
  let lock_file = !GWPARAM.adm_file "gwd.lck" in
  (* FIXME: we silently ignore errors if we cannot lock the database. *)
  let on_exn _exn _bt = ATnormal in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  let list, r, changed = get_actlog check_from utm from_addr base_password in
  if changed then set_actlog list;
  r

let mkpasswd () =
  let rec loop len =
    if len = 9 then Buff.get len
    else
      let v = Char.code 'a' + Random.int 26 in
      loop (Buff.store len (Char.chr v))
  in
  loop 0

let random_self_init () =
  let seed = int_of_float (mod_float (Unix.time ()) (float max_int)) in
  Random.init seed

let set_token utm from_addr base_file acc user username =
  let lock_file = !GWPARAM.adm_file "gwd.lck" in
  (* FIXME: we silently ignore errors if we cannot lock the database. *)
  let on_exn _exn _bt = "" in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  random_self_init ();
  let list, _, _ = get_actlog false utm "" "" in
  let x, xx =
    let base = base_file ^ "_" in
    let rec loop ntimes =
      if ntimes = 0 then failwith "set_token"
      else
        let x = mkpasswd () in
        let xx = base ^ x in
        if
          List.exists
            (fun (tok, _) -> compatible_tokens false tok (from_addr, xx))
            list
        then loop (ntimes - 1)
        else (x, xx)
    in
    loop 50
  in
  let list = ((from_addr, xx), (utm, acc, user, username)) :: list in
  set_actlog list;
  x

let index_not_name s =
  let rec loop i =
    if i = String.length s then i
    else
      match s.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> loop (i + 1)
      | _ -> i
  in
  loop 0

let refresh_url conf bname =
  let url =
    let serv = "http://" ^ Util.get_server_string conf in
    let req =
      if conf.cgi then
        let str = Util.get_request_string conf in
        let scriptname = String.sub str 0 (String.index str '?') in
        scriptname ^ "?b=" ^ bname
      else "/" ^ bname ^ "?"
    in
    serv ^ req
  in
  http conf Def.OK;
  Output.header conf "Content-type: text/html";
  Output.printf conf
    "<head>\n\
     <meta http-equiv=\"REFRESH\"\n\
     content=\"1;URL=%s\">\n\
     </head>\n\
     <body>\n\
     <a href=\"%s\">%s</a>\n\
     </body>"
    url url url;
  raise Exit

let http_preferred_language request =
  let v = Mutil.extract_param "accept-language: " '\n' request in
  if v = "" then ""
  else
    let s = String.lowercase_ascii v in
    let list =
      let rec loop list i len =
        if i = String.length s then List.rev (Buff.get len :: list)
        else if s.[i] = ',' then loop (Buff.get len :: list) (i + 1) 0
        else loop list (i + 1) (Buff.store len s.[i])
      in
      loop [] 0 0
    in
    let list = List.map String.trim list in
    let rec loop = function
      | lang :: list ->
          if List.mem lang Version.available_languages then lang
          else if String.length lang = 5 then
            let blang = String.sub lang 0 2 in
            if List.mem blang Version.available_languages then blang
            else loop list
          else loop list
      | [] -> ""
    in
    loop list

let allowed_denied_titles key extra_line env base_env () =
  if p_getenv env "all_titles" = Some "on" then []
  else
    try
      let fname = List.assoc key base_env in
      if fname = "" then []
      else
        let ic = Secure.open_in (Filename.concat (Secure.base_dir ()) fname) in
        let rec loop set =
          let line, eof =
            try (input_line ic, false) with End_of_file -> ("", true)
          in
          let set =
            let line = if eof then extra_line |> Mutil.decode else line in
            if line = "" || line.[0] = ' ' || line.[0] = '#' then set
            else
              let line =
                match String.index_opt line '/' with
                | Some i ->
                    let len = String.length line in
                    let tit = String.sub line 0 i in
                    let pla = String.sub line (i + 1) (len - i - 1) in
                    (if tit = "*" then tit else Name.lower tit)
                    ^ "/"
                    ^ if pla = "*" then pla else Name.lower pla
                | None -> Name.lower line
              in
              StrSet.add line set
          in
          if eof then (
            close_in ic;
            StrSet.elements set)
          else loop set
        in
        loop StrSet.empty
    with Not_found | Sys_error _ -> []

let allowed_titles env =
  let extra_line =
    try List.assoc "extra_title" env with Not_found -> Adef.encoded ""
  in
  allowed_denied_titles "allowed_titles_file" extra_line env

let denied_titles = allowed_denied_titles "denied_titles_file" (Adef.encoded "")

let parse_digest s =
  let rec parse_main (strm__ : _ Stream.t) =
    match try Some (ident strm__) with Stream.Failure -> None with
    | Some s ->
        let _ =
          try spaces strm__ with Stream.Failure -> raise (Stream.Error "")
        in
        let kvl =
          try key_eq_val_list strm__
          with Stream.Failure -> raise (Stream.Error "")
        in
        if s = "Digest" then kvl else []
    | _ -> []
  and ident (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
    | Some (('A' .. 'Z' | 'a' .. 'z') as c) ->
        Stream.junk strm__;
        let len =
          try ident_kont (Buff.store 0 c) strm__
          with Stream.Failure -> raise (Stream.Error "")
        in
        Buff.get len
    | _ -> raise Stream.Failure
  and ident_kont len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
    | Some (('A' .. 'Z' | 'a' .. 'z') as c) ->
        Stream.junk strm__;
        ident_kont (Buff.store len c) strm__
    | _ -> len
  and spaces (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
    | Some ' ' -> (
        Stream.junk strm__;
        try spaces strm__ with Stream.Failure -> raise (Stream.Error ""))
    | _ -> ()
  and key_eq_val_list (strm__ : _ Stream.t) =
    match try Some (key_eq_val strm__) with Stream.Failure -> None with
    | Some kv ->
        let kvl =
          try key_eq_val_list_kont strm__
          with Stream.Failure -> raise (Stream.Error "")
        in
        kv :: kvl
    | _ -> []
  and key_eq_val_list_kont (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
    | Some ',' ->
        Stream.junk strm__;
        let _ =
          try spaces strm__ with Stream.Failure -> raise (Stream.Error "")
        in
        let kv =
          try key_eq_val strm__ with Stream.Failure -> raise (Stream.Error "")
        in
        let kvl =
          try key_eq_val_list_kont strm__
          with Stream.Failure -> raise (Stream.Error "")
        in
        kv :: kvl
    | _ -> []
  and key_eq_val (strm__ : _ Stream.t) =
    let k = ident strm__ in
    match Stream.peek strm__ with
    | Some '=' ->
        Stream.junk strm__;
        let v =
          try val_or_str strm__ with Stream.Failure -> raise (Stream.Error "")
        in
        (k, v)
    | _ -> raise (Stream.Error "")
  and val_or_str (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
    | Some '"' ->
        Stream.junk strm__;
        let v =
          try string 0 strm__ with Stream.Failure -> raise (Stream.Error "")
        in
        let _ =
          try spaces strm__ with Stream.Failure -> raise (Stream.Error "")
        in
        v
    | _ ->
        let v = any_val 0 strm__ in
        let _ =
          try spaces strm__ with Stream.Failure -> raise (Stream.Error "")
        in
        v
  and string len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
    | Some '"' ->
        Stream.junk strm__;
        Buff.get len
    | Some c ->
        Stream.junk strm__;
        string (Buff.store len c) strm__
    | _ -> raise Stream.Failure
  and any_val len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
    | Some (('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-') as c) ->
        Stream.junk strm__;
        any_val (Buff.store len c) strm__
    | _ -> Buff.get len
  in
  parse_main (Stream.of_string s)

let basic_authorization from_addr request base_env passwd access_type utm
    base_file command =
  let wizard_passwd =
    try List.assoc "wizard_passwd" base_env with Not_found -> !wizard_passwd
  in
  let wizard_passwd_file =
    try List.assoc "wizard_passwd_file" base_env with Not_found -> ""
  in
  let friend_passwd =
    try List.assoc "friend_passwd" base_env with Not_found -> !friend_passwd
  in
  let friend_passwd_file =
    try List.assoc "friend_passwd_file" base_env with Not_found -> ""
  in
  let passwd1 =
    let auth = Mutil.extract_param "authorization: " '\r' request in
    if auth = "" then ""
    else
      let s = "Basic " in
      if Mutil.start_with s 0 auth then
        let i = String.length s in
        Base64.decode (String.sub auth i (String.length auth - i))
      else ""
  in
  let uauth = if passwd = "w" || passwd = "f" then passwd1 else passwd in
  let auto = Mutil.extract_param "gw-connection-type: " '\r' request in
  let uauth = if auto = "auto" then passwd1 else uauth in
  let ok, wizard, friend, username =
    if (not !Wserver.cgi) && (passwd = "w" || passwd = "f") then
      if passwd = "w" then
        if wizard_passwd = "" && wizard_passwd_file = "" then
          (true, true, friend_passwd = "", "")
        else
          match
            basic_match_auth wizard_passwd wizard_passwd_file uauth base_file
          with
          | Some username -> (true, true, false, username)
          | None -> (false, false, false, "")
      else if passwd = "f" then
        if friend_passwd = "" && friend_passwd_file = "" then
          (true, false, true, "")
        else
          match
            basic_match_auth friend_passwd friend_passwd_file uauth base_file
          with
          | Some username -> (true, false, true, username)
          | None -> (false, false, false, "")
      else assert false
    else if wizard_passwd = "" && wizard_passwd_file = "" then
      (true, true, friend_passwd = "", "")
    else
      match
        basic_match_auth wizard_passwd wizard_passwd_file uauth base_file
      with
      | Some username -> (true, true, false, username)
      | _ -> (
          if friend_passwd = "" && friend_passwd_file = "" then
            (true, false, true, "")
          else
            match
              basic_match_auth friend_passwd friend_passwd_file uauth base_file
            with
            | Some username -> (true, false, true, username)
            | None -> (true, false, false, ""))
  in
  let user =
    match String.index_opt uauth ':' with
    | Some i ->
        let s = String.sub uauth 0 i in
        if s = wizard_passwd || s = friend_passwd then "" else s
    | None -> ""
  in
  let command, passwd =
    if access_type = ATset then
      if wizard then
        let pwd_id = set_token utm from_addr base_file 'w' user username in
        if !Wserver.cgi then (command, pwd_id)
        else (base_file ^ "_" ^ pwd_id, "")
      else if friend then
        let pwd_id = set_token utm from_addr base_file 'f' user username in
        if !Wserver.cgi then (command, pwd_id)
        else (base_file ^ "_" ^ pwd_id, "")
      else if !Wserver.cgi then (command, "")
      else (base_file, "")
    else if !Wserver.cgi then (command, passwd)
    else if passwd = "" then
      if auto = "auto" then
        let suffix = if wizard then "_w" else if friend then "_f" else "" in
        (base_file ^ suffix, passwd)
      else (base_file, "")
    else (base_file ^ "_" ^ passwd, passwd)
  in
  let auth_scheme =
    if (not wizard) && not friend then NoAuth
    else
      let realm =
        if wizard then "Wizard " ^ base_file else "Friend " ^ base_file
      in
      let u, p =
        match String.index_opt passwd1 ':' with
        | Some i ->
            let u = String.sub passwd1 0 i in
            let p =
              String.sub passwd1 (i + 1) (String.length passwd1 - i - 1)
            in
            (u, p)
        | None -> ("", passwd)
      in
      HttpAuth (Basic { bs_realm = realm; bs_user = u; bs_pass = p })
  in
  {
    ar_ok = ok;
    ar_command = command;
    ar_passwd = passwd;
    ar_scheme = auth_scheme;
    ar_user = user;
    ar_name = username;
    ar_wizard = wizard;
    ar_friend = friend;
    ar_uauth = uauth;
    ar_can_stale = false;
  }

let bad_nonce_report command passwd_char =
  {
    ar_ok = false;
    ar_command = command;
    ar_passwd = passwd_char;
    ar_scheme = NoAuth;
    ar_user = "";
    ar_name = "";
    ar_wizard = false;
    ar_friend = false;
    ar_uauth = "";
    ar_can_stale = true;
  }

let test_passwd ds nonce command wf_passwd wf_passwd_file passwd_char wiz
    base_file =
  let asch = HttpAuth (Digest ds) in
  if wf_passwd <> "" && is_that_user_and_password asch ds.ds_username wf_passwd
  then
    if ds.ds_nonce <> nonce then bad_nonce_report command passwd_char
    else
      {
        ar_ok = true;
        ar_command = command ^ "_" ^ passwd_char;
        ar_passwd = passwd_char;
        ar_scheme = asch;
        ar_user = ds.ds_username;
        ar_name = "";
        ar_wizard = wiz;
        ar_friend = not wiz;
        ar_uauth = "";
        ar_can_stale = false;
      }
  else
    match digest_match_auth_file asch wf_passwd_file base_file with
    | Some username ->
        if ds.ds_nonce <> nonce then bad_nonce_report command passwd_char
        else
          {
            ar_ok = true;
            ar_command = command ^ "_" ^ passwd_char;
            ar_passwd = passwd_char;
            ar_scheme = asch;
            ar_user = ds.ds_username;
            ar_name = username;
            ar_wizard = wiz;
            ar_friend = not wiz;
            ar_uauth = "";
            ar_can_stale = false;
          }
    | None ->
        {
          ar_ok = false;
          ar_command = command;
          ar_passwd = passwd_char;
          ar_scheme = asch;
          ar_user = ds.ds_username;
          ar_name = "";
          ar_wizard = false;
          ar_friend = false;
          ar_uauth = "";
          ar_can_stale = false;
        }

let digest_authorization request base_env passwd utm base_file command =
  let wizard_passwd =
    try List.assoc "wizard_passwd" base_env with Not_found -> !wizard_passwd
  in
  let wizard_passwd_file =
    try List.assoc "wizard_passwd_file" base_env with Not_found -> ""
  in
  let friend_passwd =
    try List.assoc "friend_passwd" base_env with Not_found -> !friend_passwd
  in
  let friend_passwd_file =
    try List.assoc "friend_passwd_file" base_env with Not_found -> ""
  in
  let command = if !Wserver.cgi then command else base_file in
  if wizard_passwd = "" && wizard_passwd_file = "" then
    {
      ar_ok = true;
      ar_command = command;
      ar_passwd = "";
      ar_scheme = NoAuth;
      ar_user = "";
      ar_name = "";
      ar_wizard = true;
      ar_friend = friend_passwd = "";
      ar_uauth = "";
      ar_can_stale = false;
    }
  else if passwd = "w" || passwd = "f" then
    let auth = Mutil.extract_param "authorization: " '\r' request in
    if Mutil.start_with "Digest " 0 auth then
      let meth =
        match Mutil.extract_param "GET " ' ' request with
        | "" -> "POST"
        | _ -> "GET"
      in
      let _ =
        trace_auth base_env (fun oc ->
            Printf.fprintf oc "\nauth = \"%s\"\n" auth)
      in
      let digenv = parse_digest auth in
      let get_digenv s = try List.assoc s digenv with Not_found -> "" in
      let ds =
        {
          ds_username = get_digenv "username";
          ds_realm = get_digenv "realm";
          ds_nonce = get_digenv "nonce";
          ds_meth = meth;
          ds_uri = get_digenv "uri";
          ds_qop = get_digenv "qop";
          ds_nc = get_digenv "nc";
          ds_cnonce = get_digenv "cnonce";
          ds_response = get_digenv "response";
        }
      in
      let nonce = digest_nonce utm in
      let _ =
        trace_auth base_env (fun oc ->
            Printf.fprintf oc
              "\n\
               answer\n\
               - date: %s\n\
               - request:\n\
               %t- passwd: %s\n\
               - nonce: \"%s\"\n\
               - meth: \"%s\"\n\
               - uri: \"%s\"\n"
              (Mutil.sprintf_date @@ Unix.localtime utm :> string)
              (fun oc ->
                List.iter (fun s -> Printf.fprintf oc "  * %s\n" s) request)
              passwd nonce ds.ds_meth ds.ds_uri)
      in
      if passwd = "w" then
        test_passwd ds nonce command wizard_passwd wizard_passwd_file "w" true
          base_file
      else if passwd = "f" then
        test_passwd ds nonce command friend_passwd friend_passwd_file "f" false
          base_file
      else failwith (Printf.sprintf "not impl (2) %s %s" auth meth)
    else
      {
        ar_ok = false;
        ar_command = command;
        ar_passwd = passwd;
        ar_scheme = NoAuth;
        ar_user = "";
        ar_name = "";
        ar_wizard = false;
        ar_friend = false;
        ar_uauth = "";
        ar_can_stale = false;
      }
  else
    let friend = friend_passwd = "" && friend_passwd_file = "" in
    {
      ar_ok = true;
      ar_command = command;
      ar_passwd = "";
      ar_scheme = NoAuth;
      ar_user = "";
      ar_name = "";
      ar_wizard = false;
      ar_friend = friend;
      ar_uauth = "";
      ar_can_stale = false;
    }

let authorization from_addr request base_env passwd access_type utm base_file
    command =
  match access_type with
  | ATwizard (user, username) ->
      let command, passwd =
        if !Wserver.cgi then (command, passwd)
        else if passwd = "" then (base_file, "")
        else (base_file ^ "_" ^ passwd, passwd)
      in
      let auth_scheme = TokenAuth { ts_user = user; ts_pass = passwd } in
      {
        ar_ok = true;
        ar_command = command;
        ar_passwd = passwd;
        ar_scheme = auth_scheme;
        ar_user = user;
        ar_name = username;
        ar_wizard = true;
        ar_friend = false;
        ar_uauth = "";
        ar_can_stale = false;
      }
  | ATfriend (user, username) ->
      let command, passwd =
        if !Wserver.cgi then (command, passwd)
        else if passwd = "" then (base_file, "")
        else (base_file ^ "_" ^ passwd, passwd)
      in
      let auth_scheme = TokenAuth { ts_user = user; ts_pass = passwd } in
      {
        ar_ok = true;
        ar_command = command;
        ar_passwd = passwd;
        ar_scheme = auth_scheme;
        ar_user = user;
        ar_name = username;
        ar_wizard = false;
        ar_friend = true;
        ar_uauth = "";
        ar_can_stale = false;
      }
  | ATnormal ->
      let command, passwd =
        if !Wserver.cgi then (command, "") else (base_file, "")
      in
      {
        ar_ok = true;
        ar_command = command;
        ar_passwd = passwd;
        ar_scheme = NoAuth;
        ar_user = "";
        ar_name = "";
        ar_wizard = false;
        ar_friend = false;
        ar_uauth = "";
        ar_can_stale = false;
      }
  | ATnone | ATset ->
      if !use_auth_digest_scheme then
        digest_authorization request base_env passwd utm base_file command
      else
        basic_authorization from_addr request base_env passwd access_type utm
          base_file command

let string_to_char_list s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let make_conf ~secret_salt from_addr request script_name env =
  if !allowed_tags_file <> "" && not (Sys.file_exists !allowed_tags_file) then (
    let str =
      Printf.sprintf "Requested allowed_tags file (%s) absent"
        !allowed_tags_file
    in
    GWPARAM.errors_other := str :: !GWPARAM.errors_other;
    Logs.syslog `LOG_WARNING str);
  let utm = Unix.time () in
  let tm = Unix.localtime utm in
  let cgi = !Wserver.cgi in
  let command, base_file, passwd, env, access_type =
    let base_access, env =
      let x, env = extract_assoc "b" env in
      if x <> "" || cgi then (x, env) else (script_name, env)
    in
    let bname, access =
      match String.split_on_char '_' base_access with
      | [ bname ] -> (bname, "")
      | [ bname; access ] -> (bname, access)
      | _ ->
          Logs.syslog `LOG_CRIT (Format.sprintf "bad bname: (%s)\n" base_access);
          assert false
    in
    let bases = Util.get_bases_list () in
    let bname = match bases with [ x ] -> x | _ -> bname in
    let passwd, env, access_type =
      let has_passwd = List.mem_assoc "w" env in
      let x, env = extract_assoc "w" env in
      if has_passwd then
        (x, env, match x with "w" | "f" | "" -> ATnone | _ -> ATset)
      else
        let access_type =
          match access with
          | "" | "w" | "f" -> ATnone
          | _ -> get_token true utm from_addr base_access
        in
        (access, env, access_type)
    in
    let command = script_name in
    (command, bname, passwd, env, access_type)
  in
  let lang, env = extract_assoc "lang" env in
  let lang =
    if lang = "" && !choose_browser_lang then http_preferred_language request
    else lang
  in
  let lang = alias_lang lang in
  let from, env =
    let x, env = extract_assoc "opt" env in
    match x with
    | "from" -> ("from", env)
    | "" -> ("", env)
    | _ -> ("", ("opt", Mutil.encode x) :: env)
  in
  let threshold_test, env = extract_assoc "threshold" env in
  if threshold_test <> "" then
    RelationLink.threshold := int_of_string threshold_test;
  GWPARAM.test_reorg base_file;
  let base_env = Util.read_base_env base_file !gw_prefix !debug in
  let default_lang =
    try
      let x = List.assoc "default_lang" base_env in
      if x = "" then !default_lang else x
    with Not_found -> !default_lang
  in
  let browser_lang =
    if !choose_browser_lang then http_preferred_language request else ""
  in
  let default_lang = if browser_lang = "" then default_lang else browser_lang in
  let vowels =
    match List.assoc_opt "vowels" base_env with
    | Some l ->
        let rec loop acc i =
          if i < String.length l then
            let s, j = Name.unaccent_utf_8 true l i in
            loop (s :: acc) j
          else acc
        in
        loop [] 0
    | _ -> [ "a"; "e"; "i"; "o"; "u"; "y" ]
  in
  let lexicon_lang = if lang = "" then default_lang else lang in
  let lexicon = load_lexicon lexicon_lang in
  (* A l'initialisation de la config, il n'y a pas de sosa_ref. *)
  (* Il sera mis à jour par effet de bord dans request.ml       *)
  let default_sosa_ref = (Driver.Iper.dummy, None) in
  let ar =
    authorization from_addr request base_env passwd access_type utm base_file
      command
  in
  let wizard_just_friend =
    if !wizard_just_friend then true
    else
      try List.assoc "wizard_just_friend" base_env = "yes"
      with Not_found -> false
  in
  let is_rtl =
    try Hashtbl.find lexicon " !dir" = "rtl" with Not_found -> false
  in
  let manitou =
    try
      ar.ar_wizard && ar.ar_user <> ""
      && p_getenv env "manitou" <> Some "off"
      && List.assoc "manitou" base_env = ar.ar_user
    with Not_found -> false
  in
  let supervisor =
    try
      ar.ar_wizard && ar.ar_user <> ""
      && List.assoc "supervisor" base_env = ar.ar_user
    with Not_found -> false
  in
  let wizard_just_friend = if manitou then false else wizard_just_friend in
  let private_years =
    try int_of_string (List.assoc "private_years" base_env)
    with Not_found | Failure _ -> 150
  in
  let username, userkey = split_username ar.ar_name in
  let conf =
    {
      from = from_addr;
      api_mode = false;
      manitou;
      supervisor;
      wizard = ar.ar_wizard && not wizard_just_friend;
      is_printed_by_template = true;
      debug = !debug;
      query_start = Unix.gettimeofday ();
      friend = ar.ar_friend || (wizard_just_friend && ar.ar_wizard);
      semi_public =
        (try List.assoc "semi_public" base_env = "yes" with Not_found -> false);
      just_friend_wizard = ar.ar_wizard && wizard_just_friend;
      user = ar.ar_user;
      username;
      userkey = Name.lower userkey;
      user_iper = None;
      auth_scheme = ar.ar_scheme;
      command = ar.ar_command;
      indep_command = (if !Wserver.cgi then ar.ar_command else "geneweb") ^ "?";
      highlight =
        (try List.assoc "highlight_color" base_env
         with Not_found -> green_color);
      lang = (if lang = "" then default_lang else lang);
      vowels;
      default_lang;
      browser_lang;
      default_sosa_ref;
      multi_parents =
        (try List.assoc "multi_parents" base_env = "yes"
         with Not_found -> false);
      authorized_wizards_notes =
        (try List.assoc "authorized_wizards_notes" base_env = "yes"
         with Not_found -> false);
      public_if_titles =
        (try List.assoc "public_if_titles" base_env = "yes"
         with Not_found -> false);
      public_if_no_date =
        (try List.assoc "public_if_no_date" base_env = "yes"
         with Not_found -> false);
      setup_link = !setup_link;
      access_by_key =
        (try List.assoc "access_by_key" base_env = "yes"
         with Not_found -> ar.ar_wizard && ar.ar_friend);
      private_years;
      private_years_death =
        (try int_of_string (List.assoc "private_years_death" base_env)
         with Not_found | Failure _ -> private_years);
      private_years_marriage =
        (try int_of_string (List.assoc "private_years_marriage" base_env)
         with Not_found | Failure _ -> private_years);
      hide_names =
        (if ar.ar_wizard || ar.ar_friend then false
         else
           try List.assoc "hide_private_names" base_env = "yes"
           with Not_found -> false);
      use_restrict =
        (if ar.ar_wizard || ar.ar_friend then false
         else
           try List.assoc "use_restrict" base_env = "yes"
           with Not_found -> false);
      no_image =
        (if ar.ar_wizard || ar.ar_friend then false
         else
           try List.assoc "no_image_for_visitor" base_env = "yes"
           with Not_found -> false);
      no_note =
        (if ar.ar_wizard || ar.ar_friend then false
         else
           try List.assoc "no_note_for_visitor" base_env = "yes"
           with Not_found -> false);
      bname = Filename.remove_extension base_file;
      nb_of_persons = 0;
      nb_of_families = 0;
      env;
      senv = [];
      cgi_passwd = ar.ar_passwd;
      henv =
        ((if not !Wserver.cgi then []
          else if ar.ar_passwd = "" then [ ("b", Mutil.encode base_file) ]
          else [ ("b", Mutil.encode @@ base_file ^ "_" ^ ar.ar_passwd) ])
        @ (if lang = "" then [] else [ ("lang", Mutil.encode lang) ])
        @ if from = "" then [] else [ ("opt", Mutil.encode from) ]);
      base_env;
      allowed_titles = Lazy.from_fun (allowed_titles env base_env);
      denied_titles = Lazy.from_fun (denied_titles env base_env);
      request;
      lexicon;
      charset = "UTF-8";
      is_rtl;
      left = (if is_rtl then "right" else "left");
      right = (if is_rtl then "left" else "right");
      auth_file =
        (try
           let x = List.assoc "auth_file" base_env in
           if x = "" then !auth_file
           else Filename.concat (!GWPARAM.bpath base_file) x
         with Not_found -> !auth_file);
      border = (match Util.p_getint env "border" with Some i -> i | None -> 0);
      n_connect = None;
      today =
        {
          day = tm.Unix.tm_mday;
          month = succ tm.Unix.tm_mon;
          year = tm.Unix.tm_year + 1900;
          prec = Sure;
          delta = 0;
        };
      today_wd = tm.Unix.tm_wday;
      time = (tm.Unix.tm_hour, tm.Unix.tm_min, tm.Unix.tm_sec);
      ctime = utm;
      gw_prefix =
        (if !gw_prefix <> "" then !gw_prefix
         else String.concat Filename.dir_sep [ "gw" ]);
      images_prefix =
        (match (!gw_prefix, !images_prefix) with
        | gw_p, im_p when gw_p <> "" && im_p = "" ->
            String.concat Filename.dir_sep [ gw_p; "images" ]
        | _, im_p when im_p <> "" -> im_p
        | _, _ -> Filename.concat "gw" "images");
      etc_prefix =
        (match (!gw_prefix, !etc_prefix) with
        | gw_p, etc_p when gw_p <> "" && etc_p = "" ->
            String.concat Filename.dir_sep [ gw_p; "etc" ]
        | _, etc_p when etc_p <> "" -> etc_p
        | _, _ -> Filename.concat "gw" "etc");
      cgi;
      output_conf;
      forced_plugins = !forced_plugins;
      plugins = !plugins;
      secret_salt = Some secret_salt;
      predictable_mode = !predictable_mode;
    }
  in
  GWPARAM.cnt_dir := !GWPARAM.cnt_d conf.bname;
  (conf, ar)

let pp_cut_string ~max ppf s =
  if String.length s > max then Fmt.pf ppf "%s..." (String.sub s 0 max)
  else Fmt.string ppf s

let log tm conf from gauth request script_name contents =
  let referer = Mutil.extract_param "referer: " '\n' request in
  let user_agent = Mutil.extract_param "user-agent: " '\n' request in
  let tm = Unix.localtime tm in
  Logs.info (fun k ->
      k "%s (%d) %s?"
        (Mutil.sprintf_date tm :> string)
        (Unix.getpid ()) script_name);
  Logs.info (fun k -> k "%a" (pp_cut_string ~max:700) contents);
  Logs.info (fun k -> k "From: %s" from);
  if gauth <> "" then Logs.info (fun k -> k "User: %s" gauth);
  if conf.wizard && not conf.friend then
    Logs.info (fun k ->
        k "  User: %s%s(wizard)" conf.user (if conf.user = "" then "" else " "))
  else if conf.friend && not conf.wizard then
    Logs.info (fun k ->
        k "  User: %s%s(friend)" conf.user (if conf.user = "" then "" else " "));
  if user_agent <> "" then Logs.info (fun k -> k "  Agent: %s" user_agent);
  if referer <> "" then Logs.info (fun k -> k "  Referer: %s" referer)

let is_robot from =
  let lock_file = !GWPARAM.adm_file "gwd.lck" in
  (* FIXME: we silently ignore errors if we cannot lock the database. *)
  let on_exn _exn _bt = false in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  let robxcl, _ = Robot.robot_excl () in
  List.mem_assoc from robxcl.Robot.excl

let auth_err request auth_file =
  if auth_file = "" then (false, "")
  else
    let auth = Mutil.extract_param "authorization: " '\r' request in
    if auth <> "" then
      match try Some (Secure.open_in auth_file) with Sys_error _ -> None with
      | Some ic -> (
          let auth =
            let i = String.length "Basic " in
            Base64.decode (String.sub auth i (String.length auth - i))
          in
          try
            let rec loop () =
              if auth = input_line ic then (
                close_in ic;
                let s =
                  try
                    let i = String.rindex auth ':' in
                    String.sub auth 0 i
                  with Not_found -> "..."
                in
                (false, s))
              else loop ()
            in
            loop ()
          with End_of_file ->
            close_in ic;
            (true, auth))
      | _ -> (true, "(auth file '" ^ auth_file ^ "' not found)")
    else (true, "(authorization not provided)")

let no_access conf =
  let title _ = Output.print_sstring conf "Error" in
  Hutil.rheader conf title;
  Output.print_sstring conf "No access to this database in CGI mode\n";
  Hutil.trailer conf

let log_and_robot_check conf auth from request script_name contents =
  if !robot_xcl = None then
    log (Unix.time ()) conf from auth request script_name contents
  else
    let lock_file = !GWPARAM.adm_file "gwd.lck" in
    (* FIXME: we silently ignore errors if we cannot lock the database. *)
    let on_exn _exn _bt = () in
    Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
    let tm = Unix.time () in
    (match !robot_xcl with
    | Some (cnt, sec) ->
        let s = "suicide" in
        let suicide = Util.p_getenv conf.env s <> None in
        conf.n_connect <- Some (Robot.check tm from cnt sec conf suicide)
    | _ -> ());
    log tm conf from auth request script_name contents

let conf_and_connection =
  let slow_query_threshold =
    match Sys.getenv_opt "GWD_SLOW_QUERY_THRESHOLD" with
    | Some x -> float_of_string x
    | None -> infinity
  in
  let context conf contents =
    conf.bname
    ^<^ (if conf.wizard then "_w?" else if conf.friend then "_f?" else "?")
    ^<^ contents
  in
  fun ~secret_salt from request script_name (contents : Adef.encoded_string) env
    ->
    let conf, passwd_err =
      make_conf ~secret_salt from request script_name env
    in
    match !redirected_addr with
    | Some addr -> print_redirected conf from request addr
    | None -> (
        let auth_err, auth =
          if conf.auth_file = "" then (false, "")
          else if !Wserver.cgi then (true, "")
          else auth_err request conf.auth_file
        in
        let mode = Util.p_getenv conf.env "m" in
        (if mode <> Some "IM" then
           let contents =
             if List.mem_assoc "log_pwd" env then Adef.encoded "..."
             else contents
           in
           log_and_robot_check conf auth from request script_name
             (contents :> string));
        match (!Wserver.cgi, auth_err, passwd_err) with
        | true, true, _ ->
            if is_robot from then Robot.robot_error conf 0 0 else no_access conf
        | _, true, _ ->
            if is_robot from then Robot.robot_error conf 0 0
            else
              let auth_type =
                let x =
                  try List.assoc "auth_file" conf.base_env
                  with Not_found -> ""
                in
                if x = "" then "GeneWeb service" else "database " ^ conf.bname
              in
              refuse_auth conf from auth auth_type
        | _, _, ({ ar_ok = false } as ar) ->
            if is_robot from then Robot.robot_error conf 0 0
            else
              let tm = Unix.time () in
              let lock_file = !GWPARAM.adm_file "gwd.lck" in
              (* FIXME: we silently ignore errors if we cannot lock the database. *)
              let on_exn _exn _bt = () in
              Lock.control ~on_exn ~wait:true ~lock_file (fun () ->
                  log_passwd_failed ar tm from request conf.bname);
              unauth_server conf ar
        | _ -> (
            let printexc bt exn =
              Logs.syslog `LOG_CRIT
                ((context conf contents :> string)
                ^ " " ^ Printexc.to_string exn);
              if Printexc.backtrace_status () then
                let s = Format.sprintf "Backtrace:@ %s" bt in
                Logs.syslog `LOG_CRIT s
            in
            try
              let t1 = Unix.gettimeofday () in
              Request.treat_request conf;
              let t2 = Unix.gettimeofday () in
              if t2 -. t1 > slow_query_threshold then
                Logs.syslog `LOG_WARNING
                  (Printf.sprintf "%s slow query (%.3f)"
                     (context conf contents : Adef.encoded_string :> string)
                     (t2 -. t1))
            with
            | Exit -> ()
            | Def.HttpExn (code, _) as exn ->
                let bt = Printexc.get_backtrace () in
                GWPARAM.output_error conf code;
                printexc bt exn
            | exn ->
                let bt = Printexc.get_backtrace () in
                printexc bt exn))

let chop_extension name =
  let rec loop i =
    if i < 0 then name
    else if name.[i] = '.' then String.sub name 0 i
    else if name.[i] = '/' then name
    else if name.[i] = '\\' then name
    else loop (i - 1)
  in
  loop (String.length name - 1)

let match_strings regexp s =
  let rec loop i j =
    if i = String.length regexp && j = String.length s then true
    else if i = String.length regexp then false
    else if j = String.length s then false
    else if regexp.[i] = s.[j] then loop (i + 1) (j + 1)
    else if regexp.[i] = '*' then
      if i + 1 = String.length regexp then true
      else if regexp.[i + 1] = s.[j] then loop (i + 2) (j + 1)
      else loop i (j + 1)
    else false
  in
  loop 0 0

let excluded from =
  let efname = chop_extension Sys.argv.(0) ^ ".xcl" in
  try
    let ic = open_in efname in
    let rec loop () =
      match input_line ic with
      | line when match_strings line from ->
          close_in ic;
          true
      | _ -> loop ()
      | exception End_of_file ->
          close_in ic;
          false
    in
    loop ()
  with Sys_error _ -> false

let image_request conf script_name env =
  match (Util.p_getenv env "m", Util.p_getenv env "v") with
  | Some "IM", Some fname ->
      let fname =
        if fname.[0] = '/' then String.sub fname 1 (String.length fname - 1)
        else fname
      in
      let fname = Image.path_of_filename conf fname in
      let _ = ImageDisplay.print_image_file conf fname in
      true
  | _ ->
      let s = script_name in
      if Mutil.start_with "images/" 0 s then
        let i = String.length "images/" in
        let fname = String.sub s i (String.length s - i) in
        (* Je ne sais pas pourquoi on fait un basename, mais ça empeche *)
        (* empeche d'avoir des images qui se trouvent dans le dossier   *)
        (* image. Si on ne fait pas de basename, alors ça marche.       *)
        (* let fname = Filename.basename fname in *)
        let fname = Image.path_of_filename conf fname in
        let _ = ImageDisplay.print_image_file conf fname in
        true
      else false

(* Une version un peu à cheval entre avant et maintenant afin de   *)
(* pouvoir inclure une css, un fichier javascript (etc) facilement *)
(* et que le cache du navigateur puisse prendre le relais.         *)
type misc_fname =
  | Css of string
  | Eot of string
  | Js of string
  | Map of string
  | Otf of string
  | Other of string
  | Png of string
  | Svg of string
  | Ttf of string
  | Woff of string
  | Woff2 of string
  | CacheGz of string

let content_misc conf len misc_fname =
  Output.status conf Def.OK;
  let fname, t =
    match misc_fname with
    | Css fname -> (fname, "text/css; charset=UTF-8")
    | Eot fname -> (fname, "application/font-eot")
    | Js fname -> (fname, "text/javascript; charset=UTF-8")
    | Map fname -> (fname, "application/json")
    | Otf fname -> (fname, "application/font-otf")
    | Other fname -> (fname, "text/plain")
    | Png fname -> (fname, "image/png")
    | Svg fname -> (fname, "application/font-svg")
    | Ttf fname -> (fname, "application/font-ttf")
    | Woff fname -> (fname, "application/font-woff")
    | Woff2 fname -> (fname, "application/font-woff2")
    | CacheGz fname -> (fname, "application/gzip")
  in
  Output.header conf "Content-type: %s" t;
  Output.header conf "Content-length: %d" len;
  Output.header conf "Content-disposition: inline; filename=%s"
    (Filename.basename fname);
  Output.header conf "Cache-control: private, max-age=%d" (60 * 60 * 24 * 365);
  Output.flush conf

let find_misc_file conf name =
  if
    Sys.file_exists name
    && List.exists
         (fun p -> Mutil.start_with (Filename.concat p "assets") 0 name)
         !plugins
  then name
  else
    let name' = Filename.concat (!GWPARAM.etc_d conf.bname) name in
    if Sys.file_exists name' then name'
    else
      let name' = Util.search_in_assets @@ Filename.concat "etc" name in
      if Sys.file_exists name' then name' else ""

let print_misc_file conf misc_fname =
  match misc_fname with
  | Css fname
  | Js fname
  | Otf fname
  | Svg fname
  | Woff fname
  | Eot fname
  | Ttf fname
  | Woff2 fname
  | CacheGz fname -> (
      try
        let ic = Secure.open_in_bin fname in
        let buf = Bytes.create 1024 in
        let len = in_channel_length ic in
        content_misc conf len misc_fname;
        let rec loop len =
          if len = 0 then ()
          else
            let olen = min (Bytes.length buf) len in
            really_input ic buf 0 olen;
            Wserver.printf "%s" (Bytes.sub_string buf 0 olen);
            loop (len - olen)
        in
        loop len;
        close_in ic;
        true
      with Sys_error _ -> false)
  | Other _ -> false
  | Map fname | Png fname ->
      let ic = Secure.open_in_bin fname in
      let buf = Bytes.create 1024 in
      let len = in_channel_length ic in
      content_misc conf len misc_fname;
      let rec loop len =
        if len = 0 then ()
        else
          let olen = min (Bytes.length buf) len in
          really_input ic buf 0 olen;
          Output.print_sstring conf (Bytes.sub_string buf 0 olen);
          loop (len - olen)
      in
      loop len;
      close_in ic;
      true

let misc_request conf fname =
  let fname = find_misc_file conf fname in
  if fname <> "" then
    let misc_fname =
      if Filename.check_suffix fname ".css" then Css fname
      else if Filename.check_suffix fname ".js" then Js fname
      else if Filename.check_suffix fname ".map" then Map fname
      else if Filename.check_suffix fname ".otf" then Otf fname
      else if Filename.check_suffix fname ".svg" then Svg fname
      else if Filename.check_suffix fname ".woff" then Woff fname
      else if Filename.check_suffix fname ".eot" then Eot fname
      else if Filename.check_suffix fname ".ttf" then Ttf fname
      else if Filename.check_suffix fname ".woff2" then Woff2 fname
      else if Filename.check_suffix fname ".png" then Png fname
      else if Filename.check_suffix fname ".cache.gz" then CacheGz fname
      else Other fname
    in
    print_misc_file conf misc_fname
  else false

let strip_quotes s =
  let i0 = if String.length s > 0 && s.[0] = '"' then 1 else 0 in
  let i1 =
    if String.length s > 0 && s.[String.length s - 1] = '"' then
      String.length s - 1
    else String.length s
  in
  String.sub s i0 (i1 - i0)

let extract_multipart boundary str =
  let str = (str : Adef.encoded_string :> string) in
  let rec skip_nl i =
    if i < String.length str && str.[i] = '\r' then skip_nl (i + 1)
    else if i < String.length str && str.[i] = '\n' then skip_nl (i + 1)
    else i
  in
  let next_line i =
    let i = skip_nl i in
    let rec loop s i =
      if i = String.length str || str.[i] = '\n' || str.[i] = '\r' then (s, i)
      else loop (s ^ String.make 1 str.[i]) (i + 1)
    in
    loop "" i
  in
  let boundary = "--" ^ boundary in
  let rec loop i =
    if i = String.length str then []
    else
      let s, i = next_line i in
      if s = boundary then
        let s, i = next_line i in
        let s = String.lowercase_ascii s |> Adef.encoded in
        let env = Util.create_env s in
        match (Util.p_getenv env "name", Util.p_getenv env "filename") with
        | Some var, Some filename ->
            let var = strip_quotes var in
            let filename = strip_quotes filename in
            let i = skip_nl i in
            let i1 =
              let rec loop i =
                if i < String.length str then
                  if
                    i > String.length boundary
                    && String.sub str
                         (i - String.length boundary)
                         (String.length boundary)
                       = boundary
                  then i - String.length boundary
                  else loop (i + 1)
                else i
              in
              loop i
            in
            let v = String.sub str i (i1 - i) in
            (var ^ "_name", Mutil.encode filename)
            :: (var, Adef.encoded v)
            :: loop i1
        | Some var, None ->
            let var = strip_quotes var in
            let i = skip_nl i in
            let i1 =
              let rec loop i =
                if i < String.length str then
                  if
                    i > String.length boundary
                    && String.sub str
                         (i - String.length boundary)
                         (String.length boundary)
                       = boundary
                  then i - String.length boundary
                  else loop (i + 1)
                else i
              in
              loop i
            in
            let v = String.sub str i (i1 - i) |> String.trim in
            (var, Adef.encoded v) :: loop i
        | _ -> loop i
      else if s = boundary ^ "--" then []
      else loop i
  in
  let env = loop 0 in
  let str, _ =
    List.fold_left
      (fun (str, sep) (v, x) ->
        if v = "file" then (str, sep) else (str ^^^ sep ^<^ v ^<^ "=" ^<^ x, "&"))
      (Adef.encoded "", "")
      env
  in
  (str, env)

let build_env request (contents : Adef.encoded_string) :
    Adef.encoded_string * (string * Adef.encoded_string) list =
  let content_type = Mutil.extract_param "content-type: " '\n' request in
  if is_multipart_form content_type then
    let boundary =
      (extract_boundary (Adef.encoded content_type)
        : Adef.encoded_string
        :> string)
    in
    extract_multipart boundary contents
  else (contents, Util.create_env contents)

let connection ~secret_salt (addr, request) script_name contents0 =
  let from =
    match addr with
    | Unix.ADDR_UNIX x -> x
    | Unix.ADDR_INET (iaddr, _) -> (
        if !no_host_address then Unix.string_of_inet_addr iaddr
        else
          try (Unix.gethostbyaddr iaddr).Unix.h_name
          with _ -> Unix.string_of_inet_addr iaddr)
  in
  if script_name = "robots.txt" then robots_txt printer_conf
  else if excluded from then refuse_log printer_conf from
  else
    let accept =
      if !only_addresses = [] then true else List.mem from !only_addresses
    in
    if not accept then only_log printer_conf from
    else
      try
        let contents, env = build_env request contents0 in
        if
          (not (image_request printer_conf script_name env))
          && not (misc_request printer_conf script_name)
        then
          conf_and_connection ~secret_salt from request script_name contents env
      with Exit -> ()

let null_reopen flags fd =
  if Sys.unix then (
    let fd2 = Unix.openfile "/dev/null" flags 0 in
    Unix.dup2 fd2 fd;
    Unix.close fd2)

(* [generate_secret_salt ?random ()] generates a secret salt.
   If the [random] argument is [false], the salt is always the same.
   The default is [true]. *)
let generate_secret_salt ?(random = true) () =
  if random then (
    Random.self_init ();
    string_of_int @@ Random.bits ())
  else ""

let retrieve_secret_salt () =
  match Unix.getenv "SECRET_SALT" with
  | exception Not_found ->
      Logs.err (fun k ->
          k "Secret salt missing, the worker %d cannot continue its job."
            (Unix.getpid ()));
      exit 1
  | s -> s

let geneweb_server ~predictable_mode () =
  let secret_salt =
    match Unix.getenv "WSERVER" with
    | exception Not_found ->
        let hostn =
          match !selected_addr with
          | Some addr -> addr
          | None -> ( try Unix.gethostname () with _ -> "computer")
        in
        let () =
          if !daemon then
            match Unix.fork () with
            | 0 ->
                Unix.close Unix.stdin;
                null_reopen [ Unix.O_WRONLY ] Unix.stdout;
                null_reopen [ Unix.O_WRONLY ] Unix.stderr
            | _ -> exit 0
          else (
            Logs.info (fun k ->
                k
                  "Possible addresses:\n\
                   http://localhost:%d/base\n\
                   http://127.0.0.1:%d/base\n\
                   http://%s:%d/base"
                  !selected_port !selected_port hostn !selected_port);
            Logs.info (fun k ->
                k
                  "where \"base\" is the name of the database\n\
                   Type “Ctrl+C” to stop the service");
            (* taken from Michel Normand commit 1874dcbf7 *)
            Logs.debug (fun k ->
                k
                  "gwd parameters (after GWPARAM.init & cache_lexicon):\n\
                   source: %s\n\
                   branch: %s\n\
                   commit: %s\n\
                   gwd:%s\n\
                   current_dir_name: %s\n\
                   gw_prefix: %s\n\
                   etc_prefix: %s\n\
                   images_prefix: %s\n\
                   images_dir: %s\n\
                   secure asset: %a"
                  Version.src Version.branch Version.commit_id Sys.argv.(0)
                  (Sys.getcwd ()) !gw_prefix !etc_prefix !images_prefix
                  !images_dir
                  Fmt.(box @@ brackets @@ list ~sep:comma string)
                  (Secure.assets ())))
        in
        let () =
          try
            Filesystem.create_dir ~parent:true ~required_perm:0o755
              !GWPARAM.cnt_dir
          with Sys_error e ->
            Logs.err (fun k -> k "failure creating %s:@ %s" !GWPARAM.cnt_dir e)
        in
        (* A secret salt is added to the environment to ensure that workers
           use the same salt for digests on both Unix and Windows platforms. *)
        let secret_salt =
          generate_secret_salt ~random:(not predictable_mode) ()
        in
        Unix.putenv "SECRET_SALT" secret_salt;
        secret_salt
    | _ -> retrieve_secret_salt ()
  in
  Wserver.start ?addr:!selected_addr ~port:!selected_port ~timeout:!conn_timeout
    ~max_pending_requests:!max_pending_requests ~n_workers:!n_workers
    (connection ~secret_salt)

let cgi_timeout conf tmout _ =
  Output.header conf "Content-type: text/html; charset=iso-8859-1";
  Output.print_sstring conf "<head><title>Time out</title></head>\n";
  Output.print_sstring conf "<body><h1>Time out</h1>\n";
  Output.printf conf "Computation time > %d second(s)\n" tmout;
  Output.print_sstring conf "</body>\n";
  Output.flush conf;
  exit 0

let manage_cgi_timeout tmout =
  if tmout > 0 then
    let _ =
      Sys.signal Sys.sigalrm
        (Sys.Signal_handle (cgi_timeout printer_conf tmout))
    in
    let _ = Unix.alarm tmout in
    ()

let geneweb_cgi ~secret_salt addr script_name contents =
  if Sys.unix then manage_cgi_timeout !conn_timeout;
  (try Unix.mkdir !GWPARAM.cnt_dir 0o755 with Unix.Unix_error (_, _, _) -> ());
  let add k x request =
    try
      let v = Sys.getenv x in
      if v = "" then raise Not_found else (k ^ ": " ^ v) :: request
    with Not_found -> request
  in
  let request = [] in
  let request = add "cookie" "HTTP_COOKIE" request in
  let request = add "content-type" "CONTENT_TYPE" request in
  let request = add "accept-language" "HTTP_ACCEPT_LANGUAGE" request in
  let request = add "referer" "HTTP_REFERER" request in
  let request = add "user-agent" "HTTP_USER_AGENT" request in
  connection ~secret_salt (Unix.ADDR_UNIX addr, request) script_name contents

let read_input len =
  if len >= 0 then really_input_string stdin len
  else
    let buff = Buffer.create 0 in
    (try
       while true do
         let l = input_line stdin in
         Buffer.add_string buff l
       done
     with End_of_file -> ());
    Buffer.contents buff

let arg_parse_in_file fname speclist anonfun errmsg =
  try
    let ic = open_in fname in
    let list =
      let rec loop acc =
        match input_line ic with
        | line -> loop (if line <> "" then line :: acc else acc)
        | exception End_of_file ->
            close_in ic;
            List.rev acc
      in
      loop []
    in
    let list =
      match list with [ x ] -> Gutil.arg_list_of_string x | _ -> list
    in
    Arg.parse_argv ~current:(ref 0)
      (Array.of_list @@ (Sys.argv.(0) :: list))
      speclist anonfun errmsg
  with Sys_error _ -> ()

let robot_exclude_arg s =
  try robot_xcl := Scanf.sscanf s "%d,%d" (fun cnt sec -> Some (cnt, sec))
  with _ ->
    Printf.eprintf "Bad use of option -robot_xcl\n";
    Printf.eprintf "Use option -help for usage.\n";
    flush Stdlib.stderr;
    exit 2

let slashify s =
  let conv_char i = match s.[i] with '\\' -> '/' | x -> x in
  String.init (String.length s) conv_char

let make_sock_dir x =
  Filesystem.create_dir ~parent:true x;
  if Sys.unix then ()
  else (
    Wserver.sock_in := Filename.concat x "gwd.sin";
    Wserver.sock_out := Filename.concat x "gwd.sou");
  GWPARAM.sock_dir := x

let arg_plugin_doc opt doc =
  doc
  ^ " Combine with -force to enable for every base. Combine with -unsafe to \
     allow unverified plugins. e.g. \"" ^ opt ^ " -unsafe -force\"."

let arg_plugin_aux () =
  let aux (unsafe, force, p) =
    incr Arg.current;
    assert (!Arg.current < Array.length Sys.argv);
    match Sys.argv.(!Arg.current) with
    | "-unsafe" -> (true, force, p)
    | "-force" -> (unsafe, true, p)
    | p' ->
        assert (p = "");
        (unsafe, force, p')
  in
  let rec loop ((_, _, p) as acc) = if p = "" then loop (aux acc) else acc in
  loop (false, false, "")

let arg_plugin opt doc =
  ( opt,
    Arg.Unit
      (fun () ->
        let unsafe, force, s = arg_plugin_aux () in
        if unsafe then unsafe_plugins := !unsafe_plugins @ [ s ];
        if force then
          forced_plugins := !forced_plugins @ [ Filename.basename s ];
        plugins := !plugins @ [ s ]),
    arg_plugin_doc opt doc )

let arg_plugins opt doc =
  ( opt,
    Arg.Unit
      (fun () ->
        let unsafe, force, s = arg_plugin_aux () in
        let ps = Array.to_list (Sys.readdir s) in
        let deps_ht = Hashtbl.create 0 in
        let deps =
          List.map
            (fun pname ->
              let dir = Filename.concat s pname in
              if (not unsafe) && not (GwdPluginMD5.allowed dir) then failwith s;
              Hashtbl.add deps_ht pname dir;
              let f = Filename.concat dir "META" in
              if Sys.file_exists f then
                (pname, GwdPluginMETA.((parse f).depends))
              else (pname, []))
            ps
        in
        match GwdPluginDep.sort deps with
        | GwdPluginDep.ErrorCycle _ -> assert false
        | GwdPluginDep.Sorted deps ->
            List.iter
              (fun pname ->
                try
                  let s = Hashtbl.find deps_ht pname in
                  if unsafe then unsafe_plugins := !unsafe_plugins @ [ s ];
                  if force then forced_plugins := !forced_plugins @ [ pname ];
                  plugins := !plugins @ [ s ]
                with Not_found ->
                  raise
                    (Register_plugin_failure (pname, `string "Missing plugin")))
              deps),
    arg_plugin_doc opt doc )

let print_version_commit () =
  Printf.printf "Geneweb version %s\nRepository %s\n" Version.ver Version.src;
  Printf.printf "Branch %s\nLast commit %s\n" Version.branch Version.commit_id;
  exit 0

let set_log_file f =
  Logs.set_output_channel
    (match f with
    | "-" | "<stdout>" -> Logs.Stdout
    | "2" | "<stderr>" -> Logs.Stderr
    | _ ->
        let oc =
          open_out_gen [ Open_wronly; Open_creat; Open_append; Open_text ] 644 f
        in
        Logs.Channel oc)

let set_verbosity_level lvl = Logs.verbosity_level := lvl

let set_debug_flag () =
  debug := true;
  Logs.debug_flag := true;
  Printexc.record_backtrace true;
  set_verbosity_level 7;
  Sys.enable_runtime_warnings true

let set_predictable_mode () =
  Logs.warn (fun k ->
      k
        "Predictable mode must not be enabled in production. It disables \
         security enhancements and caching.");
  predictable_mode := true

let main () =
  if not Sys.unix then (
    Wserver.sock_in := "gwd.sin";
    Wserver.sock_out := "gwd.sou");
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let force_cgi = ref false in
  let cgi_secret_salt : string option ref = ref None in
  let speclist =
    [
      ( "-hd",
        Arg.String
          (fun x ->
            gw_prefix := x;
            Secure.add_assets x),
        "<DIR> Specify where the “etc”, “images” and “lang” directories are \
         installed (default if empty is “gw”)." );
      ( "-bd",
        Arg.String Secure.set_base_dir,
        "<DIR> Specify where the “bases” directory with databases is installed \
         (default if empty is “bases”)." );
      ( "-wd",
        Arg.String make_sock_dir,
        "<DIR> Directory for socket communication (Windows) and access count."
      );
      ( "-cache_langs",
        Arg.String
          (fun s ->
            List.iter (Mutil.list_ref_append cache_langs)
            @@ String.split_on_char ',' s),
        " Lexicon languages to be cached." );
      ("-cgi", Arg.Set force_cgi, " Force CGI mode.");
      ( "-cgi_secret_salt",
        Arg.String (fun s -> cgi_secret_salt := Some s),
        "<STRING> Add a secret salt to form digests." );
      ( "-etc_prefix",
        Arg.String
          (fun x ->
            etc_prefix := x;
            Secure.add_assets x),
        "<DIR> Specify where the “etc” directory is installed (default if \
         empty is [-hd value]/etc)." );
      ( "-images_prefix",
        Arg.String (fun x -> images_prefix := x),
        "<DIR> Specify where the “images” directory is installed (default if \
         empty is [-hd value]/images)." );
      ( "-images_dir",
        Arg.String (fun x -> images_dir := x),
        "<DIR> Same than previous but directory name relative to current." );
      ( "-a",
        Arg.String (fun x -> selected_addr := Some x),
        "<ADDRESS> Select a specific address (default = any address of this \
         computer)." );
      ( "-p",
        Arg.Int (fun x -> selected_port := x),
        "<NUMBER> Select a port number (default = "
        ^ string_of_int !selected_port
        ^ ")." );
      ( "-setup_link",
        Arg.Set setup_link,
        " Display a link to local gwsetup in bottom of pages." );
      ( "-allowed_tags",
        Arg.String (fun x -> Util.allowed_tags_file := x),
        "<FILE> HTML tags which are allowed to be displayed. One tag per line \
         in file." );
      ( "-wizard",
        Arg.String (fun x -> wizard_passwd := x),
        "<PASSWD> Set a wizard password." );
      ( "-friend",
        Arg.String (fun x -> friend_passwd := x),
        "<PASSWD> Set a friend password." );
      ("-wjf", Arg.Set wizard_just_friend, " Wizard just friend (permanently).");
      ( "-lang",
        Arg.String (fun x -> default_lang := x),
        "<LANG> Set a default language (default: " ^ !default_lang ^ ")." );
      ( "-blang",
        Arg.Set choose_browser_lang,
        " Select the user browser language if any." );
      ( "-only",
        Arg.String (fun x -> only_addresses := x :: !only_addresses),
        "<ADDRESS> Only inet address accepted." );
      ( "-auth",
        Arg.String (fun x -> auth_file := x),
        "<FILE> Authorization file to restrict access. The file must hold \
         lines of the form \"user:password\"." );
      ( "-no_host_address",
        Arg.Set no_host_address,
        " Force no reverse host by address." );
      ( "-digest",
        Arg.Set use_auth_digest_scheme,
        " Use Digest authorization scheme (more secure on passwords)" );
      ( "-add_lexicon",
        Arg.String (Mutil.list_ref_append lexicon_list),
        "<FILE> Add file as lexicon." );
      ( "-log",
        Arg.String set_log_file,
        {|<FILE> Log trace to this file. Use "-" or "<stdout>" to redirect output to stdout or "<stderr>" to output log to stderr.|}
      );
      ( "-log_level",
        Arg.Int set_verbosity_level,
        {|<N> Send messages with severity <= <N> to syslog (default: |}
        ^ string_of_int !Logs.verbosity_level
        ^ {|).|} );
      ( "-robot_xcl",
        Arg.String robot_exclude_arg,
        "<CNT>,<SEC> Exclude connections when more than <CNT> requests in \
         <SEC> seconds." );
      ( "-min_disp_req",
        Arg.Int (fun x -> Robot.min_disp_req := x),
        " Minimum number of requests in robot trace (default: "
        ^ string_of_int !Robot.min_disp_req
        ^ ")." );
      ( "-login_tmout",
        Arg.Int (fun x -> login_timeout := x),
        "<SEC> Login timeout for entries with passwords in CGI mode (default "
        ^ string_of_int !login_timeout
        ^ "s)." );
      ( "-redirect",
        Arg.String (fun x -> redirected_addr := Some x),
        "<ADDR> Send a message to say that this service has been redirected to \
         <ADDR>." );
      ( "-trace_failed_passwd",
        Arg.Set trace_failed_passwd,
        " Print the failed passwords in log (except if option -digest is set). "
      );
      ("-debug", Arg.Unit set_debug_flag, " Enable debug mode");
      ( "-nolock",
        Arg.Set Lock.no_lock_flag,
        " Do not lock files before writing." );
      arg_plugin "-plugin" "<PLUGIN>.cmxs load a safe plugin.";
      arg_plugins "-plugins" "<DIR> load all plugins in <DIR>.";
      ( "-version",
        Arg.Unit print_version_commit,
        " Print the Geneweb version, the source repository and last commit id \
         and message." );
    ]
  in
  let speclist =
    if Sys.unix then
      speclist
      @ [
          ( "-max_clients",
            Arg.Unit deprecated_warning_max_clients,
            "<NUM> Max number of clients treated at the same time (default: no \
             limit) (not cgi) (DEPRECATED)." );
          ( "-n_workers",
            Arg.Int (fun x -> n_workers := x),
            "<NUM> Number of workers used by the server (default: "
            ^ string_of_int default_n_workers
            ^ ")" );
          ( "-max_pending_requests",
            Arg.Int (fun x -> max_pending_requests := x),
            "<NUM> Maximum number of pending requests (default: "
            ^ string_of_int default_max_pending_requests
            ^ ")" );
          ( "-conn_tmout",
            Arg.Int (fun x -> conn_timeout := x),
            "<SEC> Connection timeout (only on Unix) (default "
            ^ string_of_int !conn_timeout
            ^ "s; 0 means no limit)." );
          ("-daemon", Arg.Set daemon, " Unix daemon mode.");
          ( "-no-fork",
            Arg.Unit
              (fun () ->
                deprecated_warning_no_fork ();
                n_workers := 0),
            " Prevent forking processes (DEPRECATED)" );
          ( "-cache-in-memory",
            Arg.String
              (fun s ->
                if Gw_ancient.is_available then
                  cache_databases := s :: !cache_databases
                else
                  failwith "-cache-in-memory option unavailable for this build."),
            "<DATABASE> Preload this database in memory" );
          ( "-predictable_mode",
            Arg.Unit set_predictable_mode,
            " Turn on the predictable mode. In this mode, the behavior of the \
             server is predictable, which is helpful for debugging or testing. \
             (default: false)" );
        ]
    else speclist
  in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
  (if Sys.unix then
     default_lang :=
       let s = try Sys.getenv "LANG" with Not_found -> "" in
       if List.mem s Version.available_languages then s
       else
         let s = try Sys.getenv "LC_CTYPE" with Not_found -> "" in
         if String.length s >= 2 then
           let s = String.sub s 0 2 in
           if List.mem s Version.available_languages then s else "en"
         else "en");
  arg_parse_in_file
    (chop_extension Sys.argv.(0) ^ ".arg")
    speclist anonfun usage;
  Arg.parse speclist anonfun usage;
  let gwd_cmd =
    let rec process acc skip_next = function
      | [] -> acc
      | arg :: rest ->
          if skip_next then process (acc ^ "xxx") false rest
          else if arg = "-cgi_secret_salt" then
            process (acc ^ "<br><b>" ^ arg ^ "</b> ") true rest
          else if arg.[0] = '-' then
            process (acc ^ "<br><b>" ^ arg ^ "</b> ") false rest
          else process (acc ^ arg) false rest
    in
    process "" false (Array.to_list Sys.argv)
  in
  Geneweb.GWPARAM.gwd_cmd := gwd_cmd;
  List.iter register_plugin !plugins;
  GWPARAM.init "";
  cache_lexicon ();
  List.iter
    (fun dbn ->
      Logs.info (fun k -> k "Caching database %s in memory… %!" dbn);
      let dbn = !GWPARAM.bpath dbn in
      Driver.load_database dbn)
    !cache_databases;
  if !auth_file <> "" && !force_cgi then
    Logs.syslog `LOG_WARNING
      "-auth option is not compatible with CGI mode.\n\
      \ Use instead friend_passwd_file= and wizard_passwd_file= in .cgf file\n";
  if !use_auth_digest_scheme && !force_cgi then
    Logs.syslog `LOG_WARNING "-digest option is not compatible with CGI mode.\n";
  (if !images_dir <> "" then
     let abs_dir =
       let f =
         Util.search_in_assets (Filename.concat !images_dir "gwback.jpg")
       in
       let d = Filename.dirname f in
       if Filename.is_relative d then Filename.concat (Sys.getcwd ()) d else d
     in
     images_prefix := "file://" ^ slashify abs_dir);
  GWPARAM.cnt_dir := !GWPARAM.cnt_d "";
  Wserver.stop_server :=
    List.fold_left Filename.concat !GWPARAM.cnt_dir [ "STOP_SERVER" ];
  let query, cgi =
    try (Sys.getenv "QUERY_STRING" |> Adef.encoded, true)
    with Not_found -> ("" |> Adef.encoded, !force_cgi)
  in
  Util.is_welcome := false;
  if cgi then (
    Wserver.cgi := true;
    let query =
      if Sys.getenv_opt "REQUEST_METHOD" = Some "POST" then (
        let len =
          try int_of_string (Sys.getenv "CONTENT_LENGTH") with Not_found -> -1
        in
        set_binary_mode_in stdin true;
        read_input len |> Adef.encoded)
      else query
    in
    let addr =
      try Sys.getenv "REMOTE_HOST"
      with Not_found -> ( try Sys.getenv "REMOTE_ADDR" with Not_found -> "")
    in
    let script =
      try Sys.getenv "SCRIPT_NAME" with Not_found -> Sys.argv.(0)
    in
    let secret_salt = match !cgi_secret_salt with None -> "" | Some s -> s in
    geneweb_cgi ~secret_salt addr (Filename.basename script) query)
  else geneweb_server ~predictable_mode:!predictable_mode ()

let () =
  try main () with
  | Unix.Unix_error (Unix.EADDRINUSE, "bind", _) ->
      Logs.err (fun k ->
          k
            "Error: the port %d\n\
            \         is already used by another GeneWeb daemon or by another \
             program. Solution: kill the other program or launch GeneWeb with \
             another port number (option -p)"
            !selected_port)
  | Unix.Unix_error (Unix.ENOTCONN, _, _) when Sys.unix ->
      Logs.syslog `LOG_WARNING
        {|Unix.Unix_error(Unix.ENOTCONN, "shutdown", "")|}
  | Unix.Unix_error (Unix.EACCES, "bind", _) when Sys.unix ->
      Logs.err (fun k ->
          k
            "Error: invalid access to the port %d: users port number less than \
             1024 are reserved to the system. Solution: do it as root or \
             choose another port number greater than 1024."
            !selected_port)
  | Register_plugin_failure (p, `dynlink_error e) ->
      Logs.syslog `LOG_CRIT (p ^ ": " ^ Dynlink.error_message e)
  | Register_plugin_failure (p, `string s) ->
      Logs.syslog `LOG_CRIT (p ^ ": " ^ s)
  | exn ->
      let bt = Printexc.get_backtrace () in
      Logs.syslog `LOG_CRIT (Printexc.to_string exn);
      if Printexc.backtrace_status () then
        let s = Format.sprintf "Backtrace:@ %s" bt in
        Logs.syslog `LOG_CRIT s
