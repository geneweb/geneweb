(* Copyright (c) 1998-2007 INRIA *)

let () = Sys.enable_runtime_warnings Geneweb.Dev_config.debug

let output_conf =
  {
    Geneweb.Config.status = Wserver.http;
    header = Wserver.header;
    body = Wserver.print_string;
    flush = Wserver.wflush;
  }

let printer_conf = { Geneweb.Config.empty with output_conf }
let auth_file = ref ""
let cache_langs = ref []
let choose_browser_lang = ref false
let conn_timeout = ref 120
let daemon = ref false
let default_lang = ref "fr"
let friend_passwd = ref ""
let images_dir = ref ""
let images_url = ref ""
let lexicon_list = ref [ Filename.concat "lang" "lexicon.txt" ]
let login_timeout = ref 1800
let max_clients = ref None
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
let notify_change = ref None

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
  List.assoc "boundary" (Geneweb.Util.create_env content_type)

let print_and_cut_if_too_big oc str =
  let rec loop i =
    if i < String.length str then (
      output_char oc str.[i];
      let i =
        if i > 700 && String.length str - i > 750 then (
          Printf.fprintf oc " ... ";
          String.length str - 700)
        else i + 1
      in
      loop i)
  in
  loop 0

type auth_report = {
  ar_ok : bool;
  ar_command : string;
  ar_passwd : string;
  ar_scheme : Geneweb.Config.auth_scheme_kind;
  ar_user : string;
  ar_name : string;
  ar_wizard : bool;
  ar_friend : bool;
  ar_uauth : string;
  ar_can_stale : bool;
}

let log_passwd_failed ar tm from request base_file =
  Log.log @@ fun oc ->
  let referer = Mutil.extract_param "referer: " '\n' request in
  let user_agent = Mutil.extract_param "user-agent: " '\n' request in
  let tm = Unix.localtime tm in
  Printf.fprintf oc "%s (%d) %s_%s => failed (%s)" (Ext_unix.sprintf_date tm)
    (Unix.getpid ()) base_file ar.ar_passwd ar.ar_user;
  if !trace_failed_passwd then
    Printf.fprintf oc " (%s)" (String.escaped ar.ar_uauth);
  Printf.fprintf oc "\n  From: %s\n  Agent: %s\n" from
    (Option.value ~default:"" user_agent);
  Option.iter (Printf.fprintf oc "  Referer: %s\n") referer

let copy_file conf fname =
  match Geneweb.Util.open_etc_file fname with
  | Some (ic, _fname) ->
      (try
         while true do
           let c = input_char ic in
           Geneweb.Output.printf conf "%c" c
         done
       with _ -> ());
      close_in ic;
      true
  | None -> false

let http conf status =
  Geneweb.Output.status conf status;
  Geneweb.Output.header conf "Content-type: text/html; charset=iso-8859-1"

let robots_txt conf =
  Log.syslog `LOG_NOTICE "Robot request";
  Geneweb.Output.status conf Def.OK;
  Geneweb.Output.header conf "Content-type: text/plain";
  if copy_file conf "robots" then ()
  else (
    Geneweb.Output.print_sstring conf "User-Agent: *\n";
    Geneweb.Output.print_sstring conf "Disallow: /\n")

let refuse_log conf from =
  Log.syslog `LOG_NOTICE @@ "Excluded: " ^ from;
  http conf Def.Forbidden;
  Geneweb.Output.header conf "Content-type: text/html";
  Geneweb.Output.print_sstring conf
    "Your access has been disconnected by administrator.\n";
  let _ = (copy_file conf "refuse" : bool) in
  ()

let only_log conf from =
  Log.syslog `LOG_NOTICE @@ "Connection refused from " ^ from;
  http conf Def.OK;
  Geneweb.Output.header conf "Content-type: text/html; charset=iso-8859-1";
  Geneweb.Output.print_sstring conf
    "<head><title>Invalid access</title></head>\n";
  Geneweb.Output.print_sstring conf "<body><h1>Invalid access</h1></body>\n"

let refuse_auth conf from auth auth_type =
  Log.syslog `LOG_NOTICE
  @@ Printf.sprintf
       "Access failed --- From: %s --- Basic realm: %s --- Response: %s" from
       auth_type auth;
  Geneweb.Util.unauthorized conf auth_type

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
          Files.read_or_create_value ~wait:true ~magic:Mutil.random_magic fname
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
                  Mutil.input_lexicon lang ht (fun () ->
                      Secure.open_in (Geneweb.Util.search_in_assets fname)))
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
  Gwd_lib.GwdPlugin.assets := assets;
  (try
     if Sys.file_exists dir then Secure.add_assets assets;
     Dynlink.loadfile plugin
   with Dynlink.Error e ->
     raise (Register_plugin_failure (plugin, `dynlink_error e)));
  Gwd_lib.GwdPlugin.assets := ""

let log_redirect from request req =
  Lock.control
    (Geneweb.SrcfileDisplay.adm_file "gwd.lck")
    true
    ~onerror:(fun () -> ())
    (fun () ->
      let referer = Mutil.extract_param "referer: " '\n' request in
      Log.syslog `LOG_NOTICE
      @@ Printf.sprintf "%s --- From: %s --- Referer: %s" req from
           (Option.value ~default:"" referer))

let print_redirected conf from request new_addr =
  let req = Geneweb.Util.get_request_string conf in
  let link = "http://" ^ new_addr ^ req in
  let env = [ ("link", Mutil.encode link) ] in
  log_redirect from request req;
  Geneweb.Util.include_template conf env "redirect" (fun () ->
      let title _ = Geneweb.Output.print_sstring conf "Address changed" in
      Geneweb.Hutil.header conf title;
      Geneweb.Output.print_sstring conf "Use the following address:\n<p>\n";
      Geneweb.Output.printf conf "<ul><li><a href=\"%s\">%s</a></li></ul>" link
        link;
      Geneweb.Hutil.trailer conf)

let nonce_private_key =
  Lazy.from_fun (fun () ->
      let cnt_dir = Filename.concat !Geneweb.Util.cnt_dir "cnt" in
      let fname = Filename.concat cnt_dir "gwd_private.txt" in
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
  Geneweb.Output.status conf Def.Unauthorized;
  if !use_auth_digest_scheme then
    let nonce = digest_nonce conf.ctime in
    let () =
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
            (Ext_unix.sprintf_date tm)
            (fun oc ->
              List.iter (fun s -> Printf.fprintf oc "  * %s\n" s) conf.request)
            ar.ar_passwd nonce ar.ar_can_stale)
    in
    Geneweb.Output.header conf
      "WWW-Authenticate: Digest realm=\"%s %s\"%s%s,qop=\"auth\"" typ conf.bname
      (if nonce = "" then "" else Printf.sprintf ",nonce=\"%s\"" nonce)
      (if ar.ar_can_stale then ",stale=true" else "")
  else
    Geneweb.Output.header conf "WWW-Authenticate: Basic realm=\"%s %s\"" typ
      conf.bname;
  let url =
    let open Def in
    conf.bname ^<^ "?"
    ^<^ List.fold_left
          (fun s (k, v) ->
            if (s : Adef.encoded_string :> string) = "" then k ^<^ "=" ^<^ v
            else s ^^^ "&" ^<^ k ^<^ "=" ^<^ v)
          (Adef.encoded "")
          (conf.henv @ conf.senv @ conf.env)
  in
  let txt i =
    Geneweb.Util.transl_nth conf "wizard/wizards/friend/friends/exterior" i
  in
  let typ = txt (if ar.ar_passwd = "w" then 0 else 2) in
  let title h =
    Geneweb.Output.printf conf
      (Geneweb.Util.fcapitale
         (Geneweb.Util.ftransl conf "%s access cancelled for that page"))
      (if not h then "<em>" ^ typ ^ "</em>" else typ)
  in
  Geneweb.Hutil.header_without_http conf title ();
  Geneweb.Output.print_sstring conf "<h1>\n";
  title false;
  Geneweb.Output.print_sstring conf "</h1>\n";
  Geneweb.Output.print_sstring conf "<dl>\n";
  (let alt_bind, alt_access =
     if ar.ar_passwd = "w" then ("&w=f", txt 2) else ("&w=w", txt 0)
   in
   Geneweb.Output.print_sstring conf "<dd>\n";
   Geneweb.Output.print_sstring conf "<ul>\n";
   Geneweb.Output.print_sstring conf "<li>\n";
   Geneweb.Output.printf conf "%s : <a href=\"%s%s\">%s</a>"
     (Geneweb.Util.transl conf "access")
     (url : Adef.encoded_string :> string)
     alt_bind alt_access;
   Geneweb.Output.print_sstring conf "</li>\n";
   Geneweb.Output.print_sstring conf "<li>\n";
   Geneweb.Output.printf conf "%s : <a href=\"%s\">%s</a>"
     (Geneweb.Util.transl conf "access")
     (url : Adef.encoded_string :> string)
     (txt 4);
   Geneweb.Output.print_sstring conf "</li>\n";
   Geneweb.Output.print_sstring conf "</ul>\n";
   Geneweb.Output.print_sstring conf "</dd>\n");
  Geneweb.Output.print_sstring conf "</dl>\n";
  Geneweb.Hutil.trailer conf

let gen_match_auth_file test_user_and_password auth_file =
  if auth_file = "" then None
  else
    let aul = Geneweb.Util.read_gen_auth_file auth_file in
    let rec loop = function
      | au :: aul ->
          if test_user_and_password au then
            let s =
              Option.fold
                (String.index_opt au.Geneweb.Util.au_info ':')
                ~some:(fun i -> String.sub au.Geneweb.Util.au_info 0 i)
                ~none:""
            in
            let username =
              Option.fold (String.index_opt s '/')
                ~some:(fun i ->
                  let len = String.length s in
                  String.sub s 0 i ^ String.sub s (i + 1) (len - i - 1))
                ~none:s
            in
            Some username
          else loop aul
      | [] -> None
    in
    loop aul

let basic_match_auth_file uauth =
  gen_match_auth_file (fun au ->
      au.Geneweb.Util.au_user ^ ":" ^ au.Geneweb.Util.au_passwd = uauth)

let digest_match_auth_file asch =
  gen_match_auth_file (fun au ->
      Geneweb.Util.is_that_user_and_password asch au.Geneweb.Util.au_user
        au.Geneweb.Util.au_passwd)

let match_simple_passwd sauth uauth =
  match String.index_opt sauth ':' with
  | Some _ -> sauth = uauth
  | None -> (
      match String.index_opt uauth ':' with
      | Some i -> sauth = String.sub uauth (i + 1) (String.length uauth - i - 1)
      | None -> sauth = uauth)

let basic_match_auth passwd auth_file uauth =
  if passwd <> "" && match_simple_passwd passwd uauth then Some ""
  else basic_match_auth_file uauth auth_file

type access_type =
  | ATwizard of string
  | ATfriend of string
  | ATnormal
  | ATnone
  | ATset

let compatible_tokens check_from (addr1, base1_pw1) (addr2, base2_pw2) =
  ((not check_from) || addr1 = addr2) && base1_pw1 = base2_pw2

let get_actlog check_from utm from_addr base_password =
  let fname = Geneweb.SrcfileDisplay.adm_file "actlog" in
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
          let list, r, changed =
            if utm -. tm >= tmout then (list, r, true)
            else if
              compatible_tokens check_from (addr, db_pwd)
                (from_addr, base_password)
            then
              let r = if c = 'w' then ATwizard user else ATfriend user in
              (((from_addr, db_pwd), (utm, c, user)) :: list, r, true)
            else (((addr, db_pwd), (tm, c, user)) :: list, r, changed)
          in
          loop changed r list
      | exception End_of_file ->
          close_in ic;
          let list =
            List.sort
              (fun (_, (t1, _, _)) (_, (t2, _, _)) -> compare t2 t1)
              list
          in
          (list, r, changed)
    in
    loop false ATnormal []
  with Sys_error _ -> ([], ATnormal, false)

let set_actlog list =
  let fname = Geneweb.SrcfileDisplay.adm_file "actlog" in
  try
    let oc = Secure.open_out fname in
    List.iter
      (fun ((from, base_pw), (a, c, d)) ->
        Printf.fprintf oc "%.0f %s/%s %c%s\n" a from base_pw c
          (if d = "" then "" else " " ^ d))
      list;
    close_out oc
  with Sys_error _ -> ()

let get_token check_from utm from_addr base_password =
  Lock.control
    (Geneweb.SrcfileDisplay.adm_file "gwd.lck")
    true
    ~onerror:(fun () -> ATnormal)
    (fun () ->
      let list, r, changed =
        get_actlog check_from utm from_addr base_password
      in
      if changed then set_actlog list;
      r)

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

let set_token utm from_addr base_file acc user =
  Lock.control
    (Geneweb.SrcfileDisplay.adm_file "gwd.lck")
    true
    ~onerror:(fun () -> "")
    (fun () ->
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
      let list = ((from_addr, xx), (utm, acc, user)) :: list in
      set_actlog list;
      x)

let http_preferred_language request =
  let v = Mutil.extract_param "accept-language: " '\n' request in
  match v with
  | None -> ""
  | Some v ->
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
            if List.mem lang Geneweb.Version.available_languages then lang
            else if String.length lang = 5 then
              let blang = String.sub lang 0 2 in
              if List.mem blang Geneweb.Version.available_languages then blang
              else loop list
            else loop list
        | [] -> ""
      in
      loop list

let allowed_denied_titles key extra_line env base_env () =
  if Geneweb.Util.p_getenv env "all_titles" = Some "on" then []
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
              Ext_string.Set.add line set
          in
          if eof then (
            close_in ic;
            Ext_string.Set.elements set)
          else loop set
        in
        loop Ext_string.Set.empty
    with Not_found | Sys_error _ -> []

let allowed_titles env =
  let extra_line =
    match List.assoc_opt "extra_title" env with
    | Some extra_title -> extra_title
    | None -> Adef.encoded ""
  in
  allowed_denied_titles "allowed_titles_file" extra_line env

let denied_titles = allowed_denied_titles "denied_titles_file" (Adef.encoded "")

let parse_digest s =
  let rec parse_main (strm__ : _ Stream.t) =
    match try Some (ident strm__) with Stream.Failure -> None with
    | Some s ->
        let () =
          try spaces strm__ with Stream.Failure -> raise (Stream.Error "")
        in
        let kvl =
          try key_eq_val_list strm__
          with Stream.Failure -> raise (Stream.Error "")
        in
        if s = "Digest" then kvl else []
    | None -> []
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
    | None -> []
  and key_eq_val_list_kont (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
    | Some ',' ->
        Stream.junk strm__;
        let () =
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
        let () =
          try spaces strm__ with Stream.Failure -> raise (Stream.Error "")
        in
        v
    | _ ->
        let v = any_val 0 strm__ in
        let () =
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
    | None -> raise Stream.Failure
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
    Option.value
      (List.assoc_opt "wizard_passwd" base_env)
      ~default:!wizard_passwd
  in
  let wizard_passwd_file =
    Option.value (List.assoc_opt "wizard_passwd_file" base_env) ~default:""
  in
  let friend_passwd =
    Option.value
      (List.assoc_opt "friend_passwd" base_env)
      ~default:!friend_passwd
  in
  let friend_passwd_file =
    Option.value (List.assoc_opt "friend_passwd_file" base_env) ~default:""
  in
  let passwd1 =
    let auth = Mutil.extract_param "authorization: " '\r' request in
    match auth with
    | None -> ""
    | Some auth ->
        let s = "Basic " in
        if Ext_string.start_with s 0 auth then
          let i = String.length s in
          Geneweb.Base64.decode (String.sub auth i (String.length auth - i))
        else ""
  in
  let uauth = if passwd = "w" || passwd = "f" then passwd1 else passwd in
  let auto = Mutil.extract_param "gw-connection-type: " '\r' request in
  let uauth = if auto = Some "auto" then passwd1 else uauth in

  let ok, wizard, friend, username =
    if (not !Wserver.cgi) && (passwd = "w" || passwd = "f") then
      if passwd = "w" then
        if wizard_passwd = "" && wizard_passwd_file = "" then
          (true, true, friend_passwd = "", "")
        else
          match basic_match_auth wizard_passwd wizard_passwd_file uauth with
          | Some username -> (true, true, false, username)
          | None -> (false, false, false, "")
      else if passwd = "f" then
        if friend_passwd = "" && friend_passwd_file = "" then
          (true, false, true, "")
        else
          match basic_match_auth friend_passwd friend_passwd_file uauth with
          | Some username -> (true, false, true, username)
          | None -> (false, false, false, "")
      else assert false
    else if wizard_passwd = "" && wizard_passwd_file = "" then
      (true, true, friend_passwd = "", "")
    else
      match basic_match_auth wizard_passwd wizard_passwd_file uauth with
      | Some username -> (true, true, false, username)
      | None -> (
          if friend_passwd = "" && friend_passwd_file = "" then
            (true, false, true, "")
          else
            match basic_match_auth friend_passwd friend_passwd_file uauth with
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
        let pwd_id = set_token utm from_addr base_file 'w' user in
        if !Wserver.cgi then (command, pwd_id)
        else (base_file ^ "_" ^ pwd_id, "")
      else if friend then
        let pwd_id = set_token utm from_addr base_file 'f' user in
        if !Wserver.cgi then (command, pwd_id)
        else (base_file ^ "_" ^ pwd_id, "")
      else if !Wserver.cgi then (command, "")
      else (base_file, "")
    else if !Wserver.cgi then (command, passwd)
    else if passwd = "" then
      if auto = Some "auto" then
        let suffix = if wizard then "_w" else if friend then "_f" else "" in
        (base_file ^ suffix, passwd)
      else (base_file, "")
    else (base_file ^ "_" ^ passwd, passwd)
  in
  let auth_scheme =
    if (not wizard) && not friend then Geneweb.Config.NoAuth
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

let test_passwd ds nonce command wf_passwd wf_passwd_file passwd_char wiz =
  let asch = Geneweb.Config.HttpAuth (Digest ds) in
  if
    wf_passwd <> ""
    && Geneweb.Util.is_that_user_and_password asch ds.ds_username wf_passwd
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
    match digest_match_auth_file asch wf_passwd_file with
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
    Option.value
      (List.assoc_opt "wizard_passwd" base_env)
      ~default:!wizard_passwd
  in
  let wizard_passwd_file =
    Option.value (List.assoc_opt "wizard_passwd_file" base_env) ~default:""
  in
  let friend_passwd =
    Option.value
      (List.assoc_opt "friend_passwd" base_env)
      ~default:!friend_passwd
  in
  let friend_passwd_file =
    Option.value (List.assoc_opt "friend_passwd_file" base_env) ~default:""
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
    let auth =
      Option.value ~default:""
        (Mutil.extract_param "authorization: " '\r' request)
    in
    if Ext_string.start_with "Digest " 0 auth then
      let meth =
        match Mutil.extract_param "GET " ' ' request with
        | None -> "POST"
        | Some _ -> "GET"
      in
      let () =
        trace_auth base_env (fun oc ->
            Printf.fprintf oc "\nauth = \"%s\"\n" auth)
      in
      let digenv = parse_digest auth in
      let get_digenv s = Option.value (List.assoc_opt s digenv) ~default:"" in
      let ds =
        {
          Geneweb.Config.ds_username = get_digenv "username";
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
      let () =
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
              (Ext_unix.sprintf_date @@ Unix.localtime utm)
              (fun oc ->
                List.iter (fun s -> Printf.fprintf oc "  * %s\n" s) request)
              passwd nonce ds.ds_meth ds.ds_uri)
      in
      if passwd = "w" then
        test_passwd ds nonce command wizard_passwd wizard_passwd_file "w" true
      else if passwd = "f" then
        test_passwd ds nonce command friend_passwd friend_passwd_file "f" false
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
  | ATwizard user ->
      let command, passwd =
        if !Wserver.cgi then (command, passwd)
        else if passwd = "" then (base_file, "")
        else (base_file ^ "_" ^ passwd, passwd)
      in
      let auth_scheme =
        Geneweb.Config.TokenAuth { ts_user = user; ts_pass = passwd }
      in
      {
        ar_ok = true;
        ar_command = command;
        ar_passwd = passwd;
        ar_scheme = auth_scheme;
        ar_user = user;
        ar_name = "";
        ar_wizard = true;
        ar_friend = false;
        ar_uauth = "";
        ar_can_stale = false;
      }
  | ATfriend user ->
      let command, passwd =
        if !Wserver.cgi then (command, passwd)
        else if passwd = "" then (base_file, "")
        else (base_file ^ "_" ^ passwd, passwd)
      in
      let auth_scheme =
        Geneweb.Config.TokenAuth { ts_user = user; ts_pass = passwd }
      in
      {
        ar_ok = true;
        ar_command = command;
        ar_passwd = passwd;
        ar_scheme = auth_scheme;
        ar_user = user;
        ar_name = "";
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

let dates_format_of_string = function
  | "day_month_year" -> Geneweb.Config.DMY
  | "month_day_year" -> Geneweb.Config.MDY
  | _ -> Geneweb.Config.DMY

let get_client_preferences request =
  let parse_client_preferences preferences =
    let parse_client_preference preference =
      match Ext_string.split_on_char '=' preference with
      | [] | [ _ ] | _ :: _ :: _ :: _ -> None
      | [ key; value ] ->
          Ext_option.return_if
            (List.mem key Geneweb.Util.authorized_client_preference_keys)
            (fun () -> (key, value))
    in
    preferences
    |> Ext_string.split_on_char ','
    |> List.filter_map parse_client_preference
  in
  Option.fold ~none:[] ~some:parse_client_preferences
    (Http.Header.get ~request "Prefer")

let make_conf from_addr request script_name env =
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
      | [] | _ :: _ :: _ :: _ -> assert false
    in
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
  let from, env =
    let x, env = extract_assoc "opt" env in
    match x with
    | "from" -> ("from", env)
    | "" -> ("", env)
    | _ -> ("", ("opt", Mutil.encode x) :: env)
  in
  let threshold_test, env = extract_assoc "threshold" env in
  if threshold_test <> "" then
    Geneweb.RelationLink.threshold := int_of_string threshold_test;
  let base_env =
    get_client_preferences request @ Geneweb.Util.read_base_env ~bname:base_file
  in
  let default_lang =
    Option.fold
      (List.assoc_opt "default_lang" base_env)
      ~some:(fun x -> if x = "" then !default_lang else x)
      ~none:!default_lang
  in
  let lexicon_lang = if lang = "" then default_lang else lang in
  let lexicon = load_lexicon lexicon_lang in
  (* A l'initialisation de la config, il n'y a pas de sosa_ref. *)
  (* Il sera mis à jour par effet de bord dans request.ml       *)
  let default_sosa_ref = (Gwdb.dummy_iper, None) in
  let ar =
    authorization from_addr request base_env passwd access_type utm base_file
      command
  in
  let wizard_just_friend =
    if !wizard_just_friend then true
    else List.assoc_opt "wizard_just_friend" base_env = Some "yes"
  in
  let is_rtl = Hashtbl.find_opt lexicon " !dir" = Some "rtl" in
  let manitou =
    try
      ar.ar_wizard && ar.ar_user <> ""
      && Geneweb.Util.p_getenv env "manitou" <> Some "off"
      && List.assoc_opt "manitou" base_env = Some ar.ar_user
    with Not_found -> false
  in
  let supervisor =
    ar.ar_wizard && ar.ar_user <> ""
    && List.assoc_opt "supervisor" base_env = Some ar.ar_user
  in
  let wizard_just_friend = if manitou then false else wizard_just_friend in
  let dates_format =
    let df_opt = List.assoc_opt "dates_format" base_env in
    let df_opt = Option.map dates_format_of_string df_opt in
    Option.value ~default:Geneweb.Config.DMY df_opt
  in
  let default_contemporary_private_years =
    Option.value ~default:100
      (Option.bind
         (List.assoc_opt "default_contemporary_private_years" base_env)
         int_of_string_opt)
  in
  let conf =
    {
      Geneweb.Config.from = from_addr;
      api_mode = false;
      manitou;
      supervisor;
      wizard = ar.ar_wizard && not wizard_just_friend;
      is_printed_by_template = true;
      debug = !debug;
      friend = ar.ar_friend || (wizard_just_friend && ar.ar_wizard);
      just_friend_wizard = ar.ar_wizard && wizard_just_friend;
      user = ar.ar_user;
      username = ar.ar_name;
      auth_scheme = ar.ar_scheme;
      command = ar.ar_command;
      indep_command = (if !Wserver.cgi then ar.ar_command else "geneweb") ^ "?";
      lang = (if lang = "" then default_lang else lang);
      default_lang;
      default_sosa_ref;
      authorized_wizards_notes =
        List.assoc_opt "authorized_wizards_notes" base_env = Some "yes";
      public_if_titles = List.assoc_opt "public_if_titles" base_env = Some "yes";
      public_if_no_date =
        List.assoc_opt "public_if_no_date" base_env = Some "yes";
      setup_link = !setup_link;
      access_by_key =
        (match List.assoc_opt "access_by_key" base_env with
        | Some access_by_key -> access_by_key = "yes"
        | None -> true);
      private_years =
        Option.value ~default:150
          (Option.bind
             (List.assoc_opt "private_years" base_env)
             int_of_string_opt);
      default_contemporary_private_years;
      hide_private_names =
        List.assoc_opt "hide_private_names" base_env = Some "yes";
      use_restrict =
        (if ar.ar_wizard || ar.ar_friend then false
        else List.assoc_opt "use_restrict" base_env = Some "yes");
      no_image =
        (if ar.ar_wizard || ar.ar_friend then false
        else List.assoc_opt "no_image_for_visitor" base_env = Some "yes");
      no_note =
        (if ar.ar_wizard || ar.ar_friend then false
        else List.assoc_opt "no_note_for_visitor" base_env = Some "yes");
      bname = base_file;
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
        Option.fold
          (List.assoc_opt "auth_file" base_env)
          ~some:(fun x ->
            if x = "" then !auth_file else Geneweb.GWPARAM.bpath x)
          ~none:!auth_file;
      border =
        (match Geneweb.Util.p_getint env "border" with
        | Some i -> i
        | None -> 0);
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
      image_prefix =
        (if !images_url <> "" then !images_url
        else if !Wserver.cgi then
          match Sys.getenv_opt "GW_STATIC_PATH" with
          | Some x -> x ^ "../images"
          | None -> "../distribution/gw/images/"
        else "images");
      static_path =
        (match Sys.getenv_opt "GW_STATIC_PATH" with
        | Some x -> x
        | None -> "../distribution/gw/etc/");
      cgi;
      output_conf;
      forced_plugins = !forced_plugins;
      plugins = !plugins;
      notify_change = !notify_change;
      preferred_countries =
        (let opt =
           Option.map (String.split_on_char ',')
             (List.assoc_opt "autocompletion_countries" base_env)
         in
         match opt with Some [] -> None | Some (_ :: _) | None -> opt);
      dates_format;
    }
  in
  (conf, ar)

let log tm conf from gauth request script_name contents =
  Log.log @@ fun oc ->
  let referer = Mutil.extract_param "referer: " '\n' request in
  let user_agent = Mutil.extract_param "user-agent: " '\n' request in
  let tm = Unix.localtime tm in
  let print_header_if_any header =
    Option.iter
      (Printf.fprintf oc "  %s: %s\n" header)
      (Http.Header.get ~request header)
  in
  Printf.fprintf oc "%s (%d) %s?" (Ext_unix.sprintf_date tm) (Unix.getpid ())
    script_name;
  print_and_cut_if_too_big oc contents;
  output_char oc '\n';
  Printf.fprintf oc "  From: %s\n" from;
  if gauth <> "" then Printf.fprintf oc "  User: %s\n" gauth;
  if conf.Geneweb.Config.wizard && not conf.friend then
    Printf.fprintf oc "  User: %s%s(wizard)\n" conf.user
      (if conf.user = "" then "" else " ")
  else if conf.friend && not conf.wizard then
    Printf.fprintf oc "  User: %s%s(friend)\n" conf.user
      (if conf.user = "" then "" else " ");
  Option.iter (Printf.fprintf oc "  Agent: %s\n") user_agent;
  print_header_if_any "Prefer";
  Option.iter
    (fun referer ->
      Printf.fprintf oc "  Referer: ";
      print_and_cut_if_too_big oc referer;
      Printf.fprintf oc "\n")
    referer

let is_robot from =
  Lock.control
    (Geneweb.SrcfileDisplay.adm_file "gwd.lck")
    true
    ~onerror:(fun () -> false)
    (fun () ->
      let robxcl, _ = Robot.robot_excl () in
      List.mem_assoc from robxcl.Robot.excl)

let auth_err request auth_file =
  if auth_file = "" then (false, "")
  else
    let auth = Mutil.extract_param "authorization: " '\r' request in
    match auth with
    | Some auth -> (
        match
          try Some (Secure.open_in auth_file) with Sys_error _ -> None
        with
        | Some ic -> (
            let auth =
              let i = String.length "Basic " in
              Geneweb.Base64.decode (String.sub auth i (String.length auth - i))
            in
            try
              let rec loop () =
                if auth = input_line ic then (
                  close_in ic;
                  let s =
                    Option.fold
                      (String.rindex_opt auth ':')
                      ~some:(String.sub auth 0) ~none:"..."
                  in
                  (false, s))
                else loop ()
              in
              loop ()
            with End_of_file ->
              close_in ic;
              (true, auth))
        | None -> (true, "(auth file '" ^ auth_file ^ "' not found)"))
    | None -> (true, "(authorization not provided)")

let no_access conf =
  let title _ = Geneweb.Output.print_sstring conf "Error" in
  Geneweb.Hutil.rheader conf title;
  Geneweb.Output.print_sstring conf "No access to this database in CGI mode\n";
  Geneweb.Hutil.trailer conf

let log_and_robot_check conf auth from request script_name contents =
  if !robot_xcl = None then
    log (Unix.time ()) conf from auth request script_name contents
  else
    Lock.control (Geneweb.SrcfileDisplay.adm_file "gwd.lck")
      true ~onerror:ignore (fun () ->
        let tm = Unix.time () in
        (match !robot_xcl with
        | Some (cnt, sec) ->
            let s = "suicide" in
            let suicide = Geneweb.Util.p_getenv conf.env s <> None in
            conf.n_connect <- Some (Robot.check tm from cnt sec conf suicide)
        | None -> ());
        log tm conf from auth request script_name contents)

let conf_and_connection =
  let slow_query_threshold =
    match Sys.getenv_opt "GWD_SLOW_QUERY_THRESHOLD" with
    | Some x -> float_of_string x
    | None -> infinity
  in
  let context conf contents =
    let open Def in
    conf.Geneweb.Config.bname
    ^<^ (if conf.wizard then "_w?" else if conf.friend then "_f?" else "?")
    ^<^ contents
  in
  fun from request script_name (contents : Adef.encoded_string) env ->
    let conf, passwd_err = make_conf from request script_name env in
    match !redirected_addr with
    | Some addr -> print_redirected conf from request addr
    | None -> (
        let auth_err, auth =
          if conf.auth_file = "" then (false, "")
          else if !Wserver.cgi then (true, "")
          else auth_err request conf.auth_file
        in
        let mode = Geneweb.Util.p_getenv conf.env "m" in
        (if mode <> Some "IM" then
         let contents =
           if List.mem_assoc "log_pwd" env then Adef.encoded "..." else contents
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
                  Option.value
                    (List.assoc_opt "auth_file" conf.base_env)
                    ~default:""
                in
                if x = "" then "GeneWeb service" else "database " ^ conf.bname
              in
              refuse_auth conf from auth auth_type
        | _, _, ({ ar_ok = false } as ar) ->
            if is_robot from then Robot.robot_error conf 0 0
            else
              let tm = Unix.time () in
              Lock.control
                (Geneweb.SrcfileDisplay.adm_file "gwd.lck")
                true
                ~onerror:(fun () -> ())
                (fun () -> log_passwd_failed ar tm from request conf.bname);
              unauth_server conf ar
        | _ -> (
            let printexc e =
              Log.syslog `LOG_CRIT
                ((context conf contents :> string) ^ " " ^ Printexc.to_string e)
            in
            try
              let t1 = Unix.gettimeofday () in
              Gwd_lib.Request.treat_request conf;
              let t2 = Unix.gettimeofday () in
              if t2 -. t1 > slow_query_threshold then
                Log.syslog `LOG_WARNING
                  (Printf.sprintf "%s slow query (%.3f)"
                     (context conf contents : Adef.encoded_string :> string)
                     (t2 -. t1))
            with
            | Exit -> ()
            | Def.HttpExn (code, _) as e ->
                Geneweb.GWPARAM.output_error conf code;
                printexc e
            | e -> printexc e))

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
  match (Geneweb.Util.p_getenv env "m", Geneweb.Util.p_getenv env "v") with
  | Some "IM", Some fname ->
      let fname =
        if fname.[0] = '/' then String.sub fname 1 (String.length fname - 1)
        else fname
      in
      let (`Path fname) = Geneweb.Image.path_of_filename fname in
      let _ = Geneweb.ImageDisplay.print_image_file conf fname in
      true
  | _ ->
      let s = script_name in
      if Ext_string.start_with "images/" 0 s then
        let i = String.length "images/" in
        let fname = String.sub s i (String.length s - i) in
        let (`Path fname) = Geneweb.Image.path_of_filename fname in
        let _ = Geneweb.ImageDisplay.print_image_file conf fname in
        true
      else false

(* Une version un peu à cheval entre avant et maintenant afin de   *)
(* pouvoir inclure une css, un fichier javascript (etc) facilement *)
(* et que le cache du navigateur puisse prendre le relais.         *)
type misc_fname =
  | Css of string
  | Eot of string
  | Js of string
  | Otf of string
  | Other of string
  | Png of string
  | Svg of string
  | Ttf of string
  | Woff of string
  | Woff2 of string

let content_misc conf len misc_fname =
  Geneweb.Output.status conf Def.OK;
  let fname, t =
    match misc_fname with
    | Css fname -> (fname, "text/css")
    | Eot fname -> (fname, "application/font-eot")
    | Js fname -> (fname, "text/javascript")
    | Otf fname -> (fname, "application/font-otf")
    | Other fname -> (fname, "text/plain")
    | Png fname -> (fname, "image/png")
    | Svg fname -> (fname, "application/font-svg")
    | Ttf fname -> (fname, "application/font-ttf")
    | Woff fname -> (fname, "application/font-woff")
    | Woff2 fname -> (fname, "application/font-woff2")
  in

  Geneweb.Output.header conf "Content-type: %s" t;
  Geneweb.Output.header conf "Content-length: %d" len;
  Geneweb.Output.header conf "Content-disposition: inline; filename=%s"
    (Filename.basename fname);
  Geneweb.Output.header conf "Cache-control: private, max-age=%d"
    (60 * 60 * 24 * 365);
  Geneweb.Output.flush conf

let find_misc_file name =
  if
    Sys.file_exists name
    && List.exists
         (fun p -> Ext_string.start_with (Filename.concat p "assets") 0 name)
         !plugins
  then name
  else
    let name' = Geneweb.Util.search_in_assets @@ Filename.concat "etc" name in
    if Sys.file_exists name' then name' else ""

let print_misc_file conf misc_fname =
  match misc_fname with
  | Other _ -> false
  | Css fname
  | Eot fname
  | Js fname
  | Otf fname
  | Png fname
  | Svg fname
  | Ttf fname
  | Woff fname
  | Woff2 fname ->
      let ic = Secure.open_in_bin fname in
      let buf = Bytes.create 1024 in
      let len = in_channel_length ic in
      content_misc conf len misc_fname;
      let rec loop len =
        if len = 0 then ()
        else
          let olen = min (Bytes.length buf) len in
          really_input ic buf 0 olen;
          Geneweb.Output.print_sstring conf (Bytes.sub_string buf 0 olen);
          loop (len - olen)
      in
      loop len;
      close_in ic;
      true

let misc_request conf fname =
  let fname = find_misc_file fname in
  if fname <> "" then
    let misc_fname =
      if Filename.check_suffix fname ".css" then Css fname
      else if Filename.check_suffix fname ".js" then Js fname
      else if Filename.check_suffix fname ".otf" then Otf fname
      else if Filename.check_suffix fname ".svg" then Svg fname
      else if Filename.check_suffix fname ".woff" then Woff fname
      else if Filename.check_suffix fname ".eot" then Eot fname
      else if Filename.check_suffix fname ".ttf" then Ttf fname
      else if Filename.check_suffix fname ".woff2" then Woff2 fname
      else if Filename.check_suffix fname ".png" then Png fname
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
    else if i < String.length str && str.[i] = '\n' then i + 1
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
        let env = Geneweb.Util.create_env s in
        match
          ( Geneweb.Util.p_getenv env "name",
            Geneweb.Util.p_getenv env "filename" )
        with
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
            let s, i = next_line i in
            if s = "" then
              let s, i = next_line i in
              (var, Adef.encoded s) :: loop i
            else loop i
        | None, (Some _ | None) -> loop i
      else if s = boundary ^ "--" then []
      else loop i
  in
  let env = loop 0 in
  let str, _ =
    List.fold_left
      (fun (str, sep) (v, x) ->
        if v = "file" then (str, sep)
        else
          let open Def in
          (str ^^^ sep ^<^ v ^<^ "=" ^<^ x, "&"))
      (Adef.encoded "", "")
      env
  in
  (str, env)

let build_env request (contents : Adef.encoded_string) :
    Adef.encoded_string * (string * Adef.encoded_string) list =
  let content_type = Mutil.extract_param "content-type: " '\n' request in
  Option.fold content_type
    ~none:(contents, Geneweb.Util.create_env contents)
    ~some:(fun content_type ->
      if is_multipart_form content_type then
        let boundary =
          (extract_boundary (Adef.encoded content_type)
            : Adef.encoded_string
            :> string)
        in
        extract_multipart boundary contents
      else (contents, Geneweb.Util.create_env contents))

let connection (addr, request) script_name contents0 =
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
        then conf_and_connection from request script_name contents env
      with Exit -> ()

let null_reopen flags fd =
  let fd2 = Unix.openfile "/dev/null" flags 0 in
  Unix.dup2 fd2 fd;
  Unix.close fd2

let geneweb_server () =
  let auto_call = Option.is_some @@ Sys.getenv_opt "WSERVER" in
  if not auto_call then (
    let hostn =
      match !selected_addr with
      | Some addr -> addr
      | None -> ( try Unix.gethostname () with _ -> "computer")
    in
    Printf.eprintf "GeneWeb %s - " Geneweb.Version.txt;
    if not !daemon then (
      Printf.eprintf
        "Possible addresses:\n\
         http://localhost:%d/base\n\
         http://127.0.0.1:%d/base\n\
         http://%s:%d/base\n"
        !selected_port !selected_port hostn !selected_port;
      Printf.eprintf
        "where \"base\" is the name of the database\n\
         Type %s to stop the service\n"
        "control C");
    flush stderr;
    if !daemon then
      if Unix.fork () = 0 then (
        Unix.close Unix.stdin;
        null_reopen [ Unix.O_WRONLY ] Unix.stdout;
        null_reopen [ Unix.O_WRONLY ] Unix.stderr)
      else exit 0;
    Files.mkdir_p ~perm:0o777 (Filename.concat !Geneweb.Util.cnt_dir "cnt"));
  let max_clients = !max_clients in
  Wserver.f ~syslog:Log.syslog ~addr:!selected_addr ~port:!selected_port
    ~timeout:!conn_timeout ~max_clients ~handler:connection

let cgi_timeout conf tmout _ =
  Geneweb.Output.header conf "Content-type: text/html; charset=iso-8859-1";
  Geneweb.Output.print_sstring conf "<head><title>Time out</title></head>\n";
  Geneweb.Output.print_sstring conf "<body><h1>Time out</h1>\n";
  Geneweb.Output.printf conf "Computation time > %d second(s)\n" tmout;
  Geneweb.Output.print_sstring conf "</body>\n";
  Geneweb.Output.flush conf;
  exit 0

let manage_cgi_timeout tmout =
  if tmout > 0 then
    let _ =
      Sys.signal Sys.sigalrm
        (Sys.Signal_handle (cgi_timeout printer_conf tmout))
    in
    let _ = Unix.alarm tmout in
    ()

let geneweb_cgi addr script_name contents =
  manage_cgi_timeout !conn_timeout;
  (try Unix.mkdir (Filename.concat !Geneweb.Util.cnt_dir "cnt") 0o755
   with Unix.Unix_error (_, _, _) -> ());
  let add k x request =
    Option.value
      (Option.bind (Sys.getenv_opt x) (fun v ->
           Ext_option.return_if (v <> "") (fun () -> (k ^ ": " ^ v) :: request)))
      ~default:request
  in
  let request = [] in
  let request = add "cookie" "HTTP_COOKIE" request in
  let request = add "content-type" "CONTENT_TYPE" request in
  let request = add "accept-language" "HTTP_ACCEPT_LANGUAGE" request in
  let request = add "referer" "HTTP_REFERER" request in
  let request = add "user-agent" "HTTP_USER_AGENT" request in
  connection (Unix.ADDR_UNIX addr, request) script_name contents

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
      match list with
      | [ x ] -> Gutil.arg_list_of_string x
      | [] | _ :: _ :: _ -> list
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

let make_cnt_dir x =
  Files.mkdir_p x;
  Geneweb.Util.cnt_dir := x

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
                match Hashtbl.find_opt deps_ht pname with
                | Some s ->
                    if unsafe then unsafe_plugins := !unsafe_plugins @ [ s ];
                    if force then forced_plugins := !forced_plugins @ [ pname ];
                    plugins := !plugins @ [ s ]
                | None ->
                    raise
                      (Register_plugin_failure (pname, `string "Missing plugin")))
              deps),
    arg_plugin_doc opt doc )

let main () =
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let force_cgi = ref false in
  let speclist =
    [
      ( "-hd",
        Arg.String Secure.add_assets,
        "<DIR> Directory where the directory lang is installed." );
      ( "-bd",
        Arg.String Secure.set_base_dir,
        "<DIR> Directory where the databases are installed." );
      ("-wd", Arg.String make_cnt_dir, "<DIR> Directory for access count.");
      ( "-cache_langs",
        Arg.String
          (fun s ->
            List.iter (Ext_list.ref_append cache_langs)
            @@ String.split_on_char ',' s),
        " Lexicon languages to be cached." );
      ("-cgi", Arg.Set force_cgi, " Force CGI mode.");
      ( "-images_url",
        Arg.String (fun x -> images_url := x),
        "<URL> URL for GeneWeb images (default: gwd send them)." );
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
        Arg.String (fun x -> Geneweb.Util.allowed_tags_file := x),
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
        Arg.String (Ext_list.ref_append lexicon_list),
        "<FILE> Add file as lexicon." );
      ( "-log",
        Arg.String
          (fun x ->
            Log.oc :=
              Some
                (match x with
                | "-" | "<stdout>" -> stdout
                | "<stderr>" -> stderr
                | _ -> open_out x)),
        {|<FILE> Log trace to this file. Use "-" or "<stdout>" to redirect output to stdout or "<stderr>" to output log to stderr.|}
      );
      ( "-log_level",
        Arg.Set_int Log.verbosity,
        {|<N> Send messages with severity <= <N> to syslog (default: |}
        ^ string_of_int !Log.verbosity
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
      ( "-debug",
        Arg.Unit
          (fun () ->
            debug := true;
            Log.debug := true;
            Printexc.record_backtrace true),
        " Enable debug mode" );
      ( "-nolock",
        Arg.Set Lock.no_lock_flag,
        " Do not lock files before writing." );
      arg_plugin "-plugin" "<PLUGIN>.cmxs load a safe plugin.";
      arg_plugins "-plugins" "<DIR> load all plugins in <DIR>.";
      ( "-notify_change",
        Arg.String (fun x -> notify_change := Some x),
        "<FILE> Use given path to file as the command to be executed upon \
         changes made in a base" );
      ( "-max_clients",
        Arg.Int (fun x -> max_clients := Some x),
        "<NUM> Max number of clients treated at the same time (default: no \
         limit) (not cgi)." );
      ( "-conn_tmout",
        Arg.Int (fun x -> conn_timeout := x),
        "<SEC> Connection timeout (default "
        ^ string_of_int !conn_timeout
        ^ "s; 0 means no limit)." );
      ("-daemon", Arg.Set daemon, " Unix daemon mode.");
    ]
  in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
  (default_lang :=
     let s = Option.value (Sys.getenv_opt "LANG") ~default:"" in
     if List.mem s Geneweb.Version.available_languages then s
     else
       let s = Option.value (Sys.getenv_opt "LC_CTYPE") ~default:"" in
       if String.length s >= 2 then
         let s = String.sub s 0 2 in
         if List.mem s Geneweb.Version.available_languages then s else "en"
       else "en");
  arg_parse_in_file
    (chop_extension Sys.argv.(0) ^ ".arg")
    speclist anonfun usage;
  Arg.parse speclist anonfun usage;
  Geneweb.GWPARAM.set_syslog Log.syslog;
  List.iter register_plugin !plugins;
  Geneweb.GWPARAM.init ();
  cache_lexicon ();
  (if !images_dir <> "" then
   let abs_dir =
     let f =
       Geneweb.Util.search_in_assets (Filename.concat !images_dir "gwback.jpg")
     in
     let d = Filename.dirname f in
     if Filename.is_relative d then Filename.concat (Sys.getcwd ()) d else d
   in
   images_url := "file://" ^ slashify abs_dir);
  if !Geneweb.Util.cnt_dir = Filename.current_dir_name then
    Geneweb.Util.cnt_dir := Secure.base_dir ();
  Wserver.stop_server :=
    List.fold_left Filename.concat !Geneweb.Util.cnt_dir
      [ "cnt"; "STOP_SERVER" ];
  let query, cgi =
    match Sys.getenv_opt "QUERY_STRING" with
    | Some query -> (Adef.encoded query, true)
    | None -> ("" |> Adef.encoded, !force_cgi)
  in
  if cgi then (
    Wserver.cgi := true;
    let query =
      if Sys.getenv_opt "REQUEST_METHOD" = Some "POST" then (
        let len =
          Option.value ~default:(-1)
            (Option.bind (Sys.getenv_opt "CONTENT_LENGTH") int_of_string_opt)
        in
        set_binary_mode_in stdin true;
        read_input len |> Adef.encoded)
      else query
    in
    let addr =
      match Sys.getenv_opt "REMOTE_HOST" with
      | Some addr -> addr
      | None -> Option.value (Sys.getenv_opt "REMOTE_ADDR") ~default:""
    in
    let script =
      match Sys.getenv_opt "SCRIPT_NAME" with
      | Some script -> script
      | None -> Sys.argv.(0)
    in
    geneweb_cgi addr (Filename.basename script) query)
  else geneweb_server ()

let () =
  try main () with
  | Unix.Unix_error (Unix.EADDRINUSE, "bind", _) ->
      Printf.eprintf "\nError: ";
      Printf.eprintf "the port %d" !selected_port;
      Printf.eprintf
        " is already used by another GeneWeb daemon or by another program. \
         Solution: kill the other program or launch GeneWeb with another port \
         number (option -p)";
      flush stderr
  | Unix.Unix_error (Unix.EACCES, "bind", arg) ->
      Printf.eprintf
        "Error: invalid access to the port %d: users port number less than \
         1024 are reserved to the system. Solution: do it as root or choose \
         another port number greater than 1024."
        !selected_port;
      flush stderr
  | Register_plugin_failure (p, `dynlink_error e) ->
      Log.syslog `LOG_CRIT (p ^ ": " ^ Dynlink.error_message e)
  | Register_plugin_failure (p, `string s) -> Log.syslog `LOG_CRIT (p ^ ": " ^ s)
  | e -> Log.syslog `LOG_CRIT (Printexc.to_string e)
