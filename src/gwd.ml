(* camlp4r pa_extend.cmo ./pa_html.cmo *)
(* $Id: gwd.ml,v 1.29 1999-02-11 09:34:35 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;
open Gutil;
open Unix;

value port_selected = ref 2317;
value wizard_passwd = ref "";
value friend_passwd = ref "";
value only_address = ref "";
value cgi = ref False;
value default_lang = ref "fr";
value log_file = ref "";
value log_flags =
  [Open_wronly; Open_append; Open_creat; Open_text; Open_nonblock]
;
ifdef UNIX then
value max_clients = ref None;
value auth_file = ref "";

value log_oc () =
  if log_file.val <> "" then open_out_gen log_flags 0o644 log_file.val
  else Pervasives.stderr
;

value flush_log oc =
  if log_file.val <> "" then close_out oc else flush oc
;

value is_multipart_form =
  let s = "multipart/form-data" in
  fun content_type ->
    loop 0 where rec loop i =
      if i >= String.length content_type then False
      else if i >= String.length s then True
      else if content_type.[i] == s.[i] then loop (i + 1)
      else False
;

value log from request s =
  let content_type = Wserver.extract_param "content-type: " '\n' request in
  let s = if is_multipart_form content_type then "(multipart form)" else s in
  let referer = Wserver.extract_param "referer: " '\n' request in
  let user_agent = Wserver.extract_param "user-agent: " '\n' request in
  let oc = log_oc () in
  do let tm = Unix.localtime (Unix.time ()) in
     Printf.fprintf oc "%02d/%02d/%4d %02d:%02d:%02d" tm.Unix.tm_mday
       (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour
       tm.Unix.tm_min tm.Unix.tm_sec;
     Printf.fprintf oc " (%d)" (Unix.getpid ());
     Printf.fprintf oc " %s\n" s;
     Printf.fprintf oc "  From: %s\n" from;
     Printf.fprintf oc "  Agent: %s\n" user_agent;
     if referer <> "" then Printf.fprintf oc "  Referer: %s\n" referer else ();
     flush_log oc;
  return ()
;

value nl () =
  Wserver.wprint "\r\n"
;

value refuse_log from cgi =
  let oc = open_out_gen log_flags 0o644 "refuse_log" in
  do let tm = Unix.localtime (Unix.time ()) in
     Printf.fprintf oc "%02d/%02d/%4d %02d:%02d:%02d" tm.Unix.tm_mday
       (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour
       tm.Unix.tm_min tm.Unix.tm_sec;
      Printf.fprintf oc " excluded: %s\n" from;
     close_out oc;
     if not cgi then
       do Wserver.wprint "HTTP/1.0 403 Forbidden"; nl (); nl (); return ()
     else ();
     Wserver.wprint "Your access has been disconnected by administrator.\n";
  return ()
;

value only_log from cgi =
  let oc = log_oc () in
  do let tm = Unix.localtime (Unix.time ()) in
     Printf.fprintf oc "%02d/%02d/%4d %02d:%02d:%02d" tm.Unix.tm_mday
       (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour
       tm.Unix.tm_min tm.Unix.tm_sec;
     Printf.fprintf oc " Connection refused from %s (only %s)\n"
       from only_address.val;
     flush_log oc;
     if cgi then
       do Wserver.wprint "Content-type: text/html; charset=iso-8859-1";
          nl (); nl ();
       return ()
     else Wserver.html "";
     Wserver.wprint "<head><title>Invalid access</title></head>\n";
     Wserver.wprint "<body><h1>Invalid access</h1></body>\n";
  return ()
;

value refuse_auth from auth =
  let oc = log_oc () in
  do let tm = Unix.localtime (Unix.time ()) in
     Printf.fprintf oc "%02d/%02d/%4d %02d:%02d:%02d" tm.Unix.tm_mday
       (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour
       tm.Unix.tm_min tm.Unix.tm_sec;
     Printf.fprintf oc " Access failed from %s = %s\n" from auth;
     flush_log oc;
     Wserver.wprint "HTTP/1.0 401 Unauthorized"; nl ();
     Wserver.wprint "WWW-Authenticate: Basic realm=\"Private\"";
     nl (); nl ();
     Wserver.wprint "<head><title>Access failed</title></head>\n";
     Wserver.wprint "<body><h1>Access failed</h1></body>\n";
  return ()
;

value index c s =
  loop 0 where rec loop i =
    if i == String.length s then i else if s.[i] == c then i else loop (i + 1)
;

value rec extract_assoc key =
  fun
  [ [] -> ("", [])
  | [((k, v) as kv) :: kvl] ->
      if k = key then (v, kvl)
      else
        let (v, kvl) = extract_assoc key kvl in
        (v, [kv :: kvl]) ]
;

value input_lexicon lang =
  let t = Hashtbl.create 501 in
  try
    let ic =
      open_in
        (List.fold_right Filename.concat [Util.lang_dir.val; "lang"]
           "lexicon.txt")
    in
    let pref = lang ^ ": " in
    try
      do try
           while True do
             let k = input_line ic in
             loop (input_line ic) where rec loop line =
               if String.length line < 4 then ()
               else if line.[0] == '#' then ()
               else
                 do if String.sub line 0 4 = pref then
                      Hashtbl.add t (String.sub k 4 (String.length k - 4))
                        (String.sub line 4 (String.length line - 4))
                    else ();
                 return loop (input_line ic);
           done
         with [ End_of_file -> () ];
         close_in ic;
      return t
    with e -> do close_in ic; return raise e
  with
  [ Sys_error _ -> t ]
;

value rec cut_at_equal i s =
  if i = String.length s then (s, "")
  else if s.[i] == '=' then
    (String.sub s 0 i, String.sub s (succ i) (String.length s - succ i))
  else cut_at_equal (succ i) s
;

value read_base_env bname =
  let fname = Filename.concat Util.base_dir.val bname ^ ".cnf" in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let env =
        loop [] where rec loop env =
          match try Some (input_line ic) with [ End_of_file -> None ] with
          [ Some s ->
              if s = "" || s.[0] = '#' then loop env
              else loop [cut_at_equal 0 s :: env]
          | None ->
              env ]
      in
      env
  | None -> [] ]
;

type choice 'a 'b = [ Left of 'a | Right of 'b ];

value print_renamed conf new_n =
  let  title _ =
    Wserver.wprint "%s -&gt; %s" conf.bname new_n
  in
  let link =
    let req = Util.get_request_string conf in
    let new_req =
      let len = String.length conf.bname in
      loop 0 where rec loop i =
        if i > String.length req then ""
        else if i >= len && String.sub req (i - len) len = conf.bname then
          String.sub req 0 (i - len) ^ new_n ^
          String.sub req i (String.length req - i)
        else loop (i + 1)
    in
    "http://" ^ Util.get_server_string conf ^ new_req
  in
  do Util.header conf title;
     Wserver.wprint "The database \"%s\" has been renamed \"%s\".\n"
       conf.bname new_n;
     Wserver.wprint "Please use now:\n<p>\n";
     tag "ul" begin
       Wserver.wprint "<li>\n";
       tag "a" "href=\"%s\"" link begin
         Wserver.wprint "%s" link;
       end;
     end;
     Util.trailer conf;
  return ()
;

value start_with_base conf bname =
  let bfile = Filename.concat Util.base_dir.val bname in
  match try Left (Iobase.input bfile) with e -> Right e with
  [ Left base ->
      do Family.family conf base;
         Wserver.wflush ();
      return ()
  | Right e ->
      let transl conf w =
        try Hashtbl.find conf.lexicon w with [ Not_found -> "[" ^ w ^ "]" ]
      in
      let title _ =
        Wserver.wprint "%s" (Util.capitale (transl conf "error"))
      in
      do Util.header conf title;
         Wserver.wprint "<ul><li>%s"
           (Util.capitale (transl conf "cannot access base"));
         Wserver.wprint " \"%s\".</ul>\n" conf.bname;
         match e with
         [ Failure s ->
             Wserver.wprint
               "<em><font size=-1>Internal message: %s</font></em>\n" s
         | _ -> () ];
         Wserver.wprint "</body>\n";
      return () ]
;

value propose_base conf =
  let title _ = Wserver.wprint "Base" in
  do Util.header conf title;
     Wserver.wprint "<ul><li>\n";
     Wserver.wprint "<form method=get action=\"%s\">\n" conf.command;
     Wserver.wprint "<input name=b size=40> =&gt;\n";
     Wserver.wprint "<input type=submit value=\"Ok\">\n";
     Wserver.wprint "</ul>\n";
     Util.trailer conf;
  return ()
;

value unauth bname typ =
  do Wserver.wprint "HTTP/1.0 401 Unauthorized"; nl ();
     Wserver.wprint "WWW-Authenticate: Basic realm=\"%s %s\"" typ bname;
     nl (); nl ();
     Wserver.wprint "<head><title>%s %s access failed</title></head>\n"
       typ bname;
     Wserver.wprint "<body><h1>%s %s access failed</h1></body>\n" typ bname;
  return ()
;

value match_auth sauth uauth =
  if sauth = "" then True
  else
    match lindex sauth ':' with
    [ Some _ -> sauth = uauth
    | None ->
        match lindex uauth ':' with
        [ Some i ->
            sauth = String.sub uauth (i + 1) (String.length uauth - i - 1)
        | None -> sauth = uauth ] ]
;

value connection_accepted cgi (addr, request) str env =
  let tm = Unix.localtime (Unix.time ()) in
  let iq = index '?' str in
  let (command, base_file, passwd, env) =
    let (base_passwd, env) =
      let (x, env) = extract_assoc "b" env in
      if x <> "" || cgi then (x, env) else (String.sub str 0 iq, env)
    in
    let ip = index '_' base_passwd in
    let base_file = String.sub base_passwd 0 ip in
    let (passwd, env) =
      let has_passwd = List.mem_assoc "w" env in
      let (x, env) = extract_assoc "w" env in
      if has_passwd then (x, env)
      else
        let passwd =
          if ip = String.length base_passwd then ""
          else
            String.sub base_passwd (ip + 1)
              (String.length base_passwd - ip - 1)
        in
        (passwd, env)
    in
    let command =
      if cgi then String.sub str 0 iq
      else if passwd = "" then base_file
      else base_file ^ "_" ^ passwd
    in
    (command, base_file, passwd, env)
  in
  let (lang, env) = extract_assoc "lang" env in
  let (from, env) =
    match extract_assoc "opt" env with
    [ ("from", env) -> ("from", env)
    | ("", env) -> ("", env)
    | (x, env) -> ("", [("opt", x) :: env]) ]
  in
let (threshold_test, env) = extract_assoc "th" env in
do if threshold_test <> "" then RelationLink.threshold.val := int_of_string threshold_test else (); return
  let (sleep, env) =
    let (x, env) = extract_assoc "sleep" env in
    (if x = "" then 0 else int_of_string x, env)
  in
  let base_env = read_base_env base_file in
  let default_lang =
    try List.assoc "default_lang" base_env with
    [ Not_found -> default_lang.val ]
  in
  let lexicon = input_lexicon (if lang = "" then default_lang else lang) in
  let charset =
    try Hashtbl.find lexicon " !charset" with [ Not_found -> "iso-8859-1" ]
  in
  let real_wizard_passwd =
    try List.assoc "wizard_passwd" base_env with
    [ Not_found -> wizard_passwd.val ]
  in
  let real_friend_passwd =
    try List.assoc "friend_passwd" base_env with
    [ Not_found -> friend_passwd.val ]
  in
  let wizard_just_friend =
    try List.assoc "wizard_just_friend" base_env = "yes" with
    [ Not_found -> False ]
  in
  let (ok, wizard, friend) =
    if not cgi then
      if passwd = "w" then
        if real_wizard_passwd = "" then (True, True, real_friend_passwd = "")
        else
          let auth = Wserver.extract_param "authorization: " '\r' request in
          if auth = "" then (False, False, False)
          else
            let uauth =
              let i = String.length "Basic " in
              Base64.decode (String.sub auth i (String.length auth - i))
            in
            if match_auth real_wizard_passwd uauth then
               (True, True, False)
            else (False, False, False)
      else if passwd = "f" then
        if real_friend_passwd = "" then (True, False, True)
        else
          let auth = Wserver.extract_param "authorization: " '\r' request in
          if auth = "" then (False, False, False)
          else
            let uauth =
              let i = String.length "Basic " in
              Base64.decode (String.sub auth i (String.length auth - i))
            in
            if match_auth real_friend_passwd uauth then
               (True, False, True)
            else (False, False, False)
      else (True, passwd = real_wizard_passwd, passwd = real_friend_passwd)
    else (True, passwd = real_wizard_passwd, passwd = real_friend_passwd)
  in
  if not ok then
    unauth base_file (if passwd = "w" then "Wizard" else "Friend")
  else
  let conf =
    {wizard = wizard && not wizard_just_friend;
     friend = friend || wizard_just_friend && wizard;
     cgi = cgi;
     command = command;
     lang = if lang = "" then default_lang else lang;
     bname = base_file;
     env = env;
     senv = [];
     henv =
       (if not cgi then []
        else if passwd = "" then [("b", base_file)]
        else [("b", base_file ^ "_" ^ passwd)]) @
       (if lang = "" then [] else [("lang", lang)]) @
       (if from = "" then [] else [("opt", from)]);
     base_env = base_env;
     request = request;
     lexicon = lexicon;
     charset = charset;
     today =
       {day = tm.Unix.tm_mday;
        month = succ tm.Unix.tm_mon;
        year = tm.Unix.tm_year + 1900;
        prec = Sure};
     today_wd = tm.Unix.tm_wday}
  in
  if conf.bname = "" then propose_base conf
  else
    match
      try Some (List.assoc "renamed" base_env) with [ Not_found -> None ]
    with
    [ Some n -> print_renamed conf n
    | _ ->
        do start_with_base conf conf.bname;
           if sleep > 0 then Unix.sleep sleep else ();
        return () ]
;

value chop_extension name =
  loop (String.length name - 1) where rec loop i =
    if i < 0 then name
    else if name.[i] == '.' then String.sub name 0 i
    else if name.[i] == '/' then name
    else if name.[i] == '\\' then name
    else loop (i - 1)
;

value excluded from =
  let efname = chop_extension Sys.argv.(0) ^ ".xcl" in
  match try Some (open_in efname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop () where rec loop () =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some line ->
            if from = line then do close_in ic; return True else loop ()
        | None -> do close_in ic; return False ]
  | None -> False ]
;

value content_image cgi t len =
  do if not cgi then
       do Wserver.wprint "HTTP/1.0 200 OK"; nl (); return ()
     else ();
     Wserver.wprint "Content-type: image/%s" t; nl ();
     Wserver.wprint "Content-length: %d" len; nl ();
     nl ();
     Wserver.wflush ();
  return ()
;

value print_image cgi bname str t =
  let fname =
    let fname1 =
      List.fold_right Filename.concat [Util.base_dir.val; "images"; bname] str
    in
    let fname2 =
      List.fold_right Filename.concat [Util.lang_dir.val; "images"] str
    in
    let fname3 =
      List.fold_right Filename.concat [Util.base_dir.val; "images"] str
    in
    if Sys.file_exists fname1 then fname1
    else if Sys.file_exists fname2 then fname2
    else fname3
  in
  match try Some (open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      do try
           do content_image cgi t (in_channel_length ic);
              try
                let b = " " in
                while True do
                  b.[0] := input_char ic;
                  Wserver.wprint "%c" b.[0];
                done
              with [ End_of_file -> () ];
           return ()
         with e -> do close_in ic; return raise e;
         close_in ic;
      return ()
 | None -> () ]
;

value image_request cgi str env =
  let bname =
    let (x, _) = extract_assoc "b" env in
    if x <> "" || cgi then
      let ip = index '_' x in
      String.sub x 0 ip
    else
      let iq = index '?' str in
      let b = String.sub str 0 iq in
      let ip = index '_' b in
      String.sub str 0 (min ip iq)
  in
  match (Util.p_getenv env "m", Util.p_getenv env "v") with
  [ (Some "IM", Some fname) ->
      do if Filename.is_implicit fname then
           if Filename.check_suffix fname ".jpg"
           || Filename.check_suffix fname ".JPG" then
             print_image cgi bname fname "jpeg"
           else if Filename.check_suffix fname ".gif"
           || Filename.check_suffix fname ".GIF" then
             print_image cgi bname fname "gif"
           else ()
         else ();
      return True
  | _ -> False ]
;

value check_auth request =
  if auth_file.val = "" then None
  else
    match try Some (open_in auth_file.val) with [ Sys_error _ -> None ] with
    [ Some ic ->
        let auth = Wserver.extract_param "authorization: " '\r' request in
        let auth =
          if auth <> "" then
            let i = String.length "Basic " in
            Base64.decode (String.sub auth i (String.length auth - i))
          else auth
        in
        try
          loop () where rec loop () =
            if auth = input_line ic then do close_in ic; return None
            else loop ()
        with
        [ End_of_file -> do close_in ic; return Some auth ]
    | _ -> None ]
;

value connection cgi (addr, request) str =
  let from =
    match addr with
    [ ADDR_UNIX x -> x
    | ADDR_INET iaddr port ->
         try (gethostbyaddr iaddr).h_name with _ -> string_of_inet_addr iaddr ]
  in
  if excluded from then refuse_log from cgi
  else
    let check = if cgi then None else check_auth request in
    match check with
    [ Some auth -> refuse_auth from auth
    | _ ->
        let accept =
          if only_address.val = "" then True else only_address.val = from
        in
        if not accept then only_log from cgi
        else
          try
            let iq = index '?' str in
            let env =
              let query_string =
                if iq == String.length str then ""
                else String.sub str (iq + 1) (String.length str - iq - 1)
              in
              Util.create_env query_string
            in
            if image_request cgi str env then ()
            else
              do if cgi && log_file.val = "" then ()
                 else log from request str;
                 connection_accepted cgi (addr, request) str env;
              return ()
          with
          [ Exit -> () ] ]
;

value tmout = 120;

value geneweb_server () =
  let hostn = try Unix.gethostname () with _ -> "computer" in
  let auto_call =
    try let _ = Sys.getenv "WSERVER" in True with [ Not_found -> False ]
  in
  do if not auto_call then
       do Printf.eprintf "GeneWeb %s - " Gutil.version;
          Printf.eprintf "Copyright (c) INRIA 1999
Possible addresses:
   http://localhost:%d/base
   http://127.0.0.1:%d/base
   http://%s:%d/base
where \"base\" is the name of the data base
Type control C to stop the service
"
            port_selected.val port_selected.val hostn port_selected.val;
          flush Pervasives.stderr;
          try Unix.mkdir (Filename.concat Util.base_dir.val "cnt") 0o755 with
          [ Unix.Unix_error _ _ _ -> () ];
       return ()
     else ();
  return
  Wserver.f port_selected.val tmout
    (ifdef UNIX then max_clients.val else None) (connection False)
;

value geneweb_cgi str addr =
  do try Unix.mkdir (Filename.concat Util.base_dir.val "cnt") 0o755 with
     [ Unix.Unix_error _ _ _ -> () ];
  return
  let add v x request =
    try [v ^ ": " ^ Sys.getenv x :: request] with [ Not_found -> request ]
  in
  let request = [] in
  let request = add "user-agent" "HTTP_USER_AGENT" request in
  let request = add "referer" "HTTP_REFERER" request in
  connection True (ADDR_UNIX addr, request) str
;

value read_input len =
  if len >= 0 then
    let buff = String.create len in
    do really_input Pervasives.stdin buff 0 len; return buff
  else
    let buff = ref "" in
    do try
         while True do
           let l = input_line Pervasives.stdin in
           do Printf.eprintf "POST: %s\n" l; flush Pervasives.stderr; return
           buff.val := buff.val ^ l;
         done
       with
       [ End_of_file -> () ];
    return buff.val
;

value old_arg_parse_in_file lines =
  match lines with
  [ [x :: lines] ->
       do Util.lang_dir.val := x; return
       match lines with
       [ [x :: lines] ->
           do Util.base_dir.val := x; return
           match lines with
           [ [x :: _] -> cgi.val := x = "cgi"
           | _ -> () ]
       | _ -> () ]
  | _ -> () ]
;

value arg_parse_in_file fname speclist anonfun errmsg =
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let list =
        let list = ref [] in
        do try
             while True do list.val := [input_line ic :: list.val]; done
           with [ End_of_file -> () ];
           close_in ic;
        return List.rev list.val
      in
      match list with
      [ [x :: l] when String.length x > 0 && x.[0] == '-' ->
          Argl.parse_list speclist anonfun errmsg list
      | _ -> old_arg_parse_in_file list ]
  | _ -> () ]
;

value main () =
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [options] where options are:" in
  let speclist =
    [("-hd", Arg.String (fun x -> Util.lang_dir.val := x),
      "<dir>
       Directory where the directory lang is installed.");
     ("-bd", Arg.String (fun x -> Util.base_dir.val := x),
      "dir>
       Directory where the databases are installed.");
     ("-cgi", Arg.Set cgi,
      "
       Force cgi mode.");
     ("-p", Arg.Int (fun x -> port_selected.val := x),
      "<number>
       Select a port number (default = " ^ string_of_int port_selected.val ^
       "); > 1024 for normal users.");
     ("-wizard", Arg.String (fun x -> wizard_passwd.val := x),
      "<passwd>
       Set a wizard password: access to all dates and updating.");
     ("-friend", Arg.String (fun x -> friend_passwd.val := x),
      "<passwd>
       Set a friend password: access to all dates.");
     ("-lang", Arg.String (fun x -> default_lang.val := x),
      "<lang>
       Set a default language (default: fr).");
     ("-only", Arg.String (fun x -> only_address.val := x),
      "<address>
       Only inet address accepted.");
     ("-auth", Arg.String (fun x -> auth_file.val := x),
      "<file>
       Authorization file to restrict access. The file must hold lines
       of the form \"user:password\".");
     ("-log", Arg.String (fun x -> log_file.val := x),
      "<file>
       Redirect log trace to this file.");
     ("-nolock", Arg.Set Lock.no_lock_flag,
      "
       Do not lock files before writing.") ::
     ifdef UNIX then
       [("-max_clients",
         Arg.String
           (fun x ->
              try max_clients.val := Some (int_of_string x) with _ ->
                raise (Arg.Bad "number expected after -max_clients")),
         "<num>
       Max number of clients treated at the same time (default: no limit)
       (not cgi).")]
     else []]
  in
  let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
  do arg_parse_in_file (chop_extension Sys.argv.(0) ^ ".arg")
       speclist anonfun usage;
     Argl.parse speclist anonfun usage;
  return
  let (query, cgi) =
    try (Sys.getenv "QUERY_STRING", True) with
    [ Not_found -> ("", cgi.val) ]
  in
  if cgi then
    let is_post =
      try Sys.getenv "REQUEST_METHOD" = "POST" with
      [ Not_found -> False ]
    in
    let query =
      if is_post then
        let len =
          try int_of_string (Sys.getenv "CONTENT_LENGTH") with
          [ Not_found -> -1 ]
        in
        read_input len
      else query
    in
    let addr =
      try Sys.getenv "REMOTE_HOST" with
      [ Not_found ->
          try Sys.getenv "REMOTE_ADDR" with
          [ Not_found -> "" ] ]
    in
    let script =
      try Sys.getenv "SCRIPT_NAME" with
      [ Not_found -> Sys.argv.(0) ]
    in
    let query = Filename.basename script ^ "?" ^ query in
    geneweb_cgi query addr
  else geneweb_server ()
;

ifdef UNIX then
value test_eacces_bind err fun_name =
  if err = EACCES && fun_name = "bind" && port_selected.val <= 1024 then
    do Printf.eprintf "\n\
Error: invalid access to the port %d: users port number less than 1024
are reserved to the system. Solution: become root or choose another port
number greater than 1024.\n" port_selected.val;
       flush Pervasives.stderr;
    return True
  else False
else
value test_eacces_bind err fun_name = False;

value print_exc exc =
  match exc with
  [ Unix_error EADDRINUSE "bind" _ ->
      do Printf.eprintf "\n\
Error: the port %d is already used by another GeneWeb daemon
or by another program. Solution: kill the other program or launch
GeneWeb with another port number (option -p)\n" port_selected.val;
         flush Pervasives.stderr;
      return ()
  | Unix_error err fun_name arg ->
      if test_eacces_bind err fun_name then ()
      else
	do prerr_string "\"";
	   prerr_string fun_name;
	   prerr_string "\" failed";
	   if String.length arg > 0 then
	     do prerr_string " on \""; prerr_string arg; prerr_string "\"";
	     return ()
	   else ();
	   prerr_string ": ";
	   prerr_endline (error_message err);
	   flush Pervasives.stderr;
	return ()
  | _ -> try Printexc.print raise exc with _ -> () ]
;

try main () with exc -> print_exc exc;
