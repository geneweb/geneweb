(* camlp4r pa_extend.cmo ./pa_html.cmo ./pa_lock.cmo *)
(* $Id: gwd.ml,v 2.27 1999-08-13 02:44:56 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;
open Gutil;

value selected_port = ref 2317;
value redirected_addr = ref None;
value wizard_passwd = ref "";
value friend_passwd = ref "";
value only_address = ref "";
value cgi = ref False;
value default_lang = ref "fr";
value uid = ref None;
value gid = ref None;
value log_file = ref "";
value log_flags =
  [Open_wronly; Open_append; Open_creat; Open_text; Open_nonblock]
;
ifdef UNIX then
value max_clients = ref None;
value robot_xcl = ref None;
value auth_file = ref "";
value daemon = ref False;

value log_oc () =
  if log_file.val <> "" then open_out_gen log_flags 0o644 log_file.val
  else stderr
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

value extract_boundary content_type =
  let e = Util.create_env content_type in
  List.assoc "boundary" e
;

value fprintf_date oc tm =
  Printf.fprintf oc "%4d/%02d/%02d %02d:%02d:%02d"
    (1900 + tm.Unix.tm_year) (succ tm.Unix.tm_mon) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
;

value log oc tm from request s =
  let content_type = Wserver.extract_param "content-type: " '\n' request in
(*
  let s = if is_multipart_form content_type then "(multipart form)" else s in
*)
  let referer = Wserver.extract_param "referer: " '\n' request in
  let user_agent = Wserver.extract_param "user-agent: " '\n' request in
  do let tm = Unix.localtime tm in
     fprintf_date oc tm;
     Printf.fprintf oc " (%d)" (Unix.getpid ());
     Printf.fprintf oc " %s\n" s;
     Printf.fprintf oc "  From: %s\n" from;
     Printf.fprintf oc "  Agent: %s\n" user_agent;
     if referer <> "" then Printf.fprintf oc "  Referer: %s\n" referer else ();
  return ()
;

value nl () =
  Wserver.wprint "\r\n"
;

value copy_file fname =
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      do try
           while True do
             let c = input_char ic in
             Wserver.wprint "%c" c;
           done
         with _ -> ();
         close_in ic;
      return ()
  | None -> () ]
;

value refuse_log from cgi =
  let oc = open_out_gen log_flags 0o644 "refuse_log" in
  do let tm = Unix.localtime (Unix.time ()) in
     fprintf_date oc tm;
     Printf.fprintf oc " excluded: %s\n" from;
     close_out oc;
     if not cgi then
       do Wserver.wprint "HTTP/1.0 403 Forbidden"; nl (); return ()
     else ();
     Wserver.wprint "Content-type: text/html"; nl (); nl ();
     Wserver.wprint "Your access has been disconnected by administrator.\n";
     copy_file "refuse.txt";
  return ()
;

value only_log from cgi =
  let oc = log_oc () in
  do let tm = Unix.localtime (Unix.time ()) in
     fprintf_date oc tm;
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
     fprintf_date oc tm;
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
             let k =
               find_key (input_line ic) where rec find_key line =
                 if String.length line < 4 then find_key (input_line ic)
                 else if String.sub line 0 4 <> "    " then
                   find_key (input_line ic)
                 else line
             in
             loop (input_line ic) where rec loop line =
               if String.length line < 4 then ()
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

value read_base_env cgi bname =
  let fname =
    let f = Filename.concat Util.base_dir.val bname ^ ".gwf" in
    if Sys.file_exists f then f
    else
      let f = Filename.concat Util.base_dir.val bname ^ ".cnf" in
      do if not cgi && Sys.file_exists f then
           do Printf.eprintf "\
*** Name for config file \"%s.cnf\" is deprecated; \
rename it as \"%s.gwf\".\n" bname bname;
              flush stderr;
           return ()
         else ();
      return f
  in
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
  let env = [('o', conf.bname); ('n', new_n); ('l', link)] in
  try Util.copy_etc_file env "renamed" with
  [ Sys_error _ ->
      let  title _ =
        Wserver.wprint "%s -&gt; %s" conf.bname new_n
      in
      do Util.header conf title;
         tag "ul" begin
           Util.html_li conf;
           tag "a" "href=\"%s\"" link begin
             Wserver.wprint "%s" link;
           end;
         end;
         Util.trailer conf;
      return () ]
;

value print_redirected conf new_addr =
  let link =
    let req = Util.get_request_string conf in
    "http://" ^ new_addr ^ req
  in
  let env = [('l', link)] in
  try Util.copy_etc_file env "redirect" with
  [ Sys_error _ ->
      let  title _ = Wserver.wprint "Address changed" in
      do Util.header conf title;
         Wserver.wprint "Use the following address:\n<p>\n";
         tag "ul" begin
           Util.html_li conf;
           stag "a" "href=\"%s\"" link begin Wserver.wprint "%s" link; end;
           Wserver.wprint "\n";
         end;
         Util.trailer conf;
      return () ]
;

value log_count =
  fun
  [ Some (welcome_cnt, request_cnt, start_date) ->
      let oc = log_oc () in
      do Printf.fprintf oc "  #accesses %d (#welcome %d) since %s\n"
           (welcome_cnt + request_cnt) welcome_cnt start_date;
         flush_log oc;
      return ()
  | None -> () ]
;

value start_with_base conf bname =
  let bfile = Filename.concat Util.base_dir.val bname in
  match try Left (Iobase.input bfile) with e -> Right e with
  [ Left base ->
      let r = Family.family conf base in
      do Wserver.wflush ();
         if conf.cgi && log_file.val = "" then () else log_count r;
      return ()
  | Right e ->
      let transl conf w =
        try Hashtbl.find conf.lexicon w with [ Not_found -> "[" ^ w ^ "]" ]
      in
      let title _ =
        Wserver.wprint "%s" (Util.capitale (transl conf "error"))
      in
      do Util.header conf title;
         Wserver.wprint "<ul>";
         Util.html_li conf;
         Wserver.wprint "%s"
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
     tag "ul" begin
       Util.html_li conf;
       Wserver.wprint "<form method=get action=\"%s\">\n" conf.command;
       Wserver.wprint "<input name=b size=40> =&gt;\n";
       Wserver.wprint "<input type=submit value=\"Ok\">\n";
     end;
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
      if x <> "" || cgi then (x, env)
      else (String.sub str 0 iq, env)
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
  let base_env = read_base_env cgi base_file in
  let default_lang =
    try
      let x = List.assoc "default_lang" base_env in
      if x = "" then default_lang.val else x
    with
    [ Not_found -> default_lang.val ]
  in
  let lexicon = input_lexicon (if lang = "" then default_lang else lang) in
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
  let cancel_links =
    match Util.p_getenv env "cgl" with
    [ Some "on" -> True
    | _ -> False ]
  in
  if not ok then
    unauth base_file (if passwd = "w" then "Wizard" else "Friend")
  else
  let conf =
    {wizard = wizard && not wizard_just_friend;
     friend = friend || wizard_just_friend && wizard;
     just_friend_wizard = wizard && wizard_just_friend;
     cgi = cgi;
     command = command;
     lang = if lang = "" then default_lang else lang;
     can_send_image =
       try List.assoc "can_send_image" base_env = "yes" with
       [ Not_found ->
           try
             let r = List.assoc "can_send_photo" base_env = "yes" in
             do if not cgi then
                  do Printf.eprintf "\
*** Config file for \"%s\": \"can_send_photo\" is deprecated; \
use \"can_send_image\".\n"
                       base_file;
                    flush stderr;
                  return ()
                else ();
             return r
           with
           [ Not_found -> False ] ];
     cancel_links = cancel_links;
     access_by_key =
       try List.assoc "access_by_key" base_env = "yes" with
       [ Not_found -> False ];
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
     charset =
       try Hashtbl.find lexicon " !charset" with [ Not_found -> "iso-8859-1" ];
     is_rtl =
       try Hashtbl.find lexicon " !dir" = "rtl" with [ Not_found -> False ];
     today =
       {day = tm.Unix.tm_mday;
        month = succ tm.Unix.tm_mon;
        year = tm.Unix.tm_year + 1900;
        prec = Sure};
     today_wd = tm.Unix.tm_wday}
  in
  if conf.bname = "" then propose_base conf
  else
    match redirected_addr.val with
    [ Some addr -> print_redirected conf addr
    | None ->
        match
          try Some (List.assoc "renamed" base_env) with [ Not_found -> None ]
        with
        [ Some n when n <> "" -> print_renamed conf n
        | _ ->
            do start_with_base conf conf.bname;
               if sleep > 0 then Unix.sleep sleep else ();
            return () ] ]
;

value chop_extension name =
  loop (String.length name - 1) where rec loop i =
    if i < 0 then name
    else if name.[i] == '.' then String.sub name 0 i
    else if name.[i] == '/' then name
    else if name.[i] == '\\' then name
    else loop (i - 1)
;

value match_strings regexp s =
  loop 0 0 where rec loop i j =
    if i == String.length regexp && j == String.length s then True
    else if i == String.length regexp then False
    else if j == String.length s then False
    else if regexp.[i] = s.[j] then loop (i + 1) (j + 1)
    else if regexp.[i] = '*' then
      if i + 1 == String.length regexp then True
      else if regexp.[i + 1] = s.[j] then loop (i + 2) (j + 1)
      else loop i (j + 1)
    else False
;

value excluded from =
  let efname = chop_extension Sys.argv.(0) ^ ".xcl" in
  match try Some (open_in efname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop () where rec loop () =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some line ->
            if match_strings line from then do close_in ic; return True
            else loop ()
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
  let fname = Util.image_file_name bname str in
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
      let fname =
        if fname.[0] = '/' then String.sub fname 1 (String.length fname - 1)
        else fname
      in
      do if Filename.is_implicit fname then
           if Filename.check_suffix fname ".jpg"
           || Filename.check_suffix fname ".JPG" then
             print_image cgi bname fname "jpeg"
           else if Filename.check_suffix fname ".jpeg"
           || Filename.check_suffix fname ".JPEG" then
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

value extract_multipart boundary str =
  let rec skip_nl i =
    if i < String.length str && str.[i] == '\r' then skip_nl (i + 1)
    else if i < String.length str && str.[i] == '\n' then i + 1
    else i
  in
  let next_line i =
    let i = skip_nl i in
    loop "" i where rec loop s i =
      if i == String.length str || str.[i] == '\n' || str.[i] == '\r' then
        (s, i)
      else loop (s ^ String.make 1 str.[i]) (i + 1)
  in
  let boundary = "--" ^ boundary in
  let rec loop i =
    if i == String.length str then []
    else
      let (s, i) = next_line i in
      if s = boundary then
        let (s, i) = next_line i in
        let s = String.lowercase s in
        let env = Util.create_env s in
        match (Util.p_getenv env "name", Util.p_getenv env "filename") with
        [ (Some var, Some filename) ->
            let i = skip_nl i in
            let i1 =
              loop i where rec loop i =
                if i < String.length str then
                  if i > String.length boundary
                  && String.sub str (i - String.length boundary)
                       (String.length boundary) = boundary
                  then i - String.length boundary
                  else loop (i + 1)
                else i
            in
            let v = String.sub str i (i1 - i) in
            [("file", v) :: loop i1]
        | (Some var, None) ->
            let var =
              let i0 =
                if String.length var > 0 && var.[0] == '"' then 1 else 0
              in
              let i1 =
                if String.length var > 0
                && var.[String.length var - 1] == '"' then
                  String.length var - 1
                else String.length var
              in
              String.sub var i0 (i1 - i0)
            in
            let (s, i) = next_line i in
            if s = "" then
              let (s, i) = next_line i in
              [(var, s) :: loop i]
            else loop i
        | _ -> loop i ]
      else if s = boundary ^ "--" then []
      else loop i
  in
  let i =
    start 0 where rec start i =
      if i == String.length str then i
      else if str.[i] == '?' then (i + 1)
      else start (i + 1)
  in
  let env = loop i in
  let (str, _) =
    List.fold_left
      (fun (str, sep) (v, x) ->
         if v = "file" then (str, sep) else (str ^ sep ^ v ^ "=" ^ x, ";"))
      (String.sub str 0 i, "") env
  in
  (str, env)
;

value build_env request str =
  let iq = index '?' str in
  let content_type = Wserver.extract_param "content-type: " '\n' request in
  if is_multipart_form content_type then
    let boundary = extract_boundary content_type in
    let (str, env) = extract_multipart boundary str in
    (str, env)
  else
    let query_string =
      if iq == String.length str then ""
      else String.sub str (iq + 1) (String.length str - iq - 1)
    in
    (str, Util.create_env query_string)
;

value log_and_robot_check cgi from request str =
  if cgi && log_file.val = "" && robot_xcl.val = None then ()
  else
    let tm = Unix.time () in
    lock_wait Srcfile.adm_file "gwd.lck" with
    [ Accept ->
        let oc = log_oc () in
        do try
             do match robot_xcl.val with
                [ Some (cnt, sec) -> Robot.check oc tm from cnt sec cgi
                | None -> () ];
                if cgi && log_file.val = "" then ()
                else log oc tm from request str;
             return ()
           with e -> do flush_log oc; return raise e;
           flush_log oc;
        return ()
    | Refuse -> () ]
;

value connection cgi (addr, request) str =
  let from =
    match addr with
    [ Unix.ADDR_UNIX x -> x
    | Unix.ADDR_INET iaddr port ->
         try (Unix.gethostbyaddr iaddr).Unix.h_name with _ ->
           Unix.string_of_inet_addr iaddr ]
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
            let (str, env) = build_env request str in
            if image_request cgi str env then ()
            else
              do log_and_robot_check cgi from request str;
                 connection_accepted cgi (addr, request) str env;
              return ()
          with
          [ Exit -> () ] ]
;

value tmout = 120;

value null_reopen flags fd =
ifdef UNIX then
  let fd2 = Unix.openfile "/dev/null" flags 0 in
  do Unix.dup2 fd2 fd;
     Unix.close fd2;
  return ()
else ()
;

value geneweb_server () =
  let hostn = try Unix.gethostname () with _ -> "computer" in
  let auto_call =
    try let _ = Sys.getenv "WSERVER" in True with [ Not_found -> False ]
  in
  do if not auto_call then
       do Printf.eprintf "GeneWeb %s - " Version.txt;
          Printf.eprintf "Copyright (c) INRIA 1999\n";
          if not daemon.val then
            do Printf.eprintf "Possible addresses:";
               Printf.eprintf "
   http://localhost:%d/base
   http://127.0.0.1:%d/base
   http://%s:%d/base"
            selected_port.val selected_port.val hostn selected_port.val;
               Printf.eprintf "
where \"base\" is the name of the data base
Type control C to stop the service
";
            return ()
          else ();
          flush stderr;
          if daemon.val then
            if Unix.fork () = 0 then
              do Unix.close Unix.stdin;
                 null_reopen [Unix.O_WRONLY] Unix.stdout;
                 null_reopen [Unix.O_WRONLY] Unix.stderr;
              return ()
            else exit 0
          else ();
          try
            do Unix.mkdir (Filename.concat Util.base_dir.val "cnt") 0o755;
               if uid.val <> None then
                 Unix.chmod (Filename.concat Util.base_dir.val "cnt") 0o777
               else ();
            return ()
          with [ Unix.Unix_error _ _ _ -> () ];
       return ()
     else ();
  return
  Wserver.f selected_port.val tmout
    (ifdef UNIX then max_clients.val else None) (uid.val, gid.val)
    (connection False)
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
  let request = add "content-type" "CONTENT_TYPE" request in
  connection True (Unix.ADDR_UNIX addr, request) str
;

value read_input len =
  if len >= 0 then
    let buff = String.create len in
    do really_input stdin buff 0 len; return buff
  else
    let buff = ref "" in
    do try
         while True do
           let l = input_line stdin in
(*
           do Printf.eprintf "POST: %s\n" l; flush stderr; return
*)
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
             while True do
               let line = input_line ic in
               if line <> "" then list.val := [line :: list.val] else ();
             done
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

module G = Grammar.Make (struct value lexer = Plexer.make (); end);
value robot_xcl_arg = G.Entry.create "robot_xcl arg";
GEXTEND G
  robot_xcl_arg:
    [ [ cnt = INT; ","; sec = INT; EOI ->
          (int_of_string cnt, int_of_string sec) ] ];
END;

value robot_exclude_arg s =
  try
    robot_xcl.val :=
      Some (G.Entry.parse robot_xcl_arg (G.parsable (Stream.of_string s)))
  with
  [ Stdpp.Exc_located _ (Stream.Error _ | Token.Error _) ->
      do Printf.eprintf "Bad use of option -robot_xcl\n";
         Printf.eprintf "Use option -help for usage.\n";
         flush Pervasives.stderr;
      return exit 2 ]
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
     ("-p", Arg.Int (fun x -> selected_port.val := x),
      "<number>
       Select a port number (default = " ^
       string_of_int selected_port.val ^
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
     ("-robot_xcl", Arg.String robot_exclude_arg,
      "<cnt>,<sec>
       Exclude connections when more than <cnt> requests in <sec> seconds.");
     ("-redirect", Arg.String (fun x -> redirected_addr.val := Some x),
      "<addr>
       Send a message to say that this service has been redirected to <addr>");
     ("-nolock", Arg.Set Lock.no_lock_flag,
      "
       Do not lock files before writing.") ::
     ifdef UNIX then
       [("-max_clients", Arg.Int (fun x -> max_clients.val := Some x),
         "<num>
       Max number of clients treated at the same time (default: no limit)
       (not cgi).");
        ("-setuid", Arg.Int (fun x -> uid.val := Some x),
         "<num>
       Set user id, for example to use port < 1024 as simple user.");
        ("-setgid", Arg.Int (fun x -> gid.val := Some x),
         "<num>
       Set group id.");
        ("-daemon", Arg.Set daemon,
         "
       Unix daemon mode.")]
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
  if err = Unix.EACCES && fun_name = "bind" then
    try
      do Printf.eprintf "\n\
Error: invalid access to the port %d: users port number less than 1024
are reserved to the system. Solution: do it as root or choose another port
number greater than 1024.\n" selected_port.val;
         flush stderr;
      return True
    with
    [ Not_found -> False ]
  else False
else
value test_eacces_bind err fun_name = False;

value print_exc exc =
  match exc with
  [ Unix.Unix_error Unix.EADDRINUSE "bind" _ ->
      do Printf.eprintf "\nError: ";
         Printf.eprintf "the port %d" selected_port.val;
         Printf.eprintf " is already used by another GeneWeb daemon
or by another program. Solution: kill the other program or launch
GeneWeb with another port number (option -p)\n";
         flush stderr;
      return ()
  | Unix.Unix_error err fun_name arg ->
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
	   prerr_endline (Unix.error_message err);
	   flush stderr;
	return ()
  | _ -> try Printexc.print raise exc with _ -> () ]
;

try main () with exc -> print_exc exc;
