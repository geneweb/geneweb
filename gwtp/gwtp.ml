(* $Id: gwtp.ml,v 1.29 2000-08-18 10:43:52 ddr Exp $ *)

open Printf;

value gwtp_tmp = ref "gwtp_tmp";
value gwtp_dst = ref "gwtp_dst";
value gw_site = ref "";
value token_tmout = ref 900.0;

(* Get CGI contents *)

value read_input len =
  if len >= 0 then
    let buff = String.create len in
    do really_input stdin buff 0 len; return buff
  else
    let buff = ref "" in
    do try
         while True do
           let l = input_line stdin in
           buff.val := buff.val ^ l;
         done
       with
       [ End_of_file -> () ];
    return buff.val
;

value cgi_content_type () =
  try Sys.getenv "CONTENT_TYPE" with
  [ Not_found -> "" ]
;

value cgi_content () =
  let is_post =
    try Sys.getenv "REQUEST_METHOD" = "POST" with
    [ Not_found -> False ]
  in
  if is_post then
    let len =
       try int_of_string (Sys.getenv "CONTENT_LENGTH") with
      [ Not_found -> -1 ]
    in
    do set_binary_mode_in stdin True; return
    read_input len
  else
    try Sys.getenv "QUERY_STRING" with
    [ Not_found -> "" ]
;

value cgi_from () =
  try Sys.getenv "REMOTE_HOST" with [ Not_found -> Sys.getenv "REMOTE_ADDR" ]
;

(* Utilitaires *)

value log_open () = 
  let fname = Filename.concat gwtp_tmp.val "gwtp.log" in
  open_out_gen [Open_wronly; Open_creat; Open_append] 0o666 fname
;

value filename_basename str =
  loop (String.length str - 1) where rec loop i =
    if i < 0 then str
    else
      match str.[i] with
      [ 'A'..'Z' | 'a'..'z' | '-' | '~' | '.' -> loop (i - 1)
      | _ -> String.sub str (i + 1) (String.length str - i - 1) ]
;

value copy_template env fname =
  let ic = open_in (Filename.concat gwtp_tmp.val (fname ^ ".txt")) in
  do try
       while True do
         match input_char ic with
         [ '%' ->
             let c = input_char ic in
             try print_string (List.assoc c env) with
             [ Not_found -> do print_char '%'; print_char c; return () ]
         | c -> print_char c ];
       done
     with [ End_of_file -> () ];
     close_in ic;
  return ()
;

value sys_copy src dst =
  let ic = open_in src in
  let oc = open_out dst in
  do try
       while True do
         let c = input_char ic in
         output_char oc c;
       done
     with
     [ End_of_file -> () ];
     close_out oc;
     close_in ic;
  return ()
;

value remove_dir_contents dir =
  let dh = Unix.opendir dir in
  try
    while True do
      match Unix.readdir dh with
      [ "." | ".." -> ()
      | f -> Unix.unlink (Filename.concat dir f) ];
    done
  with
  [ End_of_file -> Unix.closedir dh ]
;

value html_escaped s =
  let s = String.escaped s in
  loop 0 0 where rec loop i len =
    if i == String.length s then Buff.get len
    else
      let len =
        match s.[i] with
        [ '<' -> Buff.mstore len "&lt;"
        | '>' -> Buff.mstore len "&gt;"
        | x -> Buff.store len x ]
      in
      loop (i + 1) len
;

value gwtp_error txt =
  do printf "content-type: text/html\r\n\r\n";
     printf "\
<head><title>Error</title></head>\n<body>
<h1><font color=red>Error</font></h1>
%s
</body>
" (String.capitalize txt);
  return ()
;

value gwtp_invalid_request str env = gwtp_error "Invalid request";

(* Login and tokens *)

value tokens_file_name () = Filename.concat gwtp_tmp.val "token";

value read_tokens fname =
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop [] where rec loop list =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some line ->
            let i = String.index line ' ' in
            let j = String.index_from line (i + 1) ' ' in
            let tm = float_of_string (String.sub line 0 i) in
            let b = String.sub line (i + 1) (j - i - 1) in
            let tok = String.sub line (j + 1) (String.length line - j - 1) in
            loop [(tm, b, tok) :: list]
        | None ->
            do close_in ic; return List.rev list ]
  | None -> [] ]
;

value write_tokens fname tokens =
  let oc = open_out fname in
  do List.iter
       (fun (tm, from_b, tok) -> fprintf oc "%.0f %s %s\n" tm from_b tok)
       tokens;
     close_out oc;
  return ()
;

Random.self_init ();

value mk_passwd () =
  loop 0 where rec loop len =
    if len = 12 then Buff.get len
    else
      let v = Char.code 'a' + Random.int 26 in
      loop (Buff.store len (Char.chr v))
;

value check_login b p =
  let line1 = b ^ ":" ^ p in
  let ic = open_in (Filename.concat gwtp_tmp.val "passwd") in
  let login_ok =
    loop () where rec loop () =
      match try Some (input_line ic) with [ End_of_file -> None ] with
      [ Some line -> if line = line1 then True else loop ()
      | None -> False ]
  in
  do close_in ic; return
  if login_ok then Some (mk_passwd ()) else None
;

value check_token fname from b tok =
  let tokens = read_tokens fname in
  let from_b = from ^ "/" ^ b in
  let tm = Unix.time () in
  loop [] tokens where rec loop tokens_out =
    fun
    [ [(tm0, from_b0, tok0) :: tokens] ->
        if tm < tm0 || tm -. tm0 > token_tmout.val then
          loop tokens_out tokens
        else if from_b = from_b0 && tok = tok0 then
          (True, List.rev tokens_out @ [(tm, from_b, tok) :: tokens])
        else loop [(tm0, from_b0, tok0) :: tokens_out] tokens
    | [] -> (False, List.rev tokens_out) ]
;

value set_token from b tok =
  let fname = tokens_file_name () in
  let from_b = from ^ "/" ^ b in
  let tokens =
    let tokens = read_tokens fname in
    let tm = Unix.time () in
    List.fold_right
      (fun (tm0, from_b0, tok0) tokens ->
         if from_b = from_b0 || tm < tm0 || tm -. tm0 > token_tmout.val then
           tokens
         else [(tm0, from_b0, tok0) :: tokens])
      tokens [(tm, from_b, tok)]
  in
  write_tokens fname tokens
;

(* Requests *)

value lowercase_start_with s s_ini =
  let len = String.length s_ini in
  String.length s >= len && String.lowercase (String.sub s 0 len) = s_ini
;

value insert_file env bdir name =
  let fname = HttpEnv.decode (List.assoc (name ^ "_name") env) in
  let fname = filename_basename fname in
  do if fname = "" then ()
     else if fname <> name then
       printf
         "You selected <b>%s</b> instead of <b>%s</b> -&gt; ignored.\n"
         fname name
     else
       let contents = List.assoc name env in
       let i =
         if lowercase_start_with contents "content-type: " then
           String.index contents '\n'
         else 0
       in
       let j = String.index_from contents (i + 1) '\n' in
       let len = String.length contents - j - 3 in
       if len > 0 then
         let oc = open_out (Filename.concat bdir name) in
         do output oc contents (j + 1) len;
            flush oc;
            printf "File \"%s\" transfered.\n" name;
            close_out oc;
         return ()
       else ();
     flush stdout;
  return ()
;

value make_temp env b =
  let bdir = Filename.concat gwtp_tmp.val (b ^ ".gwb") in
  do if Sys.file_exists bdir then remove_dir_contents bdir
     else Unix.mkdir bdir 0o777;
     insert_file env bdir "base";
     insert_file env bdir "notes";
     insert_file env bdir "patches";
     flush stdout;
   return
  let base = Iolight.input bdir in
  do printf "\n";
     printf "persons: %d\n" base.Def.data.Def.persons.Def.len;
     printf "families: %d\n\n" base.Def.data.Def.families.Def.len;
     flush stdout;
     Iobase.output bdir base;
     flush stdout;
  return ()
;

value copy_temp b =
  let bdir = Filename.concat gwtp_tmp.val (b ^ ".gwb") in
  let dir_old = Filename.concat gwtp_dst.val "old" in
  let dir_old_gwb = Filename.concat dir_old (b ^ ".gwb") in
  let dir_gwb = Filename.concat gwtp_dst.val (b ^ ".gwb") in
  do if Sys.file_exists dir_gwb then
       do if not (Sys.file_exists dir_old) then Unix.mkdir dir_old 0o777
          else ();
          if Sys.file_exists dir_old_gwb then
            do remove_dir_contents dir_old_gwb;
               Unix.rmdir dir_old_gwb;
            return ()
          else ();
          Sys.rename dir_gwb dir_old_gwb;
          let old_forum = Filename.concat dir_old_gwb "forum" in
          if Sys.file_exists old_forum then
            sys_copy old_forum (Filename.concat bdir "forum")
          else ();
       return ()
     else ();
     Sys.rename bdir dir_gwb;
  return ()
;

(* Actions *)

value printf_link_to_main b tok =
  do printf "<p><hr><div align=right>\n";
     printf "<a href=\"gwtp?m=MAIN;b=%s;t=%s\">main page</a></div>\n"
       b tok;
  return ()
;

value send_file str env b tok f fname =
  let fname = filename_basename fname in
  if fname = "base" then
    do printf "content-type: text/html\r\n\r\n\
<head><title>Gwtp...</title></head>\n<body>
<h1 align=center>Gwtp...</h1>
<pre>\n";
       flush stdout;
       make_temp env b;
       printf "\nTemporary data base created.\n";
       flush stdout;
       copy_temp b;
       printf "Data base \"%s\" updated.\n" b;
       flush stdout;
       printf "</pre>\n";
       printf_link_to_main b tok;
       printf "</body>\n";
    return ()
  else
    do printf "content-type: text/html\r\n\r\n";
       printf "\
<head><title>Error</title></head>\n<body>
<h1><font color=red>Error</font></h1>\n";
       if fname = "" then
         printf "You must select at least the <b>base</b> file\n"
       else
         printf "You selected the file <b>%s</b> instead of <b>base</b>\n"
           fname;
       printf "</body>\n";
       printf_link_to_main b tok;
    return ()
;

value gwtp_send str env b t =
  match (HttpEnv.getenv env "base", HttpEnv.getenv env "base_name") with
  [ (Some f, Some fname) -> send_file str env b t f (HttpEnv.decode fname)
  | (Some f, None) ->
      gwtp_error "Sorry, your browser seems not be able to send files."
  | _ -> gwtp_invalid_request str env ]
;

value gwtp_receive str env b tok =
  match HttpEnv.getenv env "f" with
  [ Some fname ->
      let fname = filename_basename fname in
      let bdir = Filename.concat gwtp_dst.val (b ^ ".gwb") in
      do printf "content-type: bin/geneweb\r\n\r\n";
         let ic = open_in (Filename.concat bdir fname) in
         do try
              while True do
                let c = input_char ic in
                output_char stdout c;
              done
            with [ End_of_file -> () ];
            close_in ic;
         return ();
      return ()
  | _ -> gwtp_invalid_request str env ]
;

value gwtp_upload str env b tok =
  do printf "content-type: text/html\r\n\r\n";
     copy_template [('b', b); ('t', tok)] "send";
     printf_link_to_main b tok;
     printf "</body>\n";
  return ()
;

value gwtp_download str env b tok =
  let bdir = Filename.concat gwtp_dst.val (b ^ ".gwb") in
  do printf "content-type: text/html\r\n\r\n";
     if Sys.file_exists bdir then
       let dh = Unix.opendir bdir in
       do copy_template [('b', b); ('t', tok)] "recv";
          printf "<ul>\n";
          try
            while True do
              match Unix.readdir dh with
              [ "." | ".." | "" -> ()
              | f ->
                  if f.[String.length f - 1] = '~' then ()
                  else
                    printf
                      "<li><a href=\"gwtp?m=RECV;b=%s;t=%s;f=/%s\">%s</a>\n"
                      b tok f f ];
            done
          with
          [ End_of_file -> Unix.closedir dh ];
          printf "</ul>\n";
       return ()
     else
       printf "
<head><title>Gwtp - download %s</title></head>
<body>
<h1 align=center>Gwtp - download %s</h1>
<p>Your data base does not exist or is empty.\n" b b;
     printf_link_to_main b tok;
     printf "</body>\n";
  return ()
;

value gwtp_main str env b tok =
  do printf "content-type: text/html\r\n\r\n";
     printf "\
<head><title>Gwtp - %s</title></head>\n<body>
<h1 align=center>Gwtp - %s</h1>
<ul>\n" b b;
     printf "<li><a href=\"gwtp?m=UPL;b=%s;t=%s\">Upload</a>\n" b tok;
     printf "<li><a href=\"gwtp?m=DNL;b=%s;t=%s\">Download</a>\n" b tok;
     printf "</ul>\n";
     if gw_site.val <> "" then
       do printf "<p>\n<ul>\n";
          printf "<li><a href=\"%s%s\">Browse</a>\n" gw_site.val b;
          printf "</ul>\n";
       return ()
     else ();
     printf "</body>\n";
  return ()  
;

value gwtp_login str env =
  do printf "content-type: text/html\r\n\r\n";
     printf "\
<head><title>Gwtp</title></head>\n<body>
<h1>Gwtp</h1>
<form method=POST action=gwtp>
<input type=hidden name=m value=LOGIN>
Data base: <input name=b><br>
Password: <input name=p type=password><br>
<input type=submit value=Login>
</form>
</body>
";
  return ()
;

(* Wrappers *)

value gwtp_check_login from str env gwtp_fun =
  match (HttpEnv.getenv env "b", HttpEnv.getenv env "p") with
  [ (Some b, Some p) ->
      match check_login b p with
      [ Some tok ->
          do set_token from b tok;
             gwtp_fun str env b tok;
          return ()
      | None -> gwtp_error "Invalid login" ]
  | _ -> gwtp_invalid_request str env ]
;

value gwtp_logged from str env gwtp_fun =
  match (HttpEnv.getenv env "b", HttpEnv.getenv env "t") with
  [ (Some b, Some t) ->
      let ok =
        let fname = tokens_file_name () in
        let (ok, tokens) = check_token fname from b t in
        do write_tokens fname tokens; return ok
      in
      if ok then gwtp_fun str env b t
      else gwtp_error "Login expired"
  | _ -> gwtp_invalid_request str env ]
;

(* Main *)

value log oc_log str =
  let tm = Unix.localtime (Unix.time ()) in
  let user_agent = try Sys.getenv "HTTP_USER_AGENT" with [ Not_found -> "" ] in
  let referer = try Sys.getenv "HTTP_REFERER" with [ Not_found -> "" ] in
  let from =
    try Sys.getenv "REMOTE_HOST" with
    [ Not_found ->
        try Sys.getenv "REMOTE_ADDR" with
        [ Not_found -> "" ] ]
  in
  do fprintf oc_log "%4d-%02d-%02d %02d:%02d:%02d"
       (1900 + tm.Unix.tm_year) (succ tm.Unix.tm_mon) tm.Unix.tm_mday
        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec;
     fprintf oc_log " gwtp?%s\n" str;
     if from <> "" then fprintf oc_log "  From: %s\n" from else ();
     if user_agent <> "" then
       fprintf oc_log "  Agent: %s\n" user_agent
     else ();
     if referer <> "" then fprintf oc_log "  Referer: %s\n" referer
     else ();
  return ()
;

value gwtp () =
  let content_type = cgi_content_type () in
  let content = cgi_content () in
  let from = cgi_from () in
  let (str, env) = HttpEnv.make content_type content in
  let oc_log = log_open () in
  do let _ = Unix.umask 0 in ();
     log oc_log str;
     flush oc_log;
     Unix.dup2 (Unix.descr_of_out_channel oc_log) Unix.stderr;
     match HttpEnv.getenv env "m" with
     [ Some "LOGIN" -> gwtp_check_login from str env gwtp_main
     | Some "MAIN" -> gwtp_logged from str env gwtp_main
     | Some "UPL" -> gwtp_logged from str env gwtp_upload
     | Some "DNL" -> gwtp_logged from str env gwtp_download
     | Some "SEND" -> gwtp_logged from str env gwtp_send
     | Some "RECV" -> gwtp_logged from str env gwtp_receive
     | Some _ -> gwtp_invalid_request str env
     | None -> gwtp_login str env ];
     flush stdout;
     flush oc_log;
     close_out oc_log;
  return ()
;

value usage_msg = "Usage: gwtp";
value speclist =
  [("-tmp", Arg.String (fun x -> gwtp_tmp.val := x), "<dir>");
   ("-dst", Arg.String (fun x -> gwtp_dst.val := x), "<dir>");
   ("-site", Arg.String (fun x -> gw_site.val := x), "<url>")]
;
value anonfun _ = do Arg.usage speclist usage_msg; return exit 2;

value main () =
  do Arg.parse speclist anonfun usage_msg;
     gwtp ();
  return ()
;

main ();
