(* camlp4r ../src/pa_lock.cmo *)
(* $Id: gwtp.ml,v 4.0 2001-03-16 19:32:16 ddr Exp $ *)
(* (c) Copyright 2001 INRIA *)

open Printf;

value gwtp_tmp = ref (Filename.concat ".." "gwtp_tmp");
value gwtp_dst = ref (Filename.concat ".." "gwtp_dst");
value gw_site = ref "";
value token_tmout = ref 900.0;

value filename_basename str =
  loop (String.length str - 1) where rec loop i =
    if i < 0 then str
    else
      match str.[i] with
      [ 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '~' | '.' -> loop (i - 1)
      | _ -> String.sub str (i + 1) (String.length str - i - 1) ]
;

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

value cgi_script_name () =
  try filename_basename (Sys.getenv "SCRIPT_NAME") with
  [ Not_found -> "gwtp" ]
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
  try Sys.getenv "REMOTE_HOST" with
  [ Not_found ->
      try Sys.getenv "REMOTE_ADDR" with
      [ Not_found -> "" ] ]
;

(* Utilitaires *)

value crlf () =
  do flush stdout;
     let _ : int = Unix.write Unix.stdout "\r\n" 0 2 in ();
  return ()
;

value lowercase_start_with s s_ini =
  let len = String.length s_ini in
  String.length s >= len && String.lowercase (String.sub s 0 len) = s_ini
;

value quote_escaped s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
      [ '"' | '&' | '<' | '>' -> True
      | x -> need_code (succ i) ]
    else False
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
        [ '"' -> i1 + 6
        | '&' -> i1 + 5
        | '<' | '>' -> i1 + 4
        | _ -> succ i1 ]
      in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
        [ '"' -> do String.blit "&#034;" 0 s1 i1 6; return i1 + 6
        | '&' -> do String.blit "&amp;" 0 s1 i1 5; return i1 + 5
        | '<' -> do String.blit "&lt;" 0 s1 i1 4; return i1 + 4
        | '>' -> do String.blit "&gt;" 0 s1 i1 4; return i1 + 4
        | c -> do s1.[i1] := c; return succ i1 ]
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (String.create len) 0 0
  else s
;

value log_open () = 
  let fname = Filename.concat gwtp_tmp.val "gwtp.log" in
  open_out_gen [Open_wronly; Open_creat; Open_append] 0o666 fname
;

value get_variable ic =
  loop 0 where rec loop len =
    match input_char ic with
    [ ';' -> Buff.get len
    | c -> loop (Buff.store len c) ]
;

value get_binding ic =
  loop 0 where rec loop len =
    match input_char ic with
    [ '=' -> let k = Buff.get len in (k, get_variable ic)
    | c -> loop (Buff.store len c) ]
;

value template_fname env fname =
  let dir =
    match HttpEnv.getenv env "lang" with
    [ Some x -> x
    | _ -> "en" ]
  in
  List.fold_right Filename.concat [gwtp_tmp.val; "lang"; dir] (fname ^ ".txt")
;

value copy_template genv (varenv, filenv) env fname =
  let ic = open_in (template_fname genv fname) in
  do try
       while True do
         match input_char ic with
         [ '%' ->
             match input_char ic with
             [ 'c' | 'e' as x ->
                 let (v, k) = get_binding ic in
                 try
                   if k = List.assoc v varenv then
                     print_string (if x = 'c' then " checked" else " selected")
                   else ()
                 with [ Not_found -> () ]
             | 'v' ->
                 let v = get_variable ic in
                 try print_string (quote_escaped (List.assoc v varenv)) with
                 [ Not_found -> () ]
             | 'f' ->
                 let v = get_variable ic in
                 try print_string (quote_escaped (List.assoc v filenv)) with
                 [ Not_found -> () ]
             | c ->
                 try print_string (List.assoc c env) with
                 [ Not_found -> do print_char '%'; print_char c; return () ] ]
         | c -> print_char c ];
       done
     with [ End_of_file -> () ];
     close_in ic;
  return ()
;

value variables env =
  let ic = open_in (template_fname env "conf") in
  let vlist = ref [] in
  let flist = ref [] in
  do try
       while True do
         match input_char ic with
         [ '%' ->
             match input_char ic with
             [ 'e' | 'c' ->
                 let (v, _) = get_binding ic in
                 if not (List.mem v vlist.val) then
                   vlist.val := [v :: vlist.val]
                 else ()
             | 'v' ->
                 let v = get_variable ic in
                 if not (List.mem v vlist.val) then
                   vlist.val := [v :: vlist.val]
                 else ()
             | 'f' ->
                 let v = get_variable ic in
                 if not (List.mem v vlist.val) then
                   flist.val := [v :: flist.val]
                 else ()
             | _ -> () ]
         | _ -> () ];
       done
     with
     [ End_of_file -> () ];
     close_in ic;
  return (vlist.val, flist.val)
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
  do printf "content-type: text/html"; crlf (); crlf ();
     printf "\
<head><title>Error</title></head>\n<body>
<h1><font color=red>Error</font></h1>
%s
</body>
" (String.capitalize txt);
  return ()
;

value gwtp_invalid_request str env = gwtp_error "Invalid request";

value random_self_init () =
  let seed = int_of_float (mod_float (Unix.time ()) (float max_int)) in
  let seed = seed + 256 * Unix.getpid () in
  Random.init seed
;

random_self_init ();

value mk_passwd size =
  loop 0 where rec loop len =
    if len = size then Buff.get len
    else
      let r = Random.int (26 + 26 + 10) in
      let v =
        if r < 26 then Char.code 'a' + r
        else if r < 52 then Char.code 'A' + r - 26
        else Char.code '0' + r - 52
      in
      loop (Buff.store len (Char.chr v))
;

(* Base configuration *)

value get_base_conf env b =
  let fname = Filename.concat gwtp_dst.val (b ^ ".gwf" ) in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let (variables, files) = variables env in
      let varenv =
        let varenv = ref [] in
        let rec record line =
          fun
          [ [v :: l] ->
              if lowercase_start_with line (v ^ "=") then
                let len = String.length v + 1 in
                let x = String.sub line len (String.length line - len) in
                varenv.val := [(v, x) :: varenv.val]
              else record line l
          | [] -> () ]
        in
        do try
             while True do
               let line =
                 let line = input_line ic in
                 if String.length line > 0
                 && line.[String.length line - 1] = '\r' then
                   String.sub line 0 (String.length line - 1)
                 else line
               in
               record line variables;
             done
           with
           [ End_of_file -> close_in ic ];
        return varenv.val
      in
      let filenv =
        List.map
          (fun fsuff ->
             let fname =
               List.fold_right Filename.concat [gwtp_dst.val; "lang"]
                 (b ^ "." ^ fsuff)
             in
             match try Some (open_in fname) with [ Sys_error _ -> None ] with
             [ Some ic ->
                 let len = ref 0 in
                 do try
                      while True do
                        len.val := Buff.store len.val (input_char ic);
                      done
                    with
                    [ End_of_file -> close_in ic ];
                 return (fsuff, Buff.get len.val)
             | None -> (fsuff, "") ])
          files
      in
      (varenv, filenv)
  | None ->
      ([("friend_passwd", mk_passwd 9); ("wizard_passwd", mk_passwd 9)], []) ]
;

value set_base_conf b varenv =
  let fname = Filename.concat gwtp_dst.val (b ^ ".gwf" ) in
  let fname_out = Filename.concat gwtp_dst.val (b ^ "1.gwf" ) in
  let fname_saved = fname ^ "~" in
  let varenv = List.map (fun (k, v) -> (k, v, ref False)) varenv in
  let rec extract line =
    fun
    [ [(k, v, is_set) :: varenv] ->
        if lowercase_start_with line (k ^ "=") then
          do is_set.val := True; return
          k ^ "=" ^ v
        else extract line varenv
    | [] -> line ]
  in
  let oc = open_out fname_out in
  let ic_opt =
    try Some (open_in fname) with
    [ Sys_error _ ->
        let fname = Filename.concat gwtp_tmp.val "default.gwf" in
        try Some (open_in fname) with [ Sys_error _ -> None ] ]
  in
  do match ic_opt with
     [ Some ic ->
         try
           while True do
             let line = input_line ic in
             let line_out = extract line varenv in
             fprintf oc "%s\n" line_out;
           done
         with
         [ End_of_file -> close_in ic ]
     | None -> () ];
     List.iter
       (fun (k, v, is_set) ->
          if not is_set.val && v <> "" then fprintf oc "%s=%s\n" k v else ())
       varenv;
     close_out oc;
     try Sys.remove fname_saved with [ Sys_error _ -> () ];
     let bdir = Filename.concat gwtp_dst.val (b ^ ".gwb") in
     let b_ex = Sys.file_exists bdir in
     do if b_ex then Sys.rename bdir (bdir ^ "~") else ();
        if Sys.file_exists fname then Sys.rename fname fname_saved else ();
        Sys.rename fname_out fname;
        if b_ex then Sys.rename (bdir ^ "~") bdir else ();
     return ();
  return ()
;

value set_base_files b filenv =
  List.iter
    (fun (k, v) ->
       let fname =
         List.fold_right Filename.concat [gwtp_dst.val; "lang"] (b ^ "." ^ k)
       in
       if v = "" then
         try Sys.remove fname with [ Sys_error _ -> () ]
       else
         let oc = open_out fname in
         do output_string oc v; close_out oc; return ())
    filenv
;

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

value check_login b p =
  let line1 = b ^ ":" ^ p in
  let ic = open_in (Filename.concat gwtp_tmp.val "passwd") in
  let login_ok =
    loop () where rec loop () =
      match try Some (input_line ic) with [ End_of_file -> None ] with
      [ Some line ->
          let line =
            if String.length line > 0
            && line.[String.length line - 1] = '\r' then
              String.sub line 0 (String.length line - 1)
            else line
          in
          if line = line1 then True else loop ()
      | None -> False ]
  in
  do close_in ic; return
  if login_ok then Some (mk_passwd 12) else do Unix.sleep 5; return None
;

value check_token fname from b tok =
  let tokens = read_tokens fname in
  let from_b = from ^ "/" ^ b in
  let tm = Unix.time () in
  loop tokens where rec loop =
    fun
    [ [(tm0, from_b0, tok0) :: tokens] ->
        if tm < tm0 || tm -. tm0 > token_tmout.val then loop tokens
        else if from_b = from_b0 && tok = tok0 then True
        else loop tokens
    | [] -> False ]
;

value update_tokens fname from b tok =
  let tokens = read_tokens fname in
  let from_b = from ^ "/" ^ b in
  let tm = Unix.time () in
  let tokens =
    List.fold_right
      (fun ((tm0, from_b0, tok0) as token) tokens ->
         if tm < tm0 || tm -. tm0 > token_tmout.val then tokens
         else if from_b = from_b0 && tok = tok0 then tokens
         else [token :: tokens])
      tokens []
  in
  let tokens = [(tm, from_b, tok) :: tokens] in
  write_tokens fname tokens
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
     printf "<a href=\"%s?m=MAIN;b=%s;t=%s\">main page</a></div>\n"
       (cgi_script_name ()) b tok;
  return ()
;

value send_file str env b tok f fname =
  let fname = filename_basename fname in
  let lockf = Filename.concat gwtp_tmp.val (b ^ ".lck") in
  if fname = "base" then
    do printf "content-type: text/html"; crlf (); crlf ();
       printf "\
<head><title>Gwtp...</title></head>\n<body>
<h1 align=center>Gwtp...</h1>
<pre>\n";
       flush stdout;
       lock lockf with
       [ Accept ->
           do make_temp env b;
              printf "\nTemporary data base created.\n";
              flush stdout;
              copy_temp b;
              printf "Data base \"%s\" updated.\n" b;
           return ()
       | Refuse ->
           do printf "Data base is already being transfered.<br>\n";
              printf "Please try again later.\n";
           return () ];
       flush stdout;
       printf "</pre>\n";
       printf_link_to_main b tok;
       printf "</body>\n";
    return ()
  else
    do printf "content-type: text/html"; crlf (); crlf ();
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
      do printf "content-type: bin/geneweb"; crlf ();
         printf "content-disposition: attachement; filename=%s" fname;
         crlf (); crlf ();
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

value acceptable_tags =
  ["!--"; "a"; "br"; "em"; "font"; "hr"; "i"; "img"; "p"]
;

value secure_html s =
  loop 0 0 where rec loop len i =
    if i = String.length s then Buff.get len
    else
      match s.[i] with
      [ '<' ->
          let i = i + 1 in
          let (slash, i) =
            if i = String.length s then (False, i)
            else if s.[i] = '/' then (True, i + 1) else (False, i)
          in
          let (tag, i) =
            loop "" i where rec loop tag i =
              if i = String.length s then ("", i)
              else
                match s.[i] with
                [ 'a'..'z' | 'A'..'Z' | '!' | '-' ->
                    loop (tag ^ String.make 1 s.[i]) (i + 1)
                | _ -> (tag, i) ]
          in
          let len =
            if List.mem (String.lowercase tag) acceptable_tags then
              Buff.store len '<'
            else Buff.mstore len "&lt;"
          in
          let len = if slash then Buff.store len '/' else len in
          loop (Buff.mstore len tag) i
      | c -> loop (Buff.store len c) (i + 1) ]
;

value gwtp_setconf str env b tok =
  let (variables, files) = variables env in
  let varenv =
    List.fold_right
      (fun k varenv ->
         match HttpEnv.getenv env k with
         [ Some v -> [(k, v) :: varenv]
         | None -> varenv ])
      variables []
  in
  let filenv =
    List.fold_right
      (fun k filenv ->
         match HttpEnv.getenv env k with
         [ Some v -> [(k, secure_html v) :: filenv]
         | None -> filenv ])
      files []
  in    
  do printf "content-type: text/html"; crlf (); crlf ();
     printf "\
<head><title>Gwtp - configuration %s</title></head>\n<body>
<h1 align=center>Gwtp - configuration %s</h1>
" b b;
     set_base_conf b varenv;
     set_base_files b filenv;
     printf "Configuration changed\n";
     printf_link_to_main b tok;
     printf "</body>\n";
  return ()
;

value gwtp_upload str env b tok =
  let bcnf = Filename.concat gwtp_dst.val (b ^ ".gwf") in
  if not (Sys.file_exists bcnf) then
    gwtp_error "no configuration file"
  else
    do printf "content-type: text/html"; crlf (); crlf ();
       copy_template env ([], [])
         [('s', cgi_script_name ()); ('b', b); ('t', tok)] "send";
       printf_link_to_main b tok;
       printf "</body>\n";
    return ()
;

value gwtp_download str env b tok =
  let bcnf = Filename.concat gwtp_dst.val (b ^ ".gwf") in
  let bdir = Filename.concat gwtp_dst.val (b ^ ".gwb") in
  if not (Sys.file_exists bcnf) then
    gwtp_error "no configuration file"
  else
    do printf "content-type: text/html"; crlf (); crlf ();
       if Sys.file_exists bdir then
       let dh = Unix.opendir bdir in
       do copy_template env ([], [])
            [('s', cgi_script_name ()); ('b', b); ('t', tok)] "recv";
          printf "<ul>\n";
          try
            while True do
              let f = Unix.readdir dh in
              let st = Unix.stat (Filename.concat bdir f) in
              if st.Unix.st_kind == Unix.S_REG
              && f.[String.length f - 1] <> '~' then
                do printf "<li><tt>";
                   printf "<a href=\"%s?m=RECV;b=%s;t=%s;f=/%s\">%s</a>"
                     (cgi_script_name ()) b tok f f;
                   let sz = string_of_int st.Unix.st_size in
                   printf "%t%s bytes"
                     (fun oc ->
                        for i = 1 to 25 - String.length sz - String.length f
                        do fprintf oc "&nbsp;"; done)
                     sz;
                   printf "</tt>\n";
                return ()
              else ();
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

value gwtp_config str env b tok =
  let (varenv, filenv) = get_base_conf env b in
  do printf "content-type: text/html"; crlf (); crlf ();
     copy_template env (varenv, filenv)
       [('s', cgi_script_name ()); ('b', b); ('t', tok)]
       "conf";
     printf_link_to_main b tok;
     printf "</body>\n";
  return ()
;

value gwtp_main str env b tok =
  let gwtp_comm = cgi_script_name () in
  let config_exists =
    Sys.file_exists (Filename.concat gwtp_dst.val (b ^ ".gwf"))
  in
  do printf "content-type: text/html"; crlf (); crlf ();
     printf "\
<head><title>Gwtp - %s</title></head>\n<body>
<h1 align=center>Gwtp - %s</h1>
<ul>\n" b b;
     if config_exists then
       do printf "<li><a href=\"%s?m=UPL;b=%s;t=%s\">Upload</a>\n"
            gwtp_comm b tok;
          printf "<li><a href=\"%s?m=DNL;b=%s;t=%s\">Download</a>\n"
            gwtp_comm b tok;
       return ()
     else ();
     printf "<li><a href=\"%s?m=CNF;b=%s;t=%s\">Configuration</a>\n" gwtp_comm
       b tok;
     printf "</ul>\n";
     if gw_site.val <> "" && config_exists then
       do printf "<p>\n<ul>\n";
          printf "<li><a href=\"%s%s\">Browse</a>\n" gw_site.val b;
          printf "</ul>\n";
       return ()
     else ();
     printf "</body>\n";
  return ()  
;

value gwtp_login str env =
  do printf "content-type: text/html"; crlf (); crlf ();
     printf "\
<head><title>Gwtp</title></head>\n<body>
<h1>Gwtp</h1>
<form method=POST action=%s>
<input type=hidden name=m value=LOGIN>
Data base: <input name=b><br>
Password: <input name=p type=password><br>
<input type=submit value=Login>
</form>
</body>
" (cgi_script_name ());
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
      let fname = tokens_file_name () in
      if check_token fname from b t then
        do try gwtp_fun str env b t with e ->
             do update_tokens fname from b t; return raise e;
           update_tokens fname from b t;
        return ()
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
     fprintf oc_log " %s?%s\n" (cgi_script_name ()) str;
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
  do ifdef UNIX then let _ = Unix.umask 0 in () else ();
     log oc_log str;
     flush oc_log;
     Unix.dup2 (Unix.descr_of_out_channel oc_log) Unix.stderr;
     match HttpEnv.getenv env "m" with
     [ Some "LOGIN" -> gwtp_check_login from str env gwtp_main
     | Some "MAIN" -> gwtp_logged from str env gwtp_main
     | Some "UPL" -> gwtp_logged from str env gwtp_upload
     | Some "DNL" -> gwtp_logged from str env gwtp_download
     | Some "CNF" -> gwtp_logged from str env gwtp_config
     | Some "SEND" -> gwtp_logged from str env gwtp_send
     | Some "RECV" -> gwtp_logged from str env gwtp_receive
     | Some "SCNF" -> gwtp_logged from str env gwtp_setconf
     | Some _ -> gwtp_invalid_request str env
     | None -> gwtp_login str env ];
     flush stdout;
     flush oc_log;
     close_out oc_log;
  return ()
;

value usage_msg = "Usage: gwtp";
value speclist =
  [("-tmp", Arg.String (fun x -> gwtp_tmp.val := x),
    "<dir>: directory for gwtp stuff; default: " ^ gwtp_tmp.val);
   ("-dst", Arg.String (fun x -> gwtp_dst.val := x),
    "<dir>: directory for data bases; default: " ^ gwtp_dst.val);
   ("-site", Arg.String (fun x -> gw_site.val := x),
    "<url>: site (if any) where data bases are accomodated")]
;
value anonfun _ = do Arg.usage speclist usage_msg; return exit 2;

value main () =
  do Arg.parse speclist anonfun usage_msg;
     gwtp ();
  return ()
;

try main () with exc ->
  do eprintf "Exception raised: %s\n" (Printexc.to_string exc);
     flush stderr;
  return ()
;
