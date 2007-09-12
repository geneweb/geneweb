(* camlp5r ../src/pa_lock.cmo *)
(* $Id: gwtp.ml,v 5.8 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk;
open Printf;

value gwtp_tmp = ref (Filename.concat ".." "gwtp_tmp");
value gwtp_dst = ref (Filename.concat ".." "gwtp_dst");
value gwtp_log = ref "";
value gwtp_etc = ref "";
value gw_site = ref "";
value no_upload = ref False;
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
  if len >= 0 then do {
    let buff = String.create len in really_input stdin buff 0 len; buff
  }
  else do {
    let buff = ref "" in
    try
      while True do { let l = input_line stdin in buff.val := buff.val ^ l }
    with
    [ End_of_file -> () ];
    buff.val
  }
;

value cgi_content_type () =
  try Sys.getenv "CONTENT_TYPE" with [ Not_found -> "" ]
;

value cgi_script_name () =
  try filename_basename (Sys.getenv "SCRIPT_NAME") with
  [ Not_found -> "gwtp" ]
;

value cgi_content () =
  let is_post =
    try Sys.getenv "REQUEST_METHOD" = "POST" with [ Not_found -> False ]
  in
  if is_post then do {
    let len =
      try int_of_string (Sys.getenv "CONTENT_LENGTH") with [ Not_found -> -1 ]
    in
    set_binary_mode_in stdin True;
    read_input len
  }
  else try Sys.getenv "QUERY_STRING" with [ Not_found -> "" ]
;

value cgi_from () =
  try Sys.getenv "REMOTE_HOST" with
  [ Not_found -> try Sys.getenv "REMOTE_ADDR" with [ Not_found -> "" ] ]
;

(* Utilitaires *)

value crlf () =
  do {
    flush stdout;
    let _ : int = Unix.write Unix.stdout "\013\n" 0 2 in
    ()
  }
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
        [ '"' -> do { String.blit "&#034;" 0 s1 i1 6; i1 + 6 }
        | '&' -> do { String.blit "&amp;" 0 s1 i1 5; i1 + 5 }
        | '<' -> do { String.blit "&lt;" 0 s1 i1 4; i1 + 4 }
        | '>' -> do { String.blit "&gt;" 0 s1 i1 4; i1 + 4 }
        | c -> do { s1.[i1] := c; succ i1 } ]
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (String.create len) 0 0
  else s
;

value log_open () =
  let fname = Filename.concat gwtp_log.val "gwtp.log" in
  open_out_gen [Open_wronly; Open_creat; Open_append] 0o644 fname
;

type env_val = [ Val of string | Fun of unit -> unit ];

value macro env c =
  match try Some (List.assoc c env) with [ Not_found -> None ] with
  [ Some (Val s) -> s
  | _ -> "%" ^ String.make 1 c ]
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
  List.fold_right Filename.concat [gwtp_etc.val; "lang"] (fname ^ ".txt")
;

value lindex s c =
  pos 0 where rec pos i =
    if i = String.length s then None
    else if s.[i] = c then Some i
    else pos (i + 1)
;

value input_lexicon lang =
  let ht = Hashtbl.create 501 in
  do {
    Mutil.input_lexicon lang ht
      (fun () ->
         open_in
           (List.fold_right Filename.concat [gwtp_etc.val; "lang"]
              "lexicon.txt"));
    ht
  }
;

value unfreeze_lexicon =
  let lexicon = ref None in
  fun lang ->
    match lexicon.val with
    [ Some lex -> lex
    | None ->
        let lex = input_lexicon lang in
        do {
          lexicon.val := Some lex;
          lex
        } ]
;

value transl lang w =
  let lexicon = unfreeze_lexicon lang in
  try Hashtbl.find lexicon w with [ Not_found -> "[" ^ w ^ "]" ]
;

value copy_template genv (varenv, filenv) env if_env fname =
  let lang =
    match HttpEnv.getenv genv "lang" with
    [ Some x -> x
    | _ -> "en" ]
  in
  let echo = ref True in
  let (push_echo, pop_echo) =
    let stack = ref [] in
    (fun x -> do { stack.val := [echo.val :: stack.val]; echo.val := x; },
     fun () ->
       match stack.val with
       [ [x :: l] -> do { stack.val := l; echo.val := x; }
       | [] -> echo.val := True ])
  in
  let ic = open_in (template_fname env fname) in
  let rec if_expr =
    fun
    [ 'N' -> not (if_expr (input_char ic))
    | c ->
        try List.assoc c if_env with
        [ Not_found -> do { printf "!!!!!%c!!!!!" c; True } ] ]
  in
  do {
    try
      while True do {
        match input_char ic with
        [ '%' ->
            match input_char ic with
            [ 'I' -> push_echo (echo.val && if_expr (input_char ic))
            | 'E' -> pop_echo ()
            | _ when not echo.val -> ()
            | 's' -> print_string (cgi_script_name ())
            | 'c' | 'e' as x ->
                let (v, k) = get_binding ic in
                try
                  if k = List.assoc v varenv then
                    print_string (if x = 'c' then " checked" else " selected")
                  else ()
                with
                [ Not_found -> () ]
            | 'v' ->
                let v = get_variable ic in
                try print_string (quote_escaped (List.assoc v varenv)) with
                [ Not_found -> () ]
            | 'f' ->
                let v = get_variable ic in
                try print_string (quote_escaped (List.assoc v filenv)) with
                [ Not_found -> () ]
            | 'l' ->
                print_string lang
            | 'L' ->
                let v = get_variable ic in
                let lang_def = transl lang " !languages" in
                print_string (Translate.language_name v lang_def)
            | c ->
                match
                  try Some (List.assoc c env) with [ Not_found -> None ]
                with
                [ Some (Val s) -> print_string s
                | Some (Fun f) -> f ()
                | None -> do { print_char '%'; print_char c; } ] ]
        | '[' ->
            let s =
              let c = input_char ic in
              let s =
                loop 0 (if c = '\n' then input_char ic else c)
                where rec loop len c =
                  if c = ']' then Buff.get len
                  else loop (Buff.store len c) (input_char ic)
              in
              if c = '\n' then
                let (s, alt) = Translate.inline lang '%' (macro env) s in
                if alt then "[" ^ s ^ "]" else s
              else transl lang s
            in
            if echo.val then print_string s else ()
        | c -> if echo.val then print_char c else () ]
      }
    with
    [ End_of_file -> () ];
    close_in ic;
  }
;

value variables env =
  let ic = open_in (template_fname env "conf") in
  let vlist = ref [] in
  let flist = ref [] in
  do {
    try
      while True do {
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
        | _ -> () ]
      }
    with
    [ End_of_file -> () ];
    close_in ic;
    (vlist.val, flist.val)
  }
;

value sys_copy src dst =
  let ic = open_in src in
  let oc = open_out dst in
  do {
    try while True do { let c = input_char ic in output_char oc c } with
    [ End_of_file -> () ];
    close_out oc;
    close_in ic;
  }
;

value remove_dir_contents dir =
  let dh = Unix.opendir dir in
  try
    while True do {
      match Unix.readdir dh with
      [ "." | ".." -> ()
      | f -> Unix.unlink (Filename.concat dir f) ]
    }
  with
  [ End_of_file -> Unix.closedir dh ]
;

(*
value html_escaped s =
  let s = String.escaped s in
  let rec loop i len =
    if i = String.length s then Buff.get len
    else
      let len =
        match s.[i] with
        [ '<' -> Buff.mstore len "&lt;"
        | '>' -> Buff.mstore len "&gt;"
        | x -> Buff.store len x ]
      in
      loop (i + 1) len
  in
  loop 0 0
;
*)

value gwtp_error txt =
  do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    printf "\
<head><title>Error</title></head>
<body>
<h1><font color=red>Error</font></h1>
%s
</body>
" (String.capitalize txt);
  }
;

value gwtp_invalid_request str env = gwtp_error "Invalid request";

value random_self_init () =
  let seed = int_of_float (mod_float (Unix.time ()) (float max_int)) in
  let seed = seed + 256 * Unix.getpid () in Random.init seed
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
  let fname = Filename.concat gwtp_dst.val (b ^ ".gwf") in
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
        do {
          try
            while True do {
              let line =
                let line = input_line ic in
                if String.length line > 0 &&
                   line.[String.length line - 1] = '\r' then
                  String.sub line 0 (String.length line - 1)
                else line
              in
              record line variables
            }
          with
          [ End_of_file -> close_in ic ];
          varenv.val
        }
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
                 do {
                   try
                     while True do {
                       len.val := Buff.store len.val (input_char ic)
                     }
                   with
                   [ End_of_file -> close_in ic ];
                   (fsuff, Buff.get len.val)
                 }
             | None -> (fsuff, "") ])
          files
      in
      (varenv, filenv)
  | None ->
      ([("friend_passwd", mk_passwd 9); ("wizard_passwd", mk_passwd 9)], []) ]
;

value set_base_conf b varenv =
  let fname = Filename.concat gwtp_dst.val (b ^ ".gwf") in
  let fname_out = Filename.concat gwtp_dst.val (b ^ "1.gwf") in
  let fname_saved = fname ^ "~" in
  let varenv = List.map (fun (k, v) -> (k, v, ref False)) varenv in
  let rec extract line =
    fun
    [ [(k, v, is_set) :: varenv] ->
        if lowercase_start_with line (k ^ "=") then do {
          is_set.val := True; k ^ "=" ^ v
        }
        else extract line varenv
    | [] -> line ]
  in
  let oc = open_out fname_out in
  let ic_opt =
    try Some (open_in fname) with
    [ Sys_error _ ->
        let fname = Filename.concat gwtp_etc.val "default.gwf" in
        try Some (open_in fname) with [ Sys_error _ -> None ] ]
  in
  do {
    match ic_opt with
    [ Some ic ->
        try
          while True do {
            let line = input_line ic in
            let line_out = extract line varenv in
            fprintf oc "%s\n" line_out
          }
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
    if b_ex then Sys.rename bdir (bdir ^ "~") else ();
    if Sys.file_exists fname then Sys.rename fname fname_saved else ();
    Sys.rename fname_out fname;
    if b_ex then Sys.rename (bdir ^ "~") bdir else ();
  }
;

value set_base_files b filenv =
  List.iter
    (fun (k, v) ->
       let fname =
         List.fold_right Filename.concat [gwtp_dst.val; "lang"] (b ^ "." ^ k)
       in
       if v = "" then try Sys.remove fname with [ Sys_error _ -> () ]
       else do {
         let oc = open_out fname in output_string oc v; close_out oc;
       })
    filenv
;

(* Login and tokens *)

value tokens_file_name () = Filename.concat gwtp_tmp.val "token";

value read_tokens fname =
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let rec loop list =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some line ->
            let i = String.index line ' ' in
            let j = String.index_from line (i + 1) ' ' in
            let tm = float_of_string (String.sub line 0 i) in
            let b = String.sub line (i + 1) (j - i - 1) in
            let tok = String.sub line (j + 1) (String.length line - j - 1) in
            loop [(tm, b, tok) :: list]
        | None -> do { close_in ic; List.rev list } ]
      in
      loop []
  | None -> [] ]
;

value write_tokens fname tokens =
  let oc = open_out fname in
  do {
    List.iter
      (fun (tm, from_b, tok) -> fprintf oc "%.0f %s %s\n" tm from_b tok)
      tokens;
    close_out oc;
  }
;

value check_login b p =
  let line1 = b ^ ":" ^ p in
  let ic = open_in (Filename.concat gwtp_etc.val "passwd") in
  let login_ok =
    loop () where rec loop () =
      match try Some (input_line ic) with [ End_of_file -> None ] with
      [ Some line ->
          let line =
            if String.length line > 0 &&
               line.[String.length line - 1] = '\r' then
              String.sub line 0 (String.length line - 1)
            else line
          in
          if line = line1 then True else loop ()
      | None -> False ]
  in
  do {
    close_in ic;
    if login_ok then Some (mk_passwd 12) else do { Unix.sleep 5; None }
  }
;

value check_token fname from b tok =
  let tokens = read_tokens fname in
  let from_b = from ^ "/" ^ b in
  let tm = Unix.time () in
  let rec loop =
    fun
    [ [(tm0, from_b0, tok0) :: tokens] ->
        if tm < tm0 || tm -. tm0 > token_tmout.val then loop tokens
        else if from_b = from_b0 && tok = tok0 then True
        else loop tokens
    | [] -> False ]
  in
  loop tokens
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
  let tokens = [(tm, from_b, tok) :: tokens] in write_tokens fname tokens
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
  do {
    if fname = "" then ()
    else if fname <> name then
      printf "You selected <b>%s</b> instead of <b>%s</b> -&gt; ignored.\n"
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
      if len > 0 then do {
        let oc = open_out (Filename.concat bdir name) in
        output oc contents (j + 1) len;
        flush oc;
        printf "File \"%s\" transfered.\n" name;
        close_out oc;
      }
      else ();
    flush stdout;
  }
;

value make_temp env b =
  let bdir = Filename.concat gwtp_tmp.val (b ^ ".gwb") in
  do {
    if Sys.file_exists bdir then remove_dir_contents bdir
    else Unix.mkdir bdir 0o777;
    insert_file env bdir "base";
    insert_file env bdir "notes";
    insert_file env bdir "patches";
    insert_file env bdir "particles.txt";
    flush stdout;
    let base = Iolight.input bdir in
    printf "\n";
    printf "persons: %d\n" base.data.persons.len;
    printf "families: %d\n\n" base.data.families.len;
    flush stdout;
    Secure.set_base_dir (Filename.dirname bdir);
    Outbase.output bdir base;
    flush stdout;
  }
;

value copy_temp b =
  let bdir = Filename.concat gwtp_tmp.val (b ^ ".gwb") in
  let dir_old = Filename.concat gwtp_dst.val "old" in
  let dir_old_gwb = Filename.concat dir_old (b ^ ".gwb") in
  let dir_gwb = Filename.concat gwtp_dst.val (b ^ ".gwb") in
  do {
    if Sys.file_exists dir_gwb then do {
      if not (Sys.file_exists dir_old) then Unix.mkdir dir_old 0o777 else ();
      if Sys.file_exists dir_old_gwb then do {
        remove_dir_contents dir_old_gwb; Unix.rmdir dir_old_gwb;
      }
      else ();
      Sys.rename dir_gwb dir_old_gwb;
      let old_forum = Filename.concat dir_old_gwb "forum" in
      if Sys.file_exists old_forum then
        sys_copy old_forum (Filename.concat bdir "forum")
      else ();
      let old_wiznotes = Filename.concat dir_old_gwb "wiznotes" in
      if Sys.file_exists old_wiznotes then
        Sys.rename old_wiznotes (Filename.concat bdir "wiznotes")
      else ()
    }
    else ();
    Sys.rename bdir dir_gwb;
  }
;

value printf_link_to_main env b tok =
  let lang =
    match HttpEnv.getenv env "lang" with
    [ Some x -> x
    | _ -> "en" ]
  in
  do {
    printf "<p><hr><div align=right>\n";
    printf "<a href=\"%s?m=MAIN;b=%s;t=%s;lang=%s\">%s</a></div>\n"
      (cgi_script_name ()) b tok lang (transl lang "main page");
  }
;

(* Upload from GEDCOM *)

value make_gedcom_file env b =
  let fname = Filename.concat gwtp_tmp.val (b ^ ".ged") in
  let oc = open_out fname in
  do {
    let contents = List.assoc "gedcom" env in
    let i =
      if lowercase_start_with contents "content-type: " then
        String.index contents '\n'
      else 0
    in
    let j = String.index_from contents (i + 1) '\n' in
    let len = String.length contents - j - 3 in
    output_string oc (String.sub contents (j + 1) len);
    close_out oc;
  }
;

value ged2gwb b =
  let comm =
    Filename.concat gwtp_etc.val "ged2gwb" ^ " " ^
    Filename.concat gwtp_tmp.val (b ^ ".ged") ^ " -f -o " ^
    Filename.concat gwtp_tmp.val b ^ " > " ^
    Filename.concat gwtp_tmp.val (b ^ ".log")
  in
  let _ = Sys.command comm in ()
;

value move_gedcom_to_old b =
  do {
    let dir = Filename.concat gwtp_tmp.val "ged" in
    try Unix.mkdir dir 0o775 with [ Unix.Unix_error _ _ _ -> () ];
    let fname_old = Filename.concat dir (b ^ ".ged") in
    try Sys.remove fname_old with [ Sys_error _ -> () ];
    let fname = Filename.concat gwtp_tmp.val (b ^ ".ged") in
    Sys.rename fname fname_old;
  }
;

value send_gedcom_file str env b tok f fname =
  let fname = filename_basename fname in
  if Filename.check_suffix fname ".ged" || Filename.check_suffix fname ".GED"
  then do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    printf "\
<head><title>Gwtp...</title></head>
<body>
<h1 align=center>Gwtp...</h1>
<pre>
";
    flush stdout;
    make_gedcom_file env b;
    printf "\nGedcom file transfered.\n";
    flush stdout;
    ged2gwb b;
    printf "New database created.\n";
    flush stdout;
    copy_temp b;
    printf "Database \"%s\" updated.\n" b;
    printf "<a href=\"%s?m=LOG;b=%s;t=%s\">Command output</a>\n"
      (cgi_script_name ()) b tok;
    flush stdout;
    move_gedcom_to_old b;
    printf "</pre>\n";
    printf_link_to_main env b tok;
    printf "</body>\n";
    flush stdout;
  }
  else gwtp_error "This is not a gedcom file (not ending with .GED)"
;

value gwtp_send_gedcom str env b t =
  match (HttpEnv.getenv env "gedcom", HttpEnv.getenv env "gedcom_name") with
  [ (Some f, Some fname) ->
      send_gedcom_file str env b t f (HttpEnv.decode fname)
  | (Some f, None) ->
      gwtp_error "Sorry, your browser seems not be able to send files."
  | _ -> gwtp_invalid_request str env ]
;

value gwtp_upload_gedcom str env b tok =
  let bcnf = Filename.concat gwtp_dst.val (b ^ ".gwf") in
  if not (Sys.file_exists bcnf) then gwtp_error "no configuration file"
  else do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    copy_template env ([], []) [('b', Val b); ('t', Val tok)] [] "send_gedcom";
  }
;

value gwtp_print_log str env b tok =
  do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    printf "\
<head><title>Gwtp - %s</title></head>
<body>
<h1 align=center>Gwtp - %s</h1>
" b b;
    let fname = Filename.concat gwtp_tmp.val (b ^ ".log") in
    let ic = open_in fname in
    printf "<pre>\n";
    try
      while True do {
        output_char stdout (input_char ic);
      }
    with
    [ End_of_file -> () ];
    printf "</pre>\n";
    close_in ic;
    printf_link_to_main env b tok;
    printf "</body>\n";
  }  
;

value gwtp_print_accesses of_wizards str env b tok =
  do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    printf "\
<head><title>Gwtp - %s</title></head>
<body>
<h1 align=center>Gwtp - %s</h1>
" b b;
    let (varenv, filenv) = get_base_conf env b in
    let fname =
      try
        List.assoc
          (if of_wizards then "wizard_passwd_file" else "friend_passwd_file")
          varenv
      with [ Not_found -> "" ]
    in
    let fname =
      if fname <> "" then ""
      else
        List.fold_right Filename.concat [gwtp_dst.val; "cnt"]
          (b ^ (if of_wizards then "_w.txt" else "_f.txt"))
    in
    printf "<pre>\n";
    if fname = "" then printf "[no password file]\n"
    else
      try
        do {
          let ic = open_in fname in
          try
            while True do {
              output_char stdout (input_char ic);
            }
          with
          [ End_of_file -> () ];
          close_in ic;
        }
      with
      [ Sys_error _ -> printf "[nothing]\n" ];
    printf "</pre>\n";
    printf_link_to_main env b tok;
    printf "</body>\n";
  }  
;

(* Actions *)

value send_file str env b tok f fname =
  let fname = filename_basename fname in
  let lockf = Filename.concat gwtp_tmp.val (b ^ ".lck") in
  if fname = "base" then do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    printf "\
<head><title>Gwtp...</title></head>
<body>
<h1 align=center>Gwtp...</h1>
<pre>
";
    flush stdout;
    lock lockf with
    [ Accept ->
        do {
          make_temp env b;
          printf "\nTemporary database created.\n";
          flush stdout;
          copy_temp b;
          printf "Database \"%s\" updated.\n" b;
        }
    | Refuse ->
        do {
          printf "Database is already being transfered.<br>\n";
          printf "Please try again later.\n";
        } ];
    flush stdout;
    printf "</pre>\n";
    printf_link_to_main env b tok;
    printf "</body>\n";
  }
  else do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    printf "\
<head><title>Error</title></head>
<body>
<h1><font color=red>Error</font></h1>
";
    if fname = "" then
      printf "You must select at least the <b>base</b> file\n"
    else
      printf "You selected the file <b>%s</b> instead of <b>base</b>\n" fname;
    printf "</body>\n";
    printf_link_to_main env b tok;
  }
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
      do {
        printf "content-type: bin/geneweb";
        crlf ();
        printf "content-disposition: attachement; filename=%s" fname;
        crlf ();
        crlf ();
        let ic = open_in (Filename.concat bdir fname) in
        try
          while True do { let c = input_char ic in output_char stdout c }
        with
        [ End_of_file -> () ];
        close_in ic;
      }
  | _ -> gwtp_invalid_request str env ]
;

value acceptable_tags =
  ["!--"; "a"; "b"; "br"; "em"; "font"; "hr"; "i"; "img"; "li"; "ol"; "p";
   "table"; "td"; "tr"; "ul"]
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
            else if s.[i] = '/' then (True, i + 1)
            else (False, i)
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
  do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    printf "\
<head><title>Gwtp - configuration %s</title></head>
<body>
<h1 align=center>Gwtp - configuration %s</h1>
" b b;
    set_base_conf b varenv;
    set_base_files b filenv;
    printf "Configuration changed\n";
    printf_link_to_main env b tok;
    printf "</body>\n";
  }
;

value gwtp_upload str env b tok =
  let bcnf = Filename.concat gwtp_dst.val (b ^ ".gwf") in
  if not (Sys.file_exists bcnf) then gwtp_error "no configuration file"
  else do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    copy_template env ([], []) [('b', Val b); ('t', Val tok)] [] "send";
  }
;

value gwtp_download str env b tok =
  let bcnf = Filename.concat gwtp_dst.val (b ^ ".gwf") in
  let bdir = Filename.concat gwtp_dst.val (b ^ ".gwb") in
  if not (Sys.file_exists bcnf) then gwtp_error "no configuration file"
  else do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    if Sys.file_exists bdir then do {
      let print_directory () =
        let dh = Unix.opendir bdir in
        do {
          printf "<ul>\n";
          try
            while True do {
              let f = Unix.readdir dh in
              let st = Unix.stat (Filename.concat bdir f) in
              if st.Unix.st_kind = Unix.S_REG &&
                 f.[String.length f - 1] <> '~'
              then do {
                printf "<li><tt>";
                printf "<a href=\"%s?m=RECV;b=%s;t=%s;f=/%s\">%s</a>"
                  (cgi_script_name ()) b tok f f;
                let sz = string_of_int st.Unix.st_size in
                printf "%t%s bytes"
                  (fun oc ->
                     for i = 1 to 25 - String.length sz - String.length f do {
                       fprintf oc "&nbsp;"
                     })
                  sz;
                printf "</tt>\n";
              }
              else ()
            }
          with
          [ End_of_file -> Unix.closedir dh ];
          printf "</ul>\n";
        }
      in
      copy_template env ([], [])
        [('b', Val b); ('t', Val tok); ('d', Fun print_directory)] []
        "recv";
    }
    else do {
      printf "
<head><title>Gwtp - download %s</title></head>
<body>
<h1 align=center>Gwtp - download %s</h1>
<p>Your database does not exist or is empty.
" b b;
      printf_link_to_main env b tok;
      printf "</body>\n";
    }
  }
;

value gwtp_config str env b tok =
  let (varenv, filenv) = get_base_conf env b in
  do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    copy_template env (varenv, filenv) [('b', Val b); ('t', Val tok)] []
      "conf";
  }
;

value gwtp_main str env b tok =
  do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    copy_template env ([], [])
      [('b', Val b); ('t', Val tok); ('w', Val gw_site.val)]
      [('c', Sys.file_exists (Filename.concat gwtp_dst.val (b ^ ".gwf")));
       ('g', Sys.file_exists (Filename.concat gwtp_etc.val "ged2gwb"));
       ('w', gw_site.val <> "")]
      "main";
  }
;

value gwtp_login str env =
  do {
    printf "content-type: text/html";
    crlf ();
    crlf ();
    printf "\
<head><title>Gwtp</title></head>
<body>
<h1>Gwtp</h1>
<form method=POST action=%s>
<input type=hidden name=m value=LOGIN>
Database: <input name=b><br>
Password: <input name=p type=password><br>
<input type=submit value=Login>
</form>
</body>
" (cgi_script_name ());
  }
;

(* Wrappers *)

value gwtp_check_login from str env gwtp_fun =
  match (HttpEnv.getenv env "b", HttpEnv.getenv env "p") with
  [ (Some b, Some p) ->
      match check_login b p with
      [ Some tok -> do { set_token from b tok; gwtp_fun str env b tok; }
      | None -> gwtp_error "Invalid login" ]
  | _ -> gwtp_invalid_request str env ]
;

value gwtp_logged from str env gwtp_fun =
  match (HttpEnv.getenv env "b", HttpEnv.getenv env "t") with
  [ (Some b, Some t) ->
      let fname = tokens_file_name () in
      if check_token fname from b t then do {
        try gwtp_fun str env b t with e ->
          do { update_tokens fname from b t; raise e };
        update_tokens fname from b t;
      }
      else gwtp_error "Login expired"
  | _ -> gwtp_invalid_request str env ]
;

(* Main *)

value log oc_log str =
  let tm = Unix.localtime (Unix.time ()) in
  let user_agent =
    try Sys.getenv "HTTP_USER_AGENT" with [ Not_found -> "" ]
  in
  let referer = try Sys.getenv "HTTP_REFERER" with [ Not_found -> "" ] in
  let from =
    try Sys.getenv "REMOTE_HOST" with
    [ Not_found -> try Sys.getenv "REMOTE_ADDR" with [ Not_found -> "" ] ]
  in
  do {
    fprintf oc_log "%4d-%02d-%02d %02d:%02d:%02d" (1900 + tm.Unix.tm_year)
      (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec;
    fprintf oc_log " %s?%s\n" (cgi_script_name ()) str;
    if from <> "" then fprintf oc_log "  From: %s\n" from else ();
    if user_agent <> "" then fprintf oc_log "  Agent: %s\n" user_agent
    else ();
    if referer <> "" then fprintf oc_log "  Referer: %s\n" referer else ();
  }
;

value gwtp () =
  let content_type = cgi_content_type () in
  let content = cgi_content () in
  let from = cgi_from () in
  let (str, env) = HttpEnv.make content_type content in
  let oc_log = log_open () in
  do {
    log oc_log str;
    flush oc_log;
    Unix.dup2 (Unix.descr_of_out_channel oc_log) Unix.stderr;
    match HttpEnv.getenv env "m" with
    [ Some "LOGIN" -> gwtp_check_login from str env gwtp_main
    | Some "MAIN" -> gwtp_logged from str env gwtp_main
    | Some "DNL" -> gwtp_logged from str env gwtp_download
    | Some "CNF" -> gwtp_logged from str env gwtp_config
    | Some "RECV" -> gwtp_logged from str env gwtp_receive
    | Some "SCNF" -> gwtp_logged from str env gwtp_setconf
    | Some "LOG" -> gwtp_logged from str env gwtp_print_log
    | Some "ACCW" -> gwtp_logged from str env (gwtp_print_accesses True)
    | Some "ACCF" -> gwtp_logged from str env (gwtp_print_accesses False)
    | Some "UPL" when not no_upload.val ->
        gwtp_logged from str env gwtp_upload
    | Some "UPG" when not no_upload.val ->
        gwtp_logged from str env gwtp_upload_gedcom
    | Some "SEND" when not no_upload.val ->
        gwtp_logged from str env gwtp_send
    | Some "SEND_GEDCOM" when not no_upload.val ->
        gwtp_logged from str env gwtp_send_gedcom
    | Some _ -> gwtp_invalid_request str env
    | None -> gwtp_login str env ];
    flush stdout;
    flush oc_log;
    close_out oc_log;
  }
;

value usage_msg = "Usage: gwtp";
value speclist =
  [("-tmp", Arg.String (fun x -> gwtp_tmp.val := x),
    "<dir>: directory for gwtp stuff; default: " ^ gwtp_tmp.val);
   ("-dst", Arg.String (fun x -> gwtp_dst.val := x),
    "<dir>: directory for databases; default: " ^ gwtp_dst.val);
   ("-log", Arg.String (fun x -> gwtp_log.val := x),
    "<log>: directory for log file; default: " ^ gwtp_tmp.val);
   ("-etc", Arg.String (fun x -> gwtp_etc.val := x),
    "<etc>: directory for passwd, default.gwf and lang files; default: " ^
    gwtp_tmp.val);
   ("-site", Arg.String (fun x -> gw_site.val := x),
    "<url>: site (if any) where databases are accomodated");
   ("-noup", Arg.Set no_upload, "no upload");
   ("-tmout", Arg.Float (fun x -> token_tmout.val := x),
    "<sec>: tokens time out; default = " ^
    string_of_float token_tmout.val ^ " sec") ]
;
value anonfun _ = do { Arg.usage speclist usage_msg; exit 2 };

value main () =
  do {
    Arg.parse speclist anonfun usage_msg;
    if gwtp_log.val = "" then gwtp_log.val := gwtp_tmp.val else ();
    if gwtp_etc.val = "" then gwtp_etc.val := gwtp_tmp.val else ();
    gwtp ();
  }
;

try main () with exc ->
  do {
    eprintf "Exception raised: %s\n" (Printexc.to_string exc);
    flush stderr;
  };
