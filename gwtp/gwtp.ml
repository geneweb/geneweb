(* $Id: gwtp.ml,v 1.7 2000-07-28 07:00:51 ddr Exp $ *)

open Printf;

value gwtp_dir = ref "gwtp_dir";
value comm_ged2gwb = ref "../../geneweb/ged2gwb/ged2gwb";
value comm_consang = ref "../../geneweb/src/consang";
value token_tmout = ref 600.0;

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

(*
value server_extract str =
  let content_type = Wserver.extract_param "content-type: " '\n' request in
  (content_type, str)
;
*)

(* Utilitaires *)

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
<h1 align=center><font color=red>Error</font></h1>
%s
</body>
" (String.capitalize txt);
  return ()
;

value gwtp_invalid_request str env = gwtp_error "Invalid request";

(* Login and tokens *)

value tokens_file_name () = Filename.concat gwtp_dir.val "token";

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
       (fun (tm, b, tok) -> fprintf oc "%.0f %s %s\n" tm b tok)
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
  let ic = open_in (Filename.concat gwtp_dir.val "passwd") in
  let login_ok =
    loop () where rec loop () =
      match try Some (input_line ic) with [ End_of_file -> None ] with
      [ Some line -> if line = line1 then True else loop ()
      | None -> False ]
  in
  do close_in ic; return
  if login_ok then Some (mk_passwd ()) else None
;

value check_token fname b tok =
  let tokens = read_tokens fname in
  let tm = Unix.time () in
  loop [] tokens where rec loop tokens_out =
    fun
    [ [(tm0, b0, tok0) :: tokens] ->
        if tm < tm0 || tm -. tm0 > token_tmout.val then
          loop tokens_out tokens
        else if b = b0 && tok = tok0 then
          (True, List.rev tokens_out @ [(tm, b, tok) :: tokens])
        else loop [(tm0, b0, tok0) :: tokens_out] tokens
    | [] -> (False, List.rev tokens_out) ]
;

value set_token b tok =
  let fname = tokens_file_name () in
  let tokens =
    let tokens = read_tokens fname in
    let tm = Unix.time () in
    List.fold_right
      (fun (tm0, b0, tok0) tokens ->
         if b = b0 || tm < tm0 || tm -. tm0 > token_tmout.val then tokens
         else [(tm0, b0, tok0) :: tokens])
      tokens [(tm, b, tok)]
  in
  write_tokens fname tokens
;

(* Requests *)

value lowercase_start_with s s_ini =
  let len = String.length s_ini in
  String.length s >= len && String.lowercase (String.sub s 0 len) = s_ini
;

value insert_file env bdir name =
  let fname = List.assoc (name ^ "_name") env in
  do if fname = "" then ()
     else if fname <> name then
       printf "You selected \"%s\" instead of \"%s\" -&gt; ignored.\n"
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

value send_file str env b t f fname =
  if Filename.basename fname = "base" then
    do printf "content-type: text/html\r\n\r\n\
<head><title>Gwtp...</title></head>\n<body>
<h1>Gwtp...</h1>
<pre>\n";
       Unix.dup2 Unix.stdout Unix.stderr;
       flush stdout;
    return
    let bdir = Filename.concat "tmp" (b ^ ".gwb") in
    do try Unix.mkdir bdir 0o777 with
       [ Unix.Unix_error Unix.EEXIST _ _ ->
           let dh = Unix.opendir bdir in
           try
             while True do
               match Unix.readdir dh with
               [ "." | ".." -> ()
               | f -> Unix.unlink (Filename.concat bdir f) ];
             done
           with
           [ End_of_file -> () ] ];
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
       printf "</pre>\n";
       printf "Data base \"%s\" created.\n" b;
       printf "</body>\n";
    return ()
  else gwtp_error "Bad \"base\" file selection"
;

value gwtp_send str env =
  match
    (HttpEnv.getenv env "b", HttpEnv.getenv env "t", HttpEnv.getenv env "base",
     HttpEnv.getenv env "base_name")
  with
  [ (Some b, Some t, Some f, Some fname) ->
      let ok =
        let fname = tokens_file_name () in
        let (ok, tokens) = check_token fname b t in
        do write_tokens fname tokens; return ok
      in
      if ok then send_file str env b t f fname
      else gwtp_error "Login expired"
  | _ -> gwtp_invalid_request str env ]
;

value login_ok str env b p tok =
  do set_token b tok;
     printf "content-type: text/html\r\n\r\n";
     printf "\
<head><title>Gwtp - data base \"%s\"</title></head>\n<body>
<h1>Gwtp - data base \"%s\"</h1>
<form method=POST action=gwtp enctype=\"multipart/form-data\">
<input type=hidden name=m value=SEND>
<input type=hidden name=b value=%s>
<input type=hidden name=t value=%s>
Select the file named \"base\" in your GeneWeb data base directory:<br><p>
<input type=file name=base size=60><br><p>
Select the file named \"notes\" (if any) in the same directory:<br><p>
<input type=file name=notes size=60><br><p>
Select the file named \"patches\" (if any) in the same directory:<br><p>
<input type=file name=patches size=60><br><p>
<input type=submit value=Send>
</form>
</body>
" b b b tok;
  return ()
;

value gwtp_login str env =
  match (HttpEnv.getenv env "b", HttpEnv.getenv env "p") with
  [ (Some b, Some p) ->
      match check_login b p with
      [ Some tok -> login_ok str env b p tok
      | None -> gwtp_error "Invalid login" ]
  | _ -> gwtp_invalid_request str env ]
;

value gwtp_welcome str env =
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

value gwtp () =
  let content_type = cgi_content_type () in
  let content = cgi_content () in
  let (str, env) = HttpEnv.make content_type content in
  match HttpEnv.getenv env "m" with
  [ Some "LOGIN" -> gwtp_login str env
  | Some "SEND" -> gwtp_send str env
  | Some _ -> gwtp_invalid_request str env
  | None -> gwtp_welcome str env ]
;

value main () =
  Unix.handle_unix_error gwtp ()
;

Printexc.catch main ();
