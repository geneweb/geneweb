(* $Id: gwtp.ml,v 1.5 2000-07-26 04:44:06 ddr Exp $ *)

open Printf;

value gwtp_dir = ref "gwtp_dir";
value comm_ged2gwb = ref "../../geneweb/ged2gwb/ged2gwb";
value comm_consang = ref "../../geneweb/src/consang";
value token_tmout = ref 60.0;

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
<head><title>%s</title></head>\n<body>
<h1>%s</h1>
</body>
" txt txt;
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

value send_file str env b t f fname =
  let fname = Filename.basename fname in
  do try
       let oc = open_out (Filename.concat "tmp" fname) in
       let contents = List.assoc "f" env in
       let i =
         if lowercase_start_with contents "content-type: " then
           String.index contents '\n'
         else 0
       in
       let j = String.index_from contents (i + 1) '\n' in
       do output oc contents (j + 1) (String.length contents - j - 3);
          close_out oc;
       return ()
     with
     [ Not_found -> () ];
     printf "content-type: text/html\r\n\r\n";
     printf "
<head><title>Gwtp: data base \"%s\"</title></head>\n<body>
<h1>Gwtp: data base \"%s\"</h1>
File %s transfered
" b b fname;
     flush stdout;
     if Filename.check_suffix fname ".ged"
     || Filename.check_suffix fname ".GED" then
       do printf "<p>\nThe data base is going to be created.\n";
          flush stdout;
       return
       let bfname = Filename.concat "tmp" b in
       let fname = Filename.concat "tmp" fname in
       let pid = Unix.fork () in
       if pid > 0 then let _ = Unix.waitpid [] pid in ()
       else
         do List.iter Unix.close [Unix.stdin; Unix.stdout; Unix.stderr]; return
         if Unix.fork () > 0 then exit 0
         else
           let ic = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0o666 in
           let oc =
             Unix.openfile (Filename.concat "tmp" "gwtp.log")
               [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_APPEND] 0o666
           in
           let s =
             "$ " ^ comm_ged2gwb.val ^ " " ^ fname ^ " -f -o " ^ bfname ^ "\n"
           in
           let _ = Unix.write oc s 0 (String.length s) in
           let _ =
             Unix.create_process_env comm_ged2gwb.val
               [| comm_ged2gwb.val; fname; "-f"; "-o"; bfname |] [| |]
               ic oc oc
           in
           let _ = Unix.wait () in
  (*
           let _ =
             Unix.create_process_env comm_consang.val
               [| comm_consang.val; "-q"; bfname |] [| |]
               ic oc oc
           in
           let _ = Unix.wait () in
  *)
           do Unix.close oc; return exit 0
     else ();
     printf "Bye\n";
     printf "</body>\n";
  return ()
;

value gwtp_send str env =
  match
    (HttpEnv.getenv env "b", HttpEnv.getenv env "t", HttpEnv.getenv env "f",
     HttpEnv.getenv env "f_name")
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
<head><title>Gwtp: data base \"%s\"</title></head>\n<body>
<h1>Gwtp: data base \"%s\"</h1>
<form method=POST action=gwtp enctype=\"multipart/form-data\">
<input type=hidden name=m value=SEND>
<input type=hidden name=b value=%s>
<input type=hidden name=t value=%s>
Gedcom or GeneWeb source file to send:<br>
<input type=file name=f><br>
<input type=submit>
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
<input type=submit>
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
  gwtp ()
;

main ();
