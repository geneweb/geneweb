(* $Id: gwtp.ml,v 1.4 2000-07-26 01:39:33 ddr Exp $ *)

open Printf;

value gwtp_dir = ref "gwtp_dir";
value token_tmout = ref 30.0;

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

(* Requests *)

value lowercase_start_with s s_ini =
  let len = String.length s_ini in
  String.length s >= len && String.lowercase (String.sub s 0 len) = s_ini
;

value send_file str env b t f =
  do printf "content-type: text/html\r\n\r\n";
     printf "%s<p>\n" (String.escaped str);
     printf "Environnement:\n";
     printf "<ul>\n";
     List.iter
        (fun (k, v) ->
           let v =
             if String.length v > 40 then String.sub v 0 40 ^ "..."
             else v
           in
           printf "<li>%s = %s\n" k (html_escaped v))
        env;
     printf "</ul>\n";
     printf "Fin.\n";
     flush stdout;
  return
  try
    let oc = open_out (Filename.concat "tmp" (List.assoc "f_name" env)) in
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
  [ Not_found -> () ]
;

value gwtp_send str env =
  match
    (HttpEnv.getenv env "b", HttpEnv.getenv env "t", HttpEnv.getenv env "f")
  with
  [ (Some b, Some t, Some f) -> send_file str env b t f
  | _ -> gwtp_invalid_request str env ]
;

(* Login and tokens *)

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

value set_token b tok =
  let tm = Unix.time () in
  let fname = Filename.concat gwtp_dir.val "token" in
  let tokens =
    match try Some (open_in fname) with [ Sys_error _ -> None ] with
    [ Some ic ->
        loop [] where rec loop list =
          match try Some (input_line ic) with [ End_of_file -> None ] with
          [ Some line ->
              let i = String.index line ' ' in
              let j = String.index_from line (i + 1) ' ' in
              let tm0 = float_of_string (String.sub line 0 i) in
              let b0 = String.sub line (i + 1) (j - i) in
              let tok = String.sub line (j + 1) (String.length line - j - 1) in
              let list =
                if b = b0 || tm -. tm0 > token_tmout.val then list
                else [(tm0, b0, tok) :: list]
              in
              loop list
          | None ->
              let list = [(tm, b, tok) :: list] in
              do close_in ic; return List.rev list ]
    | None -> [(tm, b, tok)] ]
  in
  let oc = open_out fname in
  do List.iter
       (fun (tm, b, tok) -> fprintf oc "%f %s %s\n" tm b tok)
       tokens;
     close_out oc;
  return ()
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
File to send:<br>
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
