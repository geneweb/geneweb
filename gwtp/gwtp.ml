(* $Id: gwtp.ml,v 1.2 2000-07-25 13:23:03 ddr Exp $ *)

open Printf;

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

value lowercase_start_with s s_ini =
  let len = String.length s_ini in
  String.length s >= len && String.lowercase (String.sub s 0 len) = s_ini
;

value gwtp_send (str, env) =
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
    let oc = open_out (Filename.concat "tmp" (List.assoc "file_name" env)) in
    let (ctype, contents) =
      let s = List.assoc "file" env in
      let i =
        if lowercase_start_with s "content-type: " then String.index s '\n'
        else 0
      in
      let j = String.index_from s (i + 1) '\n' in
      (String.sub s 0 i, String.sub s (j + 1) (String.length s - j - 3))
    in
    do output oc contents 0 (String.length contents);
       close_out oc;
    return ()
  with
  [ Not_found -> () ]
;

value gwtp () =
  let content_type = cgi_content_type () in
  let content = cgi_content () in
  let (str, env) = HttpEnv.make content_type content in
  gwtp_send (str, env)
;

value main () =
  gwtp ()
;

main ();
