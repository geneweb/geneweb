(* $Id: gwtp.ml,v 1.1 2000-07-24 23:36:10 ddr Exp $ *)

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

value gwtp_send (str, env) =
  do printf "content-type: text/html\r\n\r\n";
     printf "%s<p>\n" (String.escaped str);
     printf "Environnement:\n";
     printf "<ul>\n";
     List.iter (fun (k, v) -> printf "<li>%s = %s\n" k v) env;
     printf "</ul>\n";
     printf "Fin.\n";
  return ()
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
