(* $Id: phonygwd.ml,v 1.1.1.1 1998-09-01 14:32:12 ddr Exp $ *)

open Unix;

value port_selected = ref 2317;
value fname = ref "";

value log addr request s =
  let referer = Wserver.extract_param "referer: " '\n' request in
  let user_agent = Wserver.extract_param "user-agent: " '\n' request in
  do let tm = Unix.localtime (Unix.time ()) in
     Printf.eprintf "%02d/%02d/%02d %02d:%02d" tm.Unix.tm_mday
       (succ tm.Unix.tm_mon) tm.Unix.tm_year tm.Unix.tm_hour tm.Unix.tm_min;
     Printf.eprintf " %s\n" s;
     match addr with
     [ ADDR_UNIX x -> ()
     | ADDR_INET iaddr port ->
         Printf.eprintf "  From: %s\n"
           (try (gethostbyaddr iaddr).h_name with _ ->
              string_of_inet_addr iaddr) ];
     Printf.eprintf "  Agent: %s\n" user_agent;
     if referer <> "" then Printf.eprintf "  Referer: %s\n" referer else ();
     flush Pervasives.stderr;
  return ()
;

value print_text fname =
  let ic = open_in fname in
  do try
       while True do
         print_char (input_char ic);
       done
     with
     [ End_of_file -> () ];
     close_in ic;
     Wserver.wflush ();
  return ()
;

value connection (addr, request) str =
  do log addr request str;
     Wserver.html ();
     print_text fname.val;
     Wserver.wflush ();
  return ()
;

value main () =
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [-p #] <file>" in
  let speclist =
    [("-p", Arg.Int (fun x -> port_selected.val := x), "#: port number")]
  in
  do Argl.parse speclist (fun s -> fname.val := s) usage;
     if fname.val = "" then
       do Printf.eprintf "Missing file\n";
          Printf.eprintf "Use option -help for usage\n";
          flush Pervasives.stderr;
       return exit 1
     else ();
     close_in (open_in fname.val);
     Wserver.f port_selected.val 0 (Some 4) None connection;
  return ()
;

try
  main ()
with
[ Unix_error err fun_name arg ->
    do Printf.eprintf "Error: \"%s\", %s\n" fun_name (error_message err);
       flush Pervasives.stderr;
    return exit 1
| exc -> Printexc.catch raise exc ]
;

