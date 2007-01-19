(* $Id: phonygwd.ml,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

value port_selected = ref 2317;
value fname = ref "";

value log addr request s =
  let referer = Wserver.extract_param "referer: " '\n' request in
  let user_agent = Wserver.extract_param "user-agent: " '\n' request in
  do {
    let tm = Unix.localtime (Unix.time ()) in
    Printf.eprintf "%02d/%02d/%02d %02d:%02d" tm.Unix.tm_mday
      (succ tm.Unix.tm_mon) tm.Unix.tm_year tm.Unix.tm_hour tm.Unix.tm_min;
    Printf.eprintf " %s\n" s;
    match addr with
    [ Unix.ADDR_UNIX x -> ()
    | Unix.ADDR_INET iaddr port ->
        Printf.eprintf "  From: %s\n"
          (try (Unix.gethostbyaddr iaddr).Unix.h_name with _ ->
             Unix.string_of_inet_addr iaddr) ];
    Printf.eprintf "  Agent: %s\n" user_agent;
    if referer <> "" then Printf.eprintf "  Referer: %s\n" referer else ();
    flush stderr;
  }
;

value print_text fname =
  let ic = Secure.open_in fname in
  do {
    try while True do { print_char (input_char ic) } with
    [ End_of_file -> () ];
    close_in ic;
    Wserver.wflush ();
  }
;

value connection (addr, request) script_name contents =
  let str = script_name ^ "?" ^ contents in
  do {
    log addr request str;
    Wserver.html "";
    print_text fname.val;
    Wserver.wflush ();
  }
;

value main () =
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [-p #] <file>" in
  let speclist =
    [("-p", Arg.Int (fun x -> port_selected.val := x), "#: port number")]
  in
  do {
    Argl.parse speclist (fun s -> fname.val := s) usage;
    if fname.val = "" then do {
      Printf.eprintf "Missing file\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 1
    }
    else ();
    close_in (Secure.open_in fname.val);
    Wserver.f None port_selected.val 0 (Some 4) connection;
  }
;

try main () with
[ Unix.Unix_error err fun_name arg ->
    do {
      Printf.eprintf "Error: \"%s\", %s\n" fun_name (Unix.error_message err);
      flush stderr;
      exit 1
    }
| exc -> Printexc.catch raise exc ];
