(* $Id: wserver.ml,v 4.3 2001-04-26 14:37:27 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

value sock_in = ref "wserver.sin";
value sock_out = ref "wserver.sou";
value noproc = ref False;

value wserver_oc =
  do { set_binary_mode_out stdout True; ref stdout }
;

value wprint fmt = Printf.fprintf wserver_oc.val fmt;
value wflush () = flush wserver_oc.val;

value hexa_digit x =
  if x >= 10 then Char.chr (Char.code 'A' + x - 10)
  else Char.chr (Char.code '0' + x)
;

value hexa_val conf =
  match conf with
  [ '0'..'9' -> Char.code conf - Char.code '0'
  | 'a'..'f' -> Char.code conf - Char.code 'a' + 10
  | 'A'..'F' -> Char.code conf - Char.code 'A' + 10
  | _ -> 0 ]
;

value decode s =
  let rec need_decode i =
    if i < String.length s then
      match s.[i] with
      [ '%' | '+' -> True
      | _ -> need_decode (succ i) ]
    else False
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
        [ '%' when i + 2 < String.length s -> i + 3
        | _ -> succ i ]
      in
      compute_len i (succ i1)
    else i1
  in
  let rec copy_decode_in s1 i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
        [ '%' when i + 2 < String.length s ->
            let v = hexa_val s.[i + 1] * 16 + hexa_val s.[i + 2] in
            do { s1.[i1] := Char.chr v; i + 3 }
        | '+' -> do { s1.[i1] := ' '; succ i }
        | x -> do { s1.[i1] := x; succ i } ]
      in
      copy_decode_in s1 i (succ i1)
    else s1
  in
  let rec strip_heading_and_trailing_spaces s =
    if String.length s > 0 then
      if s.[0] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 1 (String.length s - 1))
      else if s.[String.length s - 1] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 0 (String.length s - 1))
      else s
    else s
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = String.create len in
    strip_heading_and_trailing_spaces (copy_decode_in s1 0 0)
  else s
;

value special =
  fun
  [ '\000'..'\031' | '\127'..'\255' | '<' | '>' | '"' | '#' | '%' |
    '{' | '}' | '|' | '\\' | '^' | '~' | '[' | ']' | '`' | ';' | '/' | '?' |
    ':' | '@' | '=' | '&' ->
      True
  | _ -> False ]
;

value encode s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
      [ ' ' -> True
      | x -> if special x then True else need_code (succ i) ]
    else False
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 = if special s.[i] then i1 + 3 else succ i1 in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
        [ ' ' -> do { s1.[i1] := '+'; succ i1 }
        | c ->
            if special c then do {
              s1.[i1] := '%';
              s1.[i1 + 1] := hexa_digit (Char.code c / 16);
              s1.[i1 + 2] := hexa_digit (Char.code c mod 16);
              i1 + 3
            }
            else do { s1.[i1] := c; succ i1 } ]
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (String.create len) 0 0
  else s
;

value nl () =
  do {
    wflush ();
    let _ =
      Unix.write (Unix.descr_of_out_channel wserver_oc.val) "\013\010" 0 2 in
    ()
  }
;

value html charset =
  let charset = if charset = "" then "iso-8859-1" else charset in
  do {
    wprint "HTTP/1.0 200 OK"; nl ();
    wprint "Content-type: text/html; charset=%s" charset; nl (); nl ();
  }
;

value print_exc exc =
  match exc with
  [ Unix.Unix_error err fun_name arg ->
      do {
        prerr_string "\"";
        prerr_string fun_name;
        prerr_string "\" failed";
        if String.length arg > 0 then do {
          prerr_string " on \""; prerr_string arg; prerr_string "\"";
        }
        else ();
        prerr_string ": ";
        prerr_endline (Unix.error_message err);
      }
  | Out_of_memory -> prerr_string "Out of memory\n"
  | Match_failure (file, first_char, last_char) ->
      do {
        prerr_string "Pattern matching failed, file ";
        prerr_string file;
        prerr_string ", chars ";
        prerr_int first_char;
        prerr_char '-';
        prerr_int last_char;
        prerr_char '\n';
      }
  | Assert_failure (file, first_char, last_char) ->
      do {
        prerr_string "Assertion failed, file ";
        prerr_string file;
        prerr_string ", chars ";
        prerr_int first_char;
        prerr_char '-';
        prerr_int last_char;
        prerr_char '\n';
      }
  | x ->
      do {
        prerr_string "Uncaught exception: ";
        prerr_string (Obj.magic (Obj.field (Obj.field (Obj.repr x) 0) 0));
        if Obj.size (Obj.repr x) > 1 then do {
          prerr_char '(';
          for i = 1 to Obj.size (Obj.repr x) - 1 do {
            if i > 1 then prerr_string ", " else ();
            let arg = Obj.field (Obj.repr x) i in
            if not (Obj.is_block arg) then
              prerr_int (Obj.magic arg : int)
            else if Obj.tag arg = 252 then do {
              prerr_char '"'; prerr_string (Obj.magic arg : string);
              prerr_char '"'
            }
            else prerr_char '_';
          };
          prerr_char ')';
        }
        else ();
        prerr_char '\n';
      } ]
;

value print_err_exc exc =
  do { print_exc exc; flush stderr; }
;

value case_unsensitive_eq s1 s2 =
  String.lowercase s1 = String.lowercase s2
;

value rec extract_param name stop_char =
  fun
  [ [x :: l] ->
      if String.length x >= String.length name
      && case_unsensitive_eq (String.sub x 0 (String.length name)) name then
        let i =
          loop (String.length name) where rec loop i =
            if i = String.length x then i
            else if x.[i] = stop_char then i
            else loop (i + 1)
        in
        String.sub x (String.length name) (i - String.length name)
      else extract_param name stop_char l
  | [] -> "" ]
;

value buff = ref (String.create 80);
value store len x =
  do {
    if len >= String.length buff.val then
      buff.val := buff.val ^ String.create (String.length buff.val)
    else ();
    buff.val.[len] := x;
    succ len
  }
;
value get_buff len = String.sub buff.val 0 len;

value get_request strm =
  let rec loop len =
    parser
    [ [: `'\010'; s :] ->
        if len == 0 then []
        else let str = get_buff len in [str :: loop 0 s]
    | [: `'\013'; s :] -> loop len s
    | [: `c; s :] -> loop (store len c) s
    | [: :] -> if len == 0 then [] else [get_buff len] ]
  in
  loop 0 strm
;

ifdef UNIX then
value timeout tmout spid _ =
  do {
    Unix.kill spid Sys.sigkill;
    html "";
    wprint "<head><title>Time out</title></head>\n";
    wprint "<body><h1>Time out</h1>\n";
    wprint "Computation time > %d second(s)\n" tmout;
    wprint "</body>\n";
    wflush ();
    exit 2
  }
;

value get_request_and_content strm =
  let request = get_request strm in
  let content =
    match extract_param "content-length: " ' ' request with
    [ "" -> ""
    | x ->
        let str = String.create (int_of_string x) in
        do {
          for i = 0 to String.length str - 1 do {
            str.[i] :=
              match strm with parser
              [ [: `x :] -> x
              | [: :] -> ' ' ];
          };
          str
        } ]
  in
  (request, content)
;

value string_of_sockaddr =
  fun
  [ Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET a _ -> Unix.string_of_inet_addr a ]
;
value sockaddr_of_string s = Unix.ADDR_UNIX s;

value treat_connection tmout callback addr ic =
  do {
    ifdef NOFORK then ()
    else ifdef UNIX then
      if tmout > 0 then
        let spid = Unix.fork () in
        if spid > 0 then do {
          let _ (* : Sys.signal_behavior *) =
             Sys.signal Sys.sigalrm
               (Sys.Signal_handle (timeout tmout spid))
          in ();
          let _ = Unix.alarm tmout in ();
          let _ = Unix.waitpid [] spid in ();
          let _ (* : Sys.signal_behavior *) =
            Sys.signal Sys.sigalrm Sys.Signal_default
          in ();
          exit 0;
        }
        else ()
      else ()
    else ();
    let (request, script_name, contents) =
      let (request, contents) =
        let strm = Stream.of_channel ic in
        get_request_and_content strm
      in
      let (script_name, contents) =
        match extract_param "GET /" ' ' request with
        [ "" -> (extract_param "POST /" ' ' request, contents)
        | str ->
            try
              let i = String.index str '?' in
              (String.sub str 0 i,
               String.sub str (i + 1) (String.length str - i - 1))
            with
            [ Not_found -> (str, "") ] ]
      in
      (request, script_name, contents)
    in
    if script_name = "robots.txt" then do {
      wprint "HTTP/1.0 200 Ok"; nl ();
      wprint "Content-type: text/plain"; nl (); nl ();
      wprint "User-Agent: *"; nl ();
      wprint "Disallow: /"; nl ();
      wflush ();
      Printf.eprintf "Robot request\n";
      flush stderr;
    }
    else do {
      try callback (addr, request) script_name contents with
      [ Unix.Unix_error Unix.EPIPE "write" _ -> ()
      | exc -> print_err_exc exc ];
      try wflush () with _ -> ();
      try flush stderr with _ -> ();
    }
  }
;

value buff = String.create 1024;

ifdef WIN95 then
value copy_what_necessary t oc =
  let strm =
    let len = ref 0 in
    let i = ref 0 in
    Stream.from
      (fun _ ->
	 do {
           if i.val >= len.val then do {
             len.val := Unix.read t buff 0 (String.length buff);
	     i.val := 0;
	     if len.val > 0 then output oc buff 0 len.val else ();
           }
	   else ();
           if len.val == 0 then None
	   else do { incr i; Some buff.[i.val - 1] }
         })
  in
  let _ = get_request_and_content strm in
  ()
;

value rec list_remove x =
  fun
  [ [] -> failwith "list_remove"
  | [y :: l] -> if x = y then l else [y :: list_remove x l] ]
;

ifdef NOFORK then declare end else
value pids = ref [];
ifdef NOFORK then declare end else
value cleanup_verbose = ref True;
ifdef NOFORK then declare end else
value cleanup_sons () =
  List.iter
    (fun p ->
       let pid =
         try fst (Unix.waitpid [Unix.WNOHANG] p) with
         [ Unix.Unix_error _ _ _ as exc ->
             do {
               if cleanup_verbose.val then do {
                 Printf.eprintf "*** Why error on waitpid %d?\n" p;
                 flush stderr;
                 print_exc exc;
                 Printf.eprintf "[";
                 List.iter (fun p -> Printf.eprintf " %d" p) pids.val;
                 Printf.eprintf "]\n";
                 flush stderr;
                 cleanup_verbose.val := False;
               }
               else ();
               p
             } ]
       in
       if pid = 0 then ()
       else pids.val := list_remove pid pids.val)
     pids.val
;

ifdef NOFORK then declare end else
value wait_available max_clients s =
  match max_clients with
  [ Some m ->
      do {
        if List.length pids.val >= m then
(*
let tm = Unix.localtime (Unix.time ()) in
let _ = do { Printf.eprintf "*** %02d/%02d/%4d %02d:%02d:%02d " tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec; Printf.eprintf "%d clients running; waiting...\n" m; flush stderr; } in
*)
          let (pid, _) = Unix.wait () in
(*
let tm = Unix.localtime (Unix.time ()) in
let _ = do { Printf.eprintf "*** %02d/%02d/%4d %02d:%02d:%02d " tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec; Printf.eprintf "ok: place for another client\n"; flush stderr; } in
*)
          pids.val := list_remove pid pids.val
        else ();
        if pids.val <> [] then cleanup_sons () else ();
        let stop_verbose = ref False in
        while pids.val <> [] && Unix.select [s] [] [] 15.0 = ([], [], []) do {
          cleanup_sons ();
          if pids.val <> [] && not stop_verbose.val then do {
            stop_verbose.val := True;
            let tm = Unix.localtime (Unix.time ()) in
Printf.eprintf "*** %02d/%02d/%4d %02d:%02d:%02d %d process(es) remaining after cleanup (%d)\n" tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec (List.length pids.val) (List.hd pids.val); flush stderr; ()
          }
          else ();
        };
      }
  | None -> () ]
;

value wait_and_compact s =
  if Unix.select [s] [] [] 15.0 = ([], [], []) then do {
    Printf.eprintf "Compacting... "; flush stderr;
    Gc.compact ();
    Printf.eprintf "Ok\n"; flush stderr;
  }
  else ()
;

value accept_connection tmout max_clients callback s =
  do {
    ifdef NOFORK then wait_and_compact s
    else if noproc.val then wait_and_compact s
    else wait_available max_clients s;
    let (t, addr) = Unix.accept s in
    Unix.setsockopt t Unix.SO_KEEPALIVE True;
    ifdef NOFORK then
      let cleanup () =
        do {
          try Unix.shutdown t Unix.SHUTDOWN_SEND with _ -> ();
          try Unix.shutdown t Unix.SHUTDOWN_RECEIVE with _ -> ();
          try Unix.close t with _ -> ();
        }
      in
      let ic = Unix.in_channel_of_descr t in
      do {
        wserver_oc.val := Unix.out_channel_of_descr t;
        treat_connection tmout callback addr ic;
        cleanup ();
      }
    else ifdef UNIX then
      match try Some (Unix.fork ()) with _ -> None with
      [ Some 0 ->
          do {
            try do {
              if max_clients = None && Unix.fork () <> 0 then exit 0 else ();
              Unix.close s;
              Unix.dup2 t Unix.stdout;
              Unix.dup2 t Unix.stdin;
(*  
   j'ai l'impression que cette fermeture fait parfois bloquer le serveur...
              try Unix.close t with _ -> ();
*)  
              treat_connection tmout callback addr stdin;
            }
            with exc ->
              try do { print_err_exc exc; flush stderr; }
              with _ -> ();
            exit 0
          }
      | Some id ->
          do {
            Unix.close t;
            if max_clients = None then let _ = Unix.waitpid [] id in ()
            else pids.val := [id :: pids.val];
          }
      | None ->
          do { Unix.close t; Printf.eprintf "Fork failed\n"; flush stderr } ]
    else do {
      let oc = open_out_bin sock_in.val in
      let cleanup () = try close_out oc with _ -> () in
      try copy_what_necessary t oc with
      [ Unix.Unix_error _ _ _ -> ()
      | exc -> do { cleanup (); raise exc } ];
      cleanup ();
      ifdef SYS_COMMAND then
        let comm =
          let stringify_if_spaces s =
            try let _ = String.index s ' ' in "\"" ^ s ^ "\"" with
            [ Not_found -> s ]
          in
          List.fold_left (fun s a -> s ^ stringify_if_spaces a ^ " ") ""
            (Array.to_list Sys.argv) ^
          "-wserver " ^ string_of_sockaddr addr
        in
        let _ = Sys.command comm in ()
      else if noproc.val then do {
        let ic = open_in_bin sock_in.val in
        let oc = open_out_bin sock_out.val in
        wserver_oc.val := oc;
        treat_connection tmout callback addr ic;
        flush oc;
        close_in ic;
        close_out oc;
      }
      else
        let pid =
          let env =
            Array.append (Unix.environment ())
              [| "WSERVER=" ^ string_of_sockaddr addr |]
          in
          let args = Array.map (fun x -> "\"" ^ x ^ "\"") Sys.argv in
          Unix.create_process_env Sys.argv.(0) args env Unix.stdin
            Unix.stdout Unix.stderr
        in
        let _ = Unix.waitpid [] pid in ();
      let cleanup () =
        do {
          try Unix.shutdown t Unix.SHUTDOWN_SEND with _ -> ();
          try Unix.shutdown t Unix.SHUTDOWN_RECEIVE with _ -> ();
          try Unix.close t with _ -> ();
        }
      in
      try
        let ic = open_in_bin sock_out.val in
        let cleanup () = try close_in ic with _ -> () in
        do {
          try
            loop () where rec loop () =
              let len = input ic buff 0 (String.length buff) in
              if len == 0 then ()
              else do {
                loop_write 0 where rec loop_write i =
                  let olen = Unix.write t buff i (len - i) in
                  if i + olen < len then loop_write (i + olen) else ();
                loop ()
              }
          with
          [ Unix.Unix_error _ _ _ -> ()
          | exc -> do { cleanup (); raise exc } ];
          cleanup ();
        }
      with
      [ Unix.Unix_error _ _ _ -> ()
      | exc -> do { cleanup (); raise exc } ];
      cleanup ();
    }
  }
;

value f addr_opt port tmout max_clients g =
  match
    ifdef NOFORK then None
    else ifdef WIN95 then
      ifdef SYS_COMMAND then
        let len = Array.length Sys.argv in
        if len > 2 && Sys.argv.(len - 2) = "-wserver" then
          Some Sys.argv.(len - 1)
        else None
      else
        try Some (Sys.getenv "WSERVER") with [ Not_found -> None ]
    else None
  with
  [ Some s ->
      ifdef NOFORK then ()
      else ifdef WIN95 then do {
        let addr = sockaddr_of_string s in
        let ic = open_in_bin sock_in.val in
        let oc = open_out_bin sock_out.val in
        wserver_oc.val := oc;
        treat_connection tmout g addr ic;
        exit 0
      }
      else ()
  | None ->
      let addr =
        match addr_opt with
        [ Some addr ->
            try Unix.inet_addr_of_string addr with
            [ Failure _ -> (Unix.gethostbyname addr).Unix.h_addr_list.(0) ]
        | None -> Unix.inet_addr_any ]
      in
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      do {
        Unix.setsockopt s Unix.SO_REUSEADDR True;
        Unix.bind s (Unix.ADDR_INET addr port);
        Unix.listen s 4;
        ifdef NOFORK then Sys.set_signal Sys.sigpipe Sys.Signal_ignore
        else ifdef UNIX then let _ = Unix.nice 1 in ()
        else ();
        let tm = Unix.localtime (Unix.time ()) in
        Printf.eprintf "Ready %4d-%02d-%02d %02d:%02d port"
          (1900 + tm.Unix.tm_year) (succ tm.Unix.tm_mon) tm.Unix.tm_mday
          tm.Unix.tm_hour tm.Unix.tm_min;
        Printf.eprintf " %d" port;
        Printf.eprintf "...\n";
        flush stderr;
        while True do {
          try accept_connection tmout max_clients g s with
          [ Unix.Unix_error _ "accept" _ -> ()
          | exc -> print_err_exc exc ];
          try wflush () with [ Sys_error _ -> () ];
          try flush stdout with [ Sys_error _ -> () ];
          flush stderr;
        };
      } ]
;
