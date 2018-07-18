(* $Id: wserver.ml,v 5.15 2007-09-12 09:42:26 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

value eprintf = Printf.eprintf;

value sock_in = ref "wserver.sin";
value sock_out = ref "wserver.sou";
value stop_server = ref "STOP_SERVER";
value noproc = ref False;
value cgi = ref False;

value wserver_sock = ref Unix.stdout;
value wsocket () = wserver_sock.val;

value wserver_oc = ref stdout;

value printnl fmt =
  Printf.ksprintf (fun s -> do {
    output_string wserver_oc.val s;
    output_string wserver_oc.val "\013\010"
  }) fmt
;

type printing_state =
[ Nothing
| Status
| Contents ];

value printing_state = ref Nothing;

value http status = do {
  if printing_state.val <> Nothing then
    failwith "HTTP Status already sent"
  else ();
  printing_state.val := Status;
  if status <> HttpStatus.OK || not cgi.val then
    let answer = HttpStatus.to_string status in
    if cgi.val then
      printnl "Status: %s" answer
    else
      printnl "HTTP/1.0 %s" answer
  else ()
};

value header fmt = do {
  if printing_state.val <> Status then
    if printing_state.val = Nothing then
      http HttpStatus.OK
    else
      failwith "Cannot write HTTP headers: page contents already started"
  else ();
  printnl fmt
};

value wrap_string = ref (fun s -> s);

value printf fmt = do {
  if printing_state.val <> Contents then do {
    if printing_state.val = Nothing then
      http HttpStatus.OK
    else ();
    printnl "";
    printing_state.val := Contents
  } else ();
  Printf.ksprintf (fun s -> output_string wserver_oc.val (wrap_string.val s)) fmt
};
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

value gen_decode strip_spaces s =
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
        [ '%' when i + 2 < String.length s -> do {
            let v = hexa_val s.[i+1] * 16 + hexa_val s.[i+2] in
            Bytes.set s1 i1 (Char.chr v);
            i + 3
          }
        | '+' -> do { Bytes.set s1 i1 ' '; succ i }
        | x -> do { Bytes.set s1 i1 x; succ i } ]
      in
      copy_decode_in s1 i (succ i1)
    else Bytes.unsafe_to_string s1
  in
  let rec strip_heading_and_trailing_spaces s =
    if String.length s > 0 then
      if s.[0] = ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 1 (String.length s - 1))
      else if s.[String.length s - 1] = ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 0 (String.length s - 1))
      else s
    else s
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = Bytes.create len in
    let s = copy_decode_in s1 0 0 in
    if strip_spaces then strip_heading_and_trailing_spaces s else s
  else s
;

value decode = gen_decode True;

value special =
  fun
  [ '\000'..'\031' | '\127'..'\255' | '<' | '>' | '"' | '#' | '%' | '{' |
    '}' | '|' | '\\' | '^' | '~' | '[' | ']' | '`' | ';' | '/' | '?' | ':' |
    '@' | '=' | '&' | '+' ->
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
        [ ' ' -> do { Bytes.set s1 i1 '+'; succ i1 }
        | c ->
            if special c then do {
              Bytes.set s1 i1 '%';
              Bytes.set s1 (i1+1) (hexa_digit (Char.code c / 16));
              Bytes.set s1 (i1+2) (hexa_digit (Char.code c mod 16));
              i1 + 3
            }
            else do { Bytes.set s1 i1 c; succ i1 } ]
      in
      copy_code_in s1 (succ i) i1
    else Bytes.unsafe_to_string s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in
    copy_code_in (Bytes.create len) 0 0
  else s
;

value http_redirect_temporarily url = do {
  http HttpStatus.Moved_Temporarily ;
  printnl "Location: %s" url
};

value print_exc exc =
  let () = match exc with
  [ Unix.Unix_error err fun_name arg -> do {
      prerr_string "\"";
      prerr_string fun_name;
      prerr_string "\" failed";
      if String.length arg > 0 then do {
        prerr_string " on \"";
        prerr_string arg;
        prerr_string "\""
      }
      else ();
      prerr_string ": ";
      prerr_endline (Unix.error_message err)
    }
  | Out_of_memory -> prerr_string "Out of memory\n"
  | Match_failure (file, first_char, last_char) -> do {
      prerr_string "Pattern matching failed, file ";
      prerr_string file;
      prerr_string ", chars ";
      prerr_int first_char;
      prerr_char '-';
      prerr_int last_char;
      prerr_char '\n'
    }
  | Assert_failure (file, first_char, last_char) -> do {
      prerr_string "Assertion failed, file ";
      prerr_string file;
      prerr_string ", chars ";
      prerr_int first_char;
      prerr_char '-';
      prerr_int last_char;
      prerr_char '\n'
    }
  | x -> do {
      prerr_string "Wserver: uncaught exception: ";
      prerr_string (Obj.magic (Obj.field (Obj.field (Obj.repr x) 0) 0));
      if Obj.size (Obj.repr x) > 1 then do {
        prerr_char '(';
        for i = 1 to Obj.size (Obj.repr x) - 1 do {
          if i > 1 then prerr_string ", " else ();
          let arg = Obj.field (Obj.repr x) i in
          if not (Obj.is_block arg) then
            prerr_int (Obj.magic arg : int)
          else if Obj.tag arg = 252 then do {
            prerr_char '"';
            prerr_string (Obj.magic arg : string);
            prerr_char '"'
          }
          else prerr_char '_';
        };
        prerr_char ')';
      }
      else ();
      prerr_char '\n'
    } ] in

  let () = if Printexc.backtrace_status () then do {
    prerr_string "Backtrace:\n";
    Printexc.print_backtrace stderr;
  } else () in ()
;

value print_err_exc exc = do { print_exc exc; flush stderr };

value case_unsensitive_eq s1 s2 =
  String.lowercase_ascii s1 = String.lowercase_ascii s2;

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

value buff = ref (Bytes.create 80);
value store len x = do {
  if len >= Bytes.length buff.val then
    buff.val := Bytes.extend buff.val 0 (Bytes.length buff.val)
  else ();
  Bytes.set buff.val len x;
  succ len
};
value get_buff len = Bytes.sub_string buff.val 0 len;

value get_request strm =
  let rec loop len =
    parser
    [ [: `'\010'; s :] ->
        if len = 0 then []
        else let str = get_buff len in [str :: loop 0 s]
    | [: `'\013'; s :] -> loop len s
    | [: `c; s :] -> loop (store len c) s
    | [: :] -> if len = 0 then [] else [get_buff len] ]
  in
  loop 0 strm
;

value timeout tmout spid _ =
  do {
    Unix.kill spid Sys.sigkill;
    Unix.kill spid Sys.sigterm;
    let pid = Unix.fork () in
    if pid = 0 then
      if Unix.fork () = 0 then do {
        http HttpStatus.OK;
        printnl "Content-type: text/html; charset=iso-8859-1";
        printnl "";
        printf "<head><title>Time out</title></head>\n";
        printf "<body><h1>Time out</h1>\n";
        printf "Computation time > %d second(s)\n" tmout;
        printf "</body>\n";
        wflush ();
        exit 0;
      }
      else exit 0
    else ();
    let _ = Unix.waitpid [] pid in ();
    exit 2
  }
;

value get_request_and_content strm =
  let request = get_request strm in
  let content =
    match extract_param "content-length: " ' ' request with
    [ "" -> ""
    | x -> String.init (int_of_string x) get_next_char
      where get_next_char _ =
        match strm with parser
        [ [: `x :] -> x
        | [: :] -> ' ' ]
    ]
  in
  (request, content)
;

value string_of_sockaddr =
  fun
  [ Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET a _ -> Unix.string_of_inet_addr a ]
;
value sockaddr_of_string s = Unix.ADDR_UNIX s;

value treat_connection tmout callback addr fd = do {
  if Sys.unix then
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
      let strm =
        let c = Bytes.create 1 in
        Stream.from
          (fun _ -> if Unix.read fd c 0 1 = 1 then Some (Bytes.get c 0) else None)
      in
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
  try callback (addr, request) script_name contents with
  [ Unix.Unix_error Unix.EPIPE "write" _ -> ()
  | Sys_error msg when msg = "Broken pipe" -> ()
  | exc -> print_err_exc exc ];
  try wflush () with _ -> ();
  try flush stderr with _ -> ();
};

value buff = Bytes.create 1024;

value copy_what_necessary t oc =
  let strm =
    let len = ref 0 in
    let i = ref 0 in
    Stream.from
      (fun _ ->
         do {
           if i.val >= len.val then do {
             len.val := Unix.read t buff 0 (Bytes.length buff);
             i.val := 0;
             if len.val > 0 then output oc buff 0 len.val else ();
           }
           else ();
           if len.val = 0 then None
           else do { incr i; Some (Bytes.get buff (i.val - 1)) }
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

value pids = ref [];

value cleanup_verbose = ref True;

value cleanup_sons () =
  List.iter
    (fun p ->
       let pid =
         try fst (Unix.waitpid [Unix.WNOHANG] p) with
         [ Unix.Unix_error _ _ _ as exc ->
             do {
               if cleanup_verbose.val then do {
                 eprintf "*** Why error on waitpid %d?\n" p;
                 flush stderr;
                 print_exc exc;
                 eprintf "[";
                 List.iter (fun p -> eprintf " %d" p) pids.val;
                 eprintf "]\n";
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

value wait_available max_clients s =
  match max_clients with
  [ Some m ->
      do {
        if List.length pids.val >= m then
(*
let tm = Unix.localtime (Unix.time ()) in
let _ = do { eprintf "*** %02d/%02d/%4d %02d:%02d:%02d " tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec; eprintf "%d clients running; waiting...\n" m; flush stderr; } in
*)
          let (pid, _) = Unix.wait () in
(*
let tm = Unix.localtime (Unix.time ()) in
let _ = do { eprintf "*** %02d/%02d/%4d %02d:%02d:%02d " tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec; eprintf "ok: place for another client\n"; flush stderr; } in
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
eprintf "*** %02d/%02d/%4d %02d:%02d:%02d %d process(es) remaining after cleanup (%d)\n" tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec (List.length pids.val) (List.hd pids.val); flush stderr; ()
          }
          else ();
        };
      }
  | None -> () ]
;

value wait_and_compact s =
  if Unix.select [s] [] [] 15.0 = ([], [], []) then do {
    eprintf "Compacting... "; flush stderr;
    Gc.compact ();
    eprintf "Ok\n"; flush stderr;
  }
  else ()
;

value skip_possible_remaining_chars fd =
  do {
    let b = Bytes.create 3 in
    try
      loop () where rec loop () =
        match Unix.select [fd] [] [] 5.0 with
        [ ([_], [], []) ->
            let len = Unix.read fd b 0 (Bytes.length b) in
            if len = Bytes.length b then loop () else ()
        | _ -> () ]
    with
    [ Unix.Unix_error Unix.ECONNRESET _ _ -> () ]
  }
;

value check_stopping () =
  if Sys.file_exists stop_server.val then do {
    flush stdout;
    eprintf "\nServer stopped by presence of file %s.\n" stop_server.val;
    eprintf "Remove that file to allow servers to run again.\n";
    flush stderr;
    exit 0
  }
  else ()
;

value accept_connection tmout max_clients callback s =
  do {
    if noproc.val then wait_and_compact s
    else wait_available max_clients s;
    let (t, addr) = Unix.accept s in
    check_stopping ();
    Unix.setsockopt t Unix.SO_KEEPALIVE True;
    if Sys.unix then
      match try Some (Unix.fork ()) with _ -> None with
      [ Some 0 ->
          do {
            try do {
              if max_clients = None && Unix.fork () <> 0 then exit 0 else ();
              Unix.close s;
              wserver_sock.val := t;
              wserver_oc.val := Unix.out_channel_of_descr t;
(*
   j'ai l'impression que cette fermeture fait parfois bloquer le serveur...
              try Unix.close t with _ -> ();
*)
              treat_connection tmout callback addr t;
            }
            with
            [ Unix.Unix_error Unix.ECONNRESET "read" _ -> ()
            | exc ->
                try do { print_err_exc exc; flush stderr; }
                with _ -> () ];
            try Unix.shutdown t Unix.SHUTDOWN_SEND with _ -> ();
            try Unix.shutdown Unix.stdout Unix.SHUTDOWN_SEND with _ -> ();
            skip_possible_remaining_chars t;
            try Unix.shutdown t Unix.SHUTDOWN_RECEIVE with _ -> ();
            try Unix.shutdown Unix.stdin Unix.SHUTDOWN_RECEIVE with _ -> ();
            exit 0
          }
      | Some id ->
          do {
            Unix.close t;
            if max_clients = None then let _ = Unix.waitpid [] id in ()
            else pids.val := [id :: pids.val];
          }
      | None ->
          do { Unix.close t; eprintf "Fork failed\n"; flush stderr } ]
    else do {
      let oc = open_out_bin sock_in.val in
      let cleanup () = try close_out oc with _ -> () in
      try copy_what_necessary t oc with
      [ Unix.Unix_error _ _ _ -> ()
      | exc -> do { cleanup (); raise exc } ];
      cleanup ();
      if noproc.val then do {
        let fd = Unix.openfile sock_in.val [Unix.O_RDONLY] 0 in
        let oc = open_out_bin sock_out.val in
        wserver_oc.val := oc;
        treat_connection tmout callback addr fd;
        flush oc;
        close_out oc;
        Unix.close fd;
      }
      else
        let pid =
          let env =
            Array.append (Unix.environment ())
              [| "WSERVER=" ^ string_of_sockaddr addr |]
          in
(*
          let args = Array.map (fun x -> "\"" ^ x ^ "\"") Sys.argv in
*)
let args = Sys.argv in
(**)
          Unix.create_process_env Sys.argv.(0) args env Unix.stdin
            Unix.stdout Unix.stderr
        in
        let _ = Unix.waitpid [] pid in
        let ic = open_in_bin sock_in.val in
        close_in ic;
      let cleanup () =
        do {
          try Unix.shutdown t Unix.SHUTDOWN_SEND with _ -> ();
          skip_possible_remaining_chars t;
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
              let len = input ic buff 0 (Bytes.length buff) in
              if len = 0 then ()
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
    if Sys.unix then None
    else
      try Some (Sys.getenv "WSERVER") with [ Not_found -> None ]
  with
  [ Some s ->
      if Sys.unix then ()
      else do {
        let addr = sockaddr_of_string s in
        let fd = Unix.openfile sock_in.val [Unix.O_RDONLY] 0 in
        let oc = open_out_bin sock_out.val in
        wserver_oc.val := oc;
        ignore (treat_connection tmout g addr fd);
        exit 0
      }
  | None ->
      do {
        check_stopping ();
        let addr =
          match addr_opt with
          [ Some addr ->
              try Unix.inet_addr_of_string addr with
              [ Failure _ -> (Unix.gethostbyname addr).Unix.h_addr_list.(0) ]
          | None -> Unix.inet_addr_any ]
        in
        let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Unix.setsockopt s Unix.SO_REUSEADDR True;
        Unix.bind s (Unix.ADDR_INET addr port);
        Unix.listen s 4;
        if Sys.unix then let _ = Unix.nice 1 in ()
        else ();
        let tm = Unix.localtime (Unix.time ()) in
        eprintf "Ready %4d-%02d-%02d %02d:%02d port"
          (1900 + tm.Unix.tm_year) (succ tm.Unix.tm_mon) tm.Unix.tm_mday
          tm.Unix.tm_hour tm.Unix.tm_min;
        eprintf " %d" port;
        eprintf "...\n";
        flush stderr;
        while True do {
          try accept_connection tmout max_clients g s with
          [ Unix.Unix_error Unix.ECONNRESET "accept" _ -> ()
          | Unix.Unix_error (Unix.EBADF | Unix.ENOTSOCK) "accept" _ as x ->
              (* oops! *) raise x
          | Sys_error msg when msg = "Broken pipe" -> ()
          | exc -> print_err_exc exc ];
          try wflush () with [ Sys_error _ -> () ];
          try flush stdout with [ Sys_error _ -> () ];
          flush stderr;
        };
      } ]
;
