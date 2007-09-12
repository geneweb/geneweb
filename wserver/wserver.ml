(* $Id: wserver.ml,v 5.15 2007-09-12 09:42:26 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Printf;

value sock_in = ref "wserver.sin";
value sock_out = ref "wserver.sou";
value stop_server = ref "STOP_SERVER";
value noproc = ref False;

value wserver_sock = ref Unix.stdout;
value wsocket () = wserver_sock.val;

value wserver_oc = ref stdout;

value wrap_string = ref (fun s -> s);

value wprint fmt =
  kprintf (fun s -> output_string wserver_oc.val (wrap_string.val s)) fmt
;
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
            s1.[i1] := Char.chr v;
            i + 3
          }
        | '+' -> do { s1.[i1] := ' '; succ i }
        | x -> do { s1.[i1] := x; succ i } ]
      in
      copy_decode_in s1 i (succ i1)
    else s1
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
    let s1 = String.create len in
    let s = copy_decode_in s1 0 0 in
    if strip_spaces then strip_heading_and_trailing_spaces s else s
  else s
;

value decode = gen_decode True;

value special =
  fun
  [ '\000'..'\031' | '\127'..'\255' | '<' | '>' | '"' | '#' | '%' | '{' |
    '}' | '|' | '\\' | '^' | '~' | '[' | ']' | '`' | ';' | '/' | '?' | ':' |
    '@' | '=' | '&' ->
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
              s1.[i1+1] := hexa_digit (Char.code c / 16);
              s1.[i1+2] := hexa_digit (Char.code c mod 16);
              i1 + 3
            }
            else do { s1.[i1] := c; succ i1 } ]
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in
    copy_code_in (String.create len) 0 0
  else s
;

value nl () = wprint "\013\010";

value http answer = do {
  let answer = if answer = "" then "200 OK" else answer in
  wprint "HTTP/1.0 %s" answer;
  nl ()
};

value print_exc exc =
  match exc with
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
    } ]
;

value print_err_exc exc = do { print_exc exc; flush stderr };

value case_unsensitive_eq s1 s2 = String.lowercase s1 = String.lowercase s2;

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
value store len x = do {
  if len >= String.length buff.val then
    buff.val := buff.val ^ String.create (String.length buff.val)
  else ();
  buff.val.[len] := x;
  succ len
};
value get_buff len = String.sub buff.val 0 len;

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

IFDEF UNIX THEN
value timeout tmout spid _ =
  do {
    Unix.kill spid Sys.sigkill;
    Unix.kill spid Sys.sigterm;
    let pid = Unix.fork () in
    if pid = 0 then
      if Unix.fork () = 0 then do {
        http "";
        wprint "Content-type: text/html; charset=iso-8859-1"; nl (); nl ();
        wprint "<head><title>Time out</title></head>\n";
        wprint "<body><h1>Time out</h1>\n";
        wprint "Computation time > %d second(s)\n" tmout;
        wprint "</body>\n";
        wflush ();
        exit 0;
      }
      else exit 0
    else ();
    let _ = Unix.waitpid [] pid in ();
    exit 2
  }
END;

value get_request_and_content strm =
  let request = get_request strm in
  let content =
    match extract_param "content-length: " ' ' request with
    [ "" -> ""
    | x -> do {
        let str = String.create (int_of_string x) in
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

value treat_connection tmout callback addr fd = do {
  IFDEF NOFORK THEN ()
  ELSE IFDEF UNIX THEN
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
  ELSE () END END;
  let (request, script_name, contents) =
    let (request, contents) =
      let strm =
        let c = " " in
        Stream.from
          (fun _ -> if Unix.read fd c 0 1 = 1 then Some c.[0] else None)
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
  | Sys_error "Broken pipe" -> ()
  | exc -> print_err_exc exc ];
  try wflush () with _ -> ();
  try flush stderr with _ -> ();
};

value buff = String.create 1024;

IFDEF WIN95 THEN
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
           if len.val = 0 then None
           else do { incr i; Some buff.[i.val - 1] }
         })
  in
  let _ = get_request_and_content strm in
  ()
END;

value rec list_remove x =
  fun
  [ [] -> failwith "list_remove"
  | [y :: l] -> if x = y then l else [y :: list_remove x l] ]
;

IFDEF NOFORK THEN declare end ELSE
value pids = ref []
END;
IFDEF NOFORK THEN declare end ELSE
value cleanup_verbose = ref True
END;
IFDEF NOFORK THEN declare end ELSE
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
END;

IFDEF NOFORK THEN declare end ELSE
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
END;

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
    let b = "..." in
    try
      loop () where rec loop () =
        match Unix.select [fd] [] [] 5.0 with
        [ ([_], [], []) ->
            let len = Unix.read fd b 0 (String.length b) in
            if len = String.length b then loop () else ()
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
    IFDEF NOFORK THEN wait_and_compact s
    ELSE if noproc.val then wait_and_compact s
    else wait_available max_clients s
    END;
    let (t, addr) = Unix.accept s in
    check_stopping ();
    Unix.setsockopt t Unix.SO_KEEPALIVE True;
    IFDEF NOFORK THEN
      let cleanup () =
        do {
          try Unix.shutdown t Unix.SHUTDOWN_SEND with _ -> ();
          try Unix.shutdown t Unix.SHUTDOWN_RECEIVE with _ -> ();
          try Unix.close t with _ -> ();
        }
      in
      do {
        wserver_sock.val := t;
        wserver_oc.val := Unix.out_channel_of_descr t;
        treat_connection tmout callback addr t;
        cleanup ();
      }
    ELSE IFDEF UNIX THEN
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
    ELSE do {
      let oc = open_out_bin sock_in.val in
      let cleanup () = try close_out oc with _ -> () in
      try copy_what_necessary t oc with
      [ Unix.Unix_error _ _ _ -> ()
      | exc -> do { cleanup (); raise exc } ];
      cleanup ();
      IFDEF SYS_COMMAND THEN
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
      ELSE if noproc.val then do {
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
        close_in ic
      END;
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
              let len = input ic buff 0 (String.length buff) in
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
    END END
  }
;

value f addr_opt port tmout max_clients g =
  match
    IFDEF NOFORK THEN None
    ELSE IFDEF WIN95 THEN
      IFDEF SYS_COMMAND THEN
        let len = Array.length Sys.argv in
        if len > 2 && Sys.argv.(len - 2) = "-wserver" then
          Some Sys.argv.(len - 1)
        else None
      ELSE
        try Some (Sys.getenv "WSERVER") with [ Not_found -> None ]
      END
    ELSE None END END
  with
  [ Some s ->
      IFDEF NOFORK THEN ()
      ELSE IFDEF WIN95 THEN do {
        let addr = sockaddr_of_string s in
        let fd = Unix.openfile sock_in.val [Unix.O_RDONLY] 0 in
        let oc = open_out_bin sock_out.val in
        wserver_oc.val := oc;
        ignore (treat_connection tmout g addr fd);
        exit 0
      }
      ELSE () END END
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
        IFDEF NOFORK THEN Sys.set_signal Sys.sigpipe Sys.Signal_ignore
        ELSE IFDEF UNIX THEN let _ = Unix.nice 1 in ()
        ELSE () END END;
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
          | Sys_error "Broken pipe" -> ()
          | exc -> print_err_exc exc ];
          try wflush () with [ Sys_error _ -> () ];
          try flush stdout with [ Sys_error _ -> () ];
          flush stderr;
        };
      } ]
;
