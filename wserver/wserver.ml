(* $Id: wserver.ml,v 1.17 1999-03-04 12:36:54 ddr Exp $ *)
(* Copyright (c) INRIA *)

value wserver_oc =
  do set_binary_mode_out stdout True; return ref stdout
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
            do s1.[i1] := Char.chr v; return i + 3
        | '+' -> do s1.[i1] := ' '; return succ i
        | x -> do s1.[i1] := x; return succ i ]
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

value special x = List.mem x ['='; '&'; ';'; '"'; '<'; '>'];

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
        [ ' ' -> do s1.[i1] := '+'; return succ i1
        | c ->
            if special c then
              do s1.[i1] := '%';
                 s1.[i1 + 1] := hexa_digit (Char.code c / 16);
                 s1.[i1 + 2] := hexa_digit (Char.code c mod 16);
              return i1 + 3
            else do s1.[i1] := c; return succ i1 ]
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (String.create len) 0 0
  else s
;

value nl () =
  do wflush ();
     let _ =
        Unix.write (Unix.descr_of_out_channel wserver_oc.val) "\r\n" 0 2 in
     ();
  return ()
;

value html charset =
  let charset = if charset = "" then "iso-8859-1" else charset in
  do wprint "HTTP/1.0 200 OK"; nl ();
     wprint "Content-type: text/html; charset=%s" charset; nl (); nl ();
  return ()
;

value print_exc exc =
  match exc with
  [ Unix.Unix_error err fun_name arg ->
      do prerr_string "\"";
         prerr_string fun_name;
         prerr_string "\" failed";
         if String.length arg > 0 then
           do prerr_string " on \""; prerr_string arg; prerr_string "\"";
           return ()
         else ();
         prerr_string ": ";
         prerr_endline (Unix.error_message err);
      return ()
  | Out_of_memory -> prerr_string "Out of memory\n"
  | Match_failure (file, first_char, last_char) ->
      do prerr_string "Pattern matching failed, file ";
         prerr_string file;
         prerr_string ", chars ";
         prerr_int first_char;
         prerr_char '-';
         prerr_int last_char;
      return prerr_char '\n'
  | Assert_failure (file, first_char, last_char) ->
      do prerr_string "Assertion failed, file ";
         prerr_string file;
         prerr_string ", chars ";
         prerr_int first_char;
         prerr_char '-';
         prerr_int last_char;
      return prerr_char '\n'
  | x ->
      do prerr_string "Uncaught exception: ";
         prerr_string (Obj.magic (Obj.field (Obj.field (Obj.repr x) 0) 0));
         if Obj.size (Obj.repr x) > 1 then
           do prerr_char '(';
              for i = 1 to Obj.size (Obj.repr x) - 1 do
                if i > 1 then prerr_string ", " else ();
                let arg = Obj.field (Obj.repr x) i in
                if not (Obj.is_block arg) then
                  prerr_int (Obj.magic arg : int)
                else if Obj.tag arg = 252 then
                  do prerr_char '"'; prerr_string (Obj.magic arg : string);
                  return prerr_char '"'
                else prerr_char '_';
              done;
           return prerr_char ')'
         else ();
      return prerr_char '\n' ]
;

value print_err_exc exc =
  do print_exc exc;
     Printf.eprintf "Please report.\n";
     flush stderr;
  return ()
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
  do if len >= String.length buff.val then
       buff.val := buff.val ^ String.create (String.length buff.val)
     else ();
     buff.val.[len] := x;
  return succ len
;
value get_buff len = String.sub buff.val 0 len;

value get_request strm =
  let rec loop len =
    parser
    [ [: `'\n'; s :] ->
        if len == 0 then []
        else let str = get_buff len in [str :: loop 0 s]
    | [: `'\r'; s :] -> loop len s
    | [: `c; s :] -> loop (store len c) s
    | [: :] -> if len == 0 then [] else [get_buff len] ]
  in
  loop 0 strm
;

ifdef UNIX then
value timeout tmout spid _ =
  do Unix.kill spid Sys.sigkill;
     html "";
     wprint "<head><title>Time out</title></head>\n";
     wprint "<body><h1>Time out</h1>\n";
     wprint "Computation time > %d second(s)\n" tmout;
     wprint "</body>\n";
     wflush ();
  return exit 2
;

value get_request_and_content strm =
  let request = get_request strm in
  let content =
    match extract_param "content-length: " ' ' request with
    [ "" -> ""
    | x ->
        let str = String.create (int_of_string x) in
        do for i = 0 to String.length str - 1 do
             str.[i] :=
               match strm with parser
               [ [: `x :] -> x
               | [: :] -> ' ' ];
           done;
        return str ]
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
  do ifdef UNIX then
       if tmout > 0 then
         let spid = Unix.fork () in
         if spid > 0 then
           do let _ (* : Sys.signal_behavior *) =
                Sys.signal Sys.sigalrm
                  (Sys.Signal_handle (timeout tmout spid))
              in ();
              let _ = Unix.alarm tmout in ();
              let _ = Unix.waitpid [] spid in ();
              let _ (* : Sys.signal_behavior *) =
                Sys.signal Sys.sigalrm Sys.Signal_default
              in ();
              exit 0;
           return ()
         else ()
       else ()
     else ();
  return
  let strm = Stream.of_channel ic in
  let (request, content) = get_request_and_content strm in
  let str =
    match extract_param "GET /" ' ' request with
    [ "" ->
        match extract_param "POST /" ' ' request with
        [ "" -> ""
        | str -> str ^ "?" ^ content ]
    | str -> str ]
  in
  if str = "robots.txt" then
    do wprint "HTTP/1.0 200 Ok"; nl ();
       wprint "Content-type: text/plain"; nl (); nl ();
       wprint "User-Agent: *"; nl ();
       wprint "Disallow: /"; nl ();
       wflush ();
       Printf.eprintf "Robot request\n";
       flush stderr;
    return ()
  else
    let _ = ifdef UNIX then Unix.nice 1 else () in
    do try callback (addr, request) str with exc -> print_err_exc exc;
       try wflush () with _ -> ();
       try flush stderr with _ -> ();
    return ()
;

value buff = String.create 1024;

ifdef WIN95 then
value copy_what_necessary t oc =
  let strm =
    let len = ref 0 in
    let i = ref 0 in
    Stream.from
      (fun _ ->
	 do if i.val >= len.val then
              do len.val := Unix.read t buff 0 (String.length buff);
	         i.val := 0;
		 if len.val > 0 then output oc buff 0 len.val else ();
	      return ()
	    else ();
	 return
	 if len.val == 0 then None
	 else do incr i; return Some buff.[i.val - 1])
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
             do if cleanup_verbose.val then
                  do Printf.eprintf "*** Why error on waitpid %d?\n" p;
                     flush stderr;
                     print_exc exc;
                     Printf.eprintf "[";
                     List.iter (fun p -> Printf.eprintf " %d" p) pids.val;
                     Printf.eprintf "]\n";
                     flush stderr;
                     cleanup_verbose.val := False;
                  return ()
                else ();
	     return p ]
       in
       if pid = 0 then ()
       else pids.val := list_remove pid pids.val)
     pids.val
;

value wait_available max_clients s =
  match max_clients with
  [ Some m ->
      do if List.length pids.val >= m then
(*
let tm = Unix.localtime (Unix.time ()) in
do Printf.eprintf "*** %02d/%02d/%4d %02d:%02d:%02d " tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec; Printf.eprintf "%d clients running; waiting...\n" m; flush stderr; return
*)
           let (pid, _) = Unix.wait () in
(*
let tm = Unix.localtime (Unix.time ()) in
do Printf.eprintf "*** %02d/%02d/%4d %02d:%02d:%02d " tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec; Printf.eprintf "ok: place for another client\n"; flush stderr; return
*)
           pids.val := list_remove pid pids.val
         else ();
         if pids.val <> [] then cleanup_sons () else ();
         let stop_verbose = ref False in
         while pids.val <> [] && Unix.select [s] [] [] 15.0 = ([], [], []) do
           cleanup_sons ();
           if pids.val <> [] && not stop_verbose.val then
             do stop_verbose.val := True; return
             let tm = Unix.localtime (Unix.time ()) in
do Printf.eprintf "*** %02d/%02d/%4d %02d:%02d:%02d %d process(es) remaining after cleanup\n" tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec (List.length pids.val); flush stderr; return ()
           else ();
         done;
     return ()
  | None -> () ]
;

value accept_connection tmout max_clients callback s =
  do wait_available max_clients s; return
  let (t, addr) = Unix.accept s in
  do Unix.setsockopt t Unix.SO_KEEPALIVE True; return
  ifdef UNIX then
    match try Some (Unix.fork ()) with _ -> None with
    [ Some 0 ->
        do try
             do if max_clients = None && Unix.fork () <> 0 then exit 0 else ();
                Unix.close s;
                Unix.dup2 t Unix.stdout;
                Unix.dup2 t Unix.stdin;
                treat_connection tmout callback addr stdin;
                try Unix.close t with _ -> ();
             return ()
           with exc ->
             try do print_err_exc exc; flush stderr; return ()
             with _ -> ();
        return exit 0
    | Some id ->
        do Unix.close t;
           if max_clients = None then let _ = Unix.waitpid [] id in ()
           else pids.val := [id :: pids.val];
        return ()
    | None ->
        do Unix.close t; Printf.eprintf "Fork failed\n"; flush stderr;
        return () ]
  else
    do let oc = open_out_bin "gwd.sin" in
       let cleanup () = try close_out oc with _ -> () in
       do try copy_what_necessary t oc with
          [ Unix.Unix_error _ _ _ -> ()
          | exc -> do cleanup (); return raise exc ];
          cleanup ();
       return ();
    return
    let pid =
      let env =
        Array.append (Unix.environment ())
          [| "WSERVER=" ^ string_of_sockaddr addr |]
      in
      Unix.create_process_env Sys.argv.(0) Sys.argv env Unix.stdin Unix.stdout
        Unix.stderr
    in
    let _ = Unix.waitpid [] pid in
    let cleanup () =
      do try Unix.shutdown t Unix.SHUTDOWN_SEND with _ -> ();
         try Unix.shutdown t Unix.SHUTDOWN_RECEIVE with _ -> ();
	 try Unix.close t with _ -> ();
      return ()
    in
    do try
         let ic = open_in_bin "gwd.sou" in
         let cleanup () = try close_in ic with _ -> () in
         do try
              loop () where rec loop () =
   	        let len = input ic buff 0 (String.length buff) in
                if len == 0 then ()
   		else
   		  do loop_write 0 where rec loop_write i =
   		       let olen = Unix.write t buff i (len - i) in
                       if i + olen < len then loop_write (i + olen) else ();
   		  return loop ()
            with
            [ Unix.Unix_error _ _ _ -> ()
	    | exc -> do cleanup (); return raise exc ];
      	    cleanup ();
         return ()
       with
       [ Unix.Unix_error _ _ _ -> ()
       | exc -> do cleanup (); return raise exc ];
      cleanup ();
   return ()
;

value f port tmout max_clients g =
  match try Some (Sys.getenv "WSERVER") with [ Not_found -> None ] with
  [ Some s ->
      let addr = sockaddr_of_string s in
      let ic = open_in_bin "gwd.sin" in
      let oc = open_out_bin "gwd.sou" in
      do wserver_oc.val := oc;
         treat_connection tmout g addr ic;
      return exit 0
  | None ->
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      do Unix.setsockopt s Unix.SO_REUSEADDR True;
         Unix.bind s (Unix.ADDR_INET Unix.inet_addr_any port);
         Unix.listen s 4;
      return
      let tm = Unix.localtime (Unix.time ()) in
      do Printf.eprintf "Ready %02d/%02d/%4d %02d:%02d port %d...\n"
           tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year)
           tm.Unix.tm_hour tm.Unix.tm_min port;
         flush stderr;
         while True do
           try accept_connection tmout max_clients g s with
           [ Unix.Unix_error _ "accept" _ -> ()
           | exc -> print_err_exc exc ];
           wflush ();
           flush stdout;
           flush stderr;
         done;
      return () ]
;
