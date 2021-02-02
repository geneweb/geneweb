(* Copyright (c) 1998-2007 INRIA *)

let connection_closed = ref false

let eprintf = Printf.eprintf

let sock_in = ref "wserver.sin"
let sock_out = ref "wserver.sou"
let stop_server = ref "STOP_SERVER"
let noproc = ref false
let cgi = ref false

let wserver_sock = ref Unix.stdout
let wsocket () = !wserver_sock

let wserver_oc = ref stdout
let woc () = !wserver_oc

let wflush () = flush !wserver_oc

let skip_possible_remaining_chars fd =
  let b = Bytes.create 3 in
  try
    let rec loop () =
      match Unix.select [fd] [] [] 5.0 with
      | [_], [], [] ->
        let len = Unix.read fd b 0 (Bytes.length b) in
        if len = Bytes.length b then loop ()
      | _ -> ()
    in
    loop ()
  with Unix.Unix_error (Unix.ECONNRESET, _, _) -> ()

let close_connection () =
  if not !connection_closed then begin
    wflush () ;
    Unix.shutdown !wserver_sock Unix.SHUTDOWN_SEND ;
    skip_possible_remaining_chars !wserver_sock ;
    (* Closing the channel flushes the data and closes the underlying file descriptor *)
    close_out !wserver_oc ;
    connection_closed := true
  end

let printnl fmt =
  Printf.ksprintf
    (fun s ->
       output_string !wserver_oc s; output_string !wserver_oc "\013\010")
    fmt

type printing_state = Nothing | Status | Contents

let printing_state = ref Nothing

let http status =
  if !printing_state <> Nothing then failwith "HTTP Status already sent";
  printing_state := Status;
  if status <> Def.OK || not !cgi then
    let answer = match status with
      | Def.OK -> "200 OK"
      | Def.Moved_Temporarily -> "302 Moved Temporarily"
      | Def.Bad_Request -> "400 Bad Request"
      | Def.Unauthorized -> "401 Unauthorized"
      | Def.Forbidden -> "403 Forbidden"
      | Def.Not_Found -> "404 Not Found"
    in
    if !cgi then printnl "Status: %s" answer else printnl "HTTP/1.0 %s" answer

let header fmt =
  if !printing_state <> Status then
    if !printing_state = Nothing then http Def.OK
    else failwith "Cannot write HTTP headers: page contents already started";
  printnl fmt

let printf fmt =
  if !printing_state <> Contents then
    begin
      if !printing_state = Nothing then http Def.OK;
      printnl "";
      printing_state := Contents
    end;
  Printf.ksprintf (fun s -> output_string !wserver_oc s) fmt

let print_string s =
  if !printing_state <> Contents then
    begin
      if !printing_state = Nothing then http Def.OK;
      printnl "";
      printing_state := Contents
    end ;
  output_string !wserver_oc s

let http_redirect_temporarily url =
  http Def.Moved_Temporarily; printnl "Location: %s" url

let syslog level msg =
  let log = Syslog.openlog @@ Filename.basename @@ Sys.executable_name in
  Syslog.syslog log level msg ;
  Syslog.closelog log

let buff = ref (Bytes.create 80)
let store len x =
  if len >= Bytes.length !buff then
    buff := Bytes.extend !buff 0 (Bytes.length !buff);
  Bytes.set !buff len x;
  succ len
let get_buff len = Bytes.sub_string !buff 0 len

let get_request strm =
  let rec loop len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '\010' ->
        Stream.junk strm__;
        let s = strm__ in
        if len = 0 then [] else let str = get_buff len in str :: loop 0 s
    | Some '\013' -> Stream.junk strm__; loop len strm__
    | Some c -> Stream.junk strm__; loop (store len c) strm__
    | _ -> if len = 0 then [] else [get_buff len]
  in
  loop 0 strm

let timeout tmout spid _ =
  Unix.kill spid Sys.sigkill;
  Unix.kill spid Sys.sigterm;
  let pid = Unix.fork () in
  if pid = 0 then
    if Unix.fork () = 0 then
      begin
        http Def.OK;
        printnl "Content-type: text/html; charset=iso-8859-1";
        printnl "";
        printf "<head><title>Time out</title></head>\n";
        printf "<body><h1>Time out</h1>\n";
        printf "Computation time > %d second(s)\n" tmout;
        printf "</body>\n";
        wflush ();
        exit 0
      end
    else exit 0;
  let _ = Unix.waitpid [] pid in (); exit 2

let get_request_and_content strm =
  let request = get_request strm in
  let content =
    match Mutil.extract_param "content-length: " ' ' request with
    | "" -> ""
    | x -> String.init (int_of_string x) (fun _ -> Stream.next strm)
  in
  request, content

let string_of_sockaddr =
  function
    Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (a, _) -> Unix.string_of_inet_addr a
let sockaddr_of_string s = Unix.ADDR_UNIX s

let treat_connection tmout callback addr fd =
  if Sys.unix then
    if tmout > 0 then
      begin let spid = Unix.fork () in
        if spid > 0 then
          begin
            ignore @@ Sys.signal Sys.sigalrm (Sys.Signal_handle (timeout tmout spid)) ;
            ignore @@ Unix.alarm tmout ;
            ignore @@ Unix.waitpid [] spid ;
            ignore @@ Sys.signal Sys.sigalrm Sys.Signal_default ;
            exit 0
          end
      end;
  let (request, script_name, contents) =
    let (request, contents) =
      let strm = Stream.of_channel (Unix.in_channel_of_descr fd) in
      get_request_and_content strm
    in
    let (script_name, contents) =
      match Mutil.extract_param "GET /" ' ' request with
        "" -> Mutil.extract_param "POST /" ' ' request, contents
      | str ->
          try
            let i = String.index str '?' in
            String.sub str 0 i,
            String.sub str (i + 1) (String.length str - i - 1)
          with Not_found -> str, ""
    in
    request, script_name, contents
  in
  callback (addr, request) script_name contents

let buff = Bytes.create 1024

let copy_what_necessary t oc =
  let strm =
    let len = ref 0 in
    let i = ref 0 in
    Stream.from
      (fun _ ->
         if !i >= !len then
           begin
             len := Unix.read t buff 0 (Bytes.length buff);
             i := 0;
             if !len > 0 then output oc buff 0 !len
           end;
         if !len = 0 then None
         else begin incr i; Some (Bytes.get buff (!i - 1)) end)
  in
  let _ = get_request_and_content strm in ()

let rec list_remove x =
  function
    [] -> failwith "list_remove"
  | y :: l -> if x = y then l else y :: list_remove x l

let pids = ref []

let cleanup_sons () =
  List.iter begin fun p ->
    let pid =
      try fst (Unix.waitpid [Unix.WNOHANG] p)
      with e ->
        syslog `LOG_ERR (__LOC__ ^ ": " ^ Printexc.to_string e) ;
        raise e
    in
    if pid <> 0 then pids := list_remove pid !pids
  end !pids

let wait_available max_clients s =
  match max_clients with
    Some m ->
      if List.length !pids >= m then
        (let (pid, _) = Unix.wait () in pids := list_remove pid !pids);
      if !pids <> [] then cleanup_sons ();
      let stop_verbose = ref false in
      while !pids <> [] && Unix.select [s] [] [] 15.0 = ([], [], []) do
        cleanup_sons ();
        if !pids <> [] && not !stop_verbose then
          begin
            stop_verbose := true;
            let tm = Unix.localtime (Unix.time ()) in
            eprintf
              "*** %02d/%02d/%4d %02d:%02d:%02d %d process(es) remaining after cleanup (%d)\n"
              tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year)
              tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
              (List.length !pids) (List.hd !pids);
            flush stderr;
            ()
          end
      done
  | None -> ()

let wait_and_compact s =
  if Unix.select [s] [] [] 15.0 = ([], [], []) then
    begin
      eprintf "Compacting... ";
      flush stderr;
      Gc.compact ();
      eprintf "Ok\n";
      flush stderr
    end

let skip_possible_remaining_chars fd =
  if not !connection_closed then skip_possible_remaining_chars fd

let check_stopping () =
  if Sys.file_exists !stop_server then
    begin
      flush stdout;
      eprintf "\nServer stopped by presence of file %s.\n" !stop_server;
      eprintf "Remove that file to allow servers to run again.\n";
      flush stderr;
      exit 0
    end

let accept_connection tmout max_clients callback s =
  if !noproc then wait_and_compact s else wait_available max_clients s;
  let (t, addr) = Unix.accept s in
  check_stopping ();
  Unix.setsockopt t Unix.SO_KEEPALIVE true;
  if Sys.unix then
    match try Some (Unix.fork ()) with _ -> None with
    | Some 0 ->
      begin
        try
          if max_clients = None && Unix.fork () <> 0 then exit 0;
          Unix.close s;
          wserver_sock := t;
          wserver_oc := Unix.out_channel_of_descr t;
          treat_connection tmout callback addr t ;
          close_connection () ;
          exit 0
        with
        | Unix.Unix_error (Unix.ECONNRESET, "read", _) -> exit 0
        | e -> raise e
      end
    | Some id ->
        Unix.close t;
        if max_clients = None then let _ = Unix.waitpid [] id in ()
        else pids := id :: !pids
    | None -> Unix.close t; eprintf "Fork failed\n"; flush stderr
  else
    let oc = open_out_bin !sock_in in
    let cleanup () = try close_out oc with _ -> () in
    begin try copy_what_necessary t oc with
      Unix.Unix_error (_, _, _) -> ()
    | exc -> cleanup (); raise exc
    end;
    cleanup ();
    if !noproc then
      let fd = Unix.openfile !sock_in [Unix.O_RDONLY] 0 in
      let oc = open_out_bin !sock_out in
      wserver_oc := oc;
      treat_connection tmout callback addr fd;
      flush oc;
      close_out oc;
      Unix.close fd
    else
      begin let pid =
        let env =
          Array.append (Unix.environment ())
            [| "WSERVER=" ^ string_of_sockaddr addr |]
        in
        let args = Sys.argv in
        Unix.create_process_env Sys.argv.(0) args env Unix.stdin Unix.stdout
          Unix.stderr
      in
        let _ = Unix.waitpid [] pid in
        let ic = open_in_bin !sock_in in close_in ic
      end;
    let cleanup () =
      (try Unix.shutdown t Unix.SHUTDOWN_SEND with _ -> ());
      skip_possible_remaining_chars t;
      (try Unix.shutdown t Unix.SHUTDOWN_RECEIVE with _ -> ());
      try Unix.close t with _ -> ()
    in
    begin try
      begin let ic = open_in_bin !sock_out in
        let cleanup () = try close_in ic with _ -> () in
        begin try
          begin let rec loop () =
            let len = input ic buff 0 (Bytes.length buff) in
            if len = 0 then ()
            else
              begin
                begin let rec loop_write i =
                  let olen = Unix.write t buff i (len - i) in
                  if i + olen < len then loop_write (i + olen)
                in
                  loop_write 0
                end;
                loop ()
              end
          in
            loop ()
          end
        with
          Unix.Unix_error (_, _, _) -> ()
        | exc -> cleanup (); raise exc
        end;
        cleanup ()
      end
    with
      Unix.Unix_error (_, _, _) -> ()
    | exc -> cleanup (); raise exc
    end;
    cleanup ()

let f addr_opt port tmout max_clients g =
  match
    if Sys.unix then None
    else try Some (Sys.getenv "WSERVER") with Not_found -> None
  with
  | Some s ->
    let addr = sockaddr_of_string s in
    let fd = Unix.openfile !sock_in [Unix.O_RDONLY] 0 in
    let oc = open_out_bin !sock_out in
    wserver_oc := oc; ignore (treat_connection tmout g addr fd); exit 0
  | None ->
      check_stopping ();
      let addr =
        match addr_opt with
          Some addr ->
            begin try Unix.inet_addr_of_string addr with
              Failure _ -> (Unix.gethostbyname addr).Unix.h_addr_list.(0)
            end
        | None -> Unix.inet_addr_any
      in
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.setsockopt s Unix.SO_REUSEADDR true;
      Unix.bind s (Unix.ADDR_INET (addr, port));
      Unix.listen s 4;
      if Sys.unix then (let _ = Unix.nice 1 in ());
      let tm = Unix.localtime (Unix.time ()) in
      eprintf "Ready %4d-%02d-%02d %02d:%02d port %d...\n"
        (1900 + tm.Unix.tm_year)
        (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
        port ;
      flush stderr;
      while true do
        begin
          try accept_connection tmout max_clients g s with
          | Unix.Unix_error (Unix.ECONNRESET, "accept", _) -> ()
          | Sys_error msg when msg = "Broken pipe" -> ()
          | e -> syslog `LOG_CRIT (__LOC__ ^ ": " ^ Printexc.to_string e)
        end ;
        flush_all () ;
      done
