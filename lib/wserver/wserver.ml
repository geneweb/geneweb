(* Copyright (c) 1998-2007 INRIA *)

module Compat = Geneweb_compat
module Logs = Geneweb_logs.Logs

type handler =
  Unix.sockaddr * string list -> string -> Adef.encoded_string -> unit

let sock_in = ref "wserver.sin"
let sock_out = ref "wserver.sou"

(* global parameters set by command arguments *)
let stop_server = ref "STOP_SERVER"
let cgi = ref false

(* state of a connection request *)
let connection_closed = ref false
let wserver_sock = ref Unix.stdout
let wserver_oc = ref stdout

(* functions to access the connection state *)
let wsocket () = !wserver_sock
let woc () = !wserver_oc
let wflush () = flush !wserver_oc

let skip_possible_remaining_chars fd =
  let b = Bytes.create 3 in
  try
    let rec loop () =
      match Unix.select [ fd ] [] [] 5.0 with
      | [ _ ], [], [] ->
          let len = Unix.read fd b 0 (Bytes.length b) in
          if len = Bytes.length b then loop ()
      | _ -> ()
    in
    loop ()
    (* Read on https://utcc.utoronto.ca/~cks/space/blog/unix/AcceptErrnoProblem:
       These days accept() is standardized to return ECONNABORTED instead of
       ECONNRESET in these circumstances, although this may not be universal.
    *)
  with Unix.Unix_error (Unix.(ECONNRESET | ECONNABORTED), _, _) -> ()

let close_connection () =
  if not !connection_closed then (
    (try
       wflush ();
       Unix.shutdown !wserver_sock Unix.SHUTDOWN_SEND;
       skip_possible_remaining_chars !wserver_sock;
       close_out !wserver_oc
     with _ -> ());
    (* Closing the channel flushes the data and closes the underlying file descriptor *)
    connection_closed := true)

let printnl () = output_string !wserver_oc "\013\010"

type printing_state = Nothing | Status | Contents

let printing_state = ref Nothing

let http status =
  if !printing_state <> Nothing then failwith "HTTP Status already sent";
  printing_state := Status;
  if status <> Def.OK || not !cgi then (
    let answer =
      match status with
      | Def.OK -> "200 OK"
      | Def.Moved_Temporarily -> "302 Moved Temporarily"
      | Def.Bad_Request -> "400 Bad Request"
      | Def.Unauthorized -> "401 Unauthorized"
      | Def.Forbidden -> "403 Forbidden"
      | Def.Not_Found -> "404 Not Found"
      | Def.Conflict -> "409 Conflict"
      | Def.Internal_Server_Error -> "500 Internal Server Error"
      | Def.Service_Unavailable -> "503 Service Unavailable"
    in
    if !cgi then (
      output_string !wserver_oc "Status: ";
      output_string !wserver_oc answer)
    else (
      output_string !wserver_oc "HTTP/1.0 ";
      output_string !wserver_oc answer);
    printnl ())

let header s =
  if !printing_state <> Status then
    if !printing_state = Nothing then http Def.OK
    else failwith "Cannot write HTTP headers: page contents already started";
  output_string !wserver_oc s;
  printnl ()

let printf fmt =
  if !printing_state <> Contents then (
    if !printing_state = Nothing then http Def.OK;
    printnl ();
    printing_state := Contents);
  Printf.fprintf !wserver_oc fmt

let print_string s =
  if !printing_state <> Contents then (
    if !printing_state = Nothing then http Def.OK;
    printnl ();
    printing_state := Contents);
  output_string !wserver_oc s

let http_redirect_temporarily url =
  http Def.Moved_Temporarily;
  output_string !wserver_oc "Location: ";
  output_string !wserver_oc url;
  printnl ()

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
    | Some '\010' ->
        Stream.junk strm__;
        let s = strm__ in
        if len = 0 then []
        else
          let str = get_buff len in
          str :: loop 0 s
    | Some '\013' ->
        Stream.junk strm__;
        loop len strm__
    | Some c ->
        Stream.junk strm__;
        loop (store len c) strm__
    | _ -> if len = 0 then [] else [ get_buff len ]
  in
  loop 0 strm

let get_request_and_content strm =
  let request = get_request strm in
  let content =
    match Mutil.extract_param "content-length: " ' ' request with
    | "" -> ""
    | x -> String.init (int_of_string x) (fun _ -> Stream.next strm)
  in
  (request, Adef.encoded content)

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (a, _) -> Unix.string_of_inet_addr a

let sockaddr_of_string s = Unix.ADDR_UNIX s

let timeout_handler ~timeout _ =
  try
    if !printing_state = Nothing then http Def.OK;
    if !printing_state <> Contents then (
      output_string !wserver_oc "Content-type: text/html; charset=iso-8859-1";
      printnl ();
      printnl ();
      printf "<head><title>Time out</title></head>\n";
      printf "<body>");
    printf "<h1>Time out</h1><p>Computation time > %d second(s)</p></body>"
      timeout;
    wflush ();
    exit 0
  with Sys_error _ ->
    (* The client may close the connection before reaching the time limit.
       In this case, we cannot print the timeout message to the socket but
       this is not an error and we must exit normally, even in no-fork mode. *)
    exit 0

let treat_connection callback client_addr client_socket =
  printing_state := Nothing;
  let request, path, query =
    let request, query =
      let strm = Stream.of_channel (Unix.in_channel_of_descr client_socket) in
      get_request_and_content strm
    in
    let path, query =
      match Mutil.extract_param "GET /" ' ' request with
      | "" -> (Mutil.extract_param "POST /" ' ' request, query)
      | str -> (
          match String.index_opt str '?' with
          | Some i ->
              ( String.sub str 0 i,
                String.sub str (i + 1) (String.length str - i - 1)
                |> Adef.encoded )
          | None -> (str, "" |> Adef.encoded))
    in
    (request, path, query)
  in
  callback (client_addr, request) path query

let buff = Bytes.create 1024

let copy_what_necessary t oc =
  let strm =
    let len = ref 0 in
    let i = ref 0 in
    Stream.from (fun _ ->
        if !i >= !len then (
          len := Unix.read t buff 0 (Bytes.length buff);
          i := 0;
          if !len > 0 then output oc buff 0 !len);
        if !len = 0 then None
        else (
          incr i;
          Some (Bytes.get buff (!i - 1))))
  in
  let _ = get_request_and_content strm in
  ()

let skip_possible_remaining_chars fd =
  if not !connection_closed then skip_possible_remaining_chars fd

let check_stopping () =
  if Sys.file_exists !stop_server then (
    flush stdout;
    Logs.err (fun k ->
        k "Server stopped by presence of file %s.\n" !stop_server);
    Logs.err (fun k -> k "Remove that file to allow servers to run again.");
    exit 0)

let accept_connection_windows socket =
  let client_socket, addr = Unix.accept socket in
  Unix.setsockopt client_socket Unix.SO_KEEPALIVE true;
  connection_closed := false;
  wserver_sock := client_socket;
  check_stopping ();
  Compat.Out_channel.with_open_bin !sock_in (fun oc ->
      try copy_what_necessary client_socket oc with Unix.Unix_error _ -> ());
  let pid =
    let env =
      Array.append (Unix.environment ())
        [| "WSERVER=" ^ string_of_sockaddr addr |]
    in
    let args = Sys.argv in
    Unix.create_process_env Sys.argv.(0) args env Unix.stdin Unix.stdout
      Unix.stderr
  in
  let _ = Unix.waitpid [] pid in
  Compat.In_channel.with_open_bin !sock_in close_in;
  let shutdown () =
    (try Unix.shutdown client_socket Unix.SHUTDOWN_SEND with _ -> ());
    skip_possible_remaining_chars client_socket;
    try Unix.shutdown client_socket Unix.SHUTDOWN_RECEIVE with _ -> ()
  in
  Fun.protect ~finally:shutdown (fun () ->
      try
        Compat.In_channel.with_open_bin !sock_out (fun ic ->
            try
              let rec loop () =
                let len = input ic buff 0 (Bytes.length buff) in
                if len = 0 then ()
                else (
                  (let rec loop_write i =
                     let olen = Unix.write client_socket buff i (len - i) in
                     if i + olen < len then loop_write (i + olen)
                   in
                   loop_write 0);
                  loop ())
              in
              loop ()
            with Unix.Unix_error _ -> ())
      with Unix.Unix_error _ ->
        (* A Unix exception could be raised by [In_channel.open_bin]
           inside [In_channel.with_open_bin]. *)
        ())

let accept_connections_windows socket =
  while true do
    try accept_connection_windows socket with
    | Unix.Unix_error (Unix.ECONNRESET, "accept", _) as e ->
        Logs.info (fun k -> k "%s" (Printexc.to_string e))
    | Sys_error msg as e when msg = "Broken pipe" ->
        Logs.info (fun k -> k "%s" (Printexc.to_string e))
  done

(* Set a Unix signal with a timeout around the execution of the function [f].
   The signal is properly cleared even if the function [f] raises an exception.

   Since a process can have only one active alarm signal at a time, this
   function should be used only once per fork of the web server.

   This function is supported only on Unix. *)
let with_timeout ~timeout handler f =
  assert Sys.unix;
  if timeout > 0 then (
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle handler);
    let finally () = ignore (Unix.alarm 0 : int) in
    let g () =
      ignore (Unix.alarm timeout : int);
      f ()
    in
    Fun.protect ~finally g)
  else f ()

let accept_connection_unix ~timeout callback socket pid =
  let client_socket, client_addr = My_unix.accept_noeintr socket in
  Logs.debug (fun k -> k "Worker %d got a job" pid);
  Unix.setsockopt client_socket Unix.SO_KEEPALIVE true;
  connection_closed := false;
  wserver_sock := client_socket;
  wserver_oc := Unix.out_channel_of_descr client_socket;
  Fun.protect ~finally:close_connection @@ fun () ->
  with_timeout ~timeout (timeout_handler ~timeout) @@ fun () ->
  treat_connection callback client_addr client_socket

let accept_connections_unix ~timeout ~n_workers callback socket =
  if n_workers > 0 then
    Pool.start n_workers (accept_connection_unix ~timeout callback socket)
  else
    (* We avoid forking in the case, which is helpful for debugging. *)
    while true do
      try accept_connection_unix ~timeout callback socket (Unix.getpid ())
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        Logs.info (fun k -> k "%a" Util.pp_exception (e, bt))
    done

let accept_connections ~timeout ~n_workers callback socket =
  if Sys.unix then accept_connections_unix ~timeout ~n_workers callback socket
  else accept_connections_windows socket

let start ?addr ~port ?(timeout = 0) ~max_pending_requests ~n_workers callback =
  match Sys.getenv "WSERVER" with
  | exception Not_found ->
      check_stopping ();
      let addr =
        match addr with
        | None ->
            if Unix.string_of_inet_addr Unix.inet6_addr_any = "::" then
              Unix.inet_addr_any
            else Unix.inet6_addr_any
        | Some addr -> (
            try Unix.inet_addr_of_string addr
            with Failure _ -> (Unix.gethostbyname addr).Unix.h_addr_list.(0))
      in
      let socket =
        Unix.socket
          (if Unix.string_of_inet_addr Unix.inet6_addr_any = "::" then
             Unix.PF_INET
           else Unix.PF_INET6)
          Unix.SOCK_STREAM 0
      in
      if Unix.string_of_inet_addr Unix.inet6_addr_any <> "::" then
        Unix.setsockopt socket Unix.IPV6_ONLY false;
      Unix.setsockopt socket Unix.SO_REUSEADDR true;
      Unix.bind socket (Unix.ADDR_INET (addr, port));
      My_unix.listen_noeintr socket max_pending_requests;
      let tm = Unix.localtime (Unix.time ()) in
      Logs.info (fun k ->
          k "Ready %4d-%02d-%02d %02d:%02d port %d..." (1900 + tm.Unix.tm_year)
            (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
            port);
      if n_workers = 0 then ignore @@ Sys.signal Sys.sigpipe Sys.Signal_ignore;
      accept_connections ~timeout ~n_workers callback socket
  | s ->
      let addr = sockaddr_of_string s in
      let client_socket = Unix.openfile !sock_in [ Unix.O_RDONLY ] 0 in
      let oc = open_out_bin !sock_out in
      wserver_oc := oc;
      ignore (treat_connection callback addr client_socket);
      exit 0
