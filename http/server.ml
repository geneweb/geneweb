(* Copyright (c) 1998-2007 INRIA *)

let src = Logs.Src.create ~doc:"HTTP" "HTTP"

let timestamp_tag : unit Logs.Tag.def =
  Logs.Tag.def "timestamp" ~doc:"POSIX timestamp" Fmt.nop

let timestamp = Logs.Tag.(empty |> add timestamp_tag ())

module Log = (val Logs.src_log src : Logs.LOG)
module Fd = Geneweb_win32.Fd

type handler =
  Connection.t -> Unix.sockaddr * string list -> string -> string -> unit

(* global parameters set by command arguments *)
let stop_server = ref "STOP_SERVER"
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
    match Header.extract_param "content-length: " ' ' request with
    | "" -> ""
    | x -> String.init (int_of_string x) (fun _ -> Stream.next strm)
  in
  (request, content)

let treat_connection callback client_addr conn =
  let request, path, query =
    let request, query =
      let strm = Stream.of_channel @@ Connection.wic conn in
      get_request_and_content strm
    in
    let path, query =
      match Header.extract_param "GET /" ' ' request with
      | "" -> (Header.extract_param "POST /" ' ' request, query)
      | str -> (
          match String.index_opt str '?' with
          | Some i ->
              ( String.sub str 0 i,
                String.sub str (i + 1) (String.length str - i - 1) )
          | None -> (str, ""))
    in
    (request, path, query)
  in
  callback conn (client_addr, request) path query

let check_stopping () =
  if Sys.file_exists !stop_server then (
    Log.err (fun k -> k "Server stopped by presence of file %s.\n" !stop_server);
    Log.err (fun k -> k "Remove that file to allow servers to run again.");
    exit 0)

let accept_connection_windows socket =
  check_stopping ();
  let client_socket, addr = Unix.accept socket in
  Unix.setsockopt client_socket Unix.SO_KEEPALIVE true;
  let fd_in, fd_out = Unix.pipe ~cloexec:true () in
  let pid =
    let env = Array.append [| "WSERVER=true" |] (Unix.environment ()) in
    Unix.create_process_env Sys.argv.(0) Sys.argv env fd_in Unix.stdout
      Unix.stderr
  in
  Unix.close fd_in;
  let oc = Unix.out_channel_of_descr fd_out in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      set_binary_mode_out oc true;
      output_value oc (Fd.file_descr_to_fd client_socket);
      output_value oc addr);
  close_in stdin;
  ignore (Unix.waitpid [] pid)

let accept_connections_windows socket =
  while true do
    try accept_connection_windows socket with
    | Unix.Unix_error (Unix.ECONNRESET, "accept", _) as e ->
        Log.info (fun k -> k "%s" (Printexc.to_string e))
    | Sys_error msg as e when msg = "Broken pipe" ->
        Log.info (fun k -> k "%s" (Printexc.to_string e))
  done

module Timeout : sig
  exception Timeout

  val with_timeout : timeout:int -> (unit -> 'a) -> 'a
  (* Set a Unix signal with a timeout around the execution of the function [f].
   The signal is properly cleared even if the function [f] raises an exception.

   Since a process can have only one active alarm signal at a time, this
   function should be used only once per fork of the web server.

   This function is supported only on Unix.

   @raise Timeout if the time limit is reached.
   @raise Failure if a timeout is already set up in the current process. *)
end = struct
  exception Timeout

  let is_set = ref false

  let with_timeout ~timeout f =
    if !is_set then failwith "timeout already set"
    else if timeout <= 0 then f ()
    else (
      Sys.set_signal Sys.sigalrm
        (Sys.Signal_handle (fun (_ : int) -> raise Timeout));
      let finally () =
        try
          ignore (Unix.alarm 0 : int);
          is_set := false
        with Timeout -> ()
      in
      Fun.protect ~finally @@ fun () ->
      is_set := true;
      ignore (Unix.alarm timeout : int);
      f ())
end

let output_timeout ~timeout conn =
  Connection.http conn Code.OK;
  Connection.header conn "Content-type: text/html; charset=utf-8";
  Connection.header conn "Connection: close";
  Connection.printf conn
    {|
<html>
  <head>
    <title>Time out</title>
  </head>
  <body>
    <h1>Time out</h1>
    <p>Computation time > %d seconds</p>
  </body>
</html>
|}
    timeout

let accept_connection_unix ~timeout callback socket pid =
  check_stopping ();
  let client_socket, client_addr = My_unix.accept_noeintr socket in
  Log.debug (fun k -> k "Worker %d got a job" pid);
  Unix.setsockopt client_socket Unix.SO_KEEPALIVE true;
  let conn = Connection.of_socket client_socket in
  Fun.protect ~finally:(fun () -> Connection.close conn) @@ fun () ->
  try
    Timeout.with_timeout ~timeout @@ fun () ->
    treat_connection callback client_addr conn
  with Timeout.Timeout -> output_timeout ~timeout conn

let accept_connections_unix ~timeout ~n_workers callback socket =
  if n_workers > 0 then
    Pool.start n_workers (accept_connection_unix ~timeout callback socket)
  else
    (* We avoid forking in the case, which is helpful for debugging. *)
    while true do
      accept_connection_unix ~timeout callback socket (Unix.getpid ())
    done

let accept_connections ~timeout ~n_workers callback socket =
  if Sys.unix then accept_connections_unix ~timeout ~n_workers callback socket
  else accept_connections_windows socket

let resolve_addr ?addr port =
  let port = string_of_int port in
  let hints = [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ] in
  match addr with
  | Some a -> Unix.getaddrinfo a port hints
  | None -> Unix.getaddrinfo "" port (Unix.AI_PASSIVE :: hints)

let enable_dual_stack ai_addr socket =
  match ai_addr with
  | Unix.ADDR_INET (a, _) when a = Unix.inet6_addr_any ->
      Unix.setsockopt socket Unix.IPV6_ONLY false
  | _ -> ()

let try_addresses l =
  let rec loop l =
    match l with
    | Unix.{ ai_family = Unix.PF_UNIX; _ } :: l -> loop l
    | Unix.{ ai_family; ai_socktype; ai_addr; _ } :: l -> (
        match Unix.socket ai_family ai_socktype 0 with
        | exception Unix.Unix_error (e, _, _) ->
            Log.debug (fun k ->
                k "failed to create socket for %a: %s" Util.pp_sockaddr ai_addr
                  (Unix.error_message e));
            loop l
        | socket -> (
            Unix.setsockopt socket Unix.SO_REUSEADDR true;
            enable_dual_stack ai_addr socket;
            match Unix.bind socket ai_addr with
            | exception Unix.Unix_error (e, _, _) ->
                Log.debug (fun k ->
                    k "failed to bind socket to %a: %s" Util.pp_sockaddr ai_addr
                      (Unix.error_message e));
                Unix.close socket;
                loop l
            | () -> Some (ai_addr, socket)))
    | [] -> None
  in
  loop l

let lan_urls port =
  match resolve_addr ~addr:(Unix.gethostname ()) port with
  | exception Unix.Unix_error (_, _, _) -> []
  | l ->
      List.filter_map
        (fun Unix.{ ai_addr; _ } ->
          if Util.is_lan_candidate ai_addr then
            Some (Fmt.str "http://%a" Util.pp_sockaddr ai_addr)
          else None)
        l
      |> List.sort_uniq String.compare

let pp_urls = Fmt.vbox (Fmt.list ~sep:Fmt.cut Fmt.string)

let pp_url ppf s =
  match Unix.getnameinfo s [ NI_NAMEREQD ] with
  | { ni_hostname; _ } ->
      Fmt.pf ppf "http://%a (%s)" Util.pp_sockaddr s ni_hostname
  | exception Not_found -> Fmt.pf ppf "http://%a" Util.pp_sockaddr s

let start ?addr ~port ?(timeout = 0) ~max_pending_requests ~n_workers callback =
  match Sys.getenv "WSERVER" with
  | exception Not_found -> (
      check_stopping ();
      match resolve_addr ?addr port with
      | (exception _) | [] ->
          (* TODO: move this code in `gwd.ml` *)
          Log.err (fun k ->
              k "Cannot resolve the interface %a:%i."
                Fmt.(option ~none:(const string "any") string)
                addr port);
          exit 2
      | l -> (
          match try_addresses l with
          | None ->
              (* TODO: move this code in `gwd.ml` *)
              Log.err (fun k ->
                  k "Cannot bind any interface for %a:%i."
                    Fmt.(option ~none:(const string "any") string)
                    addr port);
              exit 2
          | Some (addr, socket) ->
              Unix.listen socket max_pending_requests;
              (match addr with
              | Unix.ADDR_INET (a, p)
                when a = Unix.inet6_addr_any || a = Unix.inet_addr_any ->
                  Log.info (fun k ->
                      k ~tags:timestamp
                        "The server listens on every network interface.");
                  Log.info (fun k ->
                      k "Ready on %a" pp_urls
                        (Printf.sprintf "http://127.0.0.1:%d" p :: lan_urls p))
              | _ ->
                  Log.info (fun k ->
                      k ~tags:timestamp "Ready on %a." pp_url addr));
              if n_workers = 0 then
                ignore @@ Sys.signal Sys.sigpipe Sys.Signal_ignore;
              accept_connections ~timeout ~n_workers callback socket))
  | _ ->
      set_binary_mode_in stdin true;
      let client_socket = Fd.file_descr_of_fd @@ input_value stdin in
      let addr = input_value stdin in
      let conn = Connection.of_socket client_socket in
      ignore (treat_connection callback addr conn);
      exit 0

module Pool = Pool
