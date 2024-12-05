module Body = Httpun.Body
module Headers = Httpun.Headers
module Reqd = Httpun.Reqd
module Response = Httpun.Response
module Status = Httpun.Status
module Server = Httpun_lwt_unix.Server
module Y = Yojson.Safe
module U = Yojson.Safe.Util
module Json_rpc = Geneweb_rpc.Json_rpc
open Lwt.Infix

(* This module is responsible for configuring the server of `httpun-ws` for our
   specific use case. The only challenging part lies in properly handling and
   logging all errors. We need to manage and log the following:

     1. DNS and socket errors encountered while establishing the server.
     2. TLS and socket errors that occur when a client attempts to connect.
     3. Encoding/Decoding errors in the RPC handler.
     4. Uncaught exceptions in the user handler. *)

type handler =
  Unix.sockaddr -> string -> Json_rpc.Request.t -> Json_rpc.Response.t Lwt.t

let response_to_string response =
  Y.to_string @@ Json_rpc.Response.to_json response

let rpc_handler ~task_timeout handler sockaddr target content =
  try
    let j = Y.from_string content in
    match Json_rpc.Request.of_json j with
    | Ok request -> (
        let%lwt () =
          Logs_lwt.debug (fun k ->
              k "Received the Request object from %a:@ %a" Util.pp_sockaddr
                sockaddr Json_rpc.Request.pp request)
        in
        let res =
          Util.with_timeout ~timeout:task_timeout
            (Util.with_timer @@ handler sockaddr target)
            request
        in
        match%lwt res with
        | `Return (response, duration) ->
            let%lwt () =
              Logs_lwt.debug (fun k ->
                  k "Response message to %a (in %a):@ %a" Util.pp_sockaddr
                    sockaddr Util.pp_duration duration Json_rpc.Response.pp
                    response)
            in
            Lwt.return @@ response_to_string response
        | `Timeout ->
            let%lwt () =
              Logs_lwt.debug (fun k ->
                  k "Timout while processing request from %a." Util.pp_sockaddr
                    sockaddr)
            in
            let err =
              Json_rpc.Response.(
                error @@ Error.server_error ~code:(-32099) "timeout")
            in
            Lwt.return @@ response_to_string err)
    | Error e ->
        let%lwt () =
          Logs_lwt.debug (fun k ->
              k "The client sent an invalid Request object:@ %a@ error: %s"
                (Y.pretty_print ~std:true) j e)
        in
        let err = Json_rpc.Response.(error @@ Error.invalid_request ()) in
        Lwt.return @@ response_to_string err
  with U.Type_error (s, _) ->
    let%lwt () =
      Logs_lwt.debug (fun k ->
          k "The client sent an invalid JSON message:@ %s@ Parser error:@ %s"
            content s)
    in
    Lwt.return @@ response_to_string
    @@ Json_rpc.Response.(error @@ Error.parse_error ())

(* These exceptions should never occur in production. If such an exception
   is raised, it indicates that the user handler has not handled it correctly.
   This will not terminate the server, but the WebSocket connection will be
   lost. *)
let log_ws_handler_exn sockaddr exn =
  Logs.debug (fun k ->
      k "Exception raised while processing request for %a:@ %a" Util.pp_sockaddr
        sockaddr Util.pp_exn exn)

let ws_handler manager fd rpc_handler sockaddr target wsd =
  let on_read ~kind content ~off:_ ~len:_ =
    let () = Fd_manager.ping manager fd in
    Lwt.dont_wait
      (fun () ->
        rpc_handler sockaddr target @@ Bigstringaf.to_string content
        >>= fun content ->
        let len = String.length content in
        let content = Bigstringaf.of_string ~off:0 ~len content in
        Lwt.return @@ Httpun_ws.Wsd.schedule wsd ~kind content ~off:0 ~len)
      (log_ws_handler_exn sockaddr)
  in
  let frame ~opcode ~is_fin:_ ~len:_ payload =
    match (opcode : Httpun_ws.Websocket.Opcode.t) with
    | #Httpun_ws.Websocket.Opcode.standard_non_control as opcode ->
        Httpun_ws.Payload.schedule_read payload ~on_eof:ignore
          ~on_read:(on_read ~kind:opcode)
    | `Connection_close -> Httpun_ws.Wsd.close wsd
    | `Ping -> Httpun_ws.Wsd.send_pong wsd
    | `Pong | `Other _ -> ()
  in
  let eof ?error () =
    match error with
    | Some _ -> assert false
    | None ->
        Logs.err (fun k -> k "EOF");
        Httpun_ws.Wsd.close wsd
  in
  { Httpun_ws.Websocket_connection.frame; eof }

module Request = Httpun.Request

(* The HTTP request handler only accepts upgrade request to the WebSocket
   protocol. *)
let http_request_handler ws_handler sockaddr
    (reqd : Httpun.Reqd.t Gluten.Reqd.t) =
  let Httpun.Request.{ target; _ } = Reqd.request reqd.reqd in
  Logs.info (fun k ->
      k "Ask for upgrading connection by %a on target %s" Util.pp_sockaddr
        sockaddr target);
  let handler () =
    let ws_conn =
      Httpun_ws.Server_connection.create_websocket (ws_handler sockaddr target)
    in
    reqd.upgrade (Gluten.make (module Httpun_ws.Server_connection) ws_conn)
  in
  match
    Httpun_ws.Handshake.respond_with_upgrade ~sha1:Util.sha1 reqd.reqd handler
  with
  | Ok () ->
      Logs.info (fun k ->
          k "Successful upgrade by %a" Util.pp_sockaddr sockaddr)
  | Error e ->
      (* We immediately close the connection if the client does not pass
         scrunity. *)
      Logs.info (fun k ->
          k "Handshake failed by %a:@ %s" Util.pp_sockaddr sockaddr e);
      let headers = Httpun.Headers.of_list [ ("Connection", "close") ] in
      let response = Response.create ~headers `Bad_request in
      Reqd.respond_with_string reqd.reqd response e

(* In case of HTTP error, we simply return the error to the client. *)
let http_error_handler _sockaddr ?request:_ error handle =
  let message =
    match error with
    | `Exn exn -> Printexc.to_string exn
    | (#Status.client_error | #Status.server_error) as error ->
        Status.to_string error
  in
  let body = handle Headers.empty in
  Body.Writer.write_string body message;
  Body.Writer.close body

(* If an exception reaches this handler, it should indicate an error while
   establishing the server socket. This is considered an unrecoverable error,
   and the program must terminate. Any other exceptions should be treated as
   bugs, as they should have been handled earlier. *)
let log_server_exn exn =
  Logs.err (fun k -> k "Uncaught exception in the server:@ %a" Util.pp_exn exn);
  raise exn

let resolve_addr interface port =
  Lwt_unix.getaddrinfo interface (Int.to_string port)
    [ Unix.(AI_FAMILY PF_INET) ]

let start ~interface ~port ?max_connection ?idle_timeout ?task_timeout
    ?(tls = false) ?certfile ?keyfile user_handler =
  let create_connection_handler =
    match (tls, certfile, keyfile) with
    | true, Some certfile, Some keyfile ->
        fun request_handler ->
          Server.TLS.create_connection_handler_with_default ~certfile ~keyfile
            ?config:None ~request_handler ~error_handler:http_error_handler
    | false, _, _ ->
        fun request_handler ->
          Server.create_connection_handler ?config:None ~request_handler
            ~error_handler:http_error_handler
    | _ -> Fmt.invalid_arg "start"
  in
  let task_timeout = match task_timeout with Some f -> f | None -> 0. in
  let connection_handler fd_manager sockaddr fd =
    if%lwt Fd_manager.add fd_manager fd then
      let request_handler =
        http_request_handler @@ ws_handler fd_manager fd
        @@ rpc_handler ~task_timeout @@ user_handler
      in
      try%lwt
        Util.protect
          ~finally:(fun () ->
            let%lwt () = Fd_manager.close fd_manager fd in
            Logs_lwt.info (fun k ->
                k "Close connection of %a" Util.pp_sockaddr sockaddr))
          (fun () -> create_connection_handler request_handler sockaddr fd)
      with exn ->
        (* Exceptions can be raised in the client connection handler itself if
           the connection or the TLS negotiation fail. Any other exceptions
           should be treated as bugs, as they should have been handled
           earlier. *)
        Logs_lwt.info (fun k ->
            k "Uncaught exception raised in connection handler of %a:@ %a"
              Util.pp_sockaddr sockaddr Util.pp_exn exn)
    else
      Logs_lwt.info (fun k ->
          k "Refused the connection from %a" Util.pp_sockaddr sockaddr)
  in

  let fd_manager = Fd_manager.make ?max_connection ?idle_timeout () in
  Fd_manager.loop fd_manager;
  Lwt.dont_wait
    (fun () ->
      match%lwt resolve_addr interface port with
      | Unix.{ ai_addr = sockaddr; _ } :: _ ->
          let%lwt (_ : Lwt_io.server) =
            Lwt_io.establish_server_with_client_socket ~no_close:true sockaddr
              (connection_handler fd_manager)
          in
          Logs_lwt.info (fun k ->
              k "Server listening on %a..." Util.pp_sockaddr sockaddr)
      | _ -> Logs_lwt.err (fun k -> k "Cannot resolve %s:%d..." interface port))
    log_server_exn
