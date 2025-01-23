module Y = Yojson.Safe
module U = Yojson.Safe.Util
module Json_rpc = Geneweb_rpc.Json_rpc
open Lwt.Infix

let default_interface = "localhost"
let default_port = 8080

let rpc_handler content =
  try
    let j = Y.from_string content in
    match Json_rpc.Response.of_json j with
    | Ok r -> Fmt.pr "%a@." Json_rpc.Response.pp r
    | Error err -> Fmt.pr "%s@." err
  with U.Type_error (s, _) ->
    Fmt.pr "The server sent an invalid JSON message:@ %s@ Parser error:@ %s@."
      content s

let rec loop wsd i () =
  let write wsd content =
    let len = String.length content in
    Httpun_ws.Wsd.schedule wsd ~kind:`Text ~off:0 ~len
    @@ Bigstringaf.of_string content ~off:0 ~len
  in
  let request = Json_rpc.Request.make (`Int i) "ping" in
  let content = Json_rpc.Request.to_json request |> Y.(to_string ~std:true) in
  write wsd content;
  Lwt_unix.sleep 1.0 >>= loop wsd (i + 1)

let ws_handler _u wsd =
  let on_read ~kind:_ content ~off:_ ~len:_ =
    rpc_handler @@ Bigstringaf.to_string content
  in
  let frame ~opcode ~is_fin:_ ~len:_ payload =
    Httpun_ws.Payload.schedule_read payload ~on_eof:ignore
      ~on_read:(on_read ~kind:opcode)
  in
  let eof ?error () =
    match error with Some _ -> assert false | None -> assert false
  in
  Lwt.async (loop wsd 0);
  Httpun_ws.Websocket_connection.{ frame; eof }

let error_handler = function
  | `Malformed_response s -> Fmt.pr "Malformed_response:@ %s@." s
  | `Invalid_response_body_length _ -> Fmt.pr "Invalid response body length@."
  | `Exn exn -> Fmt.pr "Uncaught exception:@ %s@." (Printexc.to_string exn)

let () =
  let interface = ref default_interface in
  let port = ref default_port in
  let usage = "rpc_test.exe -i INTERFACE -p PORT" in

  Arg.parse
    [
      ("-i", Set_string interface, "Interface (localhost by default)");
      ("-p", Set_int port, "Port (8080 by default)");
    ]
    (fun (_ : string) -> ())
    usage;

  let p =
    Lwt_unix.getaddrinfo !interface (string_of_int !port)
      [ Unix.(AI_FAMILY PF_INET) ]
    >>= fun addresses ->
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr >>= fun () ->
    let nonce = "0123456789ABCDEF" in
    Httpun_lwt_unix.Client.create_connection socket >>= fun conn ->
    let headers =
      Httpun.Headers.(
        of_list
          [ ("host", String.concat ":" [ !interface; string_of_int !port ]) ])
    in
    let upgrade_request =
      Httpun_ws.Handshake.create_request ~nonce ~headers "/pingpong"
    in
    let p, u = Lwt.wait () in
    let request_body =
      Httpun_lwt_unix.Client.request conn ~error_handler
        ~response_handler:(fun _response _response_body ->
          let ws_conn = Httpun_ws.Client_connection.create (ws_handler u) in
          Httpun_lwt_unix.Client.upgrade conn
            (Gluten.make (module Httpun_ws.Client_connection) ws_conn))
        upgrade_request
    in
    Httpun.Body.Writer.close request_body;
    p
  in
  Lwt_main.run p
