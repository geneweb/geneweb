module Y = Yojson.Safe
module U = Yojson.Safe.Util
module Json_rpc = Geneweb_rpc.Json_rpc
module Payload = Httpun_ws.Payload

module Parser = struct
  exception Syntax_error
  exception Eof

  type input = { content : string; mutable offset : int }

  let make_input content = { content; offset = 0 }

  let next_char ({ content; offset } as input) =
    if offset >= String.length content then raise Eof
    else
      let r = content.[offset] in
      input.offset <- input.offset + 1;
      r

  let char_to_int c = Char.code c - 48

  let rec next_token input =
    let rec tokenize_int x =
      match next_char input with
      | '0' .. '9' as c -> tokenize_int ((10 * x) + char_to_int c)
      | ' ' | (exception Eof) -> `Int x
      | _ -> raise Syntax_error
    in
    let rec tokenize_quoted_string buf =
      match next_char input with
      | '"' -> `String (Buffer.contents buf)
      | _ as c ->
          Buffer.add_char buf c;
          tokenize_quoted_string buf
      | exception Eof -> raise Syntax_error
    in
    let rec tokenize_unquoted_string buf =
      match next_char input with
      | ' ' | (exception Eof) -> `String (Buffer.contents buf)
      | _ as c ->
          Buffer.add_char buf c;
          tokenize_unquoted_string buf
    in
    match next_char input with
    | '0' .. '9' as c -> tokenize_int (char_to_int c)
    | '"' -> tokenize_quoted_string (Buffer.create 17)
    | ' ' -> next_token input
    | _ as c ->
        let buf = Buffer.create 17 in
        Buffer.add_char buf c;
        tokenize_unquoted_string buf

  let parse_input s =
    let input = make_input s in
    let rec loop tokens =
      match next_token input with
      | exception Eof -> List.rev tokens
      | tk -> loop (tk :: tokens)
    in
    loop []
end

let prompt i =
  Fmt.pr "rpc> @?";
  match Parser.parse_input @@ read_line () with
  | `String meth :: args ->
      let params = `List args in
      let request = Json_rpc.Request.make ~params (`Int i) meth in
      Ok (Json_rpc.Request.to_json request |> Y.(to_string ~std:true))
  | (exception Parser.Syntax_error) | _ -> Error "Syntax error"

let receive response =
  match Json_rpc.Response.of_json @@ Y.from_string response with
  | Ok r -> Fmt.pr "%a@." Json_rpc.Response.pp r
  | Error err -> Fmt.epr "%s" err
  | exception U.Type_error (s, _) ->
      Fmt.epr
        "The server sent an invalid JSON message:@ %s@ Parser error:@ %s@."
        response s

let read_full_payload ~total payload k =
  let buf = Bigstringaf.create total in
  let rec on_read ~pos content ~off ~len =
    Bigstringaf.blit content ~src_off:off buf ~dst_off:pos ~len;
    let pos = pos + len in
    if pos < total then schedule_read ~pos else k @@ Bigstringaf.to_string buf
  and schedule_read ~pos =
    Payload.schedule_read payload ~on_eof:ignore ~on_read:(on_read ~pos)
  in
  schedule_read ~pos:0

let ws_handler ~send ~receive wsd =
  let cnt = ref 0 in
  let write () =
    match send !cnt with
    | Ok content ->
        let len = String.length content in
        Httpun_ws.Wsd.schedule wsd ~kind:`Text ~off:0 ~len
        @@ Bigstringaf.of_string content ~off:0 ~len
    | Error e -> Fmt.epr "%s@." e
  in
  let frame ~opcode ~is_fin ~len payload =
    assert (opcode = `Text && is_fin);
    read_full_payload ~total:len payload @@ fun response ->
    receive response;
    incr cnt;
    write ()
  in
  let eof ?error:_ () = assert false in
  write ();
  Httpun_ws.Websocket_connection.{ frame; eof }

let error_handler = function
  | `Malformed_response s -> Fmt.pr "Malformed_response:@ %s@." s
  | `Invalid_response_body_length _ -> Fmt.pr "Invalid response body length@."
  | `Exn exn -> Fmt.pr "Uncaught exception:@ %s@." (Printexc.to_string exn)

let ask_upgrade_request ~interface ~port ~service conn =
  let nonce = "0123456789ABCDEF" in
  let headers =
    Httpun.Headers.(of_list [ ("host", String.concat ":" [ interface; port ]) ])
  in
  let upgrade_request =
    Httpun_ws.Handshake.create_request ~nonce ~headers (Fmt.str "/%s" service)
  in
  let handler = ws_handler ~send:prompt ~receive in
  let request_body =
    Httpun_lwt_unix.Client.request conn ~error_handler
      ~response_handler:(fun _response _response_body ->
        let ws_conn = Httpun_ws.Client_connection.create handler in
        Httpun_lwt_unix.Client.upgrade conn
          (Gluten.make (module Httpun_ws.Client_connection) ws_conn))
      upgrade_request
  in
  Httpun.Body.Writer.close request_body

let establish_http_connection ~interface ~port =
  let open Lwt.Syntax in
  let* addresses =
    Lwt_unix.getaddrinfo interface port [ Unix.(AI_FAMILY PF_INET) ]
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  Httpun_lwt_unix.Client.create_connection socket

let main ~interface ~port ~service =
  let open Lwt.Syntax in
  let* conn = establish_http_connection ~interface ~port in
  ask_upgrade_request ~interface ~port ~service conn;
  fst @@ Lwt.wait ()

let parse_cmd () =
  let default_interface = "localhost" in
  let default_port = 8080 in
  let default_service = "search" in
  let interface = ref default_interface in
  let port = ref default_port in
  let service = ref default_service in
  let usage = "rpc_test.exe -i INTERFACE -p PORT -s SERVICE" in
  Arg.parse
    [
      ( "-i",
        Set_string interface,
        Fmt.str "Interface (%s by default)" default_interface );
      ("-p", Set_int port, Fmt.str "Port (%d by default)" default_port);
      ( "-s",
        Set_string service,
        Fmt.str "Choose the service (%s by default)" default_service );
    ]
    (fun (_ : string) -> ())
    usage;
  (!interface, string_of_int !port, !service)

let () =
  let interface, port, service = parse_cmd () in
  Lwt_main.run @@ main ~interface ~port ~service
