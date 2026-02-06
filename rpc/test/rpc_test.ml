module Y = Yojson.Safe
module U = Yojson.Safe.Util
module Json_rpc = Geneweb_rpc.Json_rpc
open Lwt.Infix

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

let rpc_handler content =
  try
    let j = Y.from_string content in
    match Json_rpc.Response.of_json j with
    | Ok r -> Fmt.pr "%a@." Json_rpc.Response.pp r
    | Error err -> Fmt.pr "%s@." err
  with U.Type_error (s, _) ->
    Fmt.pr "The server sent an invalid JSON message:@ %s@ Parser error:@ %s@."
      content s

(* FIXME: hackish synchronisation for logs. This may fail
   if the server answer to quickly. *)
let condition : unit Lwt_condition.t = Lwt_condition.create ()

let wait_answer ~timeout () =
  Lwt.pick [ Lwt_unix.sleep timeout; Lwt_condition.wait condition ]

let rec loop wsd i () =
  let open Lwt.Syntax in
  let write wsd content =
    let len = String.length content in
    Httpun_ws.Wsd.schedule wsd ~kind:`Text ~off:0 ~len
    @@ Bigstringaf.of_string content ~off:0 ~len
  in
  let* () = if i > 0 then wait_answer ~timeout:5.0 () else Lwt.return_unit in
  let* () = Lwt_io.write Lwt_io.stdout "rpc> " in
  let* s = Lwt_io.read_line Lwt_io.stdin in
  match parse_input s with
  | `String meth :: args ->
      let params = `List args in
      let request = Json_rpc.Request.make ~params (`Int i) meth in
      let content =
        Json_rpc.Request.to_json request |> Y.(to_string ~std:true)
      in
      write wsd content;
      loop wsd (i + 1) ()
  | (exception Syntax_error) | _ ->
      Fmt.pr "Syntax error@.";
      loop wsd i ()

let ws_handler _u wsd =
  let on_read ~kind:_ content ~off:_ ~len:_ =
    Lwt_condition.signal condition ();
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

let default_interface = "localhost"
let default_port = 8080
let default_service = "search"

let () =
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
      Httpun_ws.Handshake.create_request ~nonce ~headers
        (Fmt.str "/%s" !service)
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
