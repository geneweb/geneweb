module WS = Js_of_ocaml.WebSockets
module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js
module Console = Js_of_ocaml.Console
module Y = Yojson.Safe
module U = Yojson.Safe.Util
module Json_rpc = Geneweb_rpc.Json_rpc
module Service = Geneweb_rpc.Service
module Encoding = Geneweb_rpc.Encoding

type callback = Json_rpc.Response.t -> unit

module H = Hashtbl.Make (Json_rpc.Id)

let onmessage tbl (ev : WS.webSocket WS.messageEvent Js.t) =
  try
    let j = Y.from_string (Js.to_string ev##.data) in
    match Json_rpc.Response.of_json j with
    | Ok r -> (
        match r.id with
        | Some id ->
            let callback = H.find tbl id in
            H.remove tbl id;
            callback r;
            Js._true
        | None ->
            let err = Result.get_error r.result in
            let msg = Fmt.str "%a" Json_rpc.Response.Error.pp err in
            Console.console##log (Js.string msg);
            Js._false)
    | Error err ->
        Console.console##error (Js.string err);
        Js._false
  with U.Type_error _ ->
    Console.console##error (Js.string "not a valid JSON message");
    Js._false

let websocket_promise ~uri ~onmessage =
  Promise.make (fun ~resolve ~reject ->
      let socket = new%js WS.webSocket uri in
      let onopen _ev =
        resolve socket;
        Js._true
      in
      let onclose _ev =
        reject Ojs.null;
        Js._true
      in
      socket##.onopen := Dom.handler onopen;
      socket##.onclose := Dom.handler onclose;
      socket##.onmessage := Dom.handler onmessage)

let client (uri : Js.js_string Js.t) =
  let tbl = H.create 17 in
  let onmessage = onmessage tbl in
  object%js (self)
    val waiting : callback H.t = tbl
    val mutable cnt : int = 0
    val socket : WS.webSocket Js.t Promise.t = websocket_promise ~uri ~onmessage

    method call meth args : Js.js_string Js.t Promise.t =
      Promise.then_
        ~fulfilled:(fun (socket : WS.webSocket Js.t) ->
          Promise.make (fun ~resolve ~reject ->
              let callback r =
                match r.Json_rpc.Response.result with
                | Ok payload -> resolve (Y.to_string payload |> Js.string)
                | Error err ->
                    let s =
                      Json_rpc.Response.Error.to_json err
                      |> Y.to_string |> Ojs.string_to_js
                    in
                    reject s
              in
              let id = `Int self##.cnt in
              H.replace self##.waiting id callback;
              let args = Js.to_array args in
              let args =
                Array.map (fun s -> Y.from_string @@ Js.to_string s) args
                |> Array.to_list
              in
              let meth = Js.to_string meth in
              let params = `List args in
              let request = Json_rpc.Request.make ~params id meth in
              let msg =
                Json_rpc.Request.to_json request |> Y.to_string |> Js.string
              in
              self##.cnt := self##.cnt + 1;
              socket##send msg))
        self##.socket
  end

let _ = Js.export "Rpc" client
