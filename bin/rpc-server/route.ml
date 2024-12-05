module Json_rpc = Geneweb_rpc.Json_rpc
module Response = Geneweb_rpc.Json_rpc.Response
module Request = Geneweb_rpc.Json_rpc.Request
module Encoding = Geneweb_rpc.Encoding
module Service = Geneweb_rpc.Service
module MS = Map.Make (String)

type t = string * Service.t

let path n srv = (n, srv)

let not_implemented id =
  Response.(error ~id @@ Error.server_error ~code:10 "not implemented")

let route l =
  let map = MS.of_seq (List.to_seq l) in
  fun _sockaddr target request ->
    match MS.find target map with
    | exception Not_found ->
        Lwt.return @@ Response.(error @@ Error.invalid_request ())
    | srv -> (
        let Request.{ id; meth; params } = request in
        let call params =
          match Service.find meth srv with
          | None ->
              Lwt.return @@ Response.(error ~id @@ Error.method_not_found ())
          | Some (Service.Binding (desc, f)) -> (
              match%lwt Service.Desc.eval desc f params with
              | Ok r -> Lwt.return @@ Response.ok ~id r
              | Error e ->
                  (* TODO: choose an error code *)
                  Lwt.return
                  @@ Response.(
                       error ~id
                       @@ Error.server_error ~code:10 "%a" Service.pp_error e))
        in
        match params with
        | Some (`List l) -> call l
        | Some (`Assoc _) -> Lwt.return @@ not_implemented id
        | None -> call [])
