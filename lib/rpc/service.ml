module Y = Yojson.Safe
module U = Yojson.Safe.Util
module MS = Map.Make (String)
open Lwt.Infix

type error = [ Encoding.error | `Bad_arity ]
type 'a res = ('a, error) Lwt_result.t

let pp_error ppf = function
  | #Encoding.error as e -> Encoding.pp_error ppf e
  | `Bad_arity -> Fmt.string ppf "bad arity"

module Desc = struct
  type ('a, 'r) t =
    | R : 'a Encoding.t -> ('a res, 'a) t
    | N : 'a Encoding.t * ('b, 'r) t -> ('a -> 'b, 'r) t

  let rec pp_desc : type a r. (a, r) t Fmt.t =
   fun ppf desc ->
    match desc with
    | R a -> Encoding.pp ppf a
    | N (a, r) -> Fmt.pf ppf "%a -> %a" Encoding.pp a pp_desc r

  let rec eval : type a b. (a, b) t -> a -> Y.t list -> Y.t res =
   fun desc f l ->
    match (desc, l) with
    | R a, [] -> (
        f >>= fun f ->
        match f with
        | Ok f -> Lwt_result.return (Encoding.val_to_json a f)
        | Error e -> Lwt_result.fail (e :> error))
    | N (a, g), x :: xs -> (
        match Encoding.val_of_json a x with
        | Ok y -> eval g (f y) xs
        | Error e -> Lwt_result.fail (e :> error))
    | _ -> Lwt_result.fail `Bad_arity

  let rec arity : type a b. (a, b) t -> int =
   fun desc -> match desc with R _ -> 0 | N (_, g) -> 1 + arity g

  module Syntax = struct
    include Encoding.Syntax

    let ret x = R x
    let ( @-> ) x y = N (x, y)
  end
end

type ('a, 'r) meth = { name : string; desc : ('a, 'r) Desc.t; f : 'a }

let decl name desc f = { name; desc; f }

type binding = Binding : ('a, 'r) Desc.t * 'a -> binding
type t = binding MS.t

let empty = MS.empty
let add meth = MS.add meth.name (Binding (meth.desc, meth.f))
let find = MS.find_opt
let fold = MS.fold

module PingPong = struct
  let ping = decl "ping" Desc.Syntax.(ret string) (Lwt_result.return "pong")

  let echo =
    decl "echo"
      Desc.Syntax.(string @-> ret string)
      (fun s -> Lwt_result.return s)

  let srv = empty |> add ping |> add echo
end
