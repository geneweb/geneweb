module Y = Yojson.Safe
module U = Yojson.Safe.Util
module MS = Map.Make (String)
module Compat = Geneweb_compat
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

module Index = Geneweb_search.Index.Default
module Analyze = Geneweb_search.Analyze

module Search = struct
  let lookup ~fuel idx =
    decl "lookup"
      Desc.Syntax.(string @-> string @-> int @-> ret (list string))
      (fun name s size ->
        match List.assoc name idx with
        | exception Not_found ->
            (* TODO: Methods could fail and emit errors. We can implement
               this by changing the return type of methods to Lwt_result.t *)
            Lwt_result.return []
        | i ->
            let words =
              List.map
                (fun Analyze.{ content; _ } -> content)
                (Analyze.preprocess s)
            in
            (* We look up for entries in the following order of priority:
                1. Entries that exactly match all the [words].
                2. Entries that match prefixes of all the [words].
                3. Entries that match prefixes of all the [words] up to
                   Levenshtein distance of 1. *)
            let size = min size fuel in
            let r =
              (* TODO: There is no guarantee that bounding the size of forced
                 elements in the sequence will limit the running time. We
                 should find a solution to limit the running time itself. *)
              List.of_seq @@ Compat.Seq.take size @@ Compat.Seq.concat
              (* BUG: We can introduce duplicate in the output here. *)
              @@ List.to_seq
                   [
                     Index.search words i;
                     Index.search_prefix words i;
                     Index.fuzzy_search ~max_dist:1 words i;
                   ]
            in
            Lwt_result.return r)

  let make ~fuel idx = empty |> add (lookup ~fuel idx)
end
