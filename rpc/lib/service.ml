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

module Index = Geneweb_search.Index.Default
module Analyze = Geneweb_search.Analyze

module Search = struct
  let info indexes =
    decl "info"
      Desc.Syntax.(ret (list (tup2 string int)))
      (Lwt_result.return
      @@ List.map (fun (name, idx) -> (name, Index.cardinal idx)) indexes)

  (* [balance size stages] keeps at most [size] elements of [stages],
     preserving their order. Each stage gets an equal share of the slots;
     the slots a short stage does not use are redistributed to the
     others. *)
  let balance size stages =
    let n = Array.length stages in
    let quota = Array.make n 0 in
    let by_length = Array.init n Fun.id in
    Array.sort
      (fun i j -> compare (Array.length stages.(i)) (Array.length stages.(j)))
      by_length;
    let _ =
      Array.fold_left
        (fun (remaining, left) i ->
          let q = min (Array.length stages.(i)) (remaining / left) in
          quota.(i) <- q;
          (remaining - q, left - 1))
        (size, n) by_length
    in
    Array.fold_right
      (fun (s, q) acc ->
        let rec add k acc = if k < 0 then acc else add (k - 1) (s.(k) :: acc) in
        add (q - 1) acc)
      (Array.map2 (fun s q -> (s, q)) stages quota)
      []

  let lookup ~fuel indexes =
    decl "lookup"
      Desc.Syntax.(string @-> string @-> int @-> ret (list string))
      (fun name s size ->
        match List.assoc name indexes with
        | exception Not_found ->
            (* TODO: Methods could fail and emit errors. We can implement
               this by changing the return type of methods to Lwt_result.t *)
            Lwt_result.return []
        | idx ->
            let words =
              List.map
                (fun Analyze.{ content; _ } -> content)
                (Analyze.preprocess s)
            in
            (* We look up for entries in three disjoint stages, listed by
               decreasing priority:
                1. Entries that exactly match all the [words].
                2. Entries that match prefixes of all the [words], but not
                   exactly.
                3. Entries that match prefixes of all the [words] up to
                   Levenshtein distance of 1, but not plain prefixes.
               The [size] slots are shared between the stages, so that a
               frequent word (e.g. "laure") cannot fill the whole answer
               with exact matches and hide its extensions ("laurent",
               "laurence", ...). *)
            let size = min size fuel in
            let toks e =
              List.map
                (fun Analyze.{ content; _ } -> content)
                (Analyze.preprocess e)
            in
            let exact e =
              let t = toks e in
              List.for_all (fun w -> List.mem w t) words
            in
            let prefix e =
              let t = toks e in
              List.for_all
                (fun w -> List.exists (String.starts_with ~prefix:w) t)
                words
            in
            let seen = Hashtbl.create (3 * size) in
            let stage keep s =
              (* TODO: There is no guarantee that bounding the size of forced
                 elements in the sequence will limit the running time. We
                 should find a solution to limit the running time itself. *)
              Seq.filter
                (fun e ->
                  keep e
                  && (not (Hashtbl.mem seen e))
                  &&
                  (Hashtbl.add seen e ();
                   true))
                s
              |> Seq.take size |> Array.of_seq
            in
            let stages =
              [|
                stage (fun _ -> true) (Index.search words idx);
                stage
                  (fun e -> not (exact e))
                  (Index.search_prefix_spread words idx);
                stage
                  (fun e -> not (prefix e))
                  (Index.fuzzy_search ~max_dist:1 words idx);
              |]
            in
            Lwt_result.return (balance size stages))

  let make ~fuel idx = empty |> add (info idx) |> add (lookup ~fuel idx)
end
