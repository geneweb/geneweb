module Config = Geneweb.Config
module Compat = Geneweb_compat
module H = Digestif.SHA256

module HT = Hashtbl.Make (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
end)

type hook = Geneweb.Config.config -> string option -> unit
type handler = Geneweb.Config.config -> string option -> bool

let hooks : (string * hook) Queue.t = Queue.create ()
let handlers : (string * handler) Queue.t HT.t = HT.create 17

let[@inline] add_handler ~name ~meth ht h =
  let q =
    match HT.find ht meth with
    | exception Not_found ->
        let q = Queue.create () in
        Queue.add (name, h) q;
        q
    | q ->
        Queue.add (name, h) q;
        q
  in
  HT.replace ht meth q

let register ~name h1 h2 =
  List.iter (fun h -> Queue.add (name, h) hooks) h1;
  List.iter (fun (meth, h) -> add_handler handlers ~name ~meth h) h2

let call_hooks f = Queue.iter (fun (name, h) -> f ~name h) hooks

let rec until_success f seq =
  match seq () with
  | Seq.Nil -> false
  | Seq.Cons (hd, tl) -> f hd || until_success f tl

let try_handlers ~meth f =
  match HT.find handlers meth with
  | exception Not_found -> false
  | q ->
      let seq = Queue.to_seq q in
      until_success (fun (name, h) -> f ~name h) seq

let all_registered () =
  let t : unit HT.t = HT.create 17 in
  Queue.iter (fun (name, _) -> HT.add t name ()) hooks;
  HT.iter (fun _ q -> Queue.iter (fun (name, _) -> HT.add t name ()) q) handlers;
  List.of_seq @@ Seq.map fst @@ HT.to_seq t
