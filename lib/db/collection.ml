type 'a t = { length : int; get : int -> 'a option }

let make ~len get = { length = len; get }
let empty () = { length = -1; get = (fun _ -> None) }

let map (fn : 'a -> 'b) c =
  {
    length = c.length;
    get = (fun i -> match c.get i with Some x -> Some (fn x) | None -> None);
  }

let length { length; _ } = length

let iter fn { get; length } =
  for i = 0 to length - 1 do
    match get i with Some x -> fn x | None -> ()
  done

let iteri fn { get; length } =
  for i = 0 to length - 1 do
    match get i with Some x -> fn i x | None -> ()
  done

let fold ?from ?until fn acc { get; length } =
  let from = match from with Some x -> x | None -> 0 in
  let until = match until with Some x -> x + 1 | None -> length in
  let rec loop acc i =
    if i = until then acc
    else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
  in
  loop acc from

let fold_until continue fn acc { get; length } =
  let rec loop acc i =
    if (not (continue acc)) || i = length then acc
    else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
  in
  loop acc 0

let iterator { get; length } =
  let cursor = ref 0 in
  let rec next () =
    if !cursor < length then (
      match get !cursor with
      | None ->
          incr cursor;
          next ()
      | v ->
          incr cursor;
          v)
    else None
  in
  next

type 'a collection = 'a t

module Marker = struct
  type ('k, 'v) t = { get : 'k -> 'v; set : 'k -> 'v -> unit }

  let make (k : 'k -> int) c (i : 'v) : ('k, 'v) t =
    let a = Array.make (length c) i in
    {
      get = (fun x -> Array.get a (k x));
      set = (fun x v -> Array.set a (k x) v);
    }

  let dummy _ v = { get = (fun _ -> v); set = (fun _ _ -> ()) }
  let get ({ get; _ } : _ t) k = get k
  let set ({ set; _ } : _ t) k = set k
end
