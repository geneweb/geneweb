module type S = sig
  type elt
  type t
  type cmp

  module Comparator : Iterator.Comparator with type t = elt and type wit = cmp

  val of_seq : elt Seq.t -> t
  val to_seq : t -> elt Seq.t
  val mem : elt -> t -> bool
  val cardinal : t -> int
  val iterator : t -> (elt, cmp) Iterator.t
end

module type OrderedType = sig
  type t

  val dummy : t
  val compare : t -> t -> int
  val pp : t Fmt.t
end

module DisablePolymorphicComparison = struct
  [@@@ocaml.warning "-32-33"]

  let ( <> ) (a : int) (b : int) = a <> b
  let ( = ) (a : int) (b : int) = a = b
  let ( < ) (a : int) (b : int) = a < b
  let ( > ) (a : int) (b : int) = a > b
  let ( <= ) (a : int) (b : int) = a <= b
  let ( >= ) (a : int) (b : int) = a >= b
  let compare = Int.compare
end

module Make (O : OrderedType) = struct
  (* WARNING: Confusing [O.compare] with polymorphic comparison can lead to
     subtle and hard-to-debug issues. To prevent this, we completely shadow the
     polymorphic comparison from the Stdlib in this module. *)
  open DisablePolymorphicComparison

  type elt = O.t
  type t = O.t array
  type cmp

  module Comparator = struct
    type t = O.t
    type wit = cmp

    let dummy = O.dummy
    let compare = O.compare
  end

  let of_seq s =
    let l = List.of_seq s in
    let l = List.sort O.compare l in
    Array.of_list l

  let to_seq = Array.to_seq
  let cardinal = Array.length

  (* Perform a binary search of the value [e] in the slice [lo...hi[ of
     the sorted array [t]. Returns the index of [e] if found, or the
     index where it could be inserted to maintain ascending order. *)
  let binary_search e t lo hi =
    let rec loop l h =
      if l >= h then `Gap l
      else
        let mid = l + ((h - l) / 2) in
        let c = O.compare t.(mid) e in
        if c = 0 then `Found mid
        else if c < 0 then loop (mid + 1) h
        else loop l mid
    in
    loop lo hi

  let mem e t =
    match binary_search e t 0 (cardinal t) with
    | `Gap _ -> false
    | `Found _ -> true

  (* Perform a exponential search of the value [e] in the sorted array [t],
     starting from index [lo]. Returns the index of [e] if found, or the index
     where it could be inserted to maintain ascending order. *)
  let exponential_search e t lo =
    let c = O.compare t.(lo) e in
    if c = 0 then `Found lo
    else if c > 0 then `Gap lo
    else
      let len = cardinal t in
      let rec loop i =
        let j = lo + i in
        if j >= len then
          let u = min (lo + (i / 2)) len in
          binary_search e t u len
        else
          let c = O.compare t.(j) e in
          if c < 0 then loop (2 * i)
          else if c = 0 then `Found j
          else binary_search e t (lo + (i / 2)) j
      in
      loop 1

  let iterator t =
    let idx = ref 0 in
    let curr () = if !idx < cardinal t then t.(!idx) else raise Iterator.End in
    let next () = if !idx < cardinal t then incr idx in
    let seek e =
      if !idx < cardinal t then (
        let (`Gap i | `Found i) = exponential_search e t !idx in
        assert (i >= cardinal t || O.compare t.(i) e >= 0);
        idx := i)
    in
    Iterator.make (module Comparator) ~curr ~next ~seek
end
