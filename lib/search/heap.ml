module type S = sig
  type elt
  type t

  exception Empty

  val create : int -> t
  val insert : t -> elt -> unit
  val delete_min : t -> elt
  val min : t -> elt
end

module type OrderedType = sig
  type t

  val dummy : t
  val compare : t -> t -> int
end

module Make (O : OrderedType) = struct
  type elt = O.t
  type t = { tree : elt array; mutable sz : int }

  exception Empty

  let[@inline always] get { tree; _ } i = Array.unsafe_get tree i
  let[@inline always] set { tree; _ } i v = Array.unsafe_set tree i v

  let[@inline always] create cap =
    { tree = Array.init cap (fun _ -> O.dummy); sz = 0 }

  let[@inline always] full { tree; sz; _ } = Array.length tree = sz
  let[@inline always] empty { sz; _ } = sz = 0
  let[@inline always] left i = (2 * i) + 1
  let[@inline always] right i = (2 * i) + 2
  let[@inline always] parent i = (i - 1) / 2

  let swap t i p =
    let tmp = get t i in
    set t i (get t p);
    set t p tmp

  let rec percolate_up t i =
    let p = parent i in
    if O.compare (get t i) (get t p) < 0 then (
      swap t i p;
      percolate_up t p)

  let rec percolate_down t i =
    let l = left i and r = right i in
    let min = if l < t.sz && O.compare (get t l) (get t i) < 0 then l else i in
    let min =
      if r < t.sz && O.compare (get t r) (get t min) < 0 then r else min
    in
    if i <> min then (
      swap t i min;
      percolate_down t min)

  let insert t d =
    if full t then invalid_arg "Heap.insert"
    else
      let i = t.sz in
      set t i d;
      t.sz <- i + 1;
      percolate_up t i

  let delete_min t =
    if empty t then raise Empty
    else
      let d = get t 0 in
      swap t 0 (t.sz - 1);
      t.sz <- t.sz - 1;
      percolate_down t 0;
      d

  let min t = if empty t then raise Empty else get t 0
end
