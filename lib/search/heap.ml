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

  let[@inline] get { tree; _ } i = Array.unsafe_get tree i
  let[@inline] set { tree; _ } i v = Array.unsafe_set tree i v
  let[@inline] create cap = { tree = Array.init cap (fun _ -> O.dummy); sz = 0 }
  let[@inline] full { tree; sz; _ } = Array.length tree = sz
  let[@inline] empty { sz; _ } = sz = 0
  let[@inline] left i = (2 * i) + 1
  let[@inline] right i = (2 * i) + 2
  let[@inline] parent i = (i - 1) / 2

  let percolate_up t i =
    let vi = get t i in
    let rec loop j =
      let p = parent j in
      let vp = get t p in
      if j > 0 && O.compare vi vp < 0 then (
        set t j vp;
        loop p)
      else set t j vi
    in
    loop i

  let percolate_down t i =
    let vi = get t i in
    let rec loop j =
      let l = left j and r = right j in
      (* Find the smallest child to bubble up, or stay at [j] if the heap
         property is satisfied. *)
      let s =
        if l < t.sz && O.compare (get t l) vi < 0 then
          if r < t.sz && O.compare (get t r) (get t l) < 0 then r else l
        else if r < t.sz && O.compare (get t r) vi < 0 then r
        else j
      in
      if s <> j then (
        set t j (get t s);
        loop s)
      else set t j vi
    in
    loop i

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
      set t 0 (get t (t.sz - 1));
      t.sz <- t.sz - 1;
      percolate_down t 0;
      d

  let min t = if empty t then raise Empty else get t 0
end
