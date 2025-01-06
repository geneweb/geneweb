module type S = sig
  type elt
  type t

  val of_seq : elt Seq.t -> t
  val to_seq : t -> elt Seq.t
  val mem : elt -> t -> bool
  val cardinal : t -> int

  module Iterator : sig
    type t

    exception End

    val curr : t -> elt
    val next : t -> unit
    val seek : t -> elt -> unit
    val union : t list -> t
    val join : t list -> t
    val equal : t -> t -> bool
    val to_seq : t -> elt Seq.t
  end

  val iterator : t -> Iterator.t
end

module type OrderedType = sig
  type t

  val dummy : t
  val compare : t -> t -> int
end

module Make (O : OrderedType) = struct
  type elt = O.t
  type t = O.t array

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
    let c = cardinal t in
    let rec loop i = if i < c && t.(i) < e then loop (2 * i) else min i c in
    let hi = loop (max lo 1) in
    let lo = max lo (hi / 2) in
    binary_search e t lo hi

  type iterator = {
    curr : unit -> elt;
    next : unit -> unit;
    seek : elt -> unit;
  }

  module Iterator = struct
    type t = iterator

    exception End

    let[@inline always] curr it = it.curr ()
    let[@inline always] next it = it.next ()
    let[@inline always] seek it e = it.seek e

    let equal it1 it2 =
      let rec loop () =
        let v1 = try Some (it1.curr ()) with End -> None in
        let v2 = try Some (it2.curr ()) with End -> None in
        match (v1, v2) with
        | Some v1, Some v2 ->
            if O.compare v1 v2 <> 0 then false
            else (
              it1.next ();
              it2.next ();
              loop ())
        | None, Some _ | Some _, None -> false
        | None, None -> true
      in
      loop ()

    let union l =
      let arr = Array.of_list l in
      if Array.length arr = 0 then invalid_arg "union";
      let module H = Heap.Make (struct
        type t = int * elt

        let dummy = (0, O.dummy)
        let compare (_, v1) (_, v2) = O.compare v1 v2
      end) in
      let len = Array.length arr in
      let hp = H.create len in
      for i = 0 to len - 1 do
        match curr arr.(i) with exception End -> () | v -> H.insert hp (i, v)
      done;
      let seek w =
        let rec loop () =
          match H.min hp with
          | exception H.Empty -> ()
          | _, v when O.compare w v <= 0 -> ()
          | i, _ ->
              let (_ : int * elt) = H.delete_min hp in
              seek arr.(i) w;
              let () =
                match curr arr.(i) with
                | exception End -> ()
                | v -> H.insert hp (i, v)
              in
              loop ()
        in
        loop ()
      in
      let next () =
        match H.delete_min hp with
        | exception H.Empty -> ()
        | i, _ -> (
            next arr.(i);
            match curr arr.(i) with
            | exception End -> ()
            | v -> H.insert hp (i, v))
      in
      let curr () =
        match H.min hp with exception H.Empty -> raise End | _, v -> v
      in
      { curr; next; seek }

    (* This implementation follows the leapfrog join describes in the paper
       https://openproceedings.org/ICDT/2014/paper_13.pdf *)
    let join l =
      let arr = Array.of_list l in
      if Array.length arr = 0 then
        (* Intersection of 0th elements cannot be represented by a finite
           set of any type. *)
        invalid_arg "join";
      let ended = ref false in
      (* Index of an iterator [it] in [arr] such that its current value
         is the smallest among current values of iterators of [arr]. *)
      let pos = ref 0 in
      (* Helper function that advances the iterators of [arr] until
         the next meeting point. *)
      let search () =
        let k = Array.length arr in
        let rec loop x =
          let y = curr arr.(!pos) in
          if O.compare x y <> 0 then (
            seek arr.(!pos) x;
            match curr arr.(!pos) with
            | exception End -> ended := true
            | x ->
                pos := (!pos + 1) mod k;
                loop x)
        in
        loop (curr arr.((k + !pos - 1) mod k))
      in
      let () =
        try
          Array.sort (fun it1 it2 -> O.compare (curr it1) (curr it2)) arr;
          search ()
        with End -> ended := true
      in
      let seek w =
        seek arr.(!pos) w;
        match curr arr.(!pos) with
        | exception End -> ended := true
        | _ ->
            let k = Array.length arr in
            pos := (!pos + 1) mod k;
            search ()
      in
      let next () =
        next arr.(!pos);
        match curr arr.(!pos) with
        | exception End -> ended := true
        | _ ->
            let k = Array.length arr in
            pos := (!pos + 1) mod k;
            search ()
      in
      let curr () = if !ended then raise End else curr arr.(0) in
      { curr; next; seek }

    let to_seq it =
      let rec loop () =
        match curr it with
        | exception End -> Seq.Nil
        | v ->
            next it;
            Seq.Cons (v, loop)
      in
      loop
  end

  let iterator t =
    let idx = ref 0 in
    let curr () = if !idx < cardinal t then t.(!idx) else raise Iterator.End in
    let next () = if !idx < cardinal t then incr idx in
    let seek e =
      if !idx < cardinal t then
        let (`Gap i | `Found i) = exponential_search e t !idx in
        idx := i
    in
    { curr; next; seek }
end
