type ('k, 'v, 'c) t = {
  curr : unit -> 'k * 'v;
  next : unit -> unit;
  seek : 'k -> unit;
}

exception End

module type Comparator = sig
  type t
  type wit

  val dummy : t
  val compare : t -> t -> int
end

type ('k, 'c) comparator =
  (module Comparator with type t = 'k and type wit = 'c)

let[@inline] make _cmp ~curr ~next ~seek = { curr; next; seek }
let[@inline always] curr it = it.curr ()
let[@inline always] next it = it.next ()
let[@inline always] seek it e = it.seek e

let equal (type k v c) (module C : Comparator with type t = k and type wit = c)
    (it1 : (k, v, c) t) (it2 : (k, v, c) t) =
  let rec loop () =
    let o1 = try Some (it1.curr ()) with End -> None in
    let o2 = try Some (it2.curr ()) with End -> None in
    match (o1, o2) with
    | Some (k1, v1), Some (k2, v2) ->
        (* FIXME: remove the polymorphic comparison. *)
        if C.compare k1 k2 <> 0 && v1 = v2 then false
        else (
          it1.next ();
          it2.next ();
          loop ())
    | None, Some _ | Some _, None -> false
    | None, None -> true
  in
  loop ()

let union (type k v c) (module C : Comparator with type t = k and type wit = c)
    (l : (k, v, c) t list) =
  let arr = Array.of_list l in
  let module H = Heap.Make (struct
    type t = int * k * v

    (* FIXME: Remove the `Obj.magic`. *)
    let dummy = (0, C.dummy, Obj.magic 0)

    (* FXME: We silently ignore collision. *)
    let compare (_, k1, _) (_, k2, _) = C.compare k1 k2
  end) in
  let len = Array.length arr in
  let hp = H.create len in
  for i = 0 to len - 1 do
    match curr arr.(i) with exception End -> () | k, v -> H.insert hp (i, k, v)
  done;
  let seek w =
    let rec loop () =
      match H.min hp with
      | exception H.Empty -> ()
      | _, k, _ when C.compare w k <= 0 -> ()
      | i, _, _ ->
          let (_ : int * k * v) = H.delete_min hp in
          seek arr.(i) w;
          let () =
            match curr arr.(i) with
            | exception End -> ()
            | k, v -> H.insert hp (i, k, v)
          in
          loop ()
    in
    loop ()
  in
  let next () =
    match H.delete_min hp with
    | exception H.Empty -> ()
    | i, _, _ -> (
        next arr.(i);
        match curr arr.(i) with
        | exception End -> ()
        | k, v -> H.insert hp (i, k, v))
  in
  let curr () =
    match H.min hp with exception H.Empty -> raise End | _, k, v -> (k, v)
  in
  { curr; next; seek }

(* This implementation follows the leapfrog join describes in the paper
   https://openproceedings.org/ICDT/2014/paper_13.pdf *)
let join (type k v c) (module C : Comparator with type t = k and type wit = c)
    (l : (k, v, c) t list) =
  let arr = Array.of_list l in
  if Array.length arr = 0 then
    (* Intersection of 0th elements cannot be represented by a finite
       set for any type. *)
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
      let y, _ = curr arr.(!pos) in
      if C.compare y x < 0 then (
        seek arr.(!pos) x;
        match curr arr.(!pos) with
        | exception End -> ended := true
        | x', _ ->
            (* As y < x and the iterator [arr.(!pos)] has not
               reached its end, the previous seek call must
               advance this iterator. *)
            (* assert (C.compare y x' < 0); *)
            assert (C.compare y x' <= 0);
            pos := (!pos + 1) mod k;
            loop x')
    in
    let x, _ = curr arr.((k + !pos - 1) mod k) in
    loop x
  in
  let () =
    try
      Array.sort
        (fun it1 it2 -> C.compare (fst @@ curr it1) (fst @@ curr it2))
        arr;
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
    | v -> (
        next it;
        (* XXX: hot fix *)
        match curr it with
        | exception End -> Seq.Cons (v, loop)
        | w -> if v = w then loop () else Seq.Cons (v, loop))
  in
  loop
