type ('a, 'c) t = { curr : unit -> 'a; next : unit -> unit; seek : 'a -> unit }

exception End

module type Comparator = sig
  type t
  type wit

  val dummy : t
  val compare : t -> t -> int
end

type ('a, 'c) comparator =
  (module Comparator with type t = 'a and type wit = 'c)

let make _cmp ~curr ~next ~seek = { curr; next; seek }
let[@inline always] curr it = it.curr ()
let[@inline always] next it = it.next ()
let[@inline always] seek it e = it.seek e

let equal (type a c) (module C : Comparator with type t = a and type wit = c)
    (it1 : (a, c) t) (it2 : (a, c) t) =
  let rec loop () =
    let v1 = try Some (it1.curr ()) with End -> None in
    let v2 = try Some (it2.curr ()) with End -> None in
    match (v1, v2) with
    | Some v1, Some v2 ->
        if C.compare v1 v2 <> 0 then false
        else (
          it1.next ();
          it2.next ();
          loop ())
    | None, Some _ | Some _, None -> false
    | None, None -> true
  in
  loop ()

let union (type a c) (module C : Comparator with type t = a and type wit = c)
    (l : (a, c) t list) =
  let arr = Array.of_list l in
  let module H = Heap.Make (struct
    type t = int * a

    let dummy = (0, C.dummy)
    let compare (_, v1) (_, v2) = C.compare v1 v2
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
      | _, v when C.compare w v <= 0 -> ()
      | i, _ ->
          let (_ : int * a) = H.delete_min hp in
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
        match curr arr.(i) with exception End -> () | v -> H.insert hp (i, v))
  in
  let curr () =
    match H.min hp with exception H.Empty -> raise End | _, v -> v
  in
  { curr; next; seek }

(* This implementation follows the leapfrog join describes in the paper
   https://openproceedings.org/ICDT/2014/paper_13.pdf *)
let join (type a c) (module C : Comparator with type t = a and type wit = c)
    (l : (a, c) t list) =
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
      if C.compare y x < 0 then (
        seek arr.(!pos) x;
        match curr arr.(!pos) with
        | exception End -> ended := true
        | x' ->
            (* As y < x and the iterator [arr.(!pos)] has not
               reached its end, the previous seek call must
               advance this iterator. *)
            assert (C.compare y x' < 0);
            pos := (!pos + 1) mod k;
            loop x')
    in
    loop (curr arr.((k + !pos - 1) mod k))
  in
  let () =
    try
      Array.sort (fun it1 it2 -> C.compare (curr it1) (curr it2)) arr;
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
