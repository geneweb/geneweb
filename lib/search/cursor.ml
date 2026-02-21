type ('k, 'v, 'c) t = {
  curr : unit -> 'k * 'v;
  next : unit -> unit;
  seek : 'k -> unit;
}

exception End

let[@inline] make _cmp ~curr ~next ~seek = { curr; next; seek }

let equal (type k v c)
    (module C : Comparator.S with type t = k and type wit = c)
    (c1 : (k, v, c) t) (c2 : (k, v, c) t) =
  let rec loop () =
    let o1 = try Some (c1.curr ()) with End -> None in
    let o2 = try Some (c2.curr ()) with End -> None in
    match (o1, o2) with
    | Some (k1, v1), Some (k2, v2) ->
        (* FIXME: remove the polymorphic comparison. *)
        if C.compare k1 k2 <> 0 || v1 != v2 then false
        else (
          c1.next ();
          c2.next ();
          loop ())
    | None, Some _ | Some _, None -> false
    | None, None -> true
  in
  loop ()

let union (type k v c)
    (module C : Comparator.S with type t = k and type wit = c)
    (l : (k, v, c) t list) =
  let arr = Array.of_list l in
  let module H = Heap.Make (struct
    type t = k * int

    let dummy = (C.dummy, -1)
    let compare (k1, _) (k2, _) = C.compare k1 k2
  end) in
  let len = Array.length arr in
  let hp = H.create len in
  for i = 0 to len - 1 do
    match arr.(i).curr () with exception End -> () | k, _ -> H.insert hp (k, i)
  done;
  let seek w =
    let rec loop () =
      match H.min hp with
      | exception H.Empty -> ()
      | k, _ when C.compare w k <= 0 -> ()
      | _, i ->
          let (_ : k * int) = H.delete_min hp in
          arr.(i).seek w;
          let () =
            match arr.(i).curr () with
            | exception End -> ()
            | k, _ -> H.insert hp (k, i)
          in
          loop ()
    in
    loop ()
  in
  let rec skip k =
    match H.min hp with
    | k', _ when C.compare k k' = 0 ->
        let _, i = H.delete_min hp in
        arr.(i).next ();
        let () =
          match arr.(i).curr () with
          | exception End -> ()
          | k, _ -> H.insert hp (k, i)
        in
        skip k
    | (exception H.Empty) | _ -> ()
  in
  let next () =
    match H.delete_min hp with
    | exception H.Empty -> ()
    | k', i -> (
        skip k';
        arr.(i).next ();
        match arr.(i).curr () with
        | exception End -> ()
        | k, _ ->
            H.insert hp (k, i))
  in
  let curr () =
    match H.min hp with
    | exception H.Empty -> raise End
    | _, i -> arr.(i).curr ()
  in
  { curr; next; seek }

(* This implementation follows the leapfrog join describes in the paper
   https://openproceedings.org/ICDT/2014/paper_13.pdf *)
let join (type k v c) (module C : Comparator.S with type t = k and type wit = c)
    (l : (k, v, c) t list) =
  let arr = Array.of_list l in
  if Array.length arr = 0 then
    (* Intersection of 0th elements cannot be represented by a finite
       set for any type. *)
    invalid_arg "join";
  let ended = ref false in
  (* Index of a cursor [c] in [arr] such that its current value
     is the smallest among current values of cursors of [arr]. *)
  let pos = ref 0 in
  (* Helper function that advances the cursors of [arr] until
     the next meeting point. *)
  let search () =
    let k = Array.length arr in
    let rec loop x =
      let y, _ = arr.(!pos).curr () in
      if C.compare y x < 0 then (
        arr.(!pos).seek x;
        match arr.(!pos).curr () with
        | exception End -> ended := true
        | x', _ ->
            (* As y < x and the cursor [arr.(!pos)] has not
               reached its end, the previous seek call must
               advance this cursor. *)
            (* assert (C.compare y x' < 0); *)
            assert (C.compare y x' <= 0);
            pos := (!pos + 1) mod k;
            loop x')
    in
    let x, _ = arr.((k + !pos - 1) mod k).curr () in
    loop x
  in
  let () =
    try
      Array.sort
        (fun it1 it2 -> C.compare (fst @@ it1.curr ()) (fst @@ it2.curr ()))
        arr;
      search ()
    with End -> ended := true
  in
  let seek w =
    arr.(!pos).seek w;
    match arr.(!pos).curr () with
    | exception End -> ended := true
    | _ ->
        let k = Array.length arr in
        pos := (!pos + 1) mod k;
        search ()
  in
  let next () =
    arr.(!pos).next ();
    match arr.(!pos).curr () with
    | exception End -> ended := true
    | _ ->
        let k = Array.length arr in
        pos := (!pos + 1) mod k;
        search ()
  in
  let curr () = if !ended then raise End else arr.(0).curr () in
  { curr; next; seek }

let to_seq c =
  let rec loop () =
    match c.curr () with
    | exception End -> Seq.Nil
    | v ->
        c.next ();
        Seq.Cons (v, loop)
  in
  loop

let[@inline] curr it = it.curr ()
let[@inline] next it = it.next ()
let[@inline] seek it e = it.seek e
