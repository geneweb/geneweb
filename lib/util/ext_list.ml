let cons_opt x xs = Option.fold ~none:xs ~some:(fun x -> x :: xs) x

module Infix = struct
  let ( @?: ) = cons_opt
  let ( @:: ) = List.cons
end

let take l n =
  let rec aux res l n =
    match l with x :: l when n > 0 -> aux (x :: res) l (n - 1) | _ -> res
  in
  List.rev (aux [] l n)

let rec drop l n = match l with _ :: l when n > 0 -> drop l (n - 1) | _ -> l
let sublist l pos len = take (drop l pos) len

let cmp cmp l1 l2 =
  let rec aux l1 l2 =
    match (l1, l2) with
    | x :: xs, y :: ys when cmp x y -> aux xs ys
    | [], [] -> true
    | _ -> false
  in
  aux l1 l2

let is_subset s1 s2 = List.for_all (fun e -> List.mem e s2) s1

let elements_cmp l1 =
  let l1 = List.sort compare l1 in
  fun l2 -> List.sort compare l2 = l1

let iter_first f = function
  | [] -> ()
  | hd :: tl ->
      f true hd;
      List.iter (f false) tl

let rec compare cmp l1 l2 =
  match (l1, l2) with
  | x1 :: l1, x2 :: l2 -> (
      match cmp x1 x2 with 0 -> compare cmp l1 l2 | x -> x)
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1

let rec find_map f = function
  | [] -> None
  | x :: l -> (
      match f x with Some _ as result -> result | None -> find_map f l)

let rec last = function
  | [] -> raise (Failure "last")
  | [ x ] -> x
  | _ :: tl -> last tl

let ref_append tl hd = tl := hd :: !tl

let rec replace old_v new_v = function
  | [] -> []
  | hd :: tl -> if hd = old_v then new_v :: tl else hd :: replace old_v new_v tl

let except x =
  let rec loop acc = function
    | [] -> []
    | hd :: tl -> if hd = x then List.rev_append acc tl else loop (hd :: acc) tl
  in
  loop []

let index x list =
  let rec loop i = function
    | [] -> raise Not_found
    | hd :: tl -> if hd = x then i else loop (succ i) tl
  in
  loop 0 list

let slice a b list =
  let rec list_slice a b = function
    | [] -> []
    | hd :: tl ->
        if a <> 0 then list_slice (pred a) b tl
        else if b <> 0 then hd :: list_slice 0 (pred b) tl
        else []
  in
  list_slice a (b - a) list

(* Copied from OCaml's List.sort_uniq and adapted to our needs
   (commit e5ebec7 from Nov 7, 2019) *)
let map_sort_uniq (fn : 'a -> 'b) l =
  let open List in
  let rec rev_merge l1 l2 accu =
    match (l1, l2) with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1 :: t1, h2 :: t2 ->
        let c = Stdlib.compare h1 h2 in
        if c = 0 then rev_merge t1 t2 (h1 :: accu)
        else if c < 0 then rev_merge t1 l2 (h1 :: accu)
        else rev_merge l1 t2 (h2 :: accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match (l1, l2) with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1 :: t1, h2 :: t2 ->
        let c = Stdlib.compare h1 h2 in
        if c = 0 then rev_merge_rev t1 t2 (h1 :: accu)
        else if c > 0 then rev_merge_rev t1 l2 (h1 :: accu)
        else rev_merge_rev l1 t2 (h2 :: accu)
  in
  let rec sort n l =
    match (n, l) with
    | 2, x1 :: x2 :: tl ->
        let x1 = fn x1 in
        let x2 = fn x2 in
        let s =
          let c = Stdlib.compare x1 x2 in
          if c = 0 then [ x1 ] else if c < 0 then [ x1; x2 ] else [ x2; x1 ]
        in
        (s, tl)
    | 3, x1 :: x2 :: x3 :: tl ->
        let x1 = fn x1 in
        let x2 = fn x2 in
        let x3 = fn x3 in
        let s =
          let c = Stdlib.compare x1 x2 in
          if c = 0 then
            let c = Stdlib.compare x2 x3 in
            if c = 0 then [ x2 ] else if c < 0 then [ x2; x3 ] else [ x3; x2 ]
          else if c < 0 then
            let c = Stdlib.compare x2 x3 in
            if c = 0 then [ x1; x2 ]
            else if c < 0 then [ x1; x2; x3 ]
            else
              let c = Stdlib.compare x1 x3 in
              if c = 0 then [ x1; x2 ]
              else if c < 0 then [ x1; x3; x2 ]
              else [ x3; x1; x2 ]
          else
            let c = Stdlib.compare x1 x3 in
            if c = 0 then [ x2; x1 ]
            else if c < 0 then [ x2; x1; x3 ]
            else
              let c = Stdlib.compare x2 x3 in
              if c = 0 then [ x2; x1 ]
              else if c < 0 then [ x2; x3; x1 ]
              else [ x3; x2; x1 ]
        in
        (s, tl)
    | n, l ->
        let n1 = n asr 1 in
        let n2 = n - n1 in
        let s1, l2 = rev_sort n1 l in
        let s2, tl = rev_sort n2 l2 in
        (rev_merge_rev s1 s2 [], tl)
  and rev_sort n l =
    match (n, l) with
    | 2, x1 :: x2 :: tl ->
        let x1 = fn x1 in
        let x2 = fn x2 in
        let s =
          let c = Stdlib.compare x1 x2 in
          if c = 0 then [ x1 ] else if c > 0 then [ x1; x2 ] else [ x2; x1 ]
        in
        (s, tl)
    | 3, x1 :: x2 :: x3 :: tl ->
        let x1 = fn x1 in
        let x2 = fn x2 in
        let x3 = fn x3 in
        let s =
          let c = Stdlib.compare x1 x2 in
          if c = 0 then
            let c = Stdlib.compare x2 x3 in
            if c = 0 then [ x2 ] else if c > 0 then [ x2; x3 ] else [ x3; x2 ]
          else if c > 0 then
            let c = Stdlib.compare x2 x3 in
            if c = 0 then [ x1; x2 ]
            else if c > 0 then [ x1; x2; x3 ]
            else
              let c = Stdlib.compare x1 x3 in
              if c = 0 then [ x1; x2 ]
              else if c > 0 then [ x1; x3; x2 ]
              else [ x3; x1; x2 ]
          else
            let c = Stdlib.compare x1 x3 in
            if c = 0 then [ x2; x1 ]
            else if c > 0 then [ x2; x1; x3 ]
            else
              let c = Stdlib.compare x2 x3 in
              if c = 0 then [ x2; x1 ]
              else if c > 0 then [ x2; x3; x1 ]
              else [ x3; x2; x1 ]
        in
        (s, tl)
    | n, l ->
        let n1 = n asr 1 in
        let n2 = n - n1 in
        let s1, l2 = sort n1 l in
        let s2, tl = sort n2 l2 in
        (rev_merge s1 s2 [], tl)
  in
  let len = length l in
  if len < 2 then List.map fn l else fst (sort len l)

let rev_map_append f l1 l2 =
  let rec aux acc = function [] -> acc | hd :: tl -> aux (f hd :: acc) tl in
  aux l2 l1

let rec rev_iter fn = function
  | [] -> ()
  | hd :: tl ->
      let () = rev_iter fn tl in
      fn hd

let groupby ~key ~value list =
  let h = Hashtbl.create (List.length list) in
  List.iter
    (fun x ->
      let k = key x in
      let v = value x in
      if Hashtbl.mem h k then Hashtbl.replace h k (v :: Hashtbl.find h k)
      else Hashtbl.add h k [ v ])
    list;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []
