(* Copyright (c) 1998-2007 INRIA *)

type t = int array

let base = 0x1000000
let max_mul_base = max_int / base
let zero = [||]
let one = [| 1 |]

let of_int i =
  if i < 0 then invalid_arg "Sosa.of_int"
  else if i = 0 then zero
  else if i < base then [| i |]
  else [| i mod base; i / base |]

let to_int = function
  | [||] -> 0
  | [| i |] -> i
  | [| m; d |] -> (d * base) + m
  | _ -> assert false

let eq x y = x = y

let gt x y =
  if Array.length x > Array.length y then true
  else if Array.length x < Array.length y then false
  else
    let rec loop i =
      if i < 0 then false
      else if x.(i) > y.(i) then true
      else if x.(i) < y.(i) then false
      else loop (i - 1)
    in
    loop (Array.length x - 1)

let twice x =
  let l =
    let rec loop i r =
      if i = Array.length x then if r = 0 then [] else [ r ]
      else
        let v = (x.(i) lsl 1) + r in
        (v land (base - 1)) :: loop (i + 1) (if v >= base then 1 else 0)
    in
    loop 0 0
  in
  Array.of_list l

let half x =
  let l =
    let rec loop i r v =
      if i < 0 then v
      else
        let rd = if x.(i) land 1 = 0 then 0 else base / 2 in
        let v =
          let d = r + (x.(i) / 2) in
          if d = 0 && v = [] then v else d :: v
        in
        loop (i - 1) rd v
    in
    loop (Array.length x - 1) 0 []
  in
  Array.of_list l

let even x = if Array.length x = 0 then true else x.(0) land 1 = 0

let inc x n =
  let l =
    let rec loop i r =
      if i = Array.length x then if r = 0 then [] else [ r ]
      else
        let d = x.(i) + r in
        (d mod base) :: loop (i + 1) (d / base)
    in
    loop 0 n
  in
  Array.of_list l

let add x y =
  let l =
    let rec loop i r =
      if i >= Array.length x && i >= Array.length y then
        if r = 0 then [] else [ r ]
      else
        let d, r =
          let xi = if i >= Array.length x then 0 else x.(i) in
          let yi = if i >= Array.length y then 0 else y.(i) in
          let s = xi + yi + r in
          (s mod base, s / base)
        in
        d :: loop (i + 1) r
    in
    loop 0 0
  in
  Array.of_list l

let normalize =
  let rec loop = function
    | [] -> []
    | x :: l ->
        let r = loop l in
        if x = 0 && r = [] then r else x :: r
  in
  loop

let sub x y =
  let l =
    let rec loop i r =
      if i >= Array.length x && i >= Array.length y then
        if r = 0 then [] else invalid_arg "Sosa.sub"
      else
        let d, r =
          let xi = if i >= Array.length x then 0 else x.(i) in
          let yi = if i >= Array.length y then 0 else y.(i) in
          if yi + r <= xi then (xi - (yi + r), 0) else (base + xi - (yi + r), 1)
        in
        d :: loop (i + 1) r
    in
    loop 0 0
  in
  Array.of_list (normalize l)

let mul0 x n =
  if n > max_mul_base then invalid_arg "Sosa.mul"
  else
    let l =
      let rec loop i r =
        if i = Array.length x then if r = 0 then [] else [ r ]
        else
          let d = (x.(i) * n) + r in
          (d mod base) :: loop (i + 1) (d / base)
      in
      loop 0 0
    in
    Array.of_list l

let mul x n =
  if n < max_mul_base then mul0 x n
  else
    let rec loop r x n =
      if n < max_mul_base then add r (mul0 x n)
      else
        loop
          (add r (mul0 x (n mod max_mul_base)))
          (mul0 x max_mul_base) (n / max_mul_base)
    in
    loop zero x n

let div x n =
  if n > max_mul_base then invalid_arg "Sosa.div"
  else
    let l =
      let rec loop i l r =
        if i < 0 then l
        else
          let r = (r mod n * base) + x.(i) in
          let d = r / n in
          loop (i - 1) (d :: l) r
      in
      loop (Array.length x - 1) [] 0
    in
    Array.of_list (normalize l)

let modl x n =
  of_int
  @@
  let r = sub x (mul0 (div x n) n) in
  if Array.length r = 0 then 0 else r.(0)

let rec exp_gen x1 x2 n =
  if n = 0 || x1 = zero then one
  else if n = 1 then x1
  else exp_gen (mul x1 (to_int x2)) x2 (n - 1)

let exp x n = exp_gen x x n
let compare x y = if gt x y then 1 else if eq x y then 0 else -1

let code_of_digit d =
  let d = to_int d in
  if d < 10 then Char.code '0' + d else Char.code 'A' + (d - 10)

let to_string_sep_base sep base x =
  let digits =
    let rec loop d x =
      if eq x zero then d else loop (modl x base :: d) (div x base)
    in
    loop [] x
  in
  let digits = if digits = [] then [ zero ] else digits in
  let len = List.length digits in
  let slen = String.length sep in
  let s = Bytes.create (len + ((len - 1) / 3 * slen)) in
  let _ =
    List.fold_left
      (fun (i, j) d ->
        Bytes.set s j (Char.chr (code_of_digit d));
        if i < len - 1 && (len - 1 - i) mod 3 = 0 then (
          String.blit sep 0 s (j + 1) slen;
          (i + 1, j + 1 + slen))
        else (i + 1, j + 1))
      (0, 0) digits
  in
  Bytes.unsafe_to_string s

let to_string_sep sep = to_string_sep_base sep 10
let to_string = to_string_sep_base "" 10

let of_string s =
  let rec loop n i =
    if i = String.length s then n
    else
      match s.[i] with
      | '0' .. '9' ->
          loop (inc (mul0 n 10) (Char.code s.[i] - Char.code '0')) (i + 1)
      | _ -> failwith "Sosa.of_string"
  in
  loop zero 0

let gen x =
  let s = to_string_sep_base "" 2 x in
  String.length s (* coherent with %sosa.lvl *)

let branches x =
  if eq x one then []
  else
    let s = to_string_sep_base "" 2 x in
    let rec loop acc i =
      if i <= 0 then acc
      else loop ((Char.code s.[i] - Char.code '0') :: acc) (i - 1)
    in
    loop [] (String.length s - 1)
