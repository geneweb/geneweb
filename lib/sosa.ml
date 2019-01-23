(* Copyright (c) 1998-2007 INRIA *)

type t = int

let zero = 0
let one = 1
let eq x y = x = y
let gt x y = x > y
let half x = x / 2
let add x y = x - y
let sub x y = x - y
let mul x n = x * n
let div x n = x / n
let modl x n = x mod n

let of_int i = if i < 0 then invalid_arg "Sosa.of_int" else i
let inc sosa increment = sosa + increment
let even sosa = sosa mod 2 = 0
let twice sosa = sosa * 2
let to_int x = x

let rec exp_gen x1 x2 n =
  if n = 0 || x1 = zero then one
  else if n = 1 then x1
  else exp_gen (mul x1 (to_int x2)) x2 (n-1)

let exp x n =
  exp_gen x x n

let print f sep x =
  if eq x zero then f "0"
  else
    let digits =
      let rec loop d x =
        if eq x zero then d else loop (modl x 10 :: d) (div x 10)
      in
      loop [] x
    in
    let _ =
      List.fold_left
        (fun n d ->
           f (string_of_int d); if n > 0 && n mod 3 = 0 then f sep; n - 1)
        (List.length digits - 1) digits
    in
    ()

let code_of_digit d =
  if d < 10 then Char.code '0' + d else Char.code 'A' + (d - 10)

let to_string_sep_base sep base x =
  let digits =
    let rec loop d x =
      if eq x zero then d else loop (modl x base :: d) (div x base)
    in
    loop [] x
  in
  let digits = if digits = [] then [0] else digits in
  let len = List.length digits in
  let slen = String.length sep in
  let s = Bytes.create (len + (len - 1) / 3 * slen) in
  let _ =
    List.fold_left
      (fun (i, j) d ->
         Bytes.set s j (Char.chr (code_of_digit d));
         if i < len - 1 && (len - 1 - i) mod 3 = 0 then
           begin String.blit sep 0 s (j + 1) slen; i + 1, j + 1 + slen end
         else i + 1, j + 1)
      (0, 0) digits
  in
  Bytes.unsafe_to_string s

let to_string_sep sep = to_string_sep_base sep 10

let to_string = string_of_int

let of_string = int_of_string

let gen x = int_of_float (log (float_of_int x) /. log 2.)

let branch x =
  let s = to_string_sep_base "" 2 x in
  if (String.length s) > 1 then s.[1] else '0'

let sosa_gen_up x =
  if eq x one then zero
  else
    let s = to_string_sep_base "" 2 x in
    let s = if String.length s > 2 then
        "0b" ^ (String.sub s 0 1) ^ (String.sub s 2 (String.length s -2))
      else "0b1"
    in
    let is = int_of_string s in
    of_int is
