(* Copyright (c) 1998-2007 INRIA *)

type t = int

let zero = 0
let one = 1
let eq x y = x = y
let gt x y = x > y
let half x = x / 2
let add x y = x + y
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

let to_string = string_of_int

let to_string_sep = Mutil.string_of_int_sep

let of_string = int_of_string

let gen x = int_of_float (log (float_of_int x) /. log 2.) + 1

let branches x =
  let rec aux acc d =
    if d = 0 then acc else aux (d land 1 :: acc) (d lsr 1)
  in
  List.tl (aux [] x)
