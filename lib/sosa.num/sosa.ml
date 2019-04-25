(* Copyright (c) 1998-2007 INRIA *)

type t = Big_int.big_int

let of_int i = if i < 0 then invalid_arg "Sosa.of_int" else Big_int.big_int_of_int i

let zero = Big_int.zero_big_int
let one = Big_int.unit_big_int
let eq = Big_int.eq_big_int
let gt = Big_int.gt_big_int
let half = let two = Big_int.succ_big_int one in fun x -> Big_int.div_big_int x two

let add = Big_int.add_big_int
let sub = Big_int.sub_big_int
let mul x y = Big_int.mult_big_int x (Big_int.big_int_of_int y)
let div x y = Big_int.div_big_int x (Big_int.big_int_of_int y)
let modl x y = Big_int.mod_big_int x (Big_int.big_int_of_int y)
let exp = Big_int.power_big_int_positive_int

let inc sosa increment = Big_int.add_big_int sosa (of_int increment)
let even =
  let two = Big_int.big_int_of_int 2 in
  fun sosa -> Big_int.eq_big_int Big_int.zero_big_int (Big_int.mod_big_int sosa two)
let twice sosa = Big_int.shift_left_big_int sosa 1

(* Mutil.string_of_int_sep adapted to type [t] *)
let to_string_sep =
  let ten = Big_int.big_int_of_int 10 in
  fun sep x ->
  let digits, len =
    let rec loop (d, l) x =
      if Big_int.eq_big_int x @@ Big_int.zero_big_int then (d, l)
      else
        let q, r = Big_int.quomod_big_int x ten in
        loop (Char.chr (Char.code '0' + Big_int.int_of_big_int r) :: d, l + 1) q
    in
    loop ([], 0) x
  in
  let digits, len = if digits = [] then ['0'], 1 else digits, len in
  let slen = String.length sep in
  let s = Bytes.create (len + (len - 1) / 3 * slen) in
  let _ =
    List.fold_left
      (fun (i, j) c ->
         Bytes.set s j c ;
         if i < len - 1 && (len - 1 - i) mod 3 = 0 then
           begin String.blit sep 0 s (j + 1) slen; i + 1, j + 1 + slen end
         else i + 1, j + 1)
      (0, 0) digits
  in
  Bytes.unsafe_to_string s

let to_string = Big_int.string_of_big_int

let of_string = Big_int.big_int_of_string

let gen =
  let log2 = log 2. in
  fun sosa ->
  int_of_float (log (Big_int.float_of_big_int sosa) /. log2)  + 1

let branches x =
  let rec aux acc d =
    if Big_int.eq_big_int d Big_int.zero_big_int then acc
    else aux (Big_int.int_of_big_int (Big_int.and_big_int d one) :: acc) (Big_int.shift_right_big_int d 1)
  in
  List.tl (aux [] x)
