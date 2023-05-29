(* Copyright (c) 1998-2007 INRIA *)

type t = Z.t

let of_int i = if i < 0 then invalid_arg "Sosa.of_int" else Z.of_int i
let zero = Z.zero
let one = Z.one
let two = Z.succ one
let ten = Z.of_int 10
let eq = Z.equal
let gt = Z.gt
let half x = Z.div x two
let add = Z.add
let sub = Z.sub
let mul x y = Z.mul x (Z.of_int y)
let div x y = Z.div x (Z.of_int y)
let modl x y = Z.rem x (Z.of_int y)
let exp = Z.pow
let compare = Z.compare
let inc sosa increment = Z.add sosa (of_int increment)
let even = Z.is_even
let twice sosa = Z.mul sosa two

(* Mutil.string_of_int_sep adapted to type [t] *)
let to_string_sep sep x =
  let digits, len =
    let rec loop (d, l) x =
      if Z.equal Z.zero x then (d, l)
      else
        let q, r = Z.div_rem x ten in
        loop (Char.chr (Char.code '0' + Z.to_int r) :: d, l + 1) q
    in
    loop ([], 0) x
  in
  let digits, len = if digits = [] then ([ '0' ], 1) else (digits, len) in
  let slen = String.length sep in
  let s = Bytes.create (len + ((len - 1) / 3 * slen)) in
  let _ =
    List.fold_left
      (fun (i, j) c ->
        Bytes.set s j c;
        if i < len - 1 && (len - 1 - i) mod 3 = 0 then (
          String.blit sep 0 s (j + 1) slen;
          (i + 1, j + 1 + slen))
        else (i + 1, j + 1))
      (0, 0) digits
  in
  Bytes.unsafe_to_string s

let to_string = Z.to_string
let of_string = Z.of_string
let gen x = Z.log2 x + 1

let branches x =
  let rec aux acc d =
    if Z.equal Z.zero d then acc
    else aux (Z.to_int (Z.logand d one) :: acc) (Z.shift_right d 1)
  in
  List.tl (aux [] x)
