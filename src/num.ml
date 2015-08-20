(* $Id: num.ml,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type t = array int;

value base = 0x1000000;
value max_mul_base = max_int / base;

value zero = [| |];
value one = [| 1 |];
value eq x y = x = y;
value gt x y =
  if Array.length x > Array.length y then True
  else if Array.length x < Array.length y then False
  else
    loop (Array.length x - 1) where rec loop i =
      if i < 0 then False
      else if x.(i) > y.(i) then True
      else if x.(i) < y.(i) then False
      else loop (i - 1)
;
value twice x =
  let l =
    loop 0 0 where rec loop i r =
      if i = Array.length x then
        if r = 0 then [] else [r]
      else
        let v = x.(i) lsl 1 + r in
        [v land (base - 1) :: loop (i + 1) (if v >= base then 1 else 0)]
  in
  Array.of_list l
;
value half x =
  let l =
    loop (Array.length x - 1) 0 [] where rec loop i r v =
      if i < 0 then v
      else
        let rd = if x.(i) land 1 = 0 then 0 else base / 2 in
        let v =
          let d = r + x.(i) / 2 in
          if d = 0 && v = [] then v else [d :: v]
        in
        loop (i - 1) rd v
  in
  Array.of_list l
;
value even x =
  if Array.length x = 0 then True
  else x.(0) land 1 = 0
;
value inc x n =
  let l =
    loop 0 n where rec loop i r =
      if i = Array.length x then
        if r = 0 then [] else [r]
      else
        let d = x.(i) + r in
        [d mod base :: loop (i + 1) (d / base)]
  in
  Array.of_list l
;
value add x y =
  let l =
    loop 0 0 where rec loop i r =
      if i >= Array.length x && i >= Array.length y then
        if r = 0 then [] else [r]
      else
        let (d, r) =
          let xi = if i >= Array.length x then 0 else x.(i) in
          let yi = if i >= Array.length y then 0 else y.(i) in
          let s = xi + yi + r in
          (s mod base, s / base)
        in
        [d :: loop (i + 1) r]
  in
  Array.of_list l
;
value normalize =
  loop where rec loop =
    fun
    [ [] -> []
    | [x :: l] ->
        let r = loop l in
        if x = 0 && r = [] then r else [x :: r] ]
;
value sub x y =
  let l =
    loop 0 0 where rec loop i r =
      if i >= Array.length x && i >= Array.length y then
        if r = 0 then []
        else invalid_arg "Num.sub"
      else
        let (d, r) =
          let xi = if i >= Array.length x then 0 else x.(i) in
          let yi = if i >= Array.length y then 0 else y.(i) in
          if yi + r <= xi then (xi - (yi + r), 0)
          else (base + xi - (yi + r), 1)
        in
        [d :: loop (i + 1) r]
  in
  Array.of_list (normalize l)
;
value mul0 x n =
  if n > max_mul_base then invalid_arg "Num.mul"
  else
    let l =
      loop 0 0 where rec loop i r =
        if i = Array.length x then
          if r = 0 then [] else [r]
        else
          let d = x.(i) * n + r in
          [d mod base :: loop (i + 1) (d / base)]
    in
    Array.of_list l
;
value mul x n =
  if n < max_mul_base then mul0 x n
  else
    loop zero x n where rec loop r x n =
      if n < max_mul_base then add r (mul0 x n)
      else
        loop (add r (mul0 x (n mod max_mul_base)))
          (mul0 x max_mul_base) (n / max_mul_base)
;
value div x n =
  if n > max_mul_base then invalid_arg "Num.div"
  else
    let l =
      loop (Array.length x - 1) [] 0 where rec loop i l r =
        if i < 0 then l
        else
          let r = r mod n * base + x.(i) in
          let d = r / n in
          loop (i - 1) [d :: l] r
    in
    Array.of_list (normalize l)
;
value modl x n =
  let r = sub x (mul0 (div x n) n) in
  if Array.length r = 0 then 0 else r.(0)
;

value of_int i =
  if i < 0 then invalid_arg "Num.of_int"
  else if i = 0 then zero
  else if i < base then [| i |]
  else [| i mod base; i / base |]
;
value print f sep x =
  if eq x zero then f "0"
  else
    let digits = loop [] x
      where rec loop d x =
        if eq x zero then d
        else loop [modl x 10 :: d] (div x 10)
    in
    let _ =
      List.fold_left
        (fun n d ->
           do {
             f (string_of_int d);
             if n > 0 && n mod 3 = 0 then f sep else ();
             n - 1;
           })
        (List.length digits - 1) digits
    in ()
;

value code_of_digit d =
  if d < 10 then Char.code '0' + d
  else Char.code 'A' + (d - 10)
;

value to_string_sep_base sep base x =
  let digits = loop [] x
    where rec loop d x =
      if eq x zero then d
      else loop [modl x base :: d] (div x base)
  in
  let digits = if digits = [] then [0] else digits in
  let len = List.length digits in
  let slen = String.length sep in
  let s = Bytes.create (len + (len - 1) / 3 * slen) in
  let _ =
    List.fold_left
       (fun (i, j) d ->
          do {
            Bytes.set s j (Char.chr (code_of_digit d));
            if i < len - 1 && (len - 1 - i) mod 3 = 0 then do {
              String.blit sep 0 s (j + 1) slen;
              (i + 1, j + 1 + slen)
            }
            else (i + 1, j + 1)
          })
       (0, 0) digits
  in
  s
;

value to_string_sep sep = to_string_sep_base sep 10;
value to_string = to_string_sep_base "" 10;

value of_string s =
  loop zero 0 where rec loop n i =
    if i = String.length s then n
    else
      match s.[i] with
      [ '0'..'9' ->
          loop (inc (mul0 n 10) (Char.code s.[i] - Char.code '0')) (i + 1)
      | _ -> failwith "Num.of_string" ]
;
