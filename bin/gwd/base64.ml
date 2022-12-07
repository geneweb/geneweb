(* $Id: base64.ml,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* For basic credentials only *)
(* Encoding is [A-Z][a-z][0-9]+/= *)
(* 3 chars = 24 bits = 4 * 6-bit groups -> 4 chars *)

(* Init the index *)
let index64 =
  let index64 = Array.make 128 0 in
  for i = 0 to 25 do
    index64.(i + Char.code 'A') <- i
  done;
  for i = 0 to 25 do
    index64.(i + Char.code 'a') <- i + 26
  done;
  for i = 0 to 9 do
    index64.(i + Char.code '0') <- i + 52
  done;
  index64.(Char.code '+') <- 62;
  index64.(Char.code '/') <- 63;
  index64

let decode s =
  let rpos = ref 0 and wpos = ref 0 and len = String.length s in
  let res = Bytes.create (len / 4 * 3) in
  while !rpos < len do
    let v1 = index64.(Char.code s.[!rpos]) in
    let v2 = index64.(Char.code s.[!rpos + 1]) in
    let v3 = index64.(Char.code s.[!rpos + 2]) in
    let v4 = index64.(Char.code s.[!rpos + 3]) in
    let i = (v1 lsl 18) lor (v2 lsl 12) lor (v3 lsl 6) lor v4 in
    Bytes.set res !wpos (Char.chr (i lsr 16));
    Bytes.set res (!wpos + 1) (Char.chr ((i lsr 8) land 0xFF));
    Bytes.set res (!wpos + 2) (Char.chr (i land 0xFF));
    rpos := !rpos + 4;
    wpos := !wpos + 3
  done;
  let cut =
    if s.[len - 1] = '=' then if s.[len - 2] = '=' then 2 else 1 else 0
  in
  Bytes.sub_string res 0 (Bytes.length res - cut)
