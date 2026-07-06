(* $Id: base64.ml v7.1 23/06/2026 04:02:33 ddr Exp *)
(* Copyright (c) 1998-2007 INRIA *)

let index64 =
  let t = Array.make 256 0 in
  String.iteri
    (fun i c -> t.(Char.code c) <- i)
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  t

let decode s =
  let len = String.length s in
  if len = 0 || len mod 4 <> 0 then ""
  else begin
    let res = Bytes.create (len / 4 * 3) in
    for k = 0 to (len / 4) - 1 do
      let p = k * 4 in
      let i =
        (index64.(Char.code s.[p]) lsl 18)
        lor (index64.(Char.code s.[p + 1]) lsl 12)
        lor (index64.(Char.code s.[p + 2]) lsl 6)
        lor index64.(Char.code s.[p + 3])
      in
      let q = k * 3 in
      Bytes.set res q (Char.chr (i lsr 16));
      Bytes.set res (q + 1) (Char.chr ((i lsr 8) land 0xFF));
      Bytes.set res (q + 2) (Char.chr (i land 0xFF))
    done;
    let cut =
      if s.[len - 1] = '=' then if s.[len - 2] = '=' then 2 else 1 else 0
    in
    Bytes.sub_string res 0 (Bytes.length res - cut)
  end
