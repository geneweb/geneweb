(* $Id: q_codes.ml,v 5.0 2005-12-13 11:51:27 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

value f _ =
  fun  
  [ "PREFIX_SMALL_BLOCK" -> "0x80"
  | "PREFIX_SMALL_INT" -> "0x40"
  | "PREFIX_SMALL_STRING" -> "0x20"
  | "CODE_INT8" -> "0x0"
  | "CODE_INT16" -> "0x1"
  | "CODE_INT32" -> "0x2"
  | "CODE_BLOCK32" -> "0x8"
  | "CODE_STRING8" -> "0x9"
  | "CODE_STRING32" -> "0xA"
(*
  | "CODE_NZEROS8" -> "0x4"
  | "CODE_NZEROS32" -> "0x5"
*)
  | "CODE_DOUBLE_NATIVE" -> "11"
  | x ->
      Stdpp.raise_with_loc
        (IFDEF OCAML_308 THEN (Token.make_loc (0, String.length x))
         ELSE (0, String.length x)
         END)
        (Failure ("bad code " ^ x)) ]
;

Quotation.add "codes" (Quotation.ExStr f);
Quotation.default.val := "codes";

