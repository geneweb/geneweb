(* $Id: q_codes.ml,v 5.3 2007-09-12 09:42:26 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

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
      Stdpp.raise_with_loc (Token.make_loc (0, String.length x))
        (Failure ("bad code " ^ x)) ]
;

Quotation.add "codes" (Quotation.ExStr f);
Quotation.default.val := "codes";

