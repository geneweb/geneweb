(* camlp5r *)
(* $Id: templAst.mli,v 5.4 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type ast =
  [ Atext of loc and string
  | Avar of loc and string and list string
  | Atransl of loc and bool and string and string
  | Aconcat of loc and list ast
  | Awid_hei of string
  | Aif of ast and list ast and list ast
  | Aforeach of (loc * string * list string) and list (list ast) and list ast
  | Afor of string and ast and ast and list ast
  | Adefine of string and list string and list ast and list ast
  | Aapply of loc and string and list (list ast)
  | Alet of string and list ast and list ast
  | Aop1 of loc and string and ast
  | Aop2 of loc and string and ast and ast
  | Aint of loc and string
  | Aimport of string and list ast]
and loc = (int * int)
;

type expr_val 'a =
  [ VVbool of bool
  | VVstring of string
  | VVother of list string -> expr_val 'a ]
;
