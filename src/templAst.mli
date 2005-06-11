(* camlp4r *)
(* $Id: templAst.mli,v 4.10 2005-06-11 21:22:49 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

type ast =
  [ Atext of string
  | Avar of loc and string and list string
  | Atransl of bool and string and string
  | Awid_hei of string
  | Aif of ast and list ast and list ast
  | Aforeach of (loc * string * list string) and list (list ast) and list ast
  | Adefine of string and list string and list ast and list ast
  | Aapply of loc and string and list (list ast)
  | Aop1 of string and ast
  | Aop2 of string and ast and ast
  | Aint of loc and string ]
and loc = (int * int)
;

type expr_val = [ VVbool of bool | VVstring of string ];
