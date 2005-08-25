(* camlp4r *)
(* $Id: templAst.mli,v 4.12 2005-08-25 18:57:51 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

type ast =
  [ Atext of loc and string
  | Avar of loc and string and list string
  | Atransl of loc and bool and string and string
  | Awid_hei of string
  | Aif of ast and list ast and list ast
  | Aforeach of (loc * string * list string) and list (list ast) and list ast
  | Adefine of string and list string and list ast and list ast
  | Aapply of loc and string and list (list ast)
  | Alet of string and list ast and list ast
  | Aop1 of loc and string and ast
  | Aop2 of loc and string and ast and ast
  | Aint of loc and string ]
and loc = (int * int)
;

type expr_val = [ VVbool of bool | VVstring of string ];
