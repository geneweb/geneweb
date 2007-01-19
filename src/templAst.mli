(* camlp4r *)
(* $Id: templAst.mli,v 5.3 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type ast =
  [ Atext of loc and string
  | Avar of loc and string and list string
  | Atransl of loc and bool and string and string
  | Aconcat of loc and list ast
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

type expr_val 'a =
  [ VVbool of bool
  | VVstring of string
  | VVother of list string -> expr_val 'a ]
;
