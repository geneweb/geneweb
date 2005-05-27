(* camlp4r *)
(* $Id: templAst.mli,v 4.9 2005-05-27 04:50:35 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

type ast =
  [ Atext of string
  | Avar of loc and string and list string
  | Aexpr of ast_expr
  | Atransl of bool and string and string
  | Awid_hei of string
  | Aif of ast_expr and list ast and list ast
  | Aforeach of (loc * string * list string) and list ast_expr and list ast
  | Adefine of string and list string and list ast and list ast
  | Aapply of string and list ast_expr
  | AapplyWithAst of string and list (list ast) ]
and ast_expr =
  [ Eor of ast_expr and ast_expr
  | Eand of ast_expr and ast_expr
  | Eop of string and ast_expr and ast_expr
  | Enot of ast_expr
  | Estr of string
  | Eint of loc and string
  | Eapp of loc and string and list ast_expr
  | Evar of loc and string and list string
  | Etransl of bool and string and string ]
and loc = (int * int)
;

type expr_val = [ VVbool of bool | VVstring of string ];
