(* camlp4r *)
(* $Id: templAst.mli,v 4.2 2004-12-14 09:30:17 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

type ast =
  [ Atext of string
  | Avar of string and list string
  | Atransl of bool and string and string
  | Awid_hei of string
  | Aif of ast_expr and list ast and list ast
  | Aforeach of string and list string and list ast
  | Adefine of string and list string and list ast and list ast
  | Aapply of string and list ast_expr ]
and ast_expr =
  [ Eor of ast_expr and ast_expr
  | Eand of ast_expr and ast_expr
  | Eop of string and ast_expr and ast_expr
  | Enot of ast_expr
  | Estr of string
  | Eint of string
  | Evar of string and list string
  | Etransl of bool and string and string ]
;
