(* camlp4r *)
(* $Id: templ.mli,v 4.2 2005-01-23 09:41:05 ddr Exp $ *)

open Config;
open Def;
open TemplAst;

value input : config -> string -> list ast;
value subst_text : string -> string -> string -> string;
value subst : (string -> string) -> ast -> ast;

value eval_transl : config -> bool -> string -> string -> string;
value eval_bool_expr : config -> (list string -> expr_val) -> ast_expr -> bool;
value eval_expr : config -> (list string -> expr_val) -> ast_expr -> string;

value print_var :
   config -> base -> (list string -> expr_val) -> string -> list string ->
     unit;
