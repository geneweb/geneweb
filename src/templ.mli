(* camlp4r *)
(* $Id: templ.mli,v 4.3 2005-01-24 04:35:50 ddr Exp $ *)

open Config;
open Def;
open TemplAst;

value input : config -> string -> list ast;

value eval_transl : config -> bool -> string -> string -> string;
value eval_bool_expr : config -> (list string -> expr_val) -> ast_expr -> bool;

value print_var :
   config -> base -> (list string -> expr_val) -> string -> list string ->
     unit;
value print_apply :
   config -> (ast -> unit) -> (list string -> expr_val) -> list string ->
     list ast -> list ast_expr -> unit;
