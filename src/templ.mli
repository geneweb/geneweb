(* camlp4r *)
(* $Id: templ.mli,v 4.4 2005-02-06 10:17:35 ddr Exp $ *)

open Config;
open Def;
open TemplAst;

value input : config -> string -> list ast;

value eval_transl : config -> bool -> string -> string -> string;
value eval_bool_expr :
  config -> (loc -> list string -> expr_val) -> ast_expr -> bool;

value print_var :
   config -> base -> (list string -> expr_val) -> string -> list string ->
     unit;
value print_apply :
   config -> (ast -> unit) -> (loc -> list string -> expr_val) ->
     list string -> list ast -> list ast_expr -> unit;
