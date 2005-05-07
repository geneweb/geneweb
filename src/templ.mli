(* camlp4r *)
(* $Id: templ.mli,v 4.5 2005-05-07 17:50:50 ddr Exp $ *)

open Config;
open Def;
open TemplAst;

value input : config -> string -> list ast;

value eval_transl : config -> bool -> string -> string -> string;
value eval_bool_expr :
  config -> (loc -> list string -> expr_val) -> ast_expr -> bool;
value eval_expr :
  config -> (loc -> list string -> expr_val) -> ast_expr -> string;

value eval_var :
   config -> (list string -> expr_val) -> string -> list string -> string;
value eval_apply :
   string -> (ast -> string) -> list string -> list ast -> list (list ast) ->
     string;

value print_var :
   config -> base -> (list string -> expr_val) -> string -> list string ->
     unit;
value print_apply :
   config -> string -> (ast -> unit) -> (loc -> list string -> expr_val) ->
     list string -> list ast -> list ast_expr -> unit;
