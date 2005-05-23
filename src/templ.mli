(* camlp4r *)
(* $Id: templ.mli,v 4.10 2005-05-23 09:38:26 ddr Exp $ *)

open Config;
open Def;
open TemplAst;

exception Exc_located of loc and exn;

value input : config -> string -> list ast;

value eval_transl : config -> bool -> string -> string -> string;
value eval_bool_expr :
  config -> (loc -> list string -> expr_val * string -> list string -> string)
    -> ast_expr -> bool;
value eval_expr :
  config -> (loc -> list string -> expr_val) -> ast_expr -> string;
value eval_subst : string -> list string -> list string -> ast -> ast;
value eval_string_var :
   config -> (list string -> expr_val) -> string -> list string -> string;

value print_var :
   config -> base -> (list string -> expr_val) -> string -> list string ->
     unit;
value print_apply :
   config -> string -> (ast -> unit) -> (loc -> list string -> expr_val) ->
     list string -> list ast -> list ast_expr -> unit;
