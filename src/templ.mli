(* camlp5r *)
(* $Id: templ.mli,v 5.8 2007-09-12 09:58:44 ddr Exp $ *)

open Config;
open Gwdb;
open TemplAst;

type vother 'a = 'abstract;
type env 'a = list (string * 'a);

value eval_transl : config -> bool -> string -> string -> string;
value copy_from_templ : config -> list (string * string) -> in_channel -> unit;

type interp_fun 'a 'b =
  { eval_var : env 'a -> 'b -> loc -> list string -> expr_val 'b;
    eval_transl : env 'a -> bool -> string -> string -> string;
    eval_predefined_apply : env 'a -> string -> list (expr_val 'b) -> string;
    get_vother : 'a -> option (vother 'b);
    set_vother : vother 'b -> 'a;
    print_foreach : 
      (env 'a -> 'b -> ast -> unit) ->
         (env 'a -> 'b -> ast -> string) ->
         env 'a -> 'b -> loc -> string -> list string ->
         list (list ast) -> list ast -> unit }
;

value interp_ast :
  config -> option base -> interp_fun 'a 'b -> env 'a -> 'b -> list ast ->
    unit;

(**)

value template_file : ref string;
value input_templ : config -> string -> option (list ast);
value print_copyright : config -> unit;
value include_hed_trl : config -> option base -> string -> unit;
