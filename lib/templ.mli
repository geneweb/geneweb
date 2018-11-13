(* $Id: templ.mli,v 5.8 2007-09-12 09:58:44 ddr Exp $ *)

open Config
open TemplAst

type 'a vother
type 'a env = (string * 'a) list

val eval_transl : config -> bool -> string -> string -> string
val copy_from_templ : config -> (string * string) list -> in_channel -> unit

type ('a, 'b) interp_fun =
  { eval_var : 'a env -> 'b -> loc -> string list -> 'b expr_val;
    eval_transl : 'a env -> bool -> string -> string -> string;
    eval_predefined_apply : 'a env -> string -> 'b expr_val list -> string;
    get_vother : 'a -> 'b vother option;
    set_vother : 'b vother -> 'a;
    print_foreach :
      ('a env -> 'b -> ast -> unit) -> ('a env -> 'b -> ast -> string) ->
        'a env -> 'b -> loc -> string -> string list -> ast list list ->
        ast list -> unit }

val interp_ast :
  config -> ('a, 'b) interp_fun -> 'a env -> 'b -> ast list -> unit

(**)

val template_file : string ref
val input_templ : config -> string -> ast list option
val print_copyright : config -> unit
val print_copyright_with_logo : config -> unit
val include_hed_trl : config -> string -> unit
