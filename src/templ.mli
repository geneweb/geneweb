(* camlp4r *)
(* $Id: templ.mli,v 4.1 2005-01-23 05:47:47 ddr Exp $ *)

open Config;
open Def;

value input : config -> string -> list TemplAst.ast;
value subst_text : string -> string -> string -> string;
value subst : (string -> string) -> TemplAst.ast -> TemplAst.ast;
value eval_transl : config -> bool -> string -> string -> string;
value print_variable : config -> base -> string -> unit;
