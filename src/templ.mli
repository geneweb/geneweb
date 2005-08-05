(* camlp4r *)
(* $Id: templ.mli,v 4.20 2005-08-05 11:02:02 ddr Exp $ *)

open Config;
open Def;
open TemplAst;

value input : config -> string -> list ast;
value eval_transl : config -> bool -> string -> string -> string;
value copy_from_templ : config -> list (string * string) -> in_channel -> unit;

value print_ast :
  (config -> base -> 'a -> 'b -> loc -> list string -> expr_val) ->
    (config -> base -> 'a -> bool -> string -> string -> string) ->
    (config -> 'a -> string -> list string -> string) ->
    (string -> 'a -> option (list string * list ast)) ->
    (string -> list string -> list ast -> 'a -> 'a) ->
    (string -> 'a -> option string) ->
    (string -> string -> 'a -> 'a) ->
    ((config -> base -> 'a -> 'b -> ast -> unit) ->
       (config -> base -> 'a -> 'b -> ast -> string) ->
       (config -> base -> 'a -> 'b -> ast -> int) ->
       config -> base -> 'a -> 'b -> loc -> string ->
       list string -> list (list ast) -> list ast ->
       unit) ->
    config -> base -> 'a -> 'b -> ast -> unit;
