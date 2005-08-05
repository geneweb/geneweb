(* camlp4r *)
(* $Id: templ.mli,v 4.21 2005-08-05 13:07:27 ddr Exp $ *)

open Config;
open Def;
open TemplAst;

value input : config -> string -> list ast;
value eval_transl : config -> bool -> string -> string -> string;
value copy_from_templ : config -> list (string * string) -> in_channel -> unit;

type vother = 'abstract;

value print_ast :
  ('a -> 'b -> loc -> list string -> expr_val) ->
    ('a -> bool -> string -> string -> string) ->
    ('a -> string -> list string -> string) ->
    (string -> 'a -> option vother) ->
    (string -> vother -> 'a -> 'a) ->
    (('a -> 'b -> ast -> unit) ->
       ('a -> 'b -> ast -> string) ->
       ('a -> 'b -> ast -> int) ->
       'a -> 'b -> loc -> string ->
       list string -> list (list ast) -> list ast ->
       unit) ->
    config -> base -> 'a -> 'b -> ast -> unit;
