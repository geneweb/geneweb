(* camlp4r *)
(* $Id: templ.mli,v 4.26 2005-08-12 18:09:49 ddr Exp $ *)

open Config;
open Def;
open TemplAst;

type vother = 'abstract;
type env 'a = list (string * 'a);

value eval_transl : config -> bool -> string -> string -> string;
value copy_from_templ : config -> list (string * string) -> in_channel -> unit;

value interp :
  config -> base -> string ->
    (env 'a -> 'b -> loc -> list string -> expr_val) ->
    (env 'a -> bool -> string -> string -> string) ->
    (env 'a -> string -> list string -> string) ->
    ('a -> option vother) -> (vother -> 'a) ->
    (env 'a -> list string -> unit) ->
    ((env 'a -> 'b -> ast -> unit) ->
       (env 'a -> 'b -> ast -> string) ->
       env 'a -> 'b -> loc -> string -> list string ->
       list (list ast) -> list ast -> unit) ->
    env 'a -> 'b -> unit;
