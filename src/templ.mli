(* camlp4r *)
(* $Id: templ.mli,v 4.29 2005-08-30 21:36:13 ddr Exp $ *)

open Config;
open Def;
open TemplAst;

type vother 'a = 'abstract;
type env 'a = list (string * 'a);

value eval_transl : config -> bool -> string -> string -> string;
value copy_from_templ : config -> list (string * string) -> in_channel -> unit;

value interp :
  config -> base -> string ->
    (env 'a -> 'b -> loc -> list string -> expr_val 'b) ->
    (env 'a -> bool -> string -> string -> string) ->
    (env 'a -> string -> list (expr_val 'b) -> string) ->
    ('a -> option (vother 'b)) -> (vother 'b -> 'a) ->
    ((env 'a -> 'b -> ast -> unit) ->
       (env 'a -> 'b -> ast -> string) ->
       env 'a -> 'b -> loc -> string -> list string ->
       list (list ast) -> list ast -> unit) ->
    env 'a -> 'b -> unit;
