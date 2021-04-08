(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb


val search_key_aux
  : (config -> base -> person list -> string -> person list)
  -> config -> base -> string -> person list
val search_by_name : config -> base -> string -> person list
val search_partial_key : config -> base -> string -> person list
val search_by_sosa : config -> base -> string -> person list
val search_by_key : config -> base -> string -> person list
val search_approx_key : config -> base -> string -> person list

val print :
  config -> base -> (config -> base -> string -> person list -> unit) ->
    (config -> string -> unit) -> unit
