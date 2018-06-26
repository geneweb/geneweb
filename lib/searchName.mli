(* $Id: searchName.mli,v 5.2 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb

val search_partial_key : config -> base -> string -> person list
val print :
  config -> base -> (config -> base -> string -> person list -> unit) ->
    (config -> string -> unit) -> unit
val search_by_sosa : config -> base -> string -> person list
val search_by_key : config -> base -> string -> person list
val search_approx_key : config -> base -> string -> person list
