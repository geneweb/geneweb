(* $Id: searchName.mli,v 5.2 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb

val search_partial_key : config -> base -> string -> person list
val search_by_sosa : config -> base -> string -> person list
val search_by_key : config -> base -> string -> person list
val search_approx_key : config -> base -> string -> person list

(** [print conf base specify unknown]
    Uses ["p"] and ["n"] values from [conf.env] and try
    to choose the best search function based on these inputs
    (they may be empty).

    A single result redirect to individual page.
    Multiple results call the [specify] function
    No result call the [unkown] function
 *)
val print
  : config
  -> base
  -> (config -> base -> string -> person list -> unit)
  -> (config -> string -> unit)
 -> unit
