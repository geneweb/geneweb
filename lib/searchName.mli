(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb

val search_partial_key : config -> base -> string -> person list
val search_by_sosa : config -> base -> string -> person list
val search_by_key : config -> base -> string -> person list
val search_approx_key : config -> base -> string -> person list

type search_type =
  | Sosa
  | Key
  | Surname
  | FirstName
  | ApproxKey
  | PartialKey

(** [search conf base specify unknown one surname firstname fn sn search_order] *)
val search
  : Config.config
  -> Gwdb.base
  -> (Config.config -> Gwdb.base -> string -> Gwdb.person list -> unit)
  -> (Config.config -> string -> unit)
  -> (Config.config -> Gwdb.base -> Gwdb.person -> unit)
  -> (Config.config -> Gwdb.base -> (Config.config -> string -> unit) -> string
      -> (string * (Mutil.StrSet.t * Gwdb.iper list)) list * (string -> string) -> unit)
  -> (Config.config -> Gwdb.base -> string ->
      (string * (Mutil.StrSet.t * Gwdb.iper list)) list -> unit)
  -> string
  -> string
  -> search_type list
  -> unit

(** [print ~fn ~sn conf base specify unknown]
    Uses [fn] and [sn] parameters from [conf.env] and try
    to choose the best search function based on these inputs
    (they may be empty).
    [fn] is expected to be a first name.
    [sn] can be a surname, the whole key (first name + surname), a sosa number.

    A single result redirect to individual page.
    Multiple results call the [specify] function
    No result call the [unkown] function
 *)
val print
  : config
  -> base
  -> (config -> base -> string -> person list -> unit)
  -> (config -> string -> unit)
  -> fn:string
  -> sn:string
  -> unit
