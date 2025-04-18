(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb

val search_key_aux :
  (config -> base -> person list -> string -> person list) ->
  config ->
  base ->
  string ->
  person list
(** [search_key_aux aux conf base str] search persons by misc name [str]
    (could be one of the mix). If result is empty tries to
    it search names with roman number instead of numerals (if they are present
    in the name). Applies [aux] on the result and removes all dublicates.
    Empty persons, persons with private names or persons to which there
    are no rights to access are not listed. *)

val search_by_name : config -> base -> string -> person list
(** Search persons by name that has format {i "firstname surname"}.
    Dublicates are possible (different occurrences).
    Empty persons, persons with private name or persons to which
    there are no rights to access are not listed. *)

val search_by_sosa : config -> base -> string -> person option
(** search person using its Sosa number
    this implies that Sosa 1 has been defined *)

val search_by_key : config -> base -> string -> person option
(** Same as [search_by_name] but search by key that has format {i "firstname.occ surname"}.
    only one person may match such a format *)

val search_approx_key : config -> base -> string -> person list
(** Calls [search_key_aux] with [aux] fonction that filter result list and only persons whose
    [fname^sname] or one of their misc names are equal to key. *)

val print :
  config ->
  base ->
  (config ->
  base ->
  string ->
  person list ->
  person list ->
  person list ->
  unit) ->
  (config -> string -> unit) ->
  unit
