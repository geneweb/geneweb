(* Copyright (c) 1998-2007 INRIA *)

val redirect_or_specify :
  Config.config ->
  Geneweb_db.Driver.base ->
  not_found:(Config.config -> string -> unit) ->
  redirect_to_person:
    (Config.config ->
    Geneweb_db.Driver.base ->
    Geneweb_db.Driver.person ->
    unit) ->
  string ->
  unit
(** [redirect_or_specify conf base ~not_found ~redirect_to_person input]
    resolves [input] via {!lookup_person_by_input} and dispatches:
    - empty result → {!Some.search_surname_print} with the [not_found] callback
      as the empty-result handler;
    - single hit reachable as an exact key, Sosa or canonical name →
      [redirect_to_person conf base p];
    - otherwise (single hit not reachable as canonical, or multiple hits) →
      {!Some.specify} with [input] as the query string.

    The [not_found] and [redirect_to_person] callbacks are passed in rather than
    baked in to keep this module independent of the concrete page renderers in
    [bin/gwd]. *)
