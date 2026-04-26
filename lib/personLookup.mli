(* Copyright (c) 1998-2007 INRIA *)

(** Person lookup for legacy free-form search routes (m=R, m=NG).

    The [m=R] (relation calculator with input mode) and [m=NG] (legacy name
    search) routes share a common pattern: take a free-form string typed by the
    user, try to resolve it to one or more persons, then either redirect
    directly to a unique result or render the "please specify" disambiguation
    page. This module factors that pattern. *)

val person_matches_input :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  string ->
  bool
(** [person_matches_input conf base p input] tests whether [input] is a
    recognized identifier for [p] — either ["firstname surname"] (modulo
    {!Name.strip_lower}) or one of the person's misc names (titles, public
    names, qualifiers, aliases). *)

val lookup_person_by_input :
  Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  Geneweb_db.Driver.person list * bool
(** [lookup_person_by_input conf base input] resolves [input] through three
    strategies in order:
    - as a Sosa number against the base's sosa-reference (returns a
      single-element list with [sosa_acc=true] on hit);
    - as an exact unique key via {!SearchName.search_by_key};
    - as an approximate name match via {!SearchName.search_key_aux}, preferring
      strict matches but falling back to {!SearchName.search_by_name} when
      neither strict nor partial matches are found.

    Returns [(persons, sosa_acc)]. The boolean is [true] only when the result
    came from the Sosa path. *)

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
