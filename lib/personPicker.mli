open Config
module Driver = Geneweb_db.Driver

val search_prefix :
  config ->
  Driver.base ->
  limit:int ->
  fn_prefix:string ->
  sn_prefix:string ->
  Driver.person list
(** [search_prefix conf base ~limit ~fn_prefix ~sn_prefix] scans the surname
    index from the first surname matching [sn_prefix] (alphabetical) and returns
    up to [limit] persons whose normalized first name starts with [fn_prefix].
    An empty [fn_prefix] matches any first name. Inputs must be lowered with
    [Name.lower]. Iteration stops as soon as a surname no longer prefix-matches.
    Persons hidden by privacy rules (private names, restricted access) are
    filtered out. Returns [[]] for an empty [sn_prefix]. *)

val lookup_exact : Driver.base -> string -> string -> int -> Driver.person list
(** [lookup_exact base fn sn oc] returns the singleton list containing the
    person whose key is [(Name.lower fn, Name.lower sn, oc)] if it exists, or
    [[]] otherwise. *)

val lookup_print : config -> Driver.base -> unit
(** [lookup_print conf base] HTTP entry point for [m=PNOC_LOOKUP]. Writes a JSON
    response of the form [[{"fn":"…","sn":"…","oc":N,"label":"…"}, …]]:

    - [exact=1] with [fn], [sn], [oc] (default 0) — strict key existence check,
      returns 0 or 1 element.
    - otherwise [q=…&n=…] — prefix search on a [fn sn] query (single token
      treated as surname prefix), capped at [n] (default 20, max 50) results.

    Response [Content-type: application/json; charset=utf-8]. *)
