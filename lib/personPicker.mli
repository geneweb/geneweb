open Config
module Driver = Geneweb_db.Driver

val lookup_print : config -> Driver.base -> unit
(** [lookup_print conf base] HTTP entry point for [m=PNOC_LOOKUP]. Writes a JSON
    response of the form [[{"fn":"…","sn":"…","oc":N,"label":"…"}, …]]:

    - [exact=1] with [fn], [sn], [oc] (default 0) — strict key existence check,
      returns 0 or 1 element.
    - otherwise [q=…&n=…] — prefix search on a [fn sn] query (single token
      treated as surname prefix), capped at [n] (default 20, max 50) results.

    Response [Content-type: application/json; charset=utf-8]. *)
