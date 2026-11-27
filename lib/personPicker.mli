open Config
module Driver = Geneweb_db.Driver

val lookup_print : config -> Driver.base -> unit
(** [lookup_print conf base] HTTP entry point for [m=PNOC_LOOKUP]. Writes a JSON
    response of the form [[{"fn":"…","sn":"…","oc":N,"label":"…"}, …]]:

    - [exact=1] with [fn], [sn], [oc] (default 0) — strict key existence check,
      returns 0 or 1 element. When [fn] or [sn] is "?", [oc] is the person
      index, as in [Update.insert_person].
    - [keys=fn1.oc1.sn1;fn2.oc2.sn2…] — the persons of the existing keys.
    - otherwise [q=…&n=…] — search of the persons, answered as
      [{"persons":[…],"truncated":b}]. [q] is ["surname/first names year"],
      first names and year optional, or a link key ["[[fn/sn/oc/text]]"] giving
      the person of the key. The surname is a prefix of the surname without
      particle, case nor accents ({!Geneweb_db.Dict.Surname_search}; the surname
      as typed with an index written before it), each word of the first names a
      prefix of a word of the first name, the year a prefix of the year of birth
      or death. At most [n] persons (default 20, max 200), [truncated] when
      others match. Each person has also ["i"], ["acc"] (its URL parameters),
      ["name"], ["dates"] (years), ["birth"] and ["death"] (short dates, HTML),
      ["father"] and ["mother"] ([null] if unknown) and ["spouses"], these
      relatives with the first four fields. Hidden persons are left out and
      relatives masked as anywhere else.

    Response [Content-type: application/json; charset=utf-8]. *)
