(** Binary caches of a base: the distinct values of a field type with their
    istr, written by cache_files and read by gwd tools such as chk_data, in
    [GWPARAM.cache_d]. *)

type entry = Geneweb_db.Driver.istr * string

val file : string -> string -> string
(** [file bname name] Path of the cache [name] ("fnames", "places"…) of base
    [bname]: bases/bname.gwb/caches/bname_name.bin. *)

val exists : string -> string -> bool
(** [exists bname name] Whether the cache file exists. *)

val read : string -> string -> entry list option
(** [read bname name] Entries of the cache, or [None] if it is missing or
    unreadable. *)

val write : string -> string -> entry list -> unit
(** [write bname name entries] Writes the cache through a temporary file then a
    rename, creating its directory if needed. *)

val update :
  string ->
  string ->
  old_istr:Geneweb_db.Driver.istr ->
  new_istr:Geneweb_db.Driver.istr ->
  string ->
  bool
(** [update bname name ~old_istr ~new_istr v] Replaces the entry keyed by
    [old_istr] with [(new_istr, v)] and drops any entry already keyed by
    [new_istr]. After a modification the persons reference a new istr while the
    old one keeps its former string, so the entry must be re-keyed to stay
    valid. Returns [false] if the cache or the entry is missing, or on write
    failure. *)
