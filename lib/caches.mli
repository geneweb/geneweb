val lastname_cache_fname : string -> string
val first_name_cache_fname : string -> string
val occupation_cache_fname : string -> string
val source_cache_fname : string -> string
val place_cache_fname : string -> string
val node_threshold : int
val write_caches : Gwdb.base -> unit

val has_cache :
  ?with_up_to_date_state:bool ->
  conf:Config.config ->
  mode:[< `firstname | `lastname | `occupation | `place | `source ] ->
  unit ->
  bool

val read_cache :
  conf:Config.config ->
  [< `firstname | `lastname | `occupation | `place | `source ] ->
  string list

val complete_with_patch :
  [< `firstname | `lastname | `occupation | `place | `source ] ->
  Gwdb.base ->
  (string -> bool) ->
  string list ->
  string list
