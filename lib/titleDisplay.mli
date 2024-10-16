val select_title_place :
  Config.config ->
  Gwdb.base ->
  string ->
  string ->
  (Gwdb.person * Gwdb.title) list * string * string * string list

val select_title :
  Config.config -> Gwdb.base -> string -> string list * string * string list

val print_places_list :
  Config.config -> Gwdb.base -> string -> string list -> string list -> unit

val print : Config.config -> Gwdb.base -> unit
