val compare_title_dates :
  Config.config ->
  Gwdb.base ->
  Gwdb.person * _ Def.gen_title ->
  Gwdb.person * _ Def.gen_title ->
  int

val compare_title_order :
  Config.config ->
  Gwdb.base ->
  Gwdb.person * _ Def.gen_title ->
  Gwdb.person * _ Def.gen_title ->
  int

val select_title_place :
  Config.config ->
  Gwdb.base ->
  absolute:bool ->
  string ->
  string ->
  (Gwdb.person * Gwdb.title) list * string * string * string list

val select_all_with_place :
  Config.config ->
  Gwdb.base ->
  string ->
  (Gwdb.person * Gwdb.title) list * string

val select_title :
  Config.config ->
  Gwdb.base ->
  absolute:bool ->
  string ->
  string list * string * string list

val select_place : Config.config -> Gwdb.base -> string -> string list * string
val select_all_titles : Config.config -> Gwdb.base -> (string * int) list
val select_all_places : Config.config -> Gwdb.base -> string list
