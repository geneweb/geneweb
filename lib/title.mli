val compare_title_dates :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person * 'a Def.gen_title ->
  Geneweb_db.Driver.person * 'a Def.gen_title ->
  int

val compare_title_order :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person * 'a Def.gen_title ->
  Geneweb_db.Driver.person * 'a Def.gen_title ->
  int

val select_place :
  Config.config -> Geneweb_db.Driver.base -> string -> string list

val select_all_places : Config.config -> Geneweb_db.Driver.base -> string list

val select_title_place :
  Config.config ->
  Geneweb_db.Driver.base ->
  absolute:bool ->
  string ->
  string ->
  (Geneweb_db.Driver.person * Geneweb_db.Driver.title) list * string list

val select_title :
  Config.config ->
  Geneweb_db.Driver.base ->
  absolute:bool ->
  string ->
  string list * string list

val select_all_titles :
  Config.config -> Geneweb_db.Driver.base -> (string * int) list

val select_all_with_place :
  Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  (Geneweb_db.Driver.person * Geneweb_db.Driver.title) list
