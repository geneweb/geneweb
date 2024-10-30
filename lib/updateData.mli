val get_person_from_data :
  Config.config -> Gwdb.base -> (Gwdb.istr * Gwdb.person list) list

val update_person_list :
  Config.config ->
  Gwdb.base ->
  string ->
  (string * Gwdb.person list) list ->
  int ->
  int ->
  int

val build_list_long :
  Config.config ->
  (Gwdb.istr * string) list ->
  (string * (Gwdb.istr * string) list) list

val build_list_short : Config.config -> (_ * string) list -> string list
val build_list : Config.config -> Gwdb.base -> (Gwdb.istr * string) list
val get_all_data : Config.config -> Gwdb.base -> Gwdb.istr list
