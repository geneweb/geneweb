val print_main_relationship :
  Config.config ->
  Gwdb.base ->
  bool ->
  Gwdb.person ->
  Gwdb.person ->
  ((Gwdb.person option
   * Gwdb.person option
   * (int * int * (Gwdb.person * int) list)
   * (Gwdb.iper, Consang.relationship) Gwdb.Marker.t)
   list
  * Sosa.t
  * _)
  option ->
  unit

val print_base_loop : Config.config -> Gwdb.base -> Gwdb.person -> unit

val print :
  Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.person option -> unit

val print_multi : Config.config -> Gwdb.base -> unit
