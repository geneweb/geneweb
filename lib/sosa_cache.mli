type t

val write_static_sosa_cache : conf:Config.config -> base:Gwdb.base -> unit
val get_sosa_cache : conf:Config.config -> base:Gwdb.base -> t option
val get_sosa : base:Gwdb.base -> cache:t -> iper:Gwdb.iper -> Sosa.t option

val get_sosa_person :
  conf:Config.config -> base:Gwdb.base -> person:Gwdb.person -> Sosa.t

val print_sosa :
  conf:Config.config ->
  base:Gwdb.base ->
  person:Gwdb.person ->
  link:bool ->
  unit
