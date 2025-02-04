type t

val write_static_sosa_cache : conf:Config.config -> base:Gwdb.base -> unit
val get_sosa_cache : conf:Config.config -> base:Gwdb.base -> t option

val get_sosa :
  conf:Config.config ->
  base:Gwdb.base ->
  cache:t ->
  iper:Gwdb.iper ->
  sosa_ref:Gwdb.person ->
  Sosa.t option

val get_sosa_person :
  conf:Config.config -> base:Gwdb.base -> person:Gwdb.person -> Sosa.t

val print_sosa :
  conf:Config.config ->
  base:Gwdb.base ->
  person:Gwdb.person ->
  link:bool ->
  unit
