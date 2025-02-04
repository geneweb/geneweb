type t

val build_static_sosa_cache : conf:Config.config -> base:Gwdb.base -> t option
val output_static_sosa_cache : base:Gwdb.base -> cache:t -> unit
val write_static_sosa_cache : conf:Config.config -> base:Gwdb.base -> unit
val get_sosa_cache : conf:Config.config -> base:Gwdb.base -> t option

val get_sosa :
  conf:Config.config ->
  base:Gwdb.base ->
  cache:t ->
  iper:Gwdb.iper ->
  sosa_ref:Gwdb.person ->
  Sosa.t option

val next_sosa : t -> Sosa.t -> (Sosa.t * Gwdb.iper) option
val previous_sosa : t -> Sosa.t -> (Sosa.t * Gwdb.iper) option

val get_sosa_person :
  conf:Config.config -> base:Gwdb.base -> person:Gwdb.person -> Sosa.t
