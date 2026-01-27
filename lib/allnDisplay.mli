val print_frequency :
  at_least:int option -> Config.Trimmed.t -> Gwdb.base -> bool -> unit

val print_short :
  prefix:string option ->
  at_least:int option ->
  Config.Trimmed.t ->
  Gwdb.base ->
  bool ->
  unit

val print_alphabetic :
  prefix:string option ->
  all:bool ->
  at_least:int option ->
  fast:bool ->
  index:Uchar.t list ->
  Config.Trimmed.t ->
  Gwdb.base ->
  bool ->
  unit
