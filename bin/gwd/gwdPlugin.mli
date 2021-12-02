val assets : string ref

val ht : (string, (string * (Geneweb.Config.config -> Gwdb.base option -> bool))) Hashtbl.t

val register :
  ns:string ->
  (string * (string -> Geneweb.Config.config -> Gwdb.base option -> bool)) list ->
  unit

val se : (string * (Geneweb.Config.config -> Gwdb.base option -> unit)) list ref

val register_se :
  ns:string ->
  (string -> Geneweb.Config.config -> Gwdb.base option -> unit) ->
  unit
