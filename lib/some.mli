
val surname_not_found : Config.config -> string -> unit

val persons_of_fsname :
  Config.config ->
  Gwdb.base ->
  (Gwdb.base -> string -> Gwdb.istr list) ->
  (Gwdb.istr -> Gwdb.iper list) ->
  (Gwdb.person -> Gwdb.istr) ->
  string -> (string * Gwdb.istr * Gwdb.iper list) list * (string -> string)

val first_name_print : Config.config -> Gwdb.base -> string -> unit

val surname_print :
  Config.config ->
  Gwdb.base -> (Config.config -> string -> unit) -> string -> unit

val search_surname :
  Config.config -> Gwdb.base -> string -> Gwdb.iper list

val search_surname_print :
  Config.config ->
  Gwdb.base -> (Config.config -> string -> unit) -> string -> unit

val search_first_name :
  Config.config ->
  Gwdb.base -> string -> (string * (Mutil.StrSet.t * Gwdb.iper list)) list

val search_first_name_print :
  Config.config -> Gwdb.base -> string -> unit
