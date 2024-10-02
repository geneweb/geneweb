val surname_not_found : Config.config -> string -> unit

val persons_of_fsname :
  Config.config ->
  Gwdb.base ->
  (Gwdb.base -> string -> Gwdb.istr list) ->
  (Gwdb.istr -> Gwdb.iper list) ->
  (Gwdb.person -> Gwdb.istr) ->
  string ->
  (string * Gwdb.istr * Gwdb.iper list) list * (string -> string)
(** [persons_of_fsname conf base base_strings_of_fsname find proj x] function where:

    - [base_strings_of_fsname base x] is a function that returns the list of first/surnames (as istr) being equal to [x]
    - [find istr] is a function that returns the list of persons having [istr] as a first/surname id
    - [proj iper] is a function that returns first/surname id from the giving person id.
    - [x] is a first/surname or its substring.

    Returns [(l,inj)] where [l] is a list of [(str,istr,iperl)] where [istr] is id of [str] and [iperl] is a list of persons
    found that has [istr] as a first/surname such that [str = inj x]*)

val first_name_print : Config.config -> Gwdb.base -> string -> unit

val surname_print :
  Config.config ->
  Gwdb.base ->
  (Config.config -> string -> unit) ->
  string ->
  unit

val search_surname : Config.config -> Gwdb.base -> string -> Gwdb.iper list

val search_surname_print :
  Config.config ->
  Gwdb.base ->
  (Config.config -> string -> unit) ->
  string ->
  unit

val search_first_name :
  Config.config ->
  Gwdb.base ->
  string ->
  (string * (Mutil.StrSet.t * Gwdb.iper list)) list

val search_first_name_print : Config.config -> Gwdb.base -> string -> unit
