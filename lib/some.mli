module AliasCache : sig
  val clear : unit -> unit
  val add_alias : Geneweb_db.Driver.Iper.t -> string -> unit
  val add_direct : Geneweb_db.Driver.Iper.t -> unit
  val get_alias : Geneweb_db.Driver.Iper.t -> string option
end

val surname_not_found : Config.config -> string -> unit
val name_unaccent : string -> string

val persons_of_fsname :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.base -> string -> Geneweb_db.Driver.istr list) ->
  (Geneweb_db.Driver.istr -> Geneweb_db.Driver.iper list) ->
  (Geneweb_db.Driver.person -> Geneweb_db.Driver.istr) ->
  string ->
  (string * Geneweb_db.Driver.istr * Geneweb_db.Driver.iper list) list
  * (string -> string)
(** [persons_of_fsname conf base base_strings_of_fsname find proj x] function
    where:

    - [base_strings_of_fsname base x] is a function that returns the list of
      first/surnames (as istr) being equal to [x]
    - [find istr] is a function that returns the list of persons having [istr]
      as a first/surname id
    - [proj iper] is a function that returns first/surname id from the giving
      person id.
    - [x] is a first/surname or its substring.

    Returns [(l,inj)] where [l] is a list of [(str,istr,iperl)] where [istr] is
    id of [str] and [iperl] is a list of persons found that has [istr] as a
    first/surname such that [str = inj x]*)

val search_surname_print :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Config.config -> string -> unit) ->
  string ->
  unit

val print_surname_details :
  Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  (string * Geneweb_db.Driver.person list) list ->
  unit
(** [print_multiple_display conf base query surnames_groups] displays multiple
    surname groups with their associated persons. Used when a search returns
    multiple surname matches. *)

val search_first_name :
  Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  (string * (Mutil.StrSet.t * Geneweb_db.Driver.iper list)) list

val search_first_name_print :
  Config.config -> Geneweb_db.Driver.base -> string -> unit

val first_name_print_sections :
  Config.config ->
  Geneweb_db.Driver.base ->
  (string * Geneweb_db.Driver.person list) list ->
  rev:bool ->
  unit

val first_name_print_list_multi :
  Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  (int * (string * Geneweb_db.Driver.person list) list * bool * Mutil.StrSet.t)
  list ->
  unit

val first_name_print_list :
  Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  Mutil.StrSet.t ->
  (string * Geneweb_db.Driver.person list) list ->
  rev:bool ->
  unit

val print_several_possible_surnames :
  string ->
  Config.config ->
  Geneweb_db.Driver.base ->
  'a * (string * Geneweb_db.Driver.person list) list ->
  unit
