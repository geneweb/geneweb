module AliasCache : sig
  type t

  val create : unit -> t
  (** Per-request cache mapping iper to an optional alias string. Used to
      display [alias] tags next to person names in result lists when the person
      matched a query via one of their aliases. *)

  val add_alias : t -> Geneweb_db.Driver.Iper.t -> string -> unit
  (** Register that this iper matched via the given alias string. *)

  val add_direct : t -> Geneweb_db.Driver.Iper.t -> unit
  (** Register that this iper matched directly (not via an alias). *)

  val get_alias : t -> Geneweb_db.Driver.Iper.t -> string option
  (** [Some alias] if the iper matched via that alias, [None] otherwise
      (including for ipers never registered). *)
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
  AliasCache.t ->
  (Config.config -> string -> unit) ->
  string ->
  unit

val print_surname_details :
  Config.config ->
  Geneweb_db.Driver.base ->
  AliasCache.t ->
  string ->
  (string * Geneweb_db.Driver.person list) list ->
  unit
(** [print_multiple_display conf base alias_cache query surnames_groups]
    displays multiple surname groups with their associated persons. Used when a
    search returns multiple surname matches. *)

val first_name_print_sections :
  Config.config ->
  Geneweb_db.Driver.base ->
  AliasCache.t ->
  (string * Geneweb_db.Driver.person list) list ->
  rev:bool ->
  unit

val first_name_print_list_multi :
  Config.config ->
  Geneweb_db.Driver.base ->
  AliasCache.t ->
  string ->
  (int * (string * Geneweb_db.Driver.person list) list * bool * Mutil.StrSet.t)
  list ->
  unit

val print_several_possible_surnames :
  string ->
  Config.config ->
  Geneweb_db.Driver.base ->
  AliasCache.t ->
  'a * (string * Geneweb_db.Driver.person list) list ->
  unit

val get_extra_surnames : Config.config -> string list
(** Collect v1=, v2=, ... surname values from URL env. *)

val collect_surname_suggestions :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper list ->
  string list ->
  string list
(** [collect_surname_suggestions conf base iperl known] returns surnames found
    in aliases and boundary children, excluding those already in [known]. Sorted
    alphabetically. *)
