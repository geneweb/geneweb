val advanced_search :
  query_params:Page.Advanced_search.Query_params.t ->
  Config.config ->
  Gwdb.base ->
  Gwdb.person list * int
(** [advanced_search ~query_params conf base]
    returns at most [query_params.limit] persons from the [base] that match conditions described by those fields. Seond result
    represents real number of matches (if less then [query_params.limit]). *)

module SearchingFields : sig
  val first_name : Page.Advanced_search.Query_params.t -> string
  val surname : Page.Advanced_search.Query_params.t -> string
  val occupation : Page.Advanced_search.Query_params.t -> string

  val events :
    query_params:Page.Advanced_search.Query_params.t -> Config.config -> string

  val sosa :
    query_params:Page.Advanced_search.Query_params.t ->
    Config.config ->
    Gwdb.base ->
    string

  val union : Page.Advanced_search.Query_params.t -> int
  val sex : Page.Advanced_search.Query_params.t -> int
  val other_aliases : Page.Advanced_search.Query_params.t -> string
end

val searching_fields :
  query_params:Page.Advanced_search.Query_params.t ->
  Config.config ->
  Gwdb.base ->
  Adef.safe_string
(** Returns a description string for the current advanced search results in the correct language.
    e.g. "Search all Pierre, born in Paris, died in Paris" *)

val matching_first_name_aliases :
  first_name:string -> aliases:string list -> string list

val exact_matching_first_name_aliases :
  first_name:string -> aliases:string list -> string list

val prefix_matching_first_name_aliases :
  first_name:string -> aliases:string list -> string list

val matching_surname_aliases :
  surname:string -> aliases:string list -> string list

val exact_matching_surname_aliases :
  surname:string -> aliases:string list -> string list

val prefix_matching_surname_aliases :
  surname:string -> aliases:string list -> string list

val matching_alias_public_name_qualifiers :
  string:string -> aliases:string list -> string list

val force_exact_search_by_name : Config.config -> Config.config
