val advanced_search :
  Config.config -> Gwdb.base -> int -> Gwdb.person list * int
(** [advanced_search conf base max_answers] extracts advaced request fields from
    environement [conf.env] and returns at most [max_answers] persons from the
    [base] that match conditions described by those fields. Seond result
    represents real number of matches (if less then [max_answers]). *)

module SearchingFields : sig
  val first_name : Config.config -> string
  val surname : Config.config -> string
  val occupation : Config.config -> string
  val events : Config.config -> string
  val sosa : Config.config -> Gwdb.base -> string
  val union : Config.config -> int
  val sex : Config.config -> int
end

val searching_fields : Config.config -> Gwdb.base -> Adef.safe_string
(** Returns a description string for the current advanced search results in the
    correct language. e.g. "Search all Pierre, born in Paris, died in Paris" *)

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

val force_exact_search_by_name : Config.config -> Config.config
