
(** [advanced_search conf base max_answers] extracts advaced request fields from environement [conf.env] and
    returns at most [max_answers] persons from the [base] that match conditions described by those fields. Seond result
    represents real number of matches (if less then [max_answers]). *)
val advanced_search :
  Config.config -> Gwdb.base -> int -> Gwdb.person list * int

(** Returns a description string for the current advanced search results in the correct language.
  e.g. "Search all Pierre, born in Paris, died in Paris" *)
val searching_fields : Config.config -> Gwdb.base -> Adef.safe_string
