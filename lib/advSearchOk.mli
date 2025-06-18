val advanced_search :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person list * int
(** [advanced_search conf base max_answers] extracts advaced request fields from
    environement [conf.env] and returns at most [max_answers] persons from the
    [base] that match conditions described by those fields. Seond result
    represents real number of matches (if less then [max_answers]). *)

val searching_fields :
  Config.config -> Geneweb_db.Driver.base -> Adef.safe_string
(** Returns a description string for the current advanced search results in the
    correct language. e.g. "Search all Pierre, born in Paris, died in Paris" *)
