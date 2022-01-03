
val advanced_search :
  Config.config -> Gwdb.base -> int -> Gwdb.person list * int

(** Returns a description string for the current advanced search results in the correct language.
  e.g. "Search all Pierre, born in Paris, died in Paris" *)
val searching_fields : Config.config -> Gwdb.base -> string