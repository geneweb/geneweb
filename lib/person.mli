val is_empty : Gwdb.person -> bool
(** Tells if person is an empty person (a placeholder: his surname is empty) *)

val is_contemporary : Config.config -> Gwdb.base -> Gwdb.person -> bool
(** Check if a person is contemporary *)

val is_visible : Config.config -> Gwdb.base -> Gwdb.person -> bool
(** Check if a person should be displayed or not *)
