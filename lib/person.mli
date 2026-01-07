val is_empty : Gwdb.person -> bool
(** Tells if person is an empty person (a placeholder: his surname is empty) *)

val is_contemporary : Config.config -> Gwdb.base -> Gwdb.person -> bool
(** Check if a person is contemporary *)

val is_visible : Config.config -> Gwdb.base -> Gwdb.person -> bool
(** Check if a person should be displayed or not *)

val is_hide_names : Config.config -> Gwdb.person -> bool
(** Tells if person's names are hiden (if person's access is [Private] or if
    mode [conf.hide_names] is enabled). *)

val is_restricted : Config.config -> Gwdb.base -> Gwdb.iper -> bool
(** Tells if person is restrited to acccess. If mode `use_restrict` is disabled
    returns always [false]. *)

val is_hidden : Config.config -> Gwdb.base -> Gwdb.person -> bool
val has_restricted_name : Config.config -> Gwdb.base -> Gwdb.person -> bool

val map_name_visibility :
  on_hidden_name:(Config.config -> Gwdb.base -> Gwdb.person -> 'a) ->
  on_restricted_name:(Config.config -> Gwdb.base -> Gwdb.person -> 'a) ->
  on_visible_name:(Config.config -> Gwdb.base -> Gwdb.person -> 'a) ->
  conf:Config.config ->
  base:Gwdb.base ->
  person:Gwdb.person ->
  'a

val has_visible_name : Config.config -> Gwdb.base -> Gwdb.person -> bool
val compare_by_dates : Gwdb.person -> Gwdb.person -> int

val nobtit : Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.title list
(** [nobtit conf base p] returns list of titles of [p] from the [base] that
    respects constraints imposed by [conf.allowed_titles] and
    [conf.denied_titles] *)

val is_empty_name : Gwdb.person -> bool
(** [is_empty_name p] [false] if we knwon the first name or the last name of
    [p]. *)

val is_fully_visible_to_visitors :
  Config.config -> Gwdb.base -> Gwdb.person -> bool
(** [is_fully_visible_to_visitors conf base p] is true iff [p] is fully visible
    for a visitor *)

val main_title : Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.title option
(** Returns main person's title. If person doesn't have it, then returns first
    title from the list. *)
