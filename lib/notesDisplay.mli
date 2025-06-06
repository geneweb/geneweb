val print_linked_list :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper, Gwdb.ifam) Def.NLDB.page list ->
  unit
(** Displays the page list in argument *)

val print_what_links : Config.config -> Gwdb.base -> string -> unit

val print : Config.config -> Gwdb.base -> unit
(** Displays the base notes *)

val print_json : Config.config -> Gwdb.base -> unit

val print_mod : Config.config -> Gwdb.base -> unit
(** Displays a text form for notes creation/edition *)

val print_mod_json : Config.config -> Gwdb.base -> unit

val print_mod_ok : Config.config -> Gwdb.base -> unit
(** Updates notes *)

val print_misc_notes : Config.config -> Gwdb.base -> unit
(** Displays a menu to search in notes *)

val print_misc_notes_search : Config.config -> Gwdb.base -> unit
(** Same as `print_misc_notes`, with a default search *)

val print_what_links_p : Config.config -> Gwdb.base -> Gwdb.person -> unit
(** Displays links to pages associated to the person *)
