val print_linked_list :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page list ->
  unit
(** Displays the page list in argument *)

val print_what_links : Config.config -> Geneweb_db.Driver.base -> string -> unit

val print : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays the base notes *)

val print_json : Config.config -> Geneweb_db.Driver.base -> unit

val print_mod : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays a text form for notes creation/edition *)

val print_mod_json : Config.config -> Geneweb_db.Driver.base -> unit

val print_mod_ok : Config.config -> Geneweb_db.Driver.base -> unit
(** Updates notes *)

val print_misc_notes : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays a menu to search in notes *)

val print_misc_notes_search : Config.config -> Geneweb_db.Driver.base -> unit
(** Same as `print_misc_notes`, with a default search *)

val print_what_links_p :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** Displays links to pages associated to the person *)
