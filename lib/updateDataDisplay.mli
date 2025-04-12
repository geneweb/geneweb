val print_mod : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays a menu for updating Geneweb's dictionary of names, last names,
    locations, sources and professions. *)

val print_mod_ok : Config.config -> Geneweb_db.Driver.base -> unit
(** Updates persons linked to the updated data. *)
