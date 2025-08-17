val print : Config.config -> Geneweb_db.Driver.base -> unit
(** Display the list of potential typographic errors in the database. *)

val print_redirect_to_list : Config.config -> Geneweb_db.Driver.base -> unit
(** Redirect to the list of persons *)

val print_chk_ok : Config.config -> Geneweb_db.Driver.base -> unit
(** Update entries with updated data. *)
