val print : Config.config -> Geneweb_db.Driver.base -> unit
(** Main entry point: displays the typographic error checker interface with
    search form and results. *)

val print_redirect_to_list : Config.config -> Geneweb_db.Driver.base -> unit
(** Redirects to the list of persons after a bulk modification. Extracts persons
    from UpdateData and builds redirect URL. *)

val print_chk_ok : Config.config -> Geneweb_db.Driver.base -> unit
(** Handles data modification confirmation. Returns JSON for AJAX requests, HTML
    page otherwise. *)
