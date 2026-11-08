val print_mod : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays a menu for updating Geneweb's dictionary of names, last names,
    locations, sources and professions. *)

val print_mod_ok : Config.config -> Geneweb_db.Driver.base -> unit
(** Updates persons linked to the updated data. With [ajax], answers in JSON
    instead of a page: [success], and on success the notification [label], the
    old and new values [before] and [after], the id [key] of the new value,
    [complete] ([false] when [max_nb_update] stopped the update: submitting
    again goes on), [entry] (the new value as listed on the book page [s],
    [null] when it is not on that page) and [url] (the book page of the new
    value); on failure a [message]. *)
