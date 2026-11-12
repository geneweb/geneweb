val print_mod : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays a menu for updating Geneweb's dictionary of names, last names,
    locations, sources and professions. *)

val print_list :
  Geneweb_http.Connection.t -> Config.config -> Geneweb_db.Driver.base -> unit
(** [print_list conn conf base] prints the list of persons (m=L). Opened from a
    book without persons ([data] and [key], no [i0]), it lists the persons
    linked to the entry [key] ([i0], [i1]..., [nb] and the entry text [k]); when
    a single person is linked, it redirects to their page instead, or for a
    wizard on the books of titles and estates to the titles of their edit form.
*)

val print_mod_ok : Config.config -> Geneweb_db.Driver.base -> unit
(** Updates persons linked to the updated data. With [ajax], answers in JSON
    instead of a page. When more than [book_bulk_confirm] records (150 by
    default) use the entry and the request has no [confirm=on], nothing is
    updated: [success] is [false], [confirm] gives that number of records and
    [message] asks to validate again. Otherwise [success], and on success the
    notification [label], the old and new values [before] and [after], the id
    [key] of the new value, its number of records [records], [entry] (the new
    value as listed on the book page [s], [null] when it is not on that page)
    and [url] (the book page of the new value); on failure a [message]. *)
