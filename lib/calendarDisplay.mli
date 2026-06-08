val print_calendar : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays the calendar based on the calendar.txt template. If no key is
    defined, uses today's date.
    @param conf Configuration with date parameters
    @param base Geneweb database *)
