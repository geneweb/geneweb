val eval_julian_day : Config.config -> int
(** Evaluates the Julian day from environment parameters. Supports calendars:
    - Gregorian (prefix "g")
    - Julian (prefix "j")
    - French Republican (prefix "f")
    - Hebrew (prefix "h") Handles +/- variations on year/month/day via suffixes
      "1" (decrement) and "2" (increment). Returns the corresponding Julian day
      or today by default. *)

val print_calendar : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays the calendar based on the calendar.txt template. If no key is
    defined, uses today's date.
    @param conf Configuration with date parameters
    @param base Geneweb database *)
