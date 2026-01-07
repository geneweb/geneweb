(* Copyright (c) 1998-2007 INRIA *)

val get_wday : Config.config -> Date.date -> string
(** [get_wday conf date] Return the day of the week for this [date] *)

val code_dmy : ?with_short_month:bool -> Config.config -> Date.dmy -> string
(** Returns textual representation of the date translated to the current
    language. Uses different encodings depending on day's, month's and year's
    accessibility. Doesn't consider precision. *)

val code_hebrew_date : Config.config -> int -> int -> int -> string
(** Returns textual representation of a day / month / year in the hebrew
    calendar translated to the current language. *)

val code_islamic_date : Config.config -> int -> int -> int -> string
(** Returns textual representation of a day / month / year in the Islamic
    calendar translated to the current language. *)

val string_of_dmy :
  ?with_short_month:bool -> Config.config -> Date.dmy -> Adef.safe_string
(** Converts and translate date to the textual representation for the giving
    language. Considers precision. *)

val string_of_date : Config.config -> Date.date -> Adef.safe_string
(** If date is [Dgreg] calls for [string_of_dmy] to convert date to the string
    else returns content of [Dtext]. Difference between calendars is not taken
    into the acount. *)

val string_of_ondate : Config.config -> Date.date -> Adef.safe_string
(** Converts and translate date with considering different calendars with prefix
    "on" before dates (changes for other languages). Date precision is much more
    verbose then with [string_of_date]. Decline phrase if needed. *)

val string_of_on_calendar_dmy :
  ?with_gregorian_precisions:bool ->
  calendar:[< `Julian | `French | `Hebrew | `Islamic ] ->
  Config.config ->
  Date.dmy ->
  Adef.safe_string
(** Translate a date in the [calendar] with prefix "on" before dates (changes
    for other languages). *)

val string_slash_of_date : Config.config -> Date.date -> Adef.safe_string
(** Returns date in format dd/mm/yyyy. Format could be different for other
    languages (defined by [!dates order] keyword in the lexicon). *)

val string_of_age : Config.config -> Date.dmy -> Adef.safe_string
(** Returns textual representation of the age represented by [dmy]. *)

val prec_year_text : Config.config -> Date.dmy -> string
(** Returns textual representation of date's precision and year. *)

val prec_text : Config.config -> Date.dmy -> string
(** Returns textual representation of date's precision *)

val month_text : Date.dmy -> string
(** Returns textual representation of date's month number. *)

val year_text : Date.dmy -> string
(** Returns textual representation of date's year. *)

val short_dates_text :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string
(** Returns concatenation of person's birth and death dates (if exists).
    Precision is mentionned for each date. For example :

    * 1700-1780 (birth - death) * 1700- (birth - death but don't know when) *
    1700 (birth - alive) * †1780 (unknown birth date - death) * † (unknown birth
    date - death but don't know when) *)

val short_marriage_date_text :
  Config.config ->
  Gwdb.base ->
  Gwdb.family ->
  Gwdb.person ->
  Gwdb.person ->
  Adef.safe_string
(** Retruns year of marriage for given spouses with its precision. *)

val death_symbol : Config.config -> string
(** [death_symbol conf] Return the value associated to ["death_symbol"] in
    [.gwf] file if it is defined, or use ["†"] if it is not. *)

val code_french_year : Config.config -> int -> string
(** Returns roman number of the year of French calendar *)

val string_of_date_aux :
  ?dmy:(?with_short_month:bool -> Config.config -> Date.dmy -> Adef.safe_string) ->
  ?sep:Adef.safe_string ->
  Config.config ->
  Date.date ->
  Adef.safe_string
(** Same as [string_of_ondate] except :
    - Conversion function for [Date.dmy] could be passed in in [dmy] argument
    - Doesn't consider phrase declination as [string_of_ondate] does. *)

(**/**)

val french_month : Config.config -> int -> string
(** Returns the translation of the month of Hebrew calendar First month is [0]
*)

val string_of_prec_dmy :
  Config.config ->
  Adef.safe_string ->
  Adef.safe_string ->
  Date.precision ->
  Adef.safe_string
(** [string_of_prec_dmy conf s s2 precision] Takes two date representations (as
    strings) [s] and [s2] and returns translated phrase according to the given
    [precision]. *)

val code_french_date : Config.config -> int -> int -> int -> string

val gregorian_precision :
  ?with_short_month:bool -> Config.config -> Date.dmy -> Adef.safe_string

val code_julian_date : Config.config -> Date.dmy -> string
