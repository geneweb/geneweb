type cdate = Def.cdate

type date =
  (* dmy is the date in gregorian format;
     calendar is the calendar in which we should display this date;
     e.g. if calendar = Dhebrew, we should convert dmy from gregorian
     to hebrew before printing
  *)
  | Dgreg of dmy * calendar
  (* textual form of the date *)
  | Dtext of string

and calendar = Dgregorian | Djulian | Dfrench | Dhebrew
and dmy = { day : int; month : int; year : int; prec : precision; delta : int }
and dmy2 = { day2 : int; month2 : int; year2 : int; delta2 : int }

and precision =
  | Sure
  | About
  | Maybe
  | Before
  | After
  | OrYear of dmy2
  (* inteval *)
  | YearInt of dmy2

type elapsed_time = { nb_day : int; nb_month : int; nb_year : int }

val leap_year : int -> bool
(** Says if the given year is a leap year. *)

val nb_days_in_month : int -> int -> int
(** Returns number of days for the given month and year for
    gregorian calendar. Takes into account leap years. *)

val time_elapsed : dmy -> dmy -> elapsed_time
(** [time_elapsed start stop]
    Compute the time elapsed between [start] and [stop].
    If [stop] is prior to [start], resulting [dmy]'s field
    are negative (but correct).
    Resulting [prec] can be:
    - [Sure] for exact duration
    - [Before] for "less than" duration
    - [After] for "more than" duration
    - [Maybe] for other cases
    Used to compare only gregorian calendar's dates.
 *)

val time_elapsed_opt : dmy -> dmy -> elapsed_time option
(** Same as [time_elapsed], but will return [None]
    if computation is not possible
    (e.g. time_elapsed_opt /1839 /1859). *)

(* TODO add date_of_burial/event?  *)
val dmy_of_death : Def.death -> dmy option

val date_of_death : Def.death -> date option
(** Returns date of death if present. *)

val dmy_of_dmy2 : dmy2 -> dmy
(** [dmy_of_dmy2 dmy2]
    Convert a [dmy2] to [dmy] using [Sure] as precision. *)

exception Not_comparable
(** [Not_comparable] is raised by [compare_dmy] and [compare_date] when
    [strict] mode is used and precision of dates are incompatibles to
    have a reliable result (e.g. is [compare_dmy 2019 07/2019]) or when
    one of the date in [compare_date] is [Dtext]. *)

val compare_dmy : ?strict:bool -> dmy -> dmy -> int
(** [compare_dmy ?strict d1 d2]
    Return a negative integer if [d1] is prior to [d2],
    [0] if [d1] is equal to [d2],
    and a positive integer if [d2] is prior to [d1].
    [strict] parameter enable or disable strict mode, and
    is false by default (see [Not_comparable])
*)

val compare_dmy_opt : ?strict:bool -> dmy -> dmy -> int option
(** [compare_dmy_opt ?strict d1 d2]
    Same as [compare_dmy], but do not raise an exception
*)

val compare_date : ?strict:bool -> date -> date -> int
(** [compare_date d1 d2]
    If both [d1] and [d2] are [Dgreg] date, uses [compare_dmy]
    to compare them.
    [Dtext] dates are always considered prior to any [Dgreg] date,
    and equal to any other [Dtext] date.
    [strict] parameter enable or disable strict mode, and
    is false by default (see [Not_comparable])
*)

val cdate_None : cdate
(** Absent compressed date *)

val date_of_cdate : cdate -> date
(** Convert [cdate] to [date]; fail if [cdate] is [Cnone]  *)

val od_of_cdate : cdate -> date option
(** Optional date from [cdate] *)

val cdate_to_dmy_opt : cdate -> dmy option
(** [cdate_to_dmy_opt d] is [Some dmy] iff [d] resolve to [Dgreg (dmy,_)] *)

val cdate_of_date : date -> cdate
(** Convert [date] to [cdate] *)

val cdate_of_od : date option -> cdate
(** Optional date to [cdate] *)

(* TODO date_to_dmy? *)

val to_sdn : from:calendar -> dmy -> int
(** Convert a [dmy] in calendar [from] to SDN
    if dmy is a partial date (month|day = 0) then return the SDN of a lower bound *)

val convert : from:calendar -> to_:calendar -> dmy -> dmy
(** [convert ~from ~to_ dmy] Converts a [dmy] from calendar [from] to calendar [to_];
    Correctly convert [dmy.prec]
    Can convert partial date, and recover partial dates after converting them back,
    by using dmy.delta to define a date interval *)

val gregorian_of_sdn : prec:precision -> int -> dmy
(** Convert SDN to [dmy] in gregorian calendar *)

val julian_of_sdn : prec:precision -> int -> dmy
(** Convert SDN to [dmy] in julian calendar *)

val french_of_sdn : prec:precision -> int -> dmy
(** Convert SDN to [dmy] in french calendar *)

val hebrew_of_sdn : prec:precision -> int -> dmy
(** Convert SDN to [dmy] in hebrew calendar *)

val compare_elapsed_time : elapsed_time -> elapsed_time -> int
