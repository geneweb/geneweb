(* Copyright (c) 1998-2007 INRIA *)

open Def

val leap_year : int -> bool
(** Says if the given year is a leap year. *)

val nb_days_in_month : int -> int -> int
(** Returns number of days for the given month and year for gregorian calendar.
    Takes into account leap years. *)

val time_elapsed : Def.dmy -> Def.dmy -> Def.dmy
(** [time_elapsed start stop] Compute the time elapsed between [start] and
    [stop]. If [stop] is prior to [start], resulting [dmy]'s field are negative
    (but correct). Resulting [prec] can be:
    - [Sure] for exact duration
    - [Before] for "less than" duration
    - [After] for "more than" duration
    - [Maybe] for other cases Used to compare only gregorian calendar's dates.
*)

val time_elapsed_opt : Def.dmy -> Def.dmy -> Def.dmy option
(** Same as [time_elapsed], but will return [None] if computation is not
    possible (e.g. time_elapsed_opt /1839 /1859). *)

(* TODO add date_of_burial/event?  *)
val dmy_of_death : Def.death -> Adef.dmy option

val date_of_death : Def.death -> Adef.date option
(** Returns date of death if present. *)

val dmy_of_dmy2 : dmy2 -> dmy
(** [dmy_of_dmy2 dmy2] Convert a [dmy2] to [dmy] using [Sure] as precision. *)

exception Not_comparable
(** [Not_comparable] is raised by [compare_dmy] and [compare_date] when [strict]
    mode is used and precision of dates are incompatibles to have a reliable
    result (e.g. is [compare_dmy 2019 07/2019]) or when one of the date in
    [compare_date] is [Dtext]. *)

val compare_dmy : ?strict:bool -> dmy -> dmy -> int
(** [compare_dmy ?strict d1 d2] Return a negative integer if [d1] is prior to
    [d2], [0] if [d1] is equal to [d2], and a positive integer if [d2] is prior
    to [d1]. [strict] parameter enable or disable strict mode, and is false by
    default (see [Not_comparable]) *)

val compare_dmy_opt : ?strict:bool -> dmy -> dmy -> int option
(** [compare_dmy_opt ?strict d1 d2] Same as [compare_dmy], but do not raise an
    exception *)

val compare_date : ?strict:bool -> date -> date -> int
(** [compare_date d1 d2] If both [d1] and [d2] are [Dgreg] date, uses
    [compare_dmy] to compare them. [Dtext] dates are always considered prior to
    any [Dgreg] date, and equal to any other [Dtext] date. [strict] parameter
    enable or disable strict mode, and is false by default (see
    [Not_comparable]) *)

val cdate_None : cdate
(** Absent compressed date *)

val date_of_cdate : cdate -> date
(** Convert [cdate] to [date]; fail if [cdate] is [Cnone] *)

val od_of_cdate : cdate -> date option
(** Optional date from [cdate] *)

val cdate_to_dmy_opt : cdate -> dmy option
(** [cdate_to_dmy_opt d] is [Some dmy] iff [d] resolve to [Dgreg (dmy,_)] *)

val cdate_of_date : date -> cdate
(** Convert [date] to [cdate] *)

val cdate_of_od : date option -> cdate
(** Optional date to [cdate] *)

(* TODO date_to_dmy? *)
