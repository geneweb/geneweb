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

val compare_dmy : dmy -> dmy -> int
(** [compare_dmy d1 d2] Compare two dates as points on a timeline. Return a
    negative integer if [d1] is prior to [d2], [0] if [d1] is equal to [d2], and
    a positive integer if [d2] is prior to [d1]. Date precision is handled
    permissively: dates with uncertain precision ([About], [Maybe]) are
    comparable with precise dates. This function always returns a result and can
    be used for sorting. *)

val compare_dmy_strict : dmy -> dmy -> int option
(** [compare_dmy_strict d1 d2] Compare two dates with strict precision handling.
    Return [None] if dates cannot be reliably compared due to incompatible
    precision (e.g. comparing [2019] with [07/2019], or [Before 1850] with
    [After 1840]). Return [Some x] with the same semantics as [compare_dmy] when
    comparison is meaningful. Do not use for sorting lists as it may return
    [None]. *)

val compare_date : date -> date -> int
(** [compare_date d1 d2] Compare two dates using [compare_dmy] if both are
    [Dgreg] dates. [Dtext] dates are always considered prior to any [Dgreg]
    date, and equal to any other [Dtext] date. Always returns a result and can
    be used for sorting. *)

val compare_date_strict : date -> date -> int option
(** [compare_date_strict d1 d2] Strict comparison of dates. Return [None] if
    dates are not reliably comparable (different types, or incompatible
    precision for [Dgreg] dates). Return [Some x] with same semantics as
    [compare_date] otherwise. Do not use for sorting lists. *)

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
