(* $Id: date.mli,v 5.4 2007-03-14 00:39:57 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb

val leap_year : int -> bool

val nb_days_in_month : int -> int -> int

(** [time_elapsed start stop]
    Compute the time elapsed between [start] and [stop].
    If [stop] is prior to [start], resulting [dmy]'s field
    are negative (but correct). *)
val time_elapsed : Def.dmy -> Def.dmy -> Def.dmy

val date_of_death : Def.death -> Adef.date option

(** [dmy_of_dmy2 dmy2]
    Convert a [dmy2] to [dmy] using [Sure] as precision. *)
val dmy_of_dmy2 : dmy2 -> dmy

(** [get_birth_death p]
    Return [(birth, death, approx)]. If birth/death date can not be found,
    baptism/burial date is used and [approx] is set to [true] (it is [false]
    if both birth and death dates are found).
*)
val get_birth_death_date : person -> date option * date option * bool

(** [compare_dmy d1 d2]
    Return a negative integer if [d1] is prior to [d2],
    [0] if [d1] is equal to [d2],
    and a positive integer if [d2] is prior to [d1].
*)
val compare_dmy : dmy -> dmy -> int

(** [compare_date d1 d2]
    If both [d1] and [d2] are [Dgreg] date, uses [compare_dmy]
    to compare them.
    [Dtext] dates are always considered prior to any [Dgreg] date,
    and equal to any other [Dtext] date.
*)
val compare_date : date -> date -> int
