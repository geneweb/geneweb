(* $Id: calendar.mli,v 5.0 2005-12-13 11:51:27 ddr Exp $ *)

open Def

val gregorian_of_sdn : precision -> int -> dmy
val julian_of_sdn : precision -> int -> dmy
val french_of_sdn : precision -> int -> dmy
val hebrew_of_sdn : precision -> int -> dmy

val sdn_of_gregorian : dmy -> int
val sdn_of_julian : dmy -> int
val sdn_of_french : dmy -> int
val sdn_of_hebrew : dmy -> int

val gregorian_of_julian : dmy -> dmy
val julian_of_gregorian : dmy -> dmy
val gregorian_of_french : dmy -> dmy
val french_of_gregorian : dmy -> dmy
val gregorian_of_hebrew : dmy -> dmy
val hebrew_of_gregorian : dmy -> dmy

type moon_phase = NewMoon | FirstQuarter | FullMoon | LastQuarter

val moon_phase_of_sdn : int -> (moon_phase * int * int) option * int
