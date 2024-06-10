type d = { day : int; month : int; year : int; delta : int }

val gregorian_of_sdn : int -> d
val julian_of_sdn : int -> d
val french_of_sdn : int -> d
val hebrew_of_sdn : int -> d
val sdn_of_gregorian : d -> int
val sdn_of_julian : d -> int
val sdn_of_french : d -> int
val sdn_of_hebrew : d -> int
val gregorian_of_julian : d -> d
val julian_of_gregorian : d -> d
val gregorian_of_french : d -> d
val french_of_gregorian : d -> d
val gregorian_of_hebrew : d -> d
val hebrew_of_gregorian : d -> d

type moon_phase = NewMoon | FirstQuarter | FullMoon | LastQuarter

val moon_phase_of_sdn : int -> (moon_phase * int * int) option * int
