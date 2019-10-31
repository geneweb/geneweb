val gregorian_of_sdn : Def.precision -> int -> Def.dmy
val julian_of_sdn : Def.precision -> int -> Def.dmy
val french_of_sdn : Def.precision -> int -> Def.dmy
val hebrew_of_sdn : Def.precision -> int -> Def.dmy

val sdn_of_gregorian : Def.dmy -> int
val sdn_of_julian : Def.dmy -> int
val sdn_of_french : Def.dmy -> int
val sdn_of_hebrew : Def.dmy -> int

val gregorian_of_julian : Def.dmy -> Def.dmy
val julian_of_gregorian : Def.dmy -> Def.dmy
val gregorian_of_french : Def.dmy -> Def.dmy
val french_of_gregorian : Def.dmy -> Def.dmy
val gregorian_of_hebrew : Def.dmy -> Def.dmy
val hebrew_of_gregorian : Def.dmy -> Def.dmy

type moon_phase = NewMoon | FirstQuarter | FullMoon | LastQuarter

val moon_phase_of_sdn : int -> (moon_phase * int * int) option * int
