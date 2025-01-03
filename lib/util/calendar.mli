val gregorian_of_sdn : Def.precision -> int -> Def.dmy
(** Returns date of gregorian calendar from SDN and specified precision. *)

val julian_of_sdn : Def.precision -> int -> Def.dmy
(** Returns date of julian calendar from SDN and specified precision. *)

val french_of_sdn : Def.precision -> int -> Def.dmy
(** Returns date of french calendar from SDN and specified precision. *)

val hebrew_of_sdn : Def.precision -> int -> Def.dmy
(** Returns date of hebrew calendar from SDN and specified precision. *)

val sdn_of_gregorian : Def.dmy -> int
(** Returns SDN of the date of gregorian calendar. *)

val sdn_of_julian : Def.dmy -> int
(** Returns SDN of the date of julian calendar. *)

val sdn_of_french : Def.dmy -> int
(** Returns SDN of the date of french calendar. *)

val sdn_of_hebrew : Def.dmy -> int
(** Returns SDN of the date of hebrew calendar. *)

val gregorian_of_julian : Def.dmy -> Def.dmy
(** Converts julian calendar's date to gregorian. *)

val julian_of_gregorian : Def.dmy -> Def.dmy
(** Converts gregorian calendar's date to julian date. *)

val gregorian_of_french : Def.dmy -> Def.dmy
(** Converts french calendar's date to gregorian date. *)

val french_of_gregorian : Def.dmy -> Def.dmy
(** Converts gregorian calendar's date to french date. *)

val gregorian_of_hebrew : Def.dmy -> Def.dmy
(** Converts hebrew calendar's date to gregorian date. *)

val hebrew_of_gregorian : Def.dmy -> Def.dmy
(** Converts gregorian calendar's date to hebrew date. *)

(** Moon phases *)
type moon_phase = NewMoon | FirstQuarter | FullMoon | LastQuarter

val moon_phase_of_sdn : int -> (moon_phase * int * int) option * int
(** Returns information about moon phase from the given SDN. Result
    [(Some (mph,h,m), day)] describes moon phase [mph], hour [h] and minute [m]
    when this phase appears and days [day] since last New Moon phase (moon's
    age). If result is [(None,_)], it tells that there wasn't any moon's phase
    (one of the mentionned in [moon_phase]) this day. *)
