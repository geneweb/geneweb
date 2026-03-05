(** Calendar conversion module.

    All conversions use the Serial Day Number (SDN) as pivot, numerically
    identical to the Julian Day Number (JDN). Reference: Fliegel & Van Flandern,
    1968, Communications of the ACM, vol. 11, n. 10, p. 657.

    Year zero does not exist in the Anno Domini system (1 BC is immediately
    followed by AD 1). Astronomical year numbering (ISO 8601:2019) uses year 0 =
    1 BC; all SDN/JDN formulas assume this convention internally. Callers must
    ensure year <> 0 for Gregorian/Julian inputs; Calendars.make rejects it as
    Invalid_year.

    French Republican calendar uses the astronomical equinox method (Remy
    Pialat), not the Romme arithmetic rule. This matches the original decree
    (Art. III) and Fourmilab's implementation. Valid for An I-XIV (1792-1805);
    accuracy degrades for extrapolated dates due to solar longitude
    approximation precision (~1 arc-minute).

    Hebrew calendar uses integer arithmetic throughout (helek as unit),
    implementing all four dechiyot. Reference values: molad BaHaRaD = 1d 5h
    204p, lunation = 29d 12h 793p. See Reingold & Dershowitz, Calendrical
    Calculations, 4th ed., 2018, Cambridge University Press.

    Partial dates (day=0 or month=0) bypass SDN conversion entirely in
    convert_via_sdn, as SDN cannot encode unknown components. *)
(* Calendar conversions using Calendars library (>= 2.0.0)
   
   This module wraps the external Calendars library to provide
   conversions between Geneweb's Def.dmy type and Serial Day Numbers (SDN).
   
   SDN (Serial Day Number) is a day numbering system where:
   - SDN 1 = November 25, 4714 BC (Gregorian proleptic)
   - Enables calendar-neutral date arithmetic
   - Used for inter-calendar conversions
*)

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

val dmy_of_dmy2 : Def.dmy2 -> Def.dmy
(** Convert [dmy2] to [dmy] with precision [Sure]. *)

val dmy2_of_dmy : Def.dmy -> Def.dmy2
(** Extract [dmy2] fields from [dmy], discarding precision. *)

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
