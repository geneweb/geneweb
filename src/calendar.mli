(* $Id: calendar.mli,v 4.2 2005-02-27 04:29:43 ddr Exp $ *)

open Def;

value gregorian_of_sdn : precision -> int -> dmy;

value sdn_of_gregorian : dmy -> int;
value sdn_of_julian : dmy -> int;
value sdn_of_french : dmy -> int;
value sdn_of_hebrew : dmy -> int;

value gregorian_of_julian : dmy -> dmy;
value julian_of_gregorian : dmy -> dmy;
value gregorian_of_french : dmy -> dmy;
value french_of_gregorian : dmy -> dmy;
value gregorian_of_hebrew : dmy -> dmy;
value hebrew_of_gregorian : dmy -> dmy;

type moon_day =
  [ OrdinaryMoonDay
  | NewMoon of int and int
  | FirstQuarter of int and int
  | FullMoon of int and int
  | LastQuarter of int and int ]
;

value moon_phase_of_sdn : int -> (moon_day * int);
