(* $Id: calendar.mli,v 5.0 2005-12-13 11:51:27 ddr Exp $ *)

open Def;

value gregorian_of_sdn : precision -> int -> dmy;
value julian_of_sdn : precision -> int -> dmy;
value french_of_sdn : precision -> int -> dmy;
value hebrew_of_sdn : precision -> int -> dmy;

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

type moon_phase =
  [ NewMoon
  | FirstQuarter
  | FullMoon
  | LastQuarter ]
;

value moon_phase_of_sdn : int -> (option (moon_phase * int * int) * int);
