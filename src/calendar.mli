(* $Id: calendar.mli,v 4.1 2001-04-10 23:10:28 ddr Exp $ *)

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
