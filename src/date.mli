(* $Id: date.mli,v 2.6 1999-09-14 22:33:47 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value string_of_ondate : config -> date -> string;
value string_of_date : config -> date -> string;
value print_age : config -> dmy -> unit;
value year_text : dmy -> string;
value short_dates_text : config -> base -> person -> string;
value short_marriage_date_text :
  config -> base -> family -> person -> person -> string;

(* Deprecated *)
value afficher_dates_courtes : config -> base -> person -> unit;
value display_year : dmy -> unit;
(**)
