(* $Id: date.mli,v 3.1 2000-01-10 02:14:38 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Config;

value string_of_ondate : config -> date -> string;
value string_of_date : config -> date -> string;
value print_age : config -> dmy -> unit;
value year_text : dmy -> string;
value short_dates_text : config -> base -> person -> string;
value short_marriage_date_text :
  config -> base -> family -> person -> person -> string;
value print_dates : config -> base ->person -> unit;
value print_calendar : config -> base -> unit;

(* Deprecated *)
value afficher_dates_courtes : config -> base -> person -> unit;
value display_year : dmy -> unit;
(**)
