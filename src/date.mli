(* $Id: date.mli,v 2.4 1999-08-04 04:24:56 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value string_of_ondate : config -> date -> string;
value string_of_date : config -> date -> string;
value print_age : config -> date -> unit;
value afficher_dates : config -> base -> person -> unit;
value year_text : date -> string;
value short_dates_text : config -> base -> person -> string;
value short_marriage_date_text :
  config -> base -> family -> person -> person -> string;

(* Deprecated *)
value afficher_dates_courtes : config -> base -> person -> unit;
value display_year : date -> unit;
(**)
