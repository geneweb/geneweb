(* $Id: date.mli,v 2.3 1999-04-17 14:18:04 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value string_of_ondate : config -> date -> string;
value string_of_date : config -> date -> string;
value print_age : config -> date -> unit;
value afficher_dates : config -> base -> person -> unit;
value year_text : date -> string;
value short_dates_text : config -> base -> person -> string;

(* Deprecated *)
value afficher_dates_courtes : config -> base -> person -> unit;
value display_year : date -> unit;
(**)
