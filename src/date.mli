(* $Id: date.mli,v 2.2 1999-04-05 23:42:27 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value string_of_ondate : config -> date -> string;
value string_of_date : config -> date -> string;
value print_age : config -> date -> unit;
value afficher_dates : config -> base -> person -> unit;
value display_year : date -> unit;
value afficher_dates_courtes : config -> base -> person -> unit;
