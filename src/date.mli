(* $Id: date.mli,v 3.4 2001-03-01 08:21:26 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;

value code_dmy : config -> dmy -> string;
value string_of_ondate : config -> date -> string;
value string_of_date : config -> date -> string;
value print_age : config -> dmy -> unit;
value year_text : dmy -> string;
value short_dates_text : config -> base -> person -> string;
value short_marriage_date_text :
  config -> base -> family -> person -> person -> string;
value print_dates : config -> base -> person -> unit;
value print_calendar : config -> base -> unit;
value get_birth_death_date : person -> (option date * option date * bool);

(* Deprecated *)
value afficher_dates_courtes : config -> base -> person -> unit;
value display_year : dmy -> unit;
(**)
