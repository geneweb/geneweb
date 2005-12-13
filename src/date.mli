(* $Id: date.mli,v 5.0 2005-12-13 11:51:27 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Config;

value code_dmy : config -> dmy -> string;
value string_of_ondate : config -> date -> string;
value string_of_date : config -> date -> string;
value string_of_age : config -> dmy -> string;
value year_text : dmy -> string;
value short_dates_text : config -> base -> person -> string;
value short_marriage_date_text :
  config -> base -> family -> person -> person -> string;
value print_dates : config -> base -> person -> unit;
value print_calendar : config -> base -> unit;
value get_birth_death_date : person -> (option date * option date * bool);
