(* $Id: date.mli,v 5.4 2007-03-14 00:39:57 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;

value code_dmy : config -> dmy -> string;
value string_of_ondate : config -> date -> string;
value string_of_date : config -> date -> string;
value string_slash_of_date : config -> date -> string;
value string_of_age : config -> dmy -> string;
value year_text : dmy -> string;
value short_dates_text : config -> base -> person -> string;
value short_marriage_date_text :
  config -> base -> family -> person -> person -> string;
value print_dates : config -> base -> person -> unit;
value print_calendar : config -> base -> unit;
value get_birth_death_date : person -> (option date * option date * bool);

value before_date : dmy -> dmy -> bool;
  (* [before_date d1 d2] = True if d2 before d1; I know, it is not logical *)

(* return the day of the week given the date as parameter *)
value get_wday : config -> date -> string;
