(* $Id: date.mli,v 1.2 1999-02-02 10:24:04 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

value string_of_ondate : Config.config -> Def.date -> string;
value string_of_date : Config.config -> Def.date -> string;
value print_age : Config.config -> Def.date -> unit;
value afficher_dates : Config.config -> Def.base -> Def.base_person -> unit;
value display_year : Def.date -> unit;
value afficher_dates_courtes :
  Config.config -> Def.base -> Def.base_person -> unit;
