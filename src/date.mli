(* $Id: date.mli,v 2.1 1999-03-08 11:18:32 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

value string_of_ondate : Config.config -> Def.date -> string;
value string_of_date : Config.config -> Def.date -> string;
value print_age : Config.config -> Def.date -> unit;
value afficher_dates : Config.config -> Def.base -> Def.base_person -> unit;
value display_year : Def.date -> unit;
value afficher_dates_courtes :
  Config.config -> Def.base -> Def.base_person -> unit;
