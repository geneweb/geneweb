(* $Id: updateIndOk.mli,v 5.4 2006-12-25 21:20:16 ddr Exp $ *)

open Config;
open Def;
open Gwdb;

value effective_del : config -> base -> person -> gen_person iper istr;
value effective_mod :
  config -> base -> gen_person Update.key string -> gen_person iper istr;
value all_checks_person :
  config -> base -> person -> ascend -> union -> list CheckItem.base_warning;
value print_mod_aux :
  config -> base -> (gen_person Update.key string -> unit) -> unit;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

