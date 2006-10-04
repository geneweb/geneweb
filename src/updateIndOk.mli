(* $Id: updateIndOk.mli,v 5.3 2006-10-04 14:17:54 ddr Exp $ *)

open Config;
open Def;
open Gwdb;

value effective_del : config -> base -> person -> person;
value effective_mod :
  config -> base -> gen_person Update.key string -> person;
value all_checks_person :
  config -> base -> person -> ascend -> union -> list CheckItem.base_warning;
value print_mod_aux :
  config -> base -> (gen_person Update.key string -> unit) -> unit;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

