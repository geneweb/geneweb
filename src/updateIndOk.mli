(* $Id: updateIndOk.mli,v 2.2 1999-04-05 23:42:30 ddr Exp $ *)

open Config;
open Def;

value effective_del : config -> base -> person -> unit;
value effective_mod : config -> base -> gen_person string -> person;
value all_checks_person :
  config -> base -> person -> ascend -> list Gutil.base_warning;
value print_mod_aux : config -> base -> (gen_person string -> unit) -> unit;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

