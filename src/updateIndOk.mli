(* $Id: updateIndOk.mli,v 2.1 1999-03-08 11:19:29 ddr Exp $ *)

open Config;
open Def;

value effective_del : config -> base -> base_person -> unit;
value effective_mod : config -> base -> person string -> base_person;
value all_checks_person :
  config -> base -> base_person -> ascend -> list Gutil.base_warning;
value print_mod_aux : config -> base -> (person string -> unit) -> unit;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

