(* $Id: updateIndOk.mli,v 2.3 1999-07-26 07:02:01 ddr Exp $ *)

open Config;
open Def;

value effective_del : config -> base -> person -> unit;
value effective_mod : config -> base -> gen_person Update.key string -> person;
value all_checks_person :
  config -> base -> person -> ascend -> list Gutil.base_warning;
value print_mod_aux :
  config -> base -> (gen_person Update.key string -> unit) -> unit
;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

