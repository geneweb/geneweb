(* $Id: updateIndOk.mli,v 5.6 2008-01-21 13:28:12 ddr Exp $ *)

open Config;
open Def;
open Gwdb;

value effective_del :
  config -> base -> (CheckItem.base_warning -> unit) -> person ->
    gen_person iper istr;
value effective_mod :
  config -> base -> gen_person Update.key string -> gen_person iper istr;
value all_checks_person :
  config -> base -> gen_person iper istr -> gen_ascend ifam ->
    gen_union ifam -> list CheckItem.base_warning;
value print_mod_aux :
  config -> base -> (gen_person Update.key string -> unit) -> unit;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

