(* $Id: updateFam.mli,v 1.1.1.1 1998-09-01 14:32:07 ddr Exp $ *)

open Def;
open Config;

type create = [ Create of sexe | Link ];
type str_indi = (string * string * int * create);

value print_add1 :
  config -> base -> family str_indi string -> couple str_indi -> bool -> unit;
value print_mod1 :
  config -> base -> family str_indi string -> couple str_indi -> string ->
    unit;
value person_key : base -> iper -> str_indi;
value print_family :
  config -> base -> family str_indi string -> couple str_indi -> bool -> unit;

value print_add : config -> base -> unit;
value print_mod : config -> base -> unit;
value print_del : config -> base -> unit;
value print_swi : config -> base -> unit;
value print_add_parents : config -> base -> unit;
