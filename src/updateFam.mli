(* $Id: updateFam.mli,v 2.1 1999-03-08 11:19:21 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

type create = [ Create of sex and option date | Link ];
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
