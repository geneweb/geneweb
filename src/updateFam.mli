(* $Id: updateFam.mli,v 2.2 1999-04-05 23:42:29 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

type create = [ Create of sex and option date | Link ];
type str_indi = (string * string * int * create);

value print_add1 :
  config -> base -> gen_family str_indi string -> gen_couple str_indi ->
    bool -> unit;
value print_mod1 :
  config -> base -> gen_family str_indi string -> gen_couple str_indi ->
    string -> unit;
value person_key : base -> iper -> str_indi;
value print_family :
  config -> base -> gen_family str_indi string -> gen_couple str_indi ->
    bool -> unit;

value print_add : config -> base -> unit;
value print_mod : config -> base -> unit;
value print_del : config -> base -> unit;
value print_swi : config -> base -> unit;
value print_add_parents : config -> base -> unit;
