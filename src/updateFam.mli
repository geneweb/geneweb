(* $Id: updateFam.mli,v 2.3 1999-07-28 09:48:31 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

type str_indi = (string * string * int * Update.create);

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
