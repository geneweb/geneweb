(* $Id: updateFam.mli,v 2.4 1999-07-28 13:08:33 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value print_add1 :
  config -> base -> gen_family Update.key string -> gen_couple Update.key ->
    bool -> unit;
value print_mod1 :
  config -> base -> gen_family Update.key string -> gen_couple Update.key ->
    string -> unit;
value person_key : base -> iper -> Update.key;
value print_family :
  config -> base -> gen_family Update.key string -> gen_couple Update.key ->
    bool -> unit;

value print_add : config -> base -> unit;
value print_mod : config -> base -> unit;
value print_del : config -> base -> unit;
value print_swi : config -> base -> unit;
value print_add_parents : config -> base -> unit;
