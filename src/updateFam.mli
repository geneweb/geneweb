(* $Id: updateFam.mli,v 3.0 1999-10-29 10:31:42 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value print_add1 :
  config -> base -> gen_family Update.key string ->
    gen_couple Update.key -> bool -> unit;
value print_mod1 :
  config -> base -> gen_family Update.key string ->
    gen_couple Update.key -> string -> unit;
value person_key : base -> iper -> Update.key;
value print_family :
  config -> base -> gen_family Update.key string ->
    gen_couple Update.key -> bool -> unit;

value print_add : config -> base -> unit;
value print_mod : config -> base -> unit;
value print_del : config -> base -> unit;
value print_swi : config -> base -> unit;
value print_add_parents : config -> base -> unit;
