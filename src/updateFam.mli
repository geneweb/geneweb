(* $Id: updateFam.mli,v 3.2 2000-01-10 02:14:42 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Config;

value print_add1 :
  config -> base -> gen_family Update.key string ->
    gen_couple Update.key -> gen_descend Update.key -> bool -> unit;
value print_mod1 :
  config -> base -> gen_family Update.key string ->
    gen_couple Update.key -> gen_descend Update.key -> string -> unit;
value person_key : base -> iper -> Update.key;
value print_family :
  config -> base -> gen_family Update.key string ->
    gen_couple Update.key -> gen_descend Update.key -> bool -> unit;

value print_add : config -> base -> unit;
value print_mod : config -> base -> unit;
value print_del : config -> base -> unit;
value print_swi : config -> base -> unit;
value print_add_parents : config -> base -> unit;
