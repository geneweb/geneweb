(* $Id: updateFam.mli,v 4.1 2001-06-15 15:25:30 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;

value print_add1 :
  config -> base -> gen_family Update.key string ->
    gen_couple Update.key -> gen_descend Update.key -> bool -> unit;
value print_mod1 :
  config -> base -> gen_family Update.key string ->
    gen_couple Update.key -> gen_descend Update.key -> string -> unit;
value print_merge1 :
  config -> base -> gen_family Update.key string ->
    gen_descend Update.key -> family -> string -> unit;

value print_update_fam :
  config -> base ->
    (gen_family Update.key string * gen_couple Update.key *
     gen_descend Update.key) -> string -> unit;

value print_add : config -> base -> unit;
value print_mod : config -> base -> unit;
value print_del : config -> base -> unit;
value print_swi : config -> base -> unit;
value print_add_parents : config -> base -> unit;

value person_key : base -> iper -> Update.key;
