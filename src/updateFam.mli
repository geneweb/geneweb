(* $Id: updateFam.mli,v 4.3 2001-11-16 13:38:54 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;

value person_key : base -> iper -> Update.key;

value print_update_fam :
  config -> base ->
    (gen_family Update.key string * gen_couple Update.key *
     gen_descend Update.key) -> string -> unit;

value print_add : config -> base -> unit;
value print_mod : config -> base -> unit;
value print_del : config -> base -> unit;
value print_inv : config -> base -> unit;
value print_add_parents : config -> base -> unit;

value person_key : base -> iper -> Update.key;
