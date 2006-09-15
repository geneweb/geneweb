(* $Id: updateFam.mli,v 5.2 2006-09-15 11:45:37 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;
open Def;
open Gwdb;

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
