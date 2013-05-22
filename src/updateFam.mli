(* $Id: updateFam.mli,v 5.4 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

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

value change_order : 
  config -> base -> iper -> person -> ifam -> int -> list ifam;
value print_change_order : config -> base -> unit;

value person_key : base -> iper -> Update.key;
value string_family_of :
  config -> base -> ifam ->
    (gen_family Update.key string * gen_couple Update.key *
     gen_descend Update.key);
