(* $Id: updateFam.mli,v 4.4 2004-12-14 09:30:18 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

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
