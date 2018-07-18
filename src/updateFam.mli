(* $Id: updateFam.mli,v 5.4 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

val person_key : base -> iper -> Update.key

val print_update_fam :
  config -> base ->
    (Update.key, string) gen_family * Update.key gen_couple *
      Update.key gen_descend ->
    string -> unit

val print_add : config -> base -> unit
val print_mod : config -> base -> unit
val print_del : config -> base -> unit
val print_inv : config -> base -> unit
val print_add_parents : config -> base -> unit

val change_order :
  config -> base -> iper -> person -> ifam -> int -> ifam list
val print_change_order : config -> base -> unit
val print_change_event_order : config -> base -> unit

val person_key : base -> iper -> Update.key
val string_family_of :
  config -> base -> ifam ->
    (Update.key, string) gen_family * Update.key gen_couple *
      Update.key gen_descend
