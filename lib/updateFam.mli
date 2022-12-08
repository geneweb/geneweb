(* $Id: updateFam.mli,v 5.4 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

val person_key : base -> iper -> Update.key
(** Returns the update key associated to a person *)

val print_update_fam :
  config ->
  base ->
  (Update.key, ifam, string) gen_family
  * Update.key gen_couple
  * Update.key gen_descend ->
  string ->
  unit
(** The main page for updating families. *)

val print_add : config -> base -> unit
(** Displays the form for adding families *)

val print_mod : config -> base -> unit
(** Displays a form for updating a family *)

val print_del : config -> base -> unit
(** Displays a page for validating the deletion of the family in argument *)

val print_inv : config -> base -> unit
(** Displays a menu for inverting the order of two families *)

val print_add_parents : config -> base -> unit
(** Associates parents to a person *)

val change_order : person -> ifam -> int -> ifam list
(** [change_order p f i]
    Returns the families of `p` where `f` is at the ith position.
    `i` must not be 0. *)

val print_change_order : config -> base -> unit
(** Displays a menu to change the family order *)

val print_change_event_order : config -> base -> unit
(** Displays the form for changing the order of events for a family *)

val string_family_of :
  config ->
  base ->
  ifam ->
  (Update.key, ifam, string) gen_family
  * Update.key gen_couple
  * Update.key gen_descend
