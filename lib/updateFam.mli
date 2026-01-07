(* $Id: updateFam.mli,v 5.4 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

val person_key : Gwdb.base -> Gwdb.iper -> Update.key
(** Returns the update key associated to a person *)

val print_update_fam :
  Config.config ->
  Gwdb.base ->
  (Update.key, Gwdb.ifam, string) Def.gen_family
  * Update.key Def.gen_couple
  * Update.key Def.gen_descend ->
  string ->
  unit
(** The main page for updating families. *)

val print_add : Config.config -> Gwdb.base -> unit
(** Displays the form for adding families *)

val print_mod : Config.config -> Gwdb.base -> unit
(** Displays a form for updating a family *)

val print_del : Config.config -> Gwdb.base -> unit
(** Displays a page for validating the deletion of the family in argument *)

val print_inv : Config.config -> Gwdb.base -> unit
(** Displays a menu for inverting the order of two families *)

val print_add_parents : Config.config -> Gwdb.base -> unit
(** Associates parents to a person *)

val change_order : Gwdb.person -> Gwdb.ifam -> int -> Gwdb.ifam list
(** [change_order p f i] Returns the families of `p` where `f` is at the ith
    position. `i` must not be 0. *)

val print_change_order : Config.config -> Gwdb.base -> unit
(** Displays a menu to change the family order *)

val print_change_event_order : Config.config -> Gwdb.base -> unit
(** Displays the form for changing the order of events for a family *)

val string_family_of :
  Gwdb.base ->
  Gwdb.ifam ->
  (Update.key, Gwdb.ifam, string) Def.gen_family
  * Update.key Def.gen_couple
  * Update.key Def.gen_descend
