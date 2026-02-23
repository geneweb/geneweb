(* $Id: updateInd.mli,v 5.4 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

val string_person_of :
  Gwdb.base -> Gwdb.person -> (Gwdb.iper, Update.key, string) Def.gen_person

val print_update_ind :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper, Update.key, string) Def.gen_person ->
  string ->
  unit
(** The main HTML page displayed after an update. Based on template updind.txt
*)

val print_add : Config.config -> Gwdb.base -> unit
(** Displays an HTML form with empty fields for adding a person *)

val print_del : Config.config -> Gwdb.base -> unit
(** Displays a page for validating the deletion of a person *)

val print_mod : Config.config -> Gwdb.base -> unit
(** Displays a form for updating a person *)

val print_change_event_order : Config.config -> Gwdb.base -> unit
(** Displays the form for changing the order of events for a person *)
