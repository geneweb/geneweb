(* $Id: updateInd.mli,v 5.4 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def

val string_person_of :
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  (Geneweb_db.Driver.iper, Update.key, string) gen_person

val print_update_ind :
  config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, Update.key, string) gen_person ->
  string ->
  unit
(** The main HTML page displayed after an update. Based on template updind.txt
*)

val print_add : config -> Geneweb_db.Driver.base -> unit
(** Displays an HTML form with empty fields for adding a person *)

val print_del : config -> Geneweb_db.Driver.base -> unit
(** Displays a page for validating the deletion of a person *)

val print_mod : config -> Geneweb_db.Driver.base -> unit
(** Displays a form for updating a person *)

val print_change_event_order : config -> Geneweb_db.Driver.base -> unit
(** Displays the form for changing the order of events for a person *)
