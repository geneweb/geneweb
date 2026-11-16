(* $Id: updateInd.mli,v 5.4 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def

val string_person_of :
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  (Geneweb_db.Driver.iper, Update.key, string) gen_person

val print_update_ind :
  ?snapshot:string ->
  config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, Update.key, string) gen_person ->
  string ->
  unit
(** [print_update_ind ?snapshot conf base p digest] renders the person form
    [updind.txt] filled with [p]. [digest] and [snapshot] (from
    {!UpdateConflict.person_snapshot}, none by default) are posted back to
    detect and resolve the changes saved meanwhile by other forms. *)

val print_del : config -> Geneweb_db.Driver.base -> unit
(** Displays a page for validating the deletion of a person *)

val print_mod : config -> Geneweb_db.Driver.base -> unit
(** Displays a form for updating a person *)
