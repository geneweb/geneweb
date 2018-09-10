(* $Id: notesLinks.mli,v 5.5 2007-03-25 11:30:05 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

type page =
    PgInd of Def.iper
  | PgFam of Def.ifam
  | PgNotes
  | PgMisc of string
  | PgWizard of string
type key = string * string * int
type ind_link = { lnTxt : string option; lnPos : int }
type notes_links_db = (page * (string list * (key * ind_link) list)) list
type wiki_link =
    WLpage of int * (string list * string) * string * string * string
  | WLperson of int * key * string * string option
  | WLwizard of int * string * string
  | WLnone

val char_dir_sep : char
val check_file_name : string -> (string list * string) option

val misc_notes_link : string -> int -> wiki_link

val read_db_from_file : string -> notes_links_db
val update_db : string -> page -> string list * (key * ind_link) list -> unit

val read_db : string -> notes_links_db
val write_db : string -> notes_links_db -> unit
val add_in_db :
  notes_links_db -> page -> string list * (key * ind_link) list ->
    notes_links_db
