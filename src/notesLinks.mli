(* $Id: notesLinks.mli,v 5.5 2007-03-25 11:30:05 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

type page =
  [ PgInd of Def.iper
  | PgFam of Def.ifam
  | PgNotes
  | PgMisc of string
  | PgWizard of string ]
;
type key = (string * string * int);
type ind_link = { lnTxt : option string; lnPos : int };
type notes_links_db = list (page * (list string * list (key * ind_link)));
type wiki_link =
  [ WLpage of int and (list string * string) and string and string and string
  | WLperson of int and key and string and option string
  | WLwizard of int and string and string
  | WLnone ]
;

value char_dir_sep : char;
value check_file_name : string -> option (list string * string);

value misc_notes_link : string -> int -> wiki_link;

value read_db_from_file : string -> notes_links_db;
value update_db :
  string -> page -> (list string * list (key * ind_link)) -> unit;

value read_db : string -> notes_links_db;
value write_db : string -> notes_links_db -> unit;
value add_in_db :
  notes_links_db -> page -> (list string * list (key * ind_link)) ->
    notes_links_db;
