(* $Id: notesLinks.mli,v 5.1 2006-11-20 03:58:58 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

type page =
  [ PgInd of Def.iper
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
  | WLnone ]
;

value char_dir_sep : char;
value check_file_name : string -> option (list string * string);

value misc_notes_link : string -> int -> wiki_link;

value read_db_from_file : string -> notes_links_db;
value update_db :
  string -> page -> (list string * list (key * ind_link)) -> unit;
