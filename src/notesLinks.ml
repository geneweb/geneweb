(* camlp4r *)
(* $Id: notesLinks.ml,v 1.2 2005-07-05 01:06:25 ddr Exp $ *)

open Def;

value magic_notes_links = "GWNL0003";
type page =
  [ PgInd of iper
  | PgNotes
  | PgMisc of string ]
;
type notes_links_db = list (page * list string);

value read_db_from_file fname =
  match try Some (open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      do {
        let b = String.create (String.length magic_notes_links) in
        really_input ic b 0 (String.length b);
        let r =
          if b <> magic_notes_links then []
          else try (input_value ic : notes_links_db) with _ -> []
        in
        close_in ic;
        r
      }
  | None -> [] ]
;

value update_db bdir who list =
  let fname = Filename.concat bdir "notes_links" in
  let notes_links_db = read_db_from_file fname in
  let db = List.remove_assoc who notes_links_db in
  let new_db = if list = [] then db else [(who, list) :: db] in
  let oc = open_out_bin fname in
  do {
    output_string oc magic_notes_links;
    output_value oc (new_db : notes_links_db);
    close_out oc;
  }
;
