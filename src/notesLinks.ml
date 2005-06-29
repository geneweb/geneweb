(* camlp4r *)
(* $Id: notesLinks.ml,v 1.1 2005-06-29 12:24:01 ddr Exp $ *)

value magic_notes_links = "GWNL0002";
type notes_links_db = list (int * list string);

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
  let new_db = [(who, list) :: List.remove_assoc who notes_links_db] in
  let oc = open_out_bin fname in
  do {
    output_string oc magic_notes_links;
    output_value oc (new_db : notes_links_db);
    close_out oc;
  }
;
