(* camlp4r *)
(* $Id: notesLinks.ml,v 1.6 2005-07-15 13:00:13 ddr Exp $ *)

open Def;

value magic_notes_links = "GWNL0004";
type page =
  [ PgInd of iper
  | PgNotes
  | PgMisc of string
  | PgWizard of string ]
;
type notes_links_db = list (page * list string);

value check_file_name s =
  loop 0 where rec loop i =
    if i = String.length s then True
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> loop (i + 1)
      | _ -> False ]
;

value misc_notes_link s i =
  let slen = String.length s in
  if i < slen - 2 && s.[i] = '[' && s.[i+1] = '[' && s.[i+2] = '[' then
    let j =
      loop (i + 3) where rec loop j =
        if j = slen then j
        else if
          j < slen - 2 && s.[j] = ']' && s.[j+1] = ']' && s.[j+2] = ']'
        then j + 3
        else loop (j + 1)
    in
    if j > i + 6 then
      let b = String.sub s (i + 3) (j - i - 6) in
      let (fname, sharp, text) =
        try
          let k = String.index b '/' in
          let j = try String.index b '#' with [ Not_found -> k ] in
          (String.sub b 0 j, String.sub b j (k - j),
           String.sub b (k + 1) (String.length b - k - 1))
        with
        [ Not_found -> (b, "", b) ]
      in
      if check_file_name fname then Some (j, fname, sharp, text)
      else None
    else None
  else None
;

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
