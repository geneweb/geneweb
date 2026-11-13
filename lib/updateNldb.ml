(* Build the notes links database (notes_links) and the linked pages cache
   by scanning every place of a base where wiki links may appear: base notes,
   wizard notes, misc notes (notes_d), and the notes, sources and witness
   notes of persons, families and their events.

   Person and family texts are gathered with
   [Notes.notes_bearing_text_of_person/_family], the same functions used when
   a person or family is saved in gwd: a full rebuild and an incremental
   update thus always index exactly the same fields.

   Shared by [gwc] (right after database creation) and the standalone
   [update_nldb] tool. *)

module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Collection = Geneweb_db.Collection

let is_note_file file =
  String.length file > 0
  && file.[0] <> '.'
  && file.[String.length file - 1] <> '~'

let compute ?(verbose = true) ?(debug = false) ?(progress = true) base bdir =
  let bdir =
    if Filename.check_suffix bdir ".gwb" then bdir else bdir ^ ".gwb"
  in
  let section s =
    if verbose then (
      Printf.eprintf "--- %s\n" s;
      flush stderr)
  in
  let item fmt =
    Printf.ksprintf
      (fun s ->
        if verbose then (
          prerr_string s;
          flush stderr))
      fmt
  in
  let nb_ind = Driver.nb_of_persons base in
  let nb_fam = Driver.nb_of_families base in
  (* Each page is visited exactly once during a full scan, so entries can be
     consed directly: [NotesLinks.add_in_db] would do a [List.remove_assoc]
     over the whole db for each page, which is quadratic on large bases. *)
  let db = ref [] in
  let cache_linked_pages = Hashtbl.create 1024 in
  let update_cache_linked_pages key =
    let n = Option.value ~default:0 (Hashtbl.find_opt cache_linked_pages key) in
    Hashtbl.replace cache_linked_pages key (n + 1)
  in
  let add_page pg ((list_nt, list_ind) as list) =
    if list_nt <> [] || list_ind <> [] then (
      db := (pg, list) :: !db;
      List.iter update_cache_linked_pages
        (List.sort_uniq compare (List.map fst list_ind)))
  in
  let bar_start () = if progress then ProgrBar.start () in
  let bar_run i n = if progress then ProgrBar.run i n in
  let bar_finish () = if progress then ProgrBar.finish () in

  section "database notes";
  add_page Def.NLDB.PgNotes
    (Notes.links_of_text (Driver.base_notes_read base ""));

  section "wizard notes";
  (match Sys.readdir (Filename.concat bdir (Driver.base_wiznotes_dir base)) with
  | exception Sys_error _ ->
      (* No wiznotes directory is the normal case for a fresh base. *)
      ()
  | files ->
      Array.iter
        (fun file ->
          try
            if is_note_file file && Filename.check_suffix file ".txt" then
              let wizid = Filename.chop_suffix file ".txt" in
              match
                Notes.links_of_text (Driver.base_wiznotes_read base wizid)
              with
              | [], [] -> ()
              | list ->
                  item "%s... " wizid;
                  add_page (Def.NLDB.PgWizard wizid) list
          with Sys_error _ ->
            Printf.eprintf "Warning: error while reading wizardnotes %s\n%!"
              file)
        files;
      item "\n");

  section "misc notes";
  let ndir = Filename.concat bdir (Driver.base_notes_dir base) in
  let rec loop dir name =
    try
      let files = Sys.readdir (Filename.concat ndir dir) in
      Array.iter
        (fun file ->
          if is_note_file file then
            if Filename.check_suffix file ".txt" then (
              let fnotes = Filename.chop_suffix file ".txt" in
              match
                Notes.links_of_text
                  (Driver.base_notes_read base (Filename.concat dir fnotes))
              with
              | [], [] -> ()
              | list ->
                  let fnotes =
                    if name = "" then fnotes
                    else
                      Printf.sprintf "%s%c%s" name NotesLinks.char_dir_sep
                        fnotes
                  in
                  item "%s...\n" fnotes;
                  add_page (Def.NLDB.PgMisc fnotes) list)
            else
              loop (Filename.concat dir file)
                (if name = "" then file
                 else Printf.sprintf "%s%c%s" name NotesLinks.char_dir_sep file))
        files
    with Sys_error _ ->
      Printf.eprintf "Warning: error while reading misc notes %s\n%!" name
  in
  loop Filename.current_dir_name "";

  ProgrBar.full := '*';

  section "individual notes";
  bar_start ();
  Collection.iteri
    (fun i p ->
      if debug then
        Printf.eprintf "Person: (%d) %s\n" i (Gutil.designation base p);
      bar_run i nb_ind;
      add_page
        (Def.NLDB.PgInd (Driver.get_iper p))
        (Notes.links_of_text
           (Notes.notes_bearing_text_of_person base
              (Driver.gen_person_of_person p))))
    (Driver.persons base);
  bar_finish ();

  section "families notes";
  bar_start ();
  Collection.iteri
    (fun i fam ->
      (if debug then
         let fath = Driver.poi base (Driver.get_father fam) in
         Printf.eprintf "Family: (%d) %s\n" i (Gutil.designation base fath));
      bar_run i nb_fam;
      add_page
        (Def.NLDB.PgFam (Driver.get_ifam fam))
        (Notes.links_of_text
           (Notes.notes_bearing_text_of_family base
              (Driver.gen_family_of_family fam))))
    (Driver.families base);
  bar_finish ();

  Driver.write_nldb base !db;
  Notes.save_cache_linked_pages bdir cache_linked_pages
