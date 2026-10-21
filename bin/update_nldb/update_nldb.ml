open Geneweb
open Def
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Collection = Geneweb_db.Collection
module Dirs = Geneweb_dirs

let debug = ref false
let bases_dir = ref None
let set_bases_dir s = bases_dir := Some s

let get_bases_dir () =
  match !bases_dir with
  | Some s -> s
  | None -> Dirs.path Secure.default_base_dir

let parse_cmd () =
  let fname = ref "" in
  let errmsg = Format.sprintf "usage: %s [options] <file_name>" Sys.argv.(0) in

  let speclist =
    [
      ( "-bd",
        Arg.String set_bases_dir,
        Fmt.str
          "<DIR> Specify where the bases directory with databases is installed \
           (default if empty is %S)."
          (Dirs.name Secure.default_base_dir) );
      ("-debug", Arg.Set debug, " Debug mode.");
    ]
    |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
    |> Arg.align
  in
  let anonfun s =
    if !fname = "" then fname := s
    else raise (Arg.Bad "Cannot treat several databases")
  in
  Arg.parse speclist anonfun errmsg;
  (!fname, get_bases_dir ())

let compute base bdir =
  let bdir =
    if Filename.check_suffix bdir ".gwb" then bdir else bdir ^ ".gwb"
  in
  let nb_ind = Driver.nb_of_persons base in
  let nb_fam = Driver.nb_of_families base in
  let db = ref [] in
  let cache_linked_pages = Hashtbl.create 1024 in

  let update_cache_linked_pages key =
    let current_count =
      try Hashtbl.find cache_linked_pages key with Not_found -> 0
    in
    Hashtbl.replace cache_linked_pages key (current_count + 1)
  in

  let add_page pg ((_, list_ind) as list) =
    db := NotesLinks.add_in_db !db pg list;
    List.iter update_cache_linked_pages
      (List.sort_uniq compare (List.map fst list_ind))
  in

  Printf.eprintf "--- database notes\n";
  flush stderr;
  (match Notes.links_of_text (Driver.base_notes_read base "") with
  | [], [] -> ()
  | list -> add_page NLDB.PgNotes list);

  Printf.eprintf "--- wizard notes\n";
  flush stderr;
  (try
     let files =
       Sys.readdir (Filename.concat bdir (Driver.base_wiznotes_dir base))
     in
     for i = 0 to Array.length files - 1 do
       try
         let file = files.(i) in
         if
           String.length file > 0
           && file.[0] <> '.'
           && file.[String.length file - 1] <> '~'
         then
           if Filename.check_suffix file ".txt" then
             let wizid = Filename.chop_suffix file ".txt" in
             match
               Notes.links_of_text (Driver.base_wiznotes_read base wizid)
             with
             | [], [] -> ()
             | list ->
                 Printf.eprintf "%s... " wizid;
                 flush stderr;
                 add_page (NLDB.PgWizard wizid) list
       with Sys_error _ ->
         Printf.eprintf "Warning: error while reading wizardnotes %s\n"
           files.(i)
     done;
     Printf.eprintf "\n";
     flush stderr
   with Sys_error _ ->
     Printf.eprintf "Warning: error while reading wizardnotes dir\n");

  Printf.eprintf "--- misc notes\n";
  flush stderr;
  let ndir = Filename.concat bdir (Driver.base_notes_dir base) in
  let rec loop dir name =
    try
      let cdir = Filename.concat ndir dir in
      let files = Sys.readdir cdir in
      for i = 0 to Array.length files - 1 do
        let file = files.(i) in
        if
          String.length file > 0
          && file.[0] <> '.'
          && file.[String.length file - 1] <> '~'
        then
          if Filename.check_suffix file ".txt" then (
            let fnotes = Filename.chop_suffix file ".txt" in
            let file = Filename.concat dir fnotes in
            match Notes.links_of_text (Driver.base_notes_read base file) with
            | [], [] -> ()
            | list ->
                let fnotes =
                  if name = "" then fnotes
                  else
                    Printf.sprintf "%s%c%s" name NotesLinks.char_dir_sep fnotes
                in
                Printf.eprintf "%s...\n" fnotes;
                flush stderr;
                add_page (NLDB.PgMisc fnotes) list)
          else
            loop (Filename.concat dir file)
              (if name = "" then file
               else Printf.sprintf "%s%c%s" name NotesLinks.char_dir_sep file)
      done;
      flush stderr
    with Sys_error _ ->
      Printf.eprintf "Warning: error while reading misc notes %s\n" name
  in
  loop Filename.current_dir_name "";

  let buffer = Buffer.create 1024 in
  let add_string istr =
    Buffer.add_string buffer @@ Driver.sou base istr;
    Buffer.add_char buffer ' '
  in
  ProgrBar.full := '*';

  Printf.eprintf "--- individual notes\n";
  flush stderr;
  ProgrBar.start ();
  Collection.iteri
    (fun i p ->
      if !debug then
        Printf.eprintf "Person: (%d) %s\n" i (Gutil.designation base p);
      ProgrBar.run i nb_ind;
      Buffer.reset buffer;
      add_string @@ Driver.get_notes p;
      add_string @@ Driver.get_occupation p;
      add_string @@ Driver.get_birth_note p;
      add_string @@ Driver.get_birth_src p;
      add_string @@ Driver.get_baptism_note p;
      add_string @@ Driver.get_baptism_src p;
      add_string @@ Driver.get_death_note p;
      add_string @@ Driver.get_death_src p;
      add_string @@ Driver.get_burial_note p;
      add_string @@ Driver.get_burial_src p;
      add_string @@ Driver.get_psources p;
      List.iter
        (fun { epers_note; epers_src; _ } ->
          add_string epers_note;
          add_string epers_src)
        (Driver.get_pevents p);
      (* list is: lfname :: list_nt, (key, link) :: list_ind *)
      match Notes.links_of_text (Buffer.contents buffer) with
      | [], [] -> ()
      | list ->
          add_page (NLDB.PgInd (Driver.get_iper p)) list;
          ProgrBar.run i nb_ind)
    (Geneweb_db.Driver.persons base);
  ProgrBar.finish ();
  Printf.eprintf "--- families notes\n";
  flush stderr;
  ProgrBar.start ();
  Collection.iteri
    (fun i fam ->
      (if !debug then
         let fath = Driver.poi base (Driver.get_father fam) in
         Printf.eprintf "Family: (%d) %s\n" i (Gutil.designation base fath));
      ProgrBar.run i nb_fam;
      Buffer.reset buffer;
      add_string @@ Driver.get_comment fam;
      add_string @@ Driver.get_fsources fam;
      add_string @@ Driver.get_marriage_note fam;
      add_string @@ Driver.get_marriage_src fam;
      List.iter
        (fun { efam_note; efam_src; _ } ->
          add_string @@ efam_note;
          add_string @@ efam_src)
        (Driver.get_fevents fam);
      match Notes.links_of_text (Buffer.contents buffer) with
      | [], [] -> ()
      | list ->
          add_page (NLDB.PgFam (Driver.get_ifam fam)) list;
          ProgrBar.run i nb_fam)
    (Driver.families base);
  ProgrBar.finish ();
  Driver.write_nldb base !db;

  Notes.save_cache_linked_pages bdir cache_linked_pages

let ( // ) = Filename.concat

let main () =
  let fname, bases_dir = parse_cmd () in
  Secure.set_base_dir bases_dir;
  let bname = bases_dir // fname in
  if fname = "" then (
    Printf.eprintf "Missing database name\n";
    Printf.eprintf "Use option -help for usage\n";
    flush stderr;
    exit 2);
  Driver.with_database bname @@ fun base ->
  Sys.catch_break true;
  Driver.load_strings_array base;
  Driver.load_unions_array base;
  try compute base bname
  with Sys.Break ->
    Printf.eprintf "\n";
    flush stderr

let _ = Printexc.print main ()
