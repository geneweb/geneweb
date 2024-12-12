open Geneweb
open Def
open Gwdb

let fname = ref ""
let errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <file_name>"
let speclist = []

let anonfun s =
  if !fname = "" then fname := s
  else raise (Arg.Bad "Cannot treat several databases")

let notes_links s =
  let slen = String.length s in
  let rec loop list_nt list_ind pos i =
    if i = slen then (list_nt, list_ind)
    else if i + 1 < slen && s.[i] = '%' then loop list_nt list_ind pos (i + 2)
    else
      match NotesLinks.misc_notes_link s i with
      | NotesLinks.WLpage (j, _, lfname, _, _) ->
          let list_nt =
            if List.mem lfname list_nt then list_nt else lfname :: list_nt
          in
          loop list_nt list_ind pos j
      | NotesLinks.WLperson (j, key, name, text) ->
          let list_ind =
            let text =
              match (name, text) with
              | Some name, None -> Some name
              | Some name, Some text when name = "" -> Some text
              | Some name, Some text -> Some (name ^ ";" ^ text)
              | _, _ -> None
            in
            let link = { NLDB.lnTxt = text; lnPos = pos } in
            (key, link) :: list_ind
          in
          loop list_nt list_ind (pos + 1) j
      | NotesLinks.WLwizard (j, _, _) -> loop list_nt list_ind (pos + 1) j
      | NotesLinks.WLnone (j, _) -> loop list_nt list_ind pos j
  in
  loop [] [] 1 0

let read_file_contents fname =
  match try Some (open_in fname) with Sys_error _ -> None with
  | Some ic -> (
      let len = ref 0 in
      try
        let rec loop () =
          len := Buff.store !len (input_char ic);
          loop ()
        in
        loop ()
      with End_of_file -> Buff.get !len)
  | None -> ""

type cache_linked_pages_t = (Def.NLDB.key, int) Hashtbl.t

let read_cache_linked_pages conf : cache_linked_pages_t =
  let ic = open_in_bin conf in
  let ht : cache_linked_pages_t = input_value ic in
  close_in ic;
  ht

let save_cache_linked_pages bdir cache_linked_pages =
  let oc = open_out_bin (Filename.concat bdir Notes.cache_linked_pages_name) in
  output_value oc cache_linked_pages;
  close_out oc

let compute base bdir =
  let bdir =
    if Filename.check_suffix bdir ".gwb" then bdir else bdir ^ ".gwb"
  in
  let nb_ind = nb_of_persons base in
  let nb_fam = nb_of_families base in
  let db = ref [] in
  let cache_linked_pages : cache_linked_pages_t = Hashtbl.create 1024 in

  let update_cache_linked_pages key =
    let current_count =
      try Hashtbl.find cache_linked_pages key with Not_found -> 0
    in
    Hashtbl.replace cache_linked_pages key (current_count + 1)
  in

  Printf.eprintf "--- database notes\n";
  flush stderr;
  let list = notes_links (base_notes_read base "") in
  (if list = ([], []) then ()
  else
    let pg = NLDB.PgNotes in
    db := NotesLinks.add_in_db !db pg list);

  Printf.eprintf "--- wizard notes\n";
  flush stderr;
  (try
     let files = Sys.readdir (Filename.concat bdir (base_wiznotes_dir base)) in
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
             match notes_links (base_wiznotes_read base wizid) with
             | [], [] -> ()
             | (_list_nt, list_ind) as list ->
                 Printf.eprintf "%s... " wizid;
                 flush stderr;
                 let pg = NLDB.PgWizard wizid in
                 db := NotesLinks.add_in_db !db pg list;
                 let list_ind =
                   List.fold_left
                     (fun acc (key, l) ->
                       if List.mem_assoc key acc then acc else (key, l) :: acc)
                     [] list_ind
                 in
                 List.iter
                   (fun (key, _) -> update_cache_linked_pages key)
                   list_ind
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
  let ndir = Filename.concat bdir (base_notes_dir base) in
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
            match notes_links (base_notes_read base file) with
            | [], [] -> ()
            | (_list_nt, list_ind) as list ->
                let fnotes =
                  if name = "" then fnotes
                  else
                    Printf.sprintf "%s%c%s" name NotesLinks.char_dir_sep fnotes
                in
                Printf.eprintf "%s...\n" fnotes;
                flush stderr;
                let pg = NLDB.PgMisc fnotes in
                db := NotesLinks.add_in_db !db pg list;
                let list_ind =
                  List.fold_left
                    (fun acc (key, l) ->
                      if List.mem_assoc key acc then acc else (key, l) :: acc)
                    [] list_ind
                in
                List.iter
                  (fun (key, _) -> update_cache_linked_pages key)
                  list_ind)
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
    Buffer.add_string buffer @@ sou base istr;
    Buffer.add_char buffer ' '
  in
  ProgrBar.full := '*';

  Printf.eprintf "--- individual notes\n";
  flush stderr;
  ProgrBar.start ();
  Gwdb.Collection.iteri
    (fun i p ->
      ProgrBar.run i nb_ind;
      Buffer.reset buffer;
      add_string @@ get_notes p;
      add_string @@ get_occupation p;
      add_string @@ get_birth_note p;
      add_string @@ get_birth_src p;
      add_string @@ get_baptism_note p;
      add_string @@ get_baptism_src p;
      add_string @@ get_death_note p;
      add_string @@ get_death_src p;
      add_string @@ get_burial_note p;
      add_string @@ get_burial_src p;
      add_string @@ get_psources p;
      List.iter
        (fun { epers_note; epers_src; _ } ->
          add_string epers_note;
          add_string epers_src)
        (get_pevents p);
      (* list is: lfname :: list_nt, (key, link) :: list_ind *)
      match notes_links (Buffer.contents buffer) with
      | [], [] -> ()
      | (list_nt, list_ind) as list ->
          (db := NotesLinks.add_in_db !db (NLDB.PgInd (get_iper p)) list;
           let list_ind =
             List.fold_left
               (fun acc (key, l) ->
                 if List.mem_assoc key acc then acc else (key, l) :: acc)
               [] list_ind
           in
           List.iter (fun (key, _) -> update_cache_linked_pages key) list_ind);
          ProgrBar.run i nb_ind)
    (Gwdb.persons base);
  ProgrBar.finish ();
  Printf.eprintf "--- families notes\n";
  flush stderr;
  ProgrBar.start ();
  Gwdb.Collection.iteri
    (fun i fam ->
      ProgrBar.run i nb_fam;
      Buffer.reset buffer;
      add_string @@ get_comment fam;
      add_string @@ get_fsources fam;
      add_string @@ get_marriage_note fam;
      add_string @@ get_marriage_src fam;
      List.iter
        (fun { efam_note; efam_src; _ } ->
          add_string @@ efam_note;
          add_string @@ efam_src)
        (get_fevents fam);
      match notes_links (Buffer.contents buffer) with
      | [], [] -> ()
      | (_list_nt, list_ind) as list ->
          (db := NotesLinks.add_in_db !db (NLDB.PgFam (get_ifam fam)) list;
           let list_ind =
             List.fold_left
               (fun acc (key, l) ->
                 if List.mem_assoc key acc then acc else (key, l) :: acc)
               [] list_ind
           in
           List.iter (fun (key, _) -> update_cache_linked_pages key) list_ind);
          ProgrBar.run i nb_fam)
    (Gwdb.families base);
  ProgrBar.finish ();
  write_nldb base !db;

  (* Save the cache_linked_pages to a file *)
  save_cache_linked_pages bdir cache_linked_pages

let main () =
  Arg.parse speclist anonfun errmsg;
  if !fname = "" then (
    Printf.eprintf "Missing database name\n";
    Printf.eprintf "Use option -help for usage\n";
    flush stderr;
    exit 2);
  Secure.set_base_dir (Filename.dirname !fname);
  let base = Gwdb.open_base !fname in
  Sys.catch_break true;
  let () = load_strings_array base in
  let () = load_unions_array base in
  try compute base !fname
  with Sys.Break ->
    Printf.eprintf "\n";
    flush stderr;
    ()

let _ = Printexc.print main ()
