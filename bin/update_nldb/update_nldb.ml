(* Copyright (c) 1998-2007 INRIA *)

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
      match Geneweb.NotesLinks.misc_notes_link s i with
      | Geneweb.NotesLinks.WLpage (j, _, lfname, _, _) ->
          let list_nt =
            if List.mem lfname list_nt then list_nt else lfname :: list_nt
          in
          loop list_nt list_ind pos j
      | Geneweb.NotesLinks.WLperson (j, key, _, text) ->
          let list_ind =
            let link = { Def.NLDB.lnTxt = text; lnPos = pos } in
            (key, link) :: list_ind
          in
          loop list_nt list_ind (pos + 1) j
      | Geneweb.NotesLinks.WLwizard (j, _, _) ->
          loop list_nt list_ind (pos + 1) j
      | Geneweb.NotesLinks.WLnone -> loop list_nt list_ind pos (i + 1)
  in
  loop [] [] 1 0

let compute base bdir =
  let bdir =
    if Filename.check_suffix bdir ".gwb" then bdir else bdir ^ ".gwb"
  in
  let nb_ind = Gwdb.nb_of_persons base in
  let nb_fam = Gwdb.nb_of_families base in
  let db = ref [] in
  Printf.eprintf "--- database notes\n";
  flush stderr;
  let list = notes_links (Gwdb.base_notes_read base "") in
  (if list = ([], []) then ()
   else
     let pg = Def.NLDB.PgNotes in
     db := Geneweb.NotesLinks.add_in_db !db pg list);
  Printf.eprintf "--- wizard notes\n";
  flush stderr;
  (try
     let files =
       Sys.readdir (Filename.concat bdir (Gwdb.base_wiznotes_dir base))
     in
     for i = 0 to Array.length files - 1 do
       let file = files.(i) in
       if Filename.check_suffix file ".txt" then
         let wizid = Filename.chop_suffix file ".txt" in
         let wfile =
           List.fold_left Filename.concat bdir
             [ Gwdb.base_wiznotes_dir base; file ]
         in
         let content = Mutil.read_file_content wfile in
         let list = notes_links content in
         if list = ([], []) then ()
         else (
           Printf.eprintf "%s... " wizid;
           flush stderr;
           let pg = Def.NLDB.PgWizard wizid in
           db := Geneweb.NotesLinks.add_in_db !db pg list)
     done;
     Printf.eprintf "\n";
     flush stderr
   with Sys_error _ -> ());
  Printf.eprintf "--- misc notes\n";
  flush stderr;
  let ndir = Filename.concat bdir (Gwdb.base_notes_dir base) in
  let rec loop dir name =
    try
      let cdir = Filename.concat ndir dir in
      let files = Sys.readdir cdir in
      for i = 0 to Array.length files - 1 do
        let file = files.(i) in
        if Filename.check_suffix file ".txt" then (
          let fnotes = Filename.chop_suffix file ".txt" in
          let file = Filename.concat dir fnotes in
          let list = notes_links (Gwdb.base_notes_read base file) in
          if list = ([], []) then ()
          else
            let fnotes =
              if name = "" then fnotes
              else
                Printf.sprintf "%s%c%s" name Geneweb.NotesLinks.char_dir_sep
                  fnotes
            in
            Printf.eprintf "%s...\n" fnotes;
            flush stderr;
            let pg = Def.NLDB.PgMisc fnotes in
            db := Geneweb.NotesLinks.add_in_db !db pg list)
        else
          loop (Filename.concat dir file)
            (if name = "" then file
             else
               Printf.sprintf "%s%c%s" name Geneweb.NotesLinks.char_dir_sep file)
      done;
      flush stderr
    with Sys_error _ -> ()
  in
  loop Filename.current_dir_name "";
  let buffer = Buffer.create 1024 in
  let add_string istr =
    Buffer.add_string buffer @@ Gwdb.sou base istr;
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
      add_string @@ Gwdb.get_notes p;
      add_string @@ Gwdb.get_occupation p;
      add_string @@ Gwdb.get_birth_note p;
      add_string @@ Gwdb.get_birth_src p;
      add_string @@ Gwdb.get_baptism_note p;
      add_string @@ Gwdb.get_baptism_src p;
      add_string @@ Gwdb.get_death_note p;
      add_string @@ Gwdb.get_death_src p;
      add_string @@ Gwdb.get_burial_note p;
      add_string @@ Gwdb.get_burial_src p;
      add_string @@ Gwdb.get_psources p;
      List.iter
        (fun pe ->
          add_string (Gwdb.get_pevent_note pe);
          add_string (Gwdb.get_pevent_src pe))
        (Gwdb.get_pevents p);
      match notes_links (Buffer.contents buffer) with
      | [], [] -> ()
      | list ->
          db :=
            Geneweb.NotesLinks.add_in_db !db
              (Def.NLDB.PgInd (Gwdb.get_iper p))
              list)
    (Gwdb.persons base);
  ProgrBar.finish ();
  Printf.eprintf "--- families notes\n";
  flush stderr;
  ProgrBar.start ();
  Gwdb.Collection.iteri
    (fun i fam ->
      ProgrBar.run i nb_fam;
      Buffer.reset buffer;
      add_string @@ Gwdb.get_comment fam;
      add_string @@ Gwdb.get_fsources fam;
      add_string @@ Gwdb.get_marriage_note fam;
      add_string @@ Gwdb.get_marriage_src fam;
      List.iter
        (fun fe ->
          add_string @@ Gwdb.get_fevent_note fe;
          add_string @@ Gwdb.get_fevent_src fe)
        (Gwdb.get_fevents fam);
      match notes_links (Buffer.contents buffer) with
      | [], [] -> ()
      | list ->
          db :=
            Geneweb.NotesLinks.add_in_db !db
              (Def.NLDB.PgFam (Gwdb.get_ifam fam))
              list;
          ProgrBar.run i nb_fam)
    (Gwdb.families base);
  ProgrBar.finish ();
  Gwdb.write_nldb base !db

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
  let () = Gwdb.load_strings_array base in
  let () = Gwdb.load_unions_array base in
  try compute base !fname
  with Sys.Break ->
    Printf.eprintf "\n";
    flush stderr;
    ()

let () = Printexc.print main ()
