(* camlp5r *)
(* $Id: update_nldb.ml,v 5.23 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def;
open Gwdb;

value fname = ref "";

value errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <file_name>";
value speclist = [];
value anonfun s =
  if fname.val = "" then fname.val := s
  else raise (Arg.Bad "Cannot treat several databases")
;

value notes_links s =
  let slen = String.length s in
  loop [] [] 1 0 where rec loop list_nt list_ind pos i =
    if i = slen then (list_nt, list_ind)
    else if i + 1 < slen && s.[i] = '%' then loop list_nt list_ind pos (i + 2)
    else
      match NotesLinks.misc_notes_link s i with
      [ NotesLinks.WLpage j _ lfname _ _ ->
          let list_nt =
            if List.mem lfname list_nt then list_nt else [lfname :: list_nt]
          in
          loop list_nt list_ind pos j
      | NotesLinks.WLperson j key _ text ->
          let list_ind =
            let link ={NotesLinks.lnTxt = text; NotesLinks.lnPos = pos} in
            [(key, link) :: list_ind]
          in
          loop list_nt list_ind (pos + 1) j
      | NotesLinks.WLwizard j _ _ -> loop list_nt list_ind pos j
      | NotesLinks.WLnone -> loop list_nt list_ind pos (i + 1) ]
;

value read_file_contents fname =
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let len = ref 0 in
      try
        loop () where rec loop () =
          do { len.val := Buff.store len.val (input_char ic); loop () }
      with
      [ End_of_file -> Buff.get len.val ]                   
  | None -> "" ]
;

value compute base bdir =
  let bdir =
    if Filename.check_suffix bdir ".gwb" then bdir else bdir ^ ".gwb"
  in
  let nb_ind = nb_of_persons base in
  let nb_fam = nb_of_families base in
  do {
    Printf.eprintf "--- database notes\n";
    flush stderr;
    let list = notes_links (base_notes_read base "") in
    if list = ([], []) then ()
    else 
      let pg = NotesLinks.PgNotes in
      NotesLinks.update_db bdir pg list;
    Printf.eprintf "--- wizard notes\n";
    flush stderr;
    try
      let files =
        Sys.readdir (Filename.concat bdir (base_wiznotes_dir base))
      in
      do {
        for i = 0 to Array.length files - 1 do {
          let file = files.(i) in
          if Filename.check_suffix file ".txt" then do {
            let wizid = Filename.chop_suffix file ".txt" in
            let wfile =
              List.fold_left Filename.concat bdir
                [base_wiznotes_dir base; file]
            in
            let list = notes_links (read_file_contents wfile) in
            if list = ([], []) then ()
            else do {
              Printf.eprintf "%s... " wizid; flush stderr;
              let pg = NotesLinks.PgWizard wizid in
              NotesLinks.update_db bdir pg list;
            }
          }
          else ()
        };
        Printf.eprintf "\n"; flush stderr;
      }
    with
    [ Sys_error _ -> () ];
    let db = ref (NotesLinks.read_db bdir) in
    Printf.eprintf "--- misc notes\n";
    flush stderr;
    let ndir = Filename.concat bdir (base_notes_dir base) in
    let rec loop dir name =
      try
        let cdir = Filename.concat ndir dir in
        let files = Sys.readdir cdir in
        do {
          for i = 0 to Array.length files - 1 do {
            let file = files.(i) in
            if Filename.check_suffix file ".txt" then do {
              let fnotes = Filename.chop_suffix file ".txt" in
              let file = Filename.concat dir fnotes in
              let list = notes_links (base_notes_read base file) in
              if list = ([], []) then ()
              else do {
                let fnotes =
                  if name = "" then fnotes
                  else
                    Printf.sprintf "%s%c%s" name NotesLinks.char_dir_sep
                      fnotes
                in
                Printf.eprintf "%s...\n" fnotes; flush stderr;
                let pg = NotesLinks.PgMisc fnotes in
                db.val := NotesLinks.add_in_db db.val pg list;
              }
            }
            else
              loop (Filename.concat dir file)
                (if name = "" then file
                 else
                   Printf.sprintf "%s%c%s" name NotesLinks.char_dir_sep file)
          };
          flush stderr;
        }
      with
      [ Sys_error _ -> () ]
    in
    loop Filename.current_dir_name "";
    Printf.eprintf "--- individual notes\n";
    flush stderr;
    ProgrBar.full.val := '*';
    ProgrBar.start ();
    for i = 0 to nb_ind - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let s =
        let sl =
          [get_notes; get_occupation; get_birth_src; get_baptism_src;
           get_death_src; get_burial_src; get_psources]
        in
        String.concat " " (List.map (fun f -> sou base (f p)) sl)
      in
      let list = notes_links s in
      if list = ([], []) then ()
      else
        let pg = NotesLinks.PgInd (Adef.iper_of_int i) in
        db.val := NotesLinks.add_in_db db.val pg list;
      ProgrBar.run i nb_ind
    };
    ProgrBar.finish ();
    Printf.eprintf "--- families notes\n";
    flush stderr;
    ProgrBar.full.val := '*';
    ProgrBar.start ();
    for i = 0 to nb_fam - 1 do {
      let fam = foi base (Adef.ifam_of_int i) in
      if not (is_deleted_family fam) then do {
        let s =
          let sl = [get_comment; get_fsources] in
          String.concat " " (List.map (fun f -> sou base (f fam)) sl)
        in
        let list = notes_links s in
        if list = ([], []) then ()
        else
          let pg = NotesLinks.PgFam (Adef.ifam_of_int i) in
          db.val := NotesLinks.add_in_db db.val pg list;
      }
      else ();
      ProgrBar.run i nb_fam
    };
    ProgrBar.finish ();
    NotesLinks.write_db bdir db.val;
  }
;

value main () =
  do {
    Argl.parse speclist anonfun errmsg;
    if fname.val = "" then do {
      Printf.eprintf "Missing database name\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 2;
    }
    else ();
    Secure.set_base_dir (Filename.dirname fname.val);
    let base = Gwdb.open_base fname.val in
    do {
      Sys.catch_break True;
      try compute base fname.val with
      [ Sys.Break -> do { Printf.eprintf "\n"; flush stderr; () } ];
    }
  }
;

Printexc.catch main ();
