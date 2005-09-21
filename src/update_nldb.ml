(* camlp4r *)
(* $Id: update_nldb.ml,v 1.17 2005-09-21 09:56:59 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;

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
      | NotesLinks.WLnone -> loop list_nt list_ind pos (i + 1) ]
;

value progr_bar_size = 60;
value progr_bar_draw_rep = 5;
value progr_bar_draw = "|/-\\";
value progr_bar_empty = '.';
value progr_bar_full = '*';

value progr_bar_draw_len = String.length progr_bar_draw;
value progr_bar_cnt = progr_bar_size * progr_bar_draw_rep * progr_bar_draw_len;

value start_progr_bar () =
  do {
    for i = 1 to progr_bar_size do { Printf.eprintf "%c" progr_bar_empty };
    Printf.eprintf "\013"
  }
;

value run_progr_bar cnt max_cnt =
  do {
    let already_disp = cnt * progr_bar_size / max_cnt in
    let to_disp = (cnt + 1) * progr_bar_size / max_cnt in
    for i = already_disp + 1 to to_disp do {
      Printf.eprintf "%c" progr_bar_full
    };
    let already_disp = cnt * progr_bar_cnt / max_cnt in
    let to_disp = (cnt + 1) * progr_bar_cnt / max_cnt in
    if cnt = max_cnt - 1 then Printf.eprintf " \008"
    else if to_disp > already_disp then
      let k = to_disp mod progr_bar_draw_len in
      let k = if k < 0 then progr_bar_draw_len + k else k in
      Printf.eprintf "%c\008" progr_bar_draw.[k]
    else ();
    flush stderr;
  }
;

value finish_progr_bar () =
  do {
    Printf.eprintf "\n";
    flush stderr;
  }
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
  let len = base.data.persons.len in
  do {
    Printf.eprintf "--- database notes\n";
    flush stderr;
    let list = notes_links (base.data.bnotes.nread "" RnAll) in
    if list = ([], []) then ()
    else 
      let pg = NotesLinks.PgNotes in
      NotesLinks.update_db bdir pg list;
    Printf.eprintf "--- wizard notes\n";
    flush stderr;
    try
      let files = Sys.readdir (Filename.concat bdir "wiznotes") in
      do {
        for i = 0 to Array.length files - 1 do {
          let file = files.(i) in
          if Filename.check_suffix file ".txt" then do {
            let wizid = Filename.chop_suffix file ".txt" in
            let wfile =
              List.fold_right Filename.concat [bdir; "wiznotes"] file
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
    Printf.eprintf "--- misc notes\n";
    flush stderr;
    let ndir = Filename.concat bdir "notes_d" in
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
              let list = notes_links (base.data.bnotes.nread file RnAll) in
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
                NotesLinks.update_db bdir pg list
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
    start_progr_bar ();
    for i = 0 to len - 1 do {
      let p = base.data.persons.get i in
      let list = notes_links (Gutil.sou base p.notes) in
      if list = ([], []) then ()
      else
        let pg = NotesLinks.PgInd (Adef.iper_of_int i) in
        NotesLinks.update_db bdir pg list;
      run_progr_bar i len
    };
    finish_progr_bar ();
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
    let base = Iobase.input fname.val in
    do {
      Sys.catch_break True;
      try compute base fname.val with
      [ Sys.Break -> do { Printf.eprintf "\n"; flush stderr; () } ];
    }
  }
;

Printexc.catch main ();

