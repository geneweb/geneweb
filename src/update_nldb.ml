(* camlp4r *)
(* $Id: update_nldb.ml,v 1.3 2005-07-02 13:49:13 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;

value fname = ref "";

value errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <file_name>";
value speclist = [];
value anonfun s =
  if fname.val = "" then fname.val := s
  else raise (Arg.Bad "Cannot treat several databases")
;

value check_file_name s =
  loop 0 where rec loop i =
    if i = String.length s then True
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> loop (i + 1)
      | _ -> False ]
;

value ext_file_link s i =
  let slen = String.length s in
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
    let (fname, sname, text) =
      try
        let k = String.index b '/' in
        let j = try String.index b '#' with [ Not_found -> k ] in
        (String.sub b 0 j, String.sub b j (k - j),
         String.sub b (k + 1) (String.length b - k - 1))
      with
      [ Not_found -> (b, "", b) ]
    in
    if check_file_name fname then Some (j, fname, sname, text)
    else None
  else None
;

value notes_links s =
  let slen = String.length s in
  loop [] 0 where rec loop list i =
    if i = slen then list
    else if i < slen - 2 && s.[i] = '[' && s.[i+1] = '[' && s.[i+2] = '[' then
      match ext_file_link s i with
      [ Some (j, lfname, _, _) -> loop [lfname :: list] j
      | None -> loop list (i + 3) ]
    else loop list (i + 1)
;

value progr_bar_size = 60;
value progr_bar_draw_rep = 5;
value progr_bar_draw = "|/-\\";
value progr_bar_empty = '.';
value progr_bar_full = '#';

value progr_bar_draw_len = String.length progr_bar_draw;
value progr_bar_cnt =
  progr_bar_size * progr_bar_draw_rep * progr_bar_draw_len
;

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

value compute base bdir =
  let len = base.data.persons.len in
  do {
    start_progr_bar ();
    for i = 0 to len - 1 do {
      let p = base.data.persons.get i in
      let list = notes_links (Gutil.sou base p.notes) in
      if list = [] then ()
      else NotesLinks.update_db bdir i list;
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

