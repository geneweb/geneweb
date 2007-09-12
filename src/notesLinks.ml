(* camlp5r *)
(* $Id: notesLinks.ml,v 5.8 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def;

value magic_notes_links = "GWNL0010";
type page =
  [ PgInd of iper
  | PgFam of ifam
  | PgNotes
  | PgMisc of string
  | PgWizard of string ]
;
type key = (string * string * int);
type ind_link = { lnTxt : option string; lnPos : int };
type notes_links_db = list (page * (list string * list (key * ind_link)));

value char_dir_sep = ':';

value check_file_name s =
  loop [] 0 0 where rec loop path ibeg i =
    if i = String.length s then
      if i > ibeg then
        let path = (List.rev path, String.sub s ibeg (i - ibeg)) in
        Some path
      else None
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' | '.' ->
          loop path ibeg (i + 1)
      | c ->
          if c = char_dir_sep then
            if i > ibeg then
              loop [String.sub s ibeg (i - ibeg) :: path] (i + 1) (i + 1)
            else None
          else None ]
;

type wiki_link =
  [ WLpage of int and (list string * string) and string and string and string
  | WLperson of int and key and string and option string
  | WLwizard of int and string and string
  | WLnone ]
;

value misc_notes_link s i =
  let slen = String.length s in
  if i < slen - 2 && s.[i] = '[' && s.[i+1] = '[' then
    if s.[i+2] = '[' then
      let j =
        loop (i + 3) where rec loop j =
          if j = slen then j
          else if
            j < slen - 3 && s.[j] = ']' && s.[j+1] = ']' && s.[j+2] = ']'
          then j + 3
          else loop (j + 1)
      in
      if j > i + 6 then
        let b = String.sub s (i + 3) (j - i - 6) in
        let (fname, anchor, text) =
          try
            let k = String.rindex b '/' in
            let j = try String.index b '#' with [ Not_found -> k ] in
            (String.sub b 0 j, String.sub b j (k - j),
             String.sub b (k + 1) (String.length b - k - 1))
          with
          [ Not_found -> (b, "", b) ]
        in
        match check_file_name fname with
        [ Some pg_path -> WLpage j pg_path fname anchor text
        | None -> WLnone ]
      else WLnone
    else
      let j =
        loop (i + 2) where rec loop j =
          if j = slen then j
          else if j < slen -2 && s.[j] = ']' && s.[j+1] = ']' then j + 2
          else loop (j + 1)
      in
      let b = String.sub s (i + 2) (j - i - 4) in
      let (spe, b) =
        try
          let i = String.index b ':' in
          (Some (String.sub b 0 i),
           String.sub b (i + 1) (String.length b - i - 1))
        with
        [ Not_found -> (None, b) ]
      in
      let (b, text) =
        try
          let i = String.rindex b ';' in
          (String.sub b 0 i,
           Some (String.sub b (i + 1) (String.length b - i - 1)))
        with
        [ Not_found -> (b, None) ]
      in
      if spe = Some "w" then
        let (wiz, name) =
          match
            try (Some (String.index b '/')) with [ Not_found -> None ]
          with
          [ Some i ->
              (String.sub b 0 i,
               String.sub b (i + 1) (String.length b - i - 1))
          | None -> (b, "") ]
        in
        WLwizard j wiz name
      else
        try
          let k = 0 in
          let l = String.index_from b k '/' in
          let fn = String.sub b k (l - k) in
          let k = l + 1 in
          let (fn, sn, oc, name) =
            try
              let l = String.index_from b k '/' in
              let sn = String.sub b k (l - k) in
              let (oc, name) =
                try
                  let k = l + 1 in
                  let l = String.index_from b k '/' in
                  let x = String.sub b k (l - k) in
                  (x, String.sub b (l + 1) (String.length b - l - 1))
                with
                [ Not_found ->
                    ("", String.sub b (l + 1) (String.length b - l - 1)) ]
              in
              let oc = try int_of_string oc with [ Failure _ -> 0 ] in
              (fn, sn, oc, name)
            with
            [ Not_found ->
                let sn = String.sub b k (String.length b - k) in
                let name = fn ^ " " ^ sn in
                (fn, sn, 0, name) ]
          in
          let fn = Name.lower fn in
          let sn = Name.lower sn in
          WLperson j (fn, sn, oc) name text
        with
        [ Not_found -> WLnone ]
  else WLnone
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

value share ht s =
  try Hashtbl.find ht s with
  [ Not_found -> do { Hashtbl.add ht s s; s } ]
;

value option f =
  fun
  [ Some x -> Some (f x)
  | None -> None ]
;

value share_strings db =
  let ht = Hashtbl.create 103 in
  List.map
    (fun (page, (list_nt, list_ind)) ->
       let page =
         match page with
         [ PgMisc s -> PgMisc (share ht s)
         | PgWizard s -> PgWizard (share ht s)
         | x -> x ]
       in
       let list_nt = List.map (share ht) list_nt in
       let list_ind =
         List.map
           (fun ((fn, sn, oc), {lnTxt = txt; lnPos = pos}) ->
              ((share ht fn, share ht sn, oc),
               {lnTxt = option (share ht) txt; lnPos = pos}))
           list_ind
       in
       (page, (list_nt, list_ind)))
    db
;

value update_db_list db who (list_nt, list_ind) =
  let db = List.remove_assoc who db in
  let new_db =
    if list_nt = [] && list_ind = [] then db
    else [(who, (list_nt, list_ind)) :: db]
  in
  share_strings new_db
;

value read_db bdir =
  let fname_def = Filename.concat bdir "notes_links" in
  read_db_from_file fname_def
;

value write_db bdir db = do {
  let db = share_strings db in
  let fname_tmp = Filename.concat bdir "1notes_links" in
  let fname_def = Filename.concat bdir "notes_links" in
  let fname_back = Filename.concat bdir "notes_links~" in
  let oc = open_out_bin fname_tmp in
  output_string oc magic_notes_links;
  output_value oc (db : notes_links_db);
  close_out oc;
  Mutil.remove_file fname_back;
  try Sys.rename fname_def fname_back with [ Sys_error _ -> () ];
  try Sys.rename fname_tmp fname_def with [ Sys_error _ -> () ];
};

value add_in_db db who (list_nt, list_ind) =
  let db = List.remove_assoc who db in
  if list_nt = [] && list_ind = [] then db
  else [(who, (list_nt, list_ind)) :: db]
;

value update_db bdir who list =
  let db = read_db bdir in
  let new_db = add_in_db db who list in
  write_db bdir new_db
;
