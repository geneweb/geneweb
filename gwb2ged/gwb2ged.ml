(* $Id: gwb2ged.ml,v 2.1 1999-03-08 11:17:52 ddr Exp $ *)
(* Copyright (c) INRIA *)

open Def;
open Gutil;

value ascii = ref False;

value ged_month =
  fun
  [ 1 -> "JAN"
  | 2 -> "FEB"
  | 3 -> "MAR"
  | 4 -> "APR"
  | 5 -> "MAY"
  | 6 -> "JUN"
  | 7 -> "JUL"
  | 8 -> "AUG"
  | 9 -> "SEP"
  | 10 -> "OCT"
  | 11 -> "NOV"
  | 12 -> "DEC"
  | _ -> failwith "ged_month" ]
;

value ged_header base oc ifile ofile =
  do Printf.fprintf oc "0 HEAD\n";
     Printf.fprintf oc "1 SOUR GeneWeb\n";
     Printf.fprintf oc "2 VERS %s\n" Gutil.version;
     Printf.fprintf oc "2 NAME %s\n" (Filename.basename Sys.argv.(0));
     Printf.fprintf oc "2 CORP INRIA\n";
     Printf.fprintf oc "3 ADDR Domaine de Voluceau\n";
     Printf.fprintf oc "4 CONT B.P 105 - Rocquencourt\n";
     Printf.fprintf oc "4 CITY Le Chesnay Cedex\n";
     Printf.fprintf oc "4 POST 78153\n";
     Printf.fprintf oc "4 CTRY France\n";
     Printf.fprintf oc "3 PHON +33 01 39 63 55 11\n";
     Printf.fprintf oc "2 DATA %s\n"
       (let fname = Filename.basename ifile in
        if Filename.check_suffix fname ".gwb" then fname else fname ^ ".gwb");
     try
       let tm = Unix.localtime (Unix.time ()) in
       let mon = ged_month (tm.Unix.tm_mon + 1) in
       do Printf.fprintf oc "1 DATE %02d %s %d\n" tm.Unix.tm_mday mon
            (1900 + tm.Unix.tm_year);
          Printf.fprintf oc "2 TIME %02d:%02d:%02d\n" tm.Unix.tm_hour
            tm.Unix.tm_min tm.Unix.tm_sec;
       return ()
     with _ -> ();
     if ofile <> "" then
       Printf.fprintf oc "1 FILE %s\n" (Filename.basename ofile)
     else ();
     Printf.fprintf oc "1 GEDC\n";
     Printf.fprintf oc "2 VERS 5.5\n";
     Printf.fprintf oc "2 FORM LINEAGE-LINKED\n";
     if ascii.val then Printf.fprintf oc "1 CHAR ASCII\n"
     else Printf.fprintf oc "1 CHAR ANSEL\n";  
  return ()
;

value encode s =
  if ascii.val then Ansel.to_iso_8859_1 s else s
;

value ged_1st_name base p =
  let fn = sou base p.first_name in
  match p.first_names_aliases with
  [ [n :: _] ->
      let fna = sou base n in
      if String.length fna > String.length fn
      && fn = String.sub fna 0 (String.length fn) then fna
      else fn
  | [] -> fn ]
;

value ged_name base oc per =
  do Printf.fprintf oc "1 NAME %s/%s/\n" (encode (ged_1st_name base per))
       (encode (sou base per.surname));
     let n = sou base per.public_name in
     if n <> "" then Printf.fprintf oc "2 GIVN %s\n" (encode n) else ();
     match per.nick_names with
     [ [nn :: _] -> Printf.fprintf oc "2 NICK %s\n" (encode (sou base nn))
     | [] -> () ];
     match per.surnames_aliases with
     [ [n :: _] -> Printf.fprintf oc "2 SURN %s\n" (encode (sou base n))
     | [] -> () ];
     List.iter
       (fun s ->
          Printf.fprintf oc "1 NAME %s\n" (encode (sou base s)))
       per.aliases;
  return ()
;

value ged_sex base oc per =
  match per.sex with
  [ Masculine -> Printf.fprintf oc "1 SEX M\n"
  | Feminine -> Printf.fprintf oc "1 SEX F\n"
  | Neuter -> () ]
;

value ged_date oc dt =
  do match dt.prec with
     [ Sure -> ()
     | About -> Printf.fprintf oc "ABT "
     | Maybe -> Printf.fprintf oc "EST "
     | Before -> Printf.fprintf oc "BEF "
     | After -> Printf.fprintf oc "AFT "
     | OrYear i -> Printf.fprintf oc "BET "
     | YearInt i -> Printf.fprintf oc "BET " ];
     if dt.day <> 0 then Printf.fprintf oc "%02d " dt.day else ();
     if dt.month <> 0 then Printf.fprintf oc "%s " (ged_month dt.month)
     else ();
     Printf.fprintf oc "%d" dt.year;
     match dt.prec with
     [ OrYear i -> Printf.fprintf oc " AND %d" i
     | YearInt i -> Printf.fprintf oc " AND %d" i
     | _ -> () ];
  return ()
;

value ged_ev_detail oc n d pl src =
  do match (d, pl) with
     [ (None, "") -> Printf.fprintf oc " Y"
     | _ -> () ];
     Printf.fprintf oc "\n";
     match d with
     [ Some d ->
         do Printf.fprintf oc "%d DATE " n;
            ged_date oc d;
            Printf.fprintf oc "\n";
         return ()
     | None -> () ];
     if pl <> "" then Printf.fprintf oc "%d PLAC %s\n" n (encode pl)
     else ();
     if src <> "" then Printf.fprintf oc "%d SOUR %s\n" n (encode src)
     else ();
  return ()
;

value ged_ind_ev_str base oc per =
  do let pl = sou base per.birth_place in
     let src = sou base per.birth_src in
     match (Adef.od_of_codate per.birth, pl) with
     [ (None, "") -> ()
     | (None, pl) ->
         do Printf.fprintf oc "1 BIRT";
            ged_ev_detail oc 2 None pl src;
         return ()
     | (od, pl) ->
         do Printf.fprintf oc "1 BIRT";
            ged_ev_detail oc 2 od pl src;
         return () ];
     let pl = sou base per.baptism_place in
     let src = sou base per.baptism_src in
     match (Adef.od_of_codate per.baptism, pl) with
     [ (None, "") -> ()
     | (od, pl) ->
         do Printf.fprintf oc "1 BAPM";
            ged_ev_detail oc 2 od pl src;
         return () ];
     let pl = sou base per.death_place in
     let src = sou base per.death_src in
     match per.death with
     [ NotDead -> ()
     | Death dr cd ->
         do Printf.fprintf oc "1 DEAT";
            ged_ev_detail oc 2 (Some (Adef.date_of_cdate cd)) pl src;
         return ()
     | DeadYoung | DeadDontKnowWhen ->
         do Printf.fprintf oc "1 DEAT";
            ged_ev_detail oc 2 None pl src;
         return ()
     | DontKnowIfDead -> Printf.fprintf oc "1 DEAT\n" ];
     let pl = sou base per.burial_place in
     let src = sou base per.burial_src in
     match per.burial with
     [ UnknownBurial -> ()
     | Buried cod ->
         do Printf.fprintf oc "1 BURI";
            ged_ev_detail oc 2 (Adef.od_of_codate cod) pl src;
         return ()
     | Cremated cod ->
         do Printf.fprintf oc "1 CREM";
            ged_ev_detail oc 2 (Adef.od_of_codate cod) pl src;
         return () ];
  return ()
;

value ged_title base oc per tit =
  do Printf.fprintf oc "1 TITL ";
     Printf.fprintf oc "%s" (encode (sou base tit.t_title));
     match sou base tit.t_place with
     [ "" -> ()
     | pl -> Printf.fprintf oc ", %s" (encode pl) ];
     if tit.t_nth <> 0 then Printf.fprintf oc ", %d" tit.t_nth else ();
     Printf.fprintf oc "\n";
     match
      (Adef.od_of_codate tit.t_date_start,
       Adef.od_of_codate tit.t_date_end)
     with
     [ (None, None) -> ()
     | (Some sd, None) ->
         do Printf.fprintf oc "2 DATE ";
            ged_date oc sd;
            Printf.fprintf oc "\n";
         return ()
     | (None, Some sd) ->
         do Printf.fprintf oc "2 DATE BEF ";
            ged_date oc sd;
            Printf.fprintf oc "\n";
         return ()
     | (Some sd1, Some sd2) ->
         do Printf.fprintf oc "2 DATE BET ";
            ged_date oc sd1;
            Printf.fprintf oc " AND ";
            ged_date oc sd2;
            Printf.fprintf oc "\n";
         return () ];
     match tit.t_name with
     [ Tmain ->
         Printf.fprintf oc "2 NOTE %s\n" (encode (sou base per.public_name))
     | Tname n -> Printf.fprintf oc "2 NOTE %s\n" (encode (sou base n))
     | Tnone -> () ];
  return ()
;

value ged_ind_attr_str base oc per =
  do match sou base per.occupation with
     [ "" -> ()
     | occu -> Printf.fprintf oc "1 OCCU %s\n" (encode occu) ];
     List.iter (ged_title base oc per) per.titles;
  return ()
;

value ged_famc base (per_sel, fam_sel) oc asc =
  match asc.parents with
  [ Some ifam ->
      if fam_sel ifam then
        Printf.fprintf oc "1 FAMC @F%d@\n" (Adef.int_of_ifam ifam + 1)
      else ()
  | None -> () ]
;

value ged_fams base (per_sel, fam_sel) oc ifam =
  if fam_sel ifam then
    Printf.fprintf oc "1 FAMS @F%d@\n" (Adef.int_of_ifam ifam + 1)
  else ()
;

value ged_psource base oc per =
  match sou base per.psources with
  [ "" -> ()
  | s -> Printf.fprintf oc "1 SOUR %s\n" (encode s) ]
;

value ged_multimedia_link base oc per =
  match sou base per.photo with
  [ "" -> ()
  | s ->
      do Printf.fprintf oc "1 OBJE\n";
         Printf.fprintf oc "2 FILE %s\n" s;
      return () ]
;

value br = "<br>";

value rec display_note oc s i =
  if i == String.length s then Printf.fprintf oc "\n"
  else
    if i <= String.length s - String.length br
    && String.lowercase (String.sub s i (String.length br)) = br then
      do Printf.fprintf oc "\n2 CONT "; return
      let i = i + String.length br in
      let i =
        if i < String.length s && s.[i] == '\n' then i + 1
        else i
      in
      display_note oc s i
    else if s.[i] == '\n' then
      do Printf.fprintf oc "\n2 CONC "; return
      display_note oc s (i + 1)
    else
      do output_char oc s.[i]; return
      display_note oc s (i + 1)
;

value ged_note base oc per =
  match sou base per.notes with
  [ "" -> ()
  | s ->
      do Printf.fprintf oc "1 NOTE ";
         display_note oc (encode s) 0;
      return () ]
;

value ged_marriage base oc fam =
  match
    (Adef.od_of_codate fam.marriage, sou base fam.marriage_place)
  with
  [ (None, "") -> ()
  | (d, pl) ->
      do Printf.fprintf oc "1 MARR";
         ged_ev_detail oc 2 d pl (sou base fam.marriage_src);
      return () ]
;

value ged_divorce base oc fam =
  match fam.divorce with
  [ NotDivorced -> ()
  | Divorced cd ->
      let d = Adef.od_of_codate cd in
      do Printf.fprintf oc "1 DIV";
         ged_ev_detail oc 2 d "" "";
      return () ]
;

value ged_child base (per_sel, fam_sel) oc chil =
  if per_sel chil then
    Printf.fprintf oc "1 CHIL @I%d@\n" (Adef.int_of_iper chil + 1)
  else ()
;

value ged_fsource base oc fam =
  match sou base fam.fsources with
  [ "" -> ()
  | s -> Printf.fprintf oc "1 SOUR %s\n" (encode s) ]
;

value ged_comment base oc fam =
  match sou base fam.comment with
  [ "" -> ()
  | s -> Printf.fprintf oc "1 NOTE %s\n" (encode s) ]
;

value has_personal_infos base per asc =
  if asc.parents <> None then True
  else if sou base per.first_name <> "?" then True
  else if sou base per.surname <> "?" then True
  else if per.birth <> Adef.codate_None then True
  else if sou base per.birth_place <> "" then True
  else if per.death <> NotDead && per.death <> DontKnowIfDead then True
  else if sou base per.occupation <> "" then True
  else if per.titles <> [] then True
  else False
;

value ged_ind_record base sel oc i =
  let per = base.data.persons.get i in
  let asc = base.data.ascends.get i in
  if has_personal_infos base per asc then
    do Printf.fprintf oc "0 @I%d@ INDI\n" (i + 1);
       ged_name base oc per;
       ged_sex base oc per;
       ged_ind_ev_str base oc per;
       ged_ind_attr_str base oc per;
       ged_famc base sel oc asc;
       Array.iter (ged_fams base sel oc) per.family;
       ged_psource base oc per;
       ged_multimedia_link base oc per;
       ged_note base oc per;
    return ()
  else ()
;

value ged_fam_record base ((per_sel, fam_sel) as sel) oc i =
  let fam = base.data.families.get i in
  if is_deleted_family fam then ()
  else
    let cpl = base.data.couples.get i in
    do Printf.fprintf oc "0 @F%d@ FAM\n" (i + 1);
       ged_marriage base oc fam;
       ged_divorce base oc fam;
       if has_personal_infos base (poi base cpl.father) (aoi base cpl.father)
       && per_sel cpl.father
       then
         Printf.fprintf oc "1 HUSB @I%d@\n" (Adef.int_of_iper cpl.father + 1)
       else ();
       if has_personal_infos base (poi base cpl.mother) (aoi base cpl.mother)
       && per_sel cpl.mother
       then
         Printf.fprintf oc "1 WIFE @I%d@\n" (Adef.int_of_iper cpl.mother + 1)
       else ();
       Array.iter (ged_child base sel oc) fam.children;
       ged_fsource base oc fam;
       ged_comment base oc fam;
    return ()
;

value find_person base p1 po p2 =
  try Gutil.person_ht_find_unique base p1 p2 po with
  [ Not_found ->
      do Printf.eprintf "Not found: %s%s %s\n"
           p1 (if po == 0 then "" else " " ^ string_of_int po) p2;
         flush stderr;
      return exit 2 ]
;

value gwb2ged base ifile ofile anc desc mem =
  let anc =
    match anc with
    [ Some (p1, po, p2) -> Some (find_person base p1 po p2)
    | None -> None ]
  in
  let desc =
    match desc with
    [ Some (p1, po, p2) -> Some (find_person base p1 po p2)
    | None -> None ]
  in
  do if not mem then
       let _ = base.data.persons.array () in
       let _ = base.data.ascends.array () in
       let _ = base.data.couples.array () in
       let _ = base.data.families.array () in
       ()
     else ();
  return
  let oc = if ofile = "" then stdout else open_out ofile in
  let ((per_sel, fam_sel) as sel) = Select.functions base anc desc in
  do ged_header base oc ifile ofile;
     flush oc;
     for i = 0 to base.data.persons.len - 1 do
       if per_sel (Adef.iper_of_int i) then ged_ind_record base sel oc i
       else ();
     done;
     flush oc;
     for i = 0 to base.data.families.len - 1 do
       if fam_sel (Adef.ifam_of_int i) then ged_fam_record base sel oc i
       else ();
     done;
     flush oc;
     Printf.fprintf oc "0 TRLR\n";
     flush oc;
     if ofile = "" then () else close_out oc;
  return ()
;

value ifile = ref "";
value ofile = ref "a.ged";
value mem = ref False;
value anc_1st = ref "";
value anc_occ = ref 0;
value anc_2nd = ref "";
value desc_1st = ref "";
value desc_occ = ref 0;
value desc_2nd = ref "";

type arg_state =
  [ ASnone | ASwaitAncOcc | ASwaitAncSurn | ASwaitDescOcc | ASwaitDescSurn ]
;
value arg_state = ref ASnone;

value usage = "Usage: " ^ Sys.argv.(0) ^ " <base> [options]
If both options -a and -d are used, intersection is assumed.
Options are:";

value speclist =
  [("-ascii",
    Arg.Unit (fun () -> do ascii.val := True; return arg_state.val := ASnone),
    ": ASCII output, instead of ANSEL");
   ("-o",
    Arg.String (fun x -> do  ofile.val := x; return arg_state.val := ASnone),
    "<ged>: output file name (default: a.ged)");
   ("-a",
    Arg.String
      (fun s -> do anc_1st.val := s; return arg_state.val := ASwaitAncOcc),
    "\"<1st_name>\" [num] \"<surname>\": select ancestors of");
   ("-d",
    Arg.String
      (fun s -> do desc_1st.val := s; return arg_state.val := ASwaitDescOcc),
    "\"<1st_name>\" [num] \"<surname>\": select descendants of");
   ("-mem",
    Arg.Unit (fun () -> do mem.val := True; return arg_state.val := ASnone),
    ": save memory space, but slower")]
;

value anon_fun s =
  match arg_state.val with
  [ ASnone -> ifile.val := s
  | ASwaitAncOcc ->
      try
        do anc_occ.val := int_of_string s; return
        arg_state.val := ASwaitAncSurn
      with
      [ Failure _ ->
          do anc_occ.val := 0; anc_2nd.val := s; return
          arg_state.val := ASnone ]
  | ASwaitAncSurn ->
      do anc_2nd.val := s; return arg_state.val := ASnone
  | ASwaitDescOcc ->
      try
        do desc_occ.val := int_of_string s; return
        arg_state.val := ASwaitDescSurn
      with
      [ Failure _ ->
          do desc_occ.val := 0; desc_2nd.val := s; return
          arg_state.val := ASnone ]
  | ASwaitDescSurn ->
      do desc_2nd.val := s; return arg_state.val := ASnone ]
;

value main () =
  do Argl.parse speclist anon_fun usage; return
  let anc =
    if anc_1st.val <> "" then
      if anc_2nd.val = "" then
        do Printf.eprintf "Misused option -a\n";
           Printf.eprintf "Use option -help for usage\n";
           flush stderr;
        return exit 2
      else Some (anc_1st.val, anc_occ.val, anc_2nd.val)
    else None
  in
  let desc =
    if desc_1st.val <> "" then
      if desc_2nd.val = "" then
        do Printf.eprintf "Misused option -d\n";
           Printf.eprintf "Use option -help for usage\n";
           flush stderr;
        return exit 2
      else Some (desc_1st.val, desc_occ.val, desc_2nd.val)
    else None
  in
  do if ofile.val = "-" then ofile.val := "" else ();
     if ifile.val = "" then
        do Printf.eprintf "Missing base name\n";
           Printf.eprintf "Use option -help for usage\n";
           flush stderr;
        return exit 2
     else ();
     match
       try Some (Iobase.input ifile.val) with [ Sys_error _ -> None ]
     with
     [ Some base -> gwb2ged base ifile.val ofile.val anc desc mem.val
     | None ->
         do Printf.eprintf "Can't open base %s\n" ifile.val;
            flush stderr;
         return exit 0 ];
  return ()
;

Printexc.catch main ();
