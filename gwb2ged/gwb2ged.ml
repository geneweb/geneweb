(* $Id: gwb2ged.ml,v 3.1 1999-11-10 08:44:12 ddr Exp $ *)
(* Copyright (c) INRIA *)

open Def;
open Gutil;

value ascii = ref True;

value month_txt =
  [| "JAN"; "FEB"; "MAR"; "APR"; "MAY"; "JUN";
     "JUL"; "AUG"; "SEP"; "OCT"; "NOV"; "DEC" |]
;

value french_txt =
  [| "VEND"; "BRUM"; "FRIM"; "NIVO"; "PLUV"; "VENT";
     "GERM"; "FLOR"; "PRAI"; "MESS"; "THER"; "FRUC";
     "COMP" |]
;

value hebrew_txt =
  [| "TSH"; "CSH"; "KSL"; "TVT"; "SHV"; "ADR";
     "ADS"; "NSN"; "IYR"; "SVN"; "TMZ"; "AAV"; "ELL" |]
;

value ged_month cal m =
  match cal with
  [ Dgregorian | Djulian ->
      if m >= 1 && m <= Array.length month_txt then month_txt.(m-1)
      else failwith "ged_month"
  | Dfrench ->
      if m >= 1 && m <= Array.length french_txt then french_txt.(m-1)
      else failwith "ged_month"
  | Dhebrew ->
      if m >= 1 && m <= Array.length hebrew_txt then hebrew_txt.(m-1)
      else failwith "ged_month" ]
;

value encode s =
  if ascii.val then s else Ansel.of_iso_8859_1 s
;

value max_len = 78;

value next_char_pair_overflows s len i =
  loop False (len + 1) (i + 1) where rec loop prec_was_space len i =
    if len < max_len then
      if i < String.length s then
        match s.[i] with
        [ ' ' | '\n' -> loop True (len + 1) (i + 1)
        | _ -> if prec_was_space then loop False (len + 1) (i + 1) else False ]
      else False
    else True
;

value br = "<br>";

value rec display_note_aux oc s len i =
  if i == String.length s then Printf.fprintf oc "\n"
  else
    let c = if s.[i] = '\n' then ' ' else s.[i] in
    if i <= String.length s - String.length br
    && String.lowercase (String.sub s i (String.length br)) = br then
      do Printf.fprintf oc "\n2 CONT "; return
      let i = i + String.length br in
      let i =
        if i < String.length s && s.[i] == '\n' then i + 1
        else i
      in
      display_note_aux oc s (String.length "2 CONT ") i
    else if s.[i] == '\n' then
      do Printf.fprintf oc "\n2 CONC "; return
      display_note_aux oc s (String.length "2 CONC ") (i + 1)
    else if len == max_len
         || s.[i] <> ' ' && next_char_pair_overflows s len i
      then
      do Printf.fprintf oc "\n2 CONC %c" c; return
      display_note_aux oc s (String.length "2 CONC .") (i + 1)
    else
      do output_char oc s.[i]; return
      display_note_aux oc s (len + 1) (i + 1)
;

value display_note oc s =
  do Printf.fprintf oc "1 NOTE ";
     display_note_aux oc (encode s) (String.length "1 NOTE ") 0;
  return ()
;

value ged_header base oc ifile ofile =
  do Printf.fprintf oc "0 HEAD\n";
     Printf.fprintf oc "1 SOUR GeneWeb\n";
     Printf.fprintf oc "2 VERS %s\n" Version.txt;
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
       let mon = ged_month Dgregorian (tm.Unix.tm_mon + 1) in
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
     let s = base.data.bnotes.nread 0 in
     if s = "" then () else display_note oc s;
  return ()
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
  [ Male -> Printf.fprintf oc "1 SEX M\n"
  | Female -> Printf.fprintf oc "1 SEX F\n"
  | Neuter -> () ]
;

value ged_calendar oc =
  fun
  [ Dgregorian -> ()
  | Djulian -> Printf.fprintf oc "@#DJULIAN@ "
  | Dfrench -> Printf.fprintf oc "@#DFRENCH R@ "
  | Dhebrew -> Printf.fprintf oc "@#DHEBREW@ " ]
;

value ged_date_dmy oc dt cal =
  do match dt.prec with
     [ Sure -> ()
     | About -> Printf.fprintf oc "ABT "
     | Maybe -> Printf.fprintf oc "EST "
     | Before -> Printf.fprintf oc "BEF "
     | After -> Printf.fprintf oc "AFT "
     | OrYear i -> Printf.fprintf oc "BET "
     | YearInt i -> Printf.fprintf oc "BET " ];
     ged_calendar oc cal;
     if dt.day <> 0 then Printf.fprintf oc "%02d " dt.day else ();
     if dt.month <> 0 then Printf.fprintf oc "%s " (ged_month cal dt.month)
     else ();
     Printf.fprintf oc "%d" dt.year;
     match dt.prec with
     [ OrYear i ->
         do ged_calendar oc cal;
            Printf.fprintf oc " AND %d" i;
         return ()
     | YearInt i ->
         do ged_calendar oc cal;
            Printf.fprintf oc " AND %d" i;
         return ()
     | _ -> () ];
  return ()
;

value ged_date oc =
  fun
  [ Dgreg d Dgregorian -> ged_date_dmy oc d Dgregorian
  | Dgreg d Djulian -> ged_date_dmy oc (Calendar.julian_of_gregorian d) Djulian
  | Dgreg d Dfrench -> ged_date_dmy oc (Calendar.french_of_gregorian d) Dfrench
  | Dgreg d Dhebrew -> ged_date_dmy oc (Calendar.hebrew_of_gregorian d) Dhebrew
  | Dtext t -> Printf.fprintf oc "(%s)" t ]
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

value adop_fam_list = ref [];
value adop_fam_cnt = ref 0;

value ged_adoption base oc per r =
  do Printf.fprintf oc "1 ADOP Y\n";
     adop_fam_list.val :=
       [(r.r_fath, r.r_moth, per.cle_index) :: adop_fam_list.val];
     incr adop_fam_cnt;
     Printf.fprintf oc "2 FAMC @F%d@\n"
       (base.data.families.len + adop_fam_cnt.val);
     Printf.fprintf oc "3 ADOP ";
     match (r.r_fath, r.r_moth) with
     [ (Some _, None) -> Printf.fprintf oc "HUSB"
     | (None, Some _) -> Printf.fprintf oc "WIFE"
     | (Some _, Some _) -> Printf.fprintf oc "BOTH"
     | _ -> () ];
     Printf.fprintf oc "\n";
  return ()
;
         
value ged_fam_adop base oc i (fath, moth, child) =
  do Printf.fprintf oc "0 @F%d@ FAM\n" i;
     match fath with
     [ Some i -> Printf.fprintf oc "1 HUSB @I%d@\n" (Adef.int_of_iper i + 1)
     | _ -> () ];
     match moth with
     [ Some i -> Printf.fprintf oc "1 WIFE @I%d@\n" (Adef.int_of_iper i + 1)
     | _ -> () ];
(*
     Printf.fprintf oc "1 CHIL @I%d@\n" (Adef.int_of_iper child + 1);
*)
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
     List.iter
       (fun r ->
          if r.r_type = Adoption then ged_adoption base oc per r else ())
       per.rparents;
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
     Printf.fprintf oc "%s" (encode (sou base tit.t_ident));
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
         do Printf.fprintf oc "2 DATE FROM ";
            ged_date oc sd;
            Printf.fprintf oc "\n";
         return ()
     | (None, Some sd) ->
         do Printf.fprintf oc "2 DATE TO ";
            ged_date oc sd;
            Printf.fprintf oc "\n";
         return ()
     | (Some sd1, Some sd2) ->
         do Printf.fprintf oc "2 DATE FROM ";
            ged_date oc sd1;
            Printf.fprintf oc " TO ";
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

value ged_godparent oc godp =
  fun
  [ Some ip ->
      do Printf.fprintf oc "1 ASSO @I%d@\n" (Adef.int_of_iper ip + 1);
         Printf.fprintf oc "2 RELA %s\n" godp;
      return ()
  | None -> () ]
;

value ged_asso base oc per =
  List.iter
    (fun r ->
       if r.r_type = GodParent then
         do ged_godparent oc "GODF" r.r_fath;
            ged_godparent oc "GODM" r.r_moth;
         return ()
       else ())
    per.rparents
;

value ged_psource base oc per =
  match sou base per.psources with
  [ "" -> ()
  | s -> Printf.fprintf oc "1 SOUR %s\n" (encode s) ]
;

value ged_multimedia_link base oc per =
  match sou base per.image with
  [ "" -> ()
  | s ->
      do Printf.fprintf oc "1 OBJE\n";
         Printf.fprintf oc "2 FILE %s\n" s;
      return () ]
;

value ged_note base oc per =
  match sou base per.notes with
  [ "" -> ()
  | s -> display_note oc s ]
;

value ged_marriage base oc fam =
  match (Adef.od_of_codate fam.marriage, sou base fam.marriage_place) with
  [ (None, "") -> ()
  | (d, pl) ->
      do Printf.fprintf oc "1 MARR";
         ged_ev_detail oc 2 d pl (sou base fam.marriage_src);
         if fam.not_married then Printf.fprintf oc "2 PLAC unmarried\n"
         else ();
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
  let uni = base.data.unions.get i in
  if has_personal_infos base per asc then
    do Printf.fprintf oc "0 @I%d@ INDI\n" (i + 1);
       ged_name base oc per;
       ged_sex base oc per;
       ged_ind_ev_str base oc per;
       ged_ind_attr_str base oc per;
       ged_famc base sel oc asc;
       Array.iter (ged_fams base sel oc) uni.family;
       ged_asso base oc per;
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
    let des = base.data.descends.get i in
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
       Array.iter (ged_child base sel oc) des.children;
       ged_fsource base oc fam;
       ged_comment base oc fam;
    return ()
;

value find_person base p1 po p2 =
  try Gutil.person_ht_find_unique base p1 p2 po with
  [ Not_found ->
      do Printf.printf "Not found: %s%s %s\n"
           p1 (if po == 0 then "" else " " ^ string_of_int po) p2;
         flush stdout;
      return exit 2 ]
;

value surnames = ref [];
value no_spouses_parents = ref False;

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
  let ((per_sel, fam_sel) as sel) =
    Select.functions base anc desc surnames.val no_spouses_parents.val
  in
  do ged_header base oc ifile ofile;
     flush oc;
     for i = 0 to base.data.persons.len - 1 do
       if per_sel (Adef.iper_of_int i) then ged_ind_record base sel oc i
       else ();
     done;
     for i = 0 to base.data.families.len - 1 do
       if fam_sel (Adef.ifam_of_int i) then ged_fam_record base sel oc i
       else ();
     done;
     let _ =
       List.fold_right
         (fun adop i -> do ged_fam_adop base oc i adop; return i+1)
         adop_fam_list.val (base.data.families.len + 1)
     in ();
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
If several options -s are used, union is assumed.
When option -s is used, the options -a and -d are ignored.
Options are:";

value speclist =
  [("-charset",
    Arg.String
      (fun x ->
         do arg_state.val := ASnone; return
         match x with
         [ "ASCII" -> ascii.val := True
         | "ANSEL" -> ascii.val := False
         | _ -> raise (Arg.Bad "bad -charset value") ]),
    "[ASCII|ANSEL]:
     Set charset. Default is ASCII. Warning: value ANSEL works correctly only
     on iso-8859-1 encoded data bases.");
   ("-o",
    Arg.String (fun x -> do  ofile.val := x; return arg_state.val := ASnone),
    "<ged>: output file name (default: a.ged)");
   ("-mem",
    Arg.Unit (fun () -> do mem.val := True; return arg_state.val := ASnone),
    ": save memory space, but slower");
   ("-a",
    Arg.String
      (fun s -> do anc_1st.val := s; return arg_state.val := ASwaitAncOcc),
    "\"<1st_name>\" [num] \"<surname>\": select ancestors of");
   ("-d",
    Arg.String
      (fun s -> do desc_1st.val := s; return arg_state.val := ASwaitDescOcc),
    "\"<1st_name>\" [num] \"<surname>\": select descendants of");
   ("-s", Arg.String (fun x -> surnames.val := [x :: surnames.val]),
    "\"<surname>\" : select this surname (option usable several times)");
   ("-nsp", Arg.Set no_spouses_parents,
    ": no spouses' parents (for option -s)")]
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
        do Printf.printf "Misused option -a\n";
           Printf.printf "Use option -help for usage\n";
           flush stdout;
        return exit 2
      else Some (anc_1st.val, anc_occ.val, anc_2nd.val)
    else None
  in
  let desc =
    if desc_1st.val <> "" then
      if desc_2nd.val = "" then
        do Printf.printf "Misused option -d\n";
           Printf.printf "Use option -help for usage\n";
           flush stdout;
        return exit 2
      else Some (desc_1st.val, desc_occ.val, desc_2nd.val)
    else None
  in
  do if ofile.val = "-" then ofile.val := "" else ();
     if ifile.val = "" then
        do Printf.printf "Missing base name\n";
           Printf.printf "Use option -help for usage\n";
           flush stdout;
        return exit 2
     else ();
     match
       try Some (Iobase.input ifile.val) with [ Sys_error _ -> None ]
     with
     [ Some base -> gwb2ged base ifile.val ofile.val anc desc mem.val
     | None ->
         do Printf.printf "Can't open base %s\n" ifile.val;
            flush stdout;
         return exit 2 ];
  return ()
;

Printexc.catch main ();
