(* $Id: gwb2ged.ml,v 5.29 2008-01-08 11:58:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def;
open Gutil;
open Gwdb;
open Mutil;
open Printf;

type charset =
  [ Ansel
  | Ascii
  | Utf8 ]
;

value charset = ref Utf8;
value no_notes = ref False;
value no_picture = ref False;

value month_txt =
  [| "JAN"; "FEB"; "MAR"; "APR"; "MAY"; "JUN"; "JUL"; "AUG"; "SEP"; "OCT";
     "NOV"; "DEC" |]
;

value french_txt =
  [| "VEND"; "BRUM"; "FRIM"; "NIVO"; "PLUV"; "VENT"; "GERM"; "FLOR"; "PRAI";
     "MESS"; "THER"; "FRUC"; "COMP" |]
;

value hebrew_txt =
  [| "TSH"; "CSH"; "KSL"; "TVT"; "SHV"; "ADR"; "ADS"; "NSN"; "IYR"; "SVN";
     "TMZ"; "AAV"; "ELL" |]
;

value ged_month cal m =
  match cal with
  [ Dgregorian | Djulian ->
      if m >= 1 && m <= Array.length month_txt then month_txt.(m - 1)
      else failwith "ged_month"
  | Dfrench ->
      if m >= 1 && m <= Array.length french_txt then french_txt.(m - 1)
      else failwith "ged_month"
  | Dhebrew ->
      if m >= 1 && m <= Array.length hebrew_txt then hebrew_txt.(m - 1)
      else failwith "ged_month" ]
;

value encode s =
  match charset.val with
  [ Ansel ->
      let s = if Mutil.utf_8_db.val then Mutil.iso_8859_1_of_utf_8 s else s in
      Ansel.of_iso_8859_1 s
  | Ascii ->
      if Mutil.utf_8_db.val then Mutil.iso_8859_1_of_utf_8 s else s
  | Utf8 ->
      if Mutil.utf_8_db.val then s else Mutil.utf_8_of_iso_8859_1 s ]
;

value max_len = 78;

value next_char_pair_overflows s len i =
  loop False (len + 1) (i + 1) where rec loop prec_was_space len i =
    if len < max_len then
      if i < String.length s then
        match s.[i] with
        [ ' ' | '\n' -> loop True (len + 1) (i + 1)
        | _ ->
            if prec_was_space then loop False (len + 1) (i + 1) else False ]
      else False
    else True
;

value br = "<br>";

value rec display_note_aux oc s len i =
  if i = String.length s then fprintf oc "\n"
  else
    let c = if s.[i] = '\n' then ' ' else s.[i] in
    if i <= String.length s - String.length br &&
       String.lowercase (String.sub s i (String.length br)) = br then
       do {
      fprintf oc "\n2 CONT ";
      let i = i + String.length br in
      let i = if i < String.length s && s.[i] = '\n' then i + 1 else i in
      display_note_aux oc s (String.length "2 CONT ") i
    }
    else if
      len = max_len || c <> ' ' && next_char_pair_overflows s len i
    then do {
      fprintf oc "\n2 CONC %c" c;
      display_note_aux oc s (String.length "2 CONC .") (i + 1)
    }
    else do { output_char oc c; display_note_aux oc s (len + 1) (i + 1) }
;

value display_note oc s =
  do {
    fprintf oc "1 NOTE ";
    display_note_aux oc (encode s) (String.length "1 NOTE ") 0;
  }
;

value ged_header base oc ifile ofile =
  do {
    fprintf oc "0 HEAD\n";
    fprintf oc "1 SOUR GeneWeb\n";
    fprintf oc "2 VERS %s\n" Version.txt;
    fprintf oc "2 NAME %s\n" (Filename.basename Sys.argv.(0));
    fprintf oc "2 CORP INRIA\n";
    fprintf oc "3 ADDR http://www.geneweb.org\n";
    fprintf oc "2 DATA %s\n"
      (let fname = Filename.basename ifile in
       if Filename.check_suffix fname ".gwb" then fname else fname ^ ".gwb");
    try
      let tm = Unix.localtime (Unix.time ()) in
      let mon = ged_month Dgregorian (tm.Unix.tm_mon + 1) in
      do {
        fprintf oc "1 DATE %02d %s %d\n" tm.Unix.tm_mday mon
          (1900 + tm.Unix.tm_year);
        fprintf oc "2 TIME %02d:%02d:%02d\n" tm.Unix.tm_hour
          tm.Unix.tm_min tm.Unix.tm_sec;
      }
    with _ ->
      ();
    if ofile <> "" then
      fprintf oc "1 FILE %s\n" (Filename.basename ofile)
    else ();
    fprintf oc "1 GEDC\n";
    match charset.val with
    [ Ansel | Ascii -> fprintf oc "2 VERS 5.5\n"
    | Utf8 -> fprintf oc "2 VERS 5.5.1\n" ];
    fprintf oc "2 FORM LINEAGE-LINKED\n";
    match charset.val with
    [ Ansel -> fprintf oc "1 CHAR ANSEL\n"
    | Ascii -> fprintf oc "1 CHAR ASCII\n"
    | Utf8 -> fprintf oc "1 CHAR UTF-8\n" ];
    if no_notes.val then ()
    else
      let s = base_notes_read base "" in
      if s = "" then () else display_note oc s;
  }
;

value sub_string_index s t =
  loop 0 0 where rec loop i j =
    if j = String.length t then Some (i - j)
    else if i = String.length s then None
    else if s.[i] = t.[j] then loop (i + 1) (j + 1)
    else loop (i + 1) 0
;

value ged_1st_name base p =
  let fn = sou base (get_first_name p) in
  match get_first_names_aliases p with
  [ [n :: _] ->
      let fna = sou base n in
      match sub_string_index fna fn with
      [ Some i ->
          let j = i + String.length fn in
          String.sub fna 0 i ^ "\"" ^ fn ^ "\"" ^
          String.sub fna j (String.length fna - j)
      | None -> fn ]
  | [] -> fn ]
;

value string_of_list =
  loop "" where rec loop r =
    fun
    [ [s :: l] -> if r = "" then loop s l else loop (r ^ "," ^ s) l
    | [] -> r ]
;

value ged_name base oc per =
  do {
    fprintf oc "1 NAME %s /%s/\n"
      (encode (Mutil.nominative (ged_1st_name base per)))
      (encode (Mutil.nominative (sou base (get_surname per))));
    let n = sou base (get_public_name per) in
    if n <> "" then fprintf oc "2 GIVN %s\n" (encode n) else ();
    match get_qualifiers per with
    [ [nn :: _] -> fprintf oc "2 NICK %s\n" (encode (sou base nn))
    | [] -> () ];
    match get_surnames_aliases per with
    [ [] -> ()
    | list ->
        let list = List.map (fun n -> encode (sou base n)) list in
        fprintf oc "2 SURN %s\n" (string_of_list list) ];
    List.iter (fun s -> fprintf oc "1 NAME %s\n" (encode (sou base s)))
      (get_aliases per);
  }
;

value ged_sex base oc per =
  match get_sex per with
  [ Male -> fprintf oc "1 SEX M\n"
  | Female -> fprintf oc "1 SEX F\n"
  | Neuter -> () ]
;

value ged_calendar oc =
  fun
  [ Dgregorian -> ()
  | Djulian -> fprintf oc "@#DJULIAN@ "
  | Dfrench -> fprintf oc "@#DFRENCH R@ "
  | Dhebrew -> fprintf oc "@#DHEBREW@ " ]
;

value ged_date_dmy oc dt cal =
  do {
    match dt.prec with
    [ Sure -> ()
    | About -> fprintf oc "ABT "
    | Maybe -> fprintf oc "EST "
    | Before -> fprintf oc "BEF "
    | After -> fprintf oc "AFT "
    | OrYear i -> fprintf oc "BET "
    | YearInt i -> fprintf oc "BET " ];
    ged_calendar oc cal;
    if dt.day <> 0 then fprintf oc "%02d " dt.day else ();
    if dt.month <> 0 then fprintf oc "%s " (ged_month cal dt.month)
    else ();
    fprintf oc "%d" dt.year;
    match dt.prec with
    [ OrYear i ->
        do { fprintf oc " AND "; ged_calendar oc cal; fprintf oc "%d" i; }
    | YearInt i ->
        do { fprintf oc " AND "; ged_calendar oc cal; fprintf oc "%d" i; }
    | _ -> () ];
  }
;

value ged_date oc =
  fun
  [ Dgreg d Dgregorian -> ged_date_dmy oc d Dgregorian
  | Dgreg d Djulian ->
      ged_date_dmy oc (Calendar.julian_of_gregorian d) Djulian
  | Dgreg d Dfrench ->
      ged_date_dmy oc (Calendar.french_of_gregorian d) Dfrench
  | Dgreg d Dhebrew ->
      ged_date_dmy oc (Calendar.hebrew_of_gregorian d) Dhebrew
  | Dtext t -> fprintf oc "(%s)" t ]
;

value ged_ev_detail oc n typ d pl src =
  do {
    match (typ, d, pl) with
    [ ("", None, "") -> fprintf oc " Y"
    | _ -> () ];
    fprintf oc "\n";
    if typ = "" then () else fprintf oc "%d TYPE %s\n" n typ;
    match d with
    [ Some d ->
        do {
          fprintf oc "%d DATE " n;
          ged_date oc d;
          fprintf oc "\n";
        }
    | None -> () ];
    if pl <> "" then fprintf oc "%d PLAC %s\n" n (encode pl) else ();
    if src <> "" then fprintf oc "%d SOUR %s\n" n (encode src) else ();
  }
;

value adop_fam_list = ref [];
value adop_fam_cnt = ref 0;

value ged_adoption base (per_sel, fam_sel) oc per r =
  let sel =
    match (r.r_fath, r.r_moth) with
    [ (Some ip1, Some ip2) -> per_sel ip1 && per_sel ip2
    | (Some ip1, _) -> per_sel ip1
    | (_, Some ip2) -> per_sel ip2
    | _ -> True ]
  in
  if sel then do {
    fprintf oc "1 ADOP Y\n";
    adop_fam_list.val :=
      [(r.r_fath, r.r_moth, get_key_index per) :: adop_fam_list.val];
    incr adop_fam_cnt;
    fprintf oc "2 FAMC @F%d@\n" (nb_of_families base + adop_fam_cnt.val);
    fprintf oc "3 ADOP ";
    match (r.r_fath, r.r_moth) with
    [ (Some _, None) -> fprintf oc "HUSB"
    | (None, Some _) -> fprintf oc "WIFE"
    | (Some _, Some _) -> fprintf oc "BOTH"
    | _ -> () ];
    fprintf oc "\n";
  }
  else ()
;

value ged_fam_adop base oc i (fath, moth, child) =
  do {
    fprintf oc "0 @F%d@ FAM\n" i;
    match fath with
    [ Some i -> fprintf oc "1 HUSB @I%d@\n" (Adef.int_of_iper i + 1)
    | _ -> () ];
    match moth with
    [ Some i -> fprintf oc "1 WIFE @I%d@\n" (Adef.int_of_iper i + 1)
    | _ -> () ];
  }
;

value ged_ind_ev_str base sel oc per =
  do {
    let pl = sou base (get_birth_place per) in
    let src = sou base (get_birth_src per) in
    match (Adef.od_of_codate (get_birth per), pl) with
    [ (None, "") -> ()
    | (None, pl) ->
        do {
          fprintf oc "1 BIRT"; ged_ev_detail oc 2 "" None pl src;
        }
    | (od, pl) ->
        do {
          fprintf oc "1 BIRT"; ged_ev_detail oc 2 "" od pl src;
        } ];
    List.iter
      (fun r ->
         if r.r_type = Adoption then ged_adoption base sel oc per r else ())
      (get_rparents per);
    let pl = sou base (get_baptism_place per) in
    let src = sou base (get_baptism_src per) in
    match (Adef.od_of_codate (get_baptism per), pl) with
    [ (None, "") -> ()
    | (od, pl) ->
        do {
          fprintf oc "1 BAPM"; ged_ev_detail oc 2 "" od pl src;
        } ];
    let pl = sou base (get_death_place per) in
    let src = sou base (get_death_src per) in
    match get_death per with
    [ NotDead -> ()
    | Death dr cd ->
        do {
          fprintf oc "1 DEAT";
          ged_ev_detail oc 2 "" (Some (Adef.date_of_cdate cd)) pl src;
        }
    | DeadYoung | DeadDontKnowWhen | OfCourseDead ->
        do {
          fprintf oc "1 DEAT"; ged_ev_detail oc 2 "" None pl src;
        }
    | DontKnowIfDead -> fprintf oc "1 DEAT\n" ];
    let pl = sou base (get_burial_place per) in
    let src = sou base (get_burial_src per) in
    match get_burial per with
    [ UnknownBurial -> ()
    | Buried cod ->
        do {
          fprintf oc "1 BURI";
          ged_ev_detail oc 2 "" (Adef.od_of_codate cod) pl src;
        }
    | Cremated cod ->
        do {
          fprintf oc "1 CREM";
          ged_ev_detail oc 2 "" (Adef.od_of_codate cod) pl src;
        } ];
  }
;

value ged_title base oc per tit =
  do {
    fprintf oc "1 TITL ";
    fprintf oc "%s" (encode (sou base tit.t_ident));
    match sou base tit.t_place with
    [ "" -> ()
    | pl -> fprintf oc ", %s" (encode pl) ];
    if tit.t_nth <> 0 then fprintf oc ", %d" tit.t_nth else ();
    fprintf oc "\n";
    match
      (Adef.od_of_codate tit.t_date_start, Adef.od_of_codate tit.t_date_end)
    with
    [ (None, None) -> ()
    | (Some sd, None) ->
        do {
          fprintf oc "2 DATE FROM ";
          ged_date oc sd;
          fprintf oc "\n";
        }
    | (None, Some sd) ->
        do {
          fprintf oc "2 DATE TO ";
          ged_date oc sd;
          fprintf oc "\n";
        }
    | (Some sd1, Some sd2) ->
        do {
          fprintf oc "2 DATE FROM ";
          ged_date oc sd1;
          fprintf oc " TO ";
          ged_date oc sd2;
          fprintf oc "\n";
        } ];
    match tit.t_name with
    [ Tmain ->
        fprintf oc "2 NOTE %s\n" (encode (sou base (get_public_name per)))
    | Tname n -> fprintf oc "2 NOTE %s\n" (encode (sou base n))
    | Tnone -> () ];
  }
;

value ged_ind_attr_str base oc per =
  do {
    match sou base (get_occupation per) with
    [ "" -> ()
    | occu -> fprintf oc "1 OCCU %s\n" (encode occu) ];
    List.iter (ged_title base oc per) (get_titles per);
  }
;

value ged_famc base (per_sel, fam_sel) oc asc =
  match get_parents asc with
  [ Some ifam ->
      if fam_sel ifam then
        fprintf oc "1 FAMC @F%d@\n" (Adef.int_of_ifam ifam + 1)
      else ()
  | None -> () ]
;

value ged_fams base (per_sel, fam_sel) oc ifam =
  if fam_sel ifam then
    fprintf oc "1 FAMS @F%d@\n" (Adef.int_of_ifam ifam + 1)
  else ()
;

value ged_godparent per_sel oc godp =
  fun
  [ Some ip ->
      if per_sel ip then do {
        fprintf oc "1 ASSO @I%d@\n" (Adef.int_of_iper ip + 1);
        fprintf oc "2 TYPE INDI\n";
        fprintf oc "2 RELA %s\n" godp;
      }
      else ()
  | None -> () ]
;

value ged_witness fam_sel oc ifam =
  if fam_sel ifam then do {
    fprintf oc "1 ASSO @F%d@\n" (Adef.int_of_ifam ifam + 1);
    fprintf oc "2 TYPE FAM\n";
    fprintf oc "2 RELA witness\n";
  }
  else ()
;

value ged_asso base (per_sel, fam_sel) oc per =
  do {
    List.iter
      (fun r ->
         if r.r_type = GodParent then do {
           ged_godparent per_sel oc "GODF" r.r_fath;
           ged_godparent per_sel oc "GODM" r.r_moth;
         }
         else ())
      (get_rparents per);
    List.iter
      (fun ic ->
         let c = poi base ic in
         if get_sex c = Male then
           List.iter
             (fun ifam ->
                let fam = foi base ifam in
                if array_mem (get_key_index per) (get_witnesses fam) then
                  ged_witness fam_sel oc ifam
                else ())
             (Array.to_list (get_family c))
         else ())
      (get_related per);
  }
;

value ged_psource base oc per =
  match sou base (get_psources per) with
  [ "" -> ()
  | s -> fprintf oc "1 SOUR %s\n" (encode s) ]
;

value ged_multimedia_link base oc per =
  match sou base (get_image per) with
  [ "" -> ()
  | s ->
      if not no_picture.val then
        do {fprintf oc "1 OBJE\n"; fprintf oc "2 FILE %s\n" s;}
      else () ]
;

value ged_note base oc per =
  match sou base (get_notes per) with
  [ "" -> ()
  | s -> display_note oc s ]
;

value ged_marriage base oc fam =
  match
    (Adef.od_of_codate (get_marriage fam), sou base (get_marriage_place fam),
     get_relation fam)
  with
  (* Pourquoi ne pas exporter dans ce cas ? *)
  (*[ (None, "", Married | Engaged) -> ()*)
  [ (d, pl, _) ->
      do {
        fprintf oc "1 %s"
          (if get_relation fam = Engaged then "ENGA" else "MARR");
        let typ =
          if get_relation fam = NoSexesCheckNotMarried
          || get_relation fam = NoSexesCheckMarried then "gay"
          else ""
        in
        ged_ev_detail oc 2 typ d pl (sou base (get_marriage_src fam));
        if get_relation fam = NotMarried then
          fprintf oc "2 PLAC unmarried\n"
        else ();
      } ]
;

value ged_divorce base oc fam =
  match get_divorce fam with
  [ NotDivorced -> ()
  | Separated -> ()
  | Divorced cd ->
      let d = Adef.od_of_codate cd in
      do { fprintf oc "1 DIV"; ged_ev_detail oc 2 "" d "" ""; } ]
;

value ged_child base (per_sel, fam_sel) oc chil =
  if per_sel chil then
    fprintf oc "1 CHIL @I%d@\n" (Adef.int_of_iper chil + 1)
  else ()
;

value ged_fsource base oc fam =
  match sou base (get_fsources fam) with
  [ "" -> ()
  | s -> fprintf oc "1 SOUR %s\n" (encode s) ]
;

value ged_comment base oc fam =
  match sou base (get_comment fam) with
  [ "" -> ()
  | s -> display_note oc s ]
;

value has_personal_infos base per =
  if get_parents per <> None then True
  else if sou base (get_first_name per) <> "?" then True
  else if sou base (get_surname per) <> "?" then True
  else if get_birth per <> Adef.codate_None then True
  else if sou base (get_birth_place per) <> "" then True
  else if get_death per <> NotDead && get_death per <> DontKnowIfDead then
    True
  else if sou base (get_occupation per) <> "" then True
  else if get_titles per <> [] then True
  else False
;

value ged_ind_record base sel oc i =
  let per = poi base (Adef.iper_of_int i) in
  if has_personal_infos base per then do {
    fprintf oc "0 @I%d@ INDI\n" (i + 1);
    ged_name base oc per;
    ged_sex base oc per;
    ged_ind_ev_str base sel oc per;
    ged_ind_attr_str base oc per;
    ged_famc base sel oc per;
    Array.iter (ged_fams base sel oc) (get_family per);
    ged_asso base sel oc per;
    ged_psource base oc per;
    ged_multimedia_link base oc per;
    ged_note base oc per;
  }
  else ()
;

value ged_fam_record base ((per_sel, fam_sel) as sel) oc i =
  let fam = foi base (Adef.ifam_of_int i) in
  if is_deleted_family fam then ()
  else do {
    fprintf oc "0 @F%d@ FAM\n" (i + 1);
    ged_marriage base oc fam;
    ged_divorce base oc fam;
    if has_personal_infos base (poi base (get_father fam)) &&
       per_sel (get_father fam)
    then
      fprintf oc "1 HUSB @I%d@\n" (Adef.int_of_iper (get_father fam) + 1)
    else ();
    if has_personal_infos base (poi base (get_mother fam)) &&
       per_sel (get_mother fam)
    then
      fprintf oc "1 WIFE @I%d@\n" (Adef.int_of_iper (get_mother fam) + 1)
    else ();
    Array.iter (ged_child base sel oc) (get_children fam);
    ged_fsource base oc fam;
    ged_comment base oc fam;
  }
;

value find_person base p1 po p2 =
  match person_of_key base p1 p2 po with
  [ Some ip -> ip
  | None -> do {
      printf "Not found: %s%s %s\n" p1
        (if po = 0 then "" else " " ^ string_of_int po) p2;
      flush stdout;
      exit 2
    } ]
;

value surnames = ref [];
value no_spouses_parents = ref False;
value censor = ref 0;
value with_siblings = ref False;

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
  do {
    if not mem then do {
      load_ascends_array base;
      load_unions_array base;
      load_couples_array base;
      load_descends_array base;
    }
    else ();
    let oc = if ofile = "" then stdout else open_out ofile in
    let ((per_sel, fam_sel) as sel) =
      Select.functions base anc desc surnames.val None no_spouses_parents.val
        censor.val with_siblings.val (-1)
    in
    ged_header base oc ifile ofile;
    flush oc;
    for i = 0 to nb_of_persons base - 1 do {
      if per_sel (Adef.iper_of_int i) then ged_ind_record base sel oc i
      else ()
    };
    for i = 0 to nb_of_families base - 1 do {
      if fam_sel (Adef.ifam_of_int i) then ged_fam_record base sel oc i
      else ()
    };
    let _ =
      List.fold_right
        (fun adop i -> do { ged_fam_adop base oc i adop; i + 1 })
        adop_fam_list.val (nb_of_families base + 1)
    in
    fprintf oc "0 TRLR\n";
    flush oc;
    if ofile = "" then () else close_out oc;
  }
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

value errmsg =
  "Usage: " ^ Sys.argv.(0) ^ " \
<base> [options]
If both options -a and -d are used, intersection is assumed.
If several options -s are used, union is assumed.
Options are:"
;

value speclist =
  [("-charset",
    Arg.String
      (fun x ->
         do {
           arg_state.val := ASnone;
           match x with
           [ "ASCII" -> charset.val := Ascii
           | "ANSEL" -> charset.val := Ansel
           | "UTF-8" -> charset.val := Utf8
           | _ -> raise (Arg.Bad "bad -charset value") ]
         }),
    "[ASCII|ANSEL|UTF-8]: set charset; default is UTF-8.");
   ("-o",
    Arg.String (fun x -> do { ofile.val := x; arg_state.val := ASnone }),
    "<ged>: output file name (default: a.ged)");
   ("-mem",
    Arg.Unit (fun () -> do { mem.val := True; arg_state.val := ASnone }),
    ": save memory space, but slower");
   ("-a",
    Arg.String
      (fun s -> do { anc_1st.val := s; arg_state.val := ASwaitAncOcc }),
    "\"<1st_name>\" [num] \"<surname>\": select ancestors of");
   ("-d",
    Arg.String
      (fun s -> do { desc_1st.val := s; arg_state.val := ASwaitDescOcc }),
    "\"<1st_name>\" [num] \"<surname>\": select descendants of");
   ("-aws",
    Arg.String
      (fun s ->
         do {
           anc_1st.val := s;
           arg_state.val := ASwaitAncOcc;
           with_siblings.val := True;
         }),
    "\"<1st_name>\" [num] \"<surname>\" : select ancestors with siblings");
   ("-s", Arg.String (fun x -> surnames.val := [x :: surnames.val]),
    "\"<surname>\" : select this surname (option usable several times)");
   ("-nsp", Arg.Set no_spouses_parents,
    ": no spouses' parents (for options -s and -d)");
   ("-nn", Arg.Set no_notes, ": no (database) notes");
   ("-nopicture", Arg.Set no_picture, ": Don't extract individual picture.");
   ("-c", Arg.Int (fun i -> censor.val := i), "\
<num> :
     When a person is born less than <num> years ago, it is not exported unless
     it is Public. All the spouses and descendants are also censored.")]
;

value anonfun s =
  match arg_state.val with
  [ ASnone ->
      if ifile.val = "" then ifile.val := s
      else raise (Arg.Bad "Cannot treat several databases")
  | ASwaitAncOcc ->
      try
        do { anc_occ.val := int_of_string s; arg_state.val := ASwaitAncSurn }
      with
      [ Failure _ ->
          do { anc_occ.val := 0; anc_2nd.val := s; arg_state.val := ASnone } ]
  | ASwaitAncSurn -> do { anc_2nd.val := s; arg_state.val := ASnone }
  | ASwaitDescOcc ->
      try
        do {
          desc_occ.val := int_of_string s; arg_state.val := ASwaitDescSurn
        }
      with
      [ Failure _ ->
          do {
            desc_occ.val := 0; desc_2nd.val := s; arg_state.val := ASnone
          } ]
  | ASwaitDescSurn -> do { desc_2nd.val := s; arg_state.val := ASnone } ]
;

value main () =
  do {
    Argl.parse speclist anonfun errmsg;
    Secure.set_base_dir (Filename.dirname ifile.val);
    let anc =
      if anc_1st.val <> "" then
        if anc_2nd.val = "" then do {
          printf "Misused option -a\n";
          printf "Use option -help for usage\n";
          flush stdout;
          exit 2
        }
        else Some (anc_1st.val, anc_occ.val, anc_2nd.val)
      else None
    in
    let desc =
      if desc_1st.val <> "" then
        if desc_2nd.val = "" then do {
          printf "Misused option -d\n";
          printf "Use option -help for usage\n";
          flush stdout;
          exit 2
        }
        else Some (desc_1st.val, desc_occ.val, desc_2nd.val)
      else None
    in
    if ofile.val = "-" then ofile.val := "" else ();
    if ifile.val = "" then do {
      printf "Missing base name\n";
      printf "Use option -help for usage\n";
      flush stdout;
      exit 2
    }
    else ();
    match
      try Some (Gwdb.open_base ifile.val) with [ Sys_error _ -> None ]
    with
    [ Some base -> gwb2ged base ifile.val ofile.val anc desc mem.val
    | None ->
        do {
          printf "Can't open base %s\n" ifile.val; flush stdout; exit 2
        } ];
  }
;

Printexc.catch main ();
