(* $Id: gwdiff.ml,v 5.0 2005-12-13 11:51:26 ddr Exp $ *)
(* Copyright (c) 2001 Ludovic LEDIEU *)

open Def;
open Gutil;

(*= TODO =====================================================================
  - Improve the way not to check several time the same persons.
=========================================================================== *)

value in_file1 = ref "";
value in_file2 = ref "";
value html = ref False;
value root = ref "";
value cr = ref "";

type messages =
  [ MsgBadChild of iper
  | MsgBirthDate
  | MsgBirthPlace
  | MsgChildMissing of iper
  | MsgChildren of iper
  | MsgDeathDate
  | MsgDeathPlace
  | MsgDivorce
  | MsgFirstName
  | MsgOccupation
  | MsgParentsMissing
  | MsgMarriageDate
  | MsgMarriagePlace
  | MsgSex
  | MsgSpouseMissing of iper
  | MsgSpouses of iper
  | MsgSurname ]
;

value person_string base iper =
  let p = poi base iper in
  let fn = sou base p.first_name in
  let sn = sou base p.surname in
  if sn = "?" || fn = "?" then
    fn ^ " " ^ sn ^ " (#" ^ string_of_int (Adef.int_of_iper iper) ^ ")"
  else fn ^ "." ^ (string_of_int p.occ) ^ " " ^ sn
;

value person_link bname base iper target =
  if html.val then
    Printf.sprintf "<A HREF=\"%s%s_w?i=%d\" TARGET=\"%s\">%s</A>"
      root.val bname (Adef.int_of_iper iper) target (person_string base iper)
  else
    person_string base iper
;

value print_message base1 base2 msg =
  do {
    Printf.printf " ";
    match msg with
    [ MsgBadChild iper1 ->
        Printf.printf "can not isolate one child match: %s"
          (person_link in_file1.val base1 iper1 "base1")
    | MsgBirthDate ->
        Printf.printf "birth date"
    | MsgBirthPlace ->
        Printf.printf "birth place"
    | MsgChildMissing iper1 ->
        Printf.printf "child missing: %s"
          (person_link in_file1.val base1 iper1 "base1")
    | MsgChildren iper1 ->
        Printf.printf "more than one child match: %s"
          (person_link in_file1.val base1 iper1 "base1")
    | MsgDeathDate ->
        Printf.printf "death (status or date)"
    | MsgDeathPlace ->
        Printf.printf "death place"
    | MsgDivorce ->
        Printf.printf "divorce"
    | MsgFirstName ->
        Printf.printf "first name"
    | MsgOccupation ->
        Printf.printf "occupation"
    | MsgParentsMissing ->
        Printf.printf "parents missing"
    | MsgMarriageDate ->
        Printf.printf "marriage date"
    | MsgMarriagePlace ->
        Printf.printf "marriage place"
    | MsgSex ->
        Printf.printf "sex"
    | MsgSpouseMissing iper1 ->
        Printf.printf "spouse missing: %s"
          (person_link in_file1.val base1 iper1 "base1")
    | MsgSpouses iper1 ->
        Printf.printf "more than one spouse match: %s"
          (person_link in_file1.val base1 iper1 "base1")
    | MsgSurname ->
        Printf.printf "surname" ];
    Printf.printf "%s" cr.val
  }
;

value print_f_messages base1 base2 ifam1 ifam2 res =
  let c1 = coi base1 ifam1 in
  let c2 = coi base2 ifam2 in
  do {
    Printf.printf "%s x %s%s/ %s x %s%s"
      (person_link in_file1.val base1 c1.father "base1")
      (person_link in_file1.val base1 c1.mother "base1")
      cr.val
      (person_link in_file2.val base2 c2.father "base2")
      (person_link in_file2.val base2 c2.mother "base2")
      cr.val;
    List.iter (print_message base1 base2) res
  }
;

value print_p_messages base1 base2 iper1 iper2 res =
  do { 
    Printf.printf "%s / %s%s"
      (person_link in_file1.val base1 iper1 "base1")
      (person_link in_file2.val base2 iper2 "base2")
      cr.val;
    List.iter (print_message base1 base2) res
  }
;

value compatible_names src_name dest_name_list =
  let src_name = Name.lower src_name in
  let dest_name_list = List.map Name.lower dest_name_list in
  List.mem src_name dest_name_list
;

value compatible_str_field istr1 istr2 =
  (Adef.int_of_istr istr1 = 0) || (Adef.int_of_istr istr2 != 0)
;

value dmy_to_sdn_range_l dmy =
  let sdn_of_dmy dmy =
    let sdn = Calendar.sdn_of_gregorian dmy in
    let sdn =
      if dmy.month = 0 || dmy.day = 0 then sdn + 1
      else sdn
    in
    let sdn2 =
      if dmy.delta != 0 then sdn + dmy.delta
      else
        let dmy2 =
          { year = if dmy.month = 0 ||
                      (dmy.month = 12 && dmy.day = 0) then (dmy.year +1)
                   else dmy.year;
            month = if dmy.month = 0 then 1
                    else if dmy.day = 0 then
                      if dmy.month = 12 then 1
                      else dmy.month + 1
                    else dmy.month;
            day =  if dmy.day = 0 then 1 else dmy.day;
            prec = if dmy.month = 0 || dmy.day = 0 then Before else Sure;
            delta = dmy.delta }
        in
    let sdn2 = Calendar.sdn_of_gregorian dmy2 in
    if dmy2.prec = Before then sdn2 - 1 else sdn2
    in    
  (sdn, sdn2)
  in
  match dmy.prec with
  [ Sure ->
      let (sdn1, sdn2) = sdn_of_dmy dmy in
      [ (Some sdn1, Some sdn2) ]
  | Maybe ->
      let (sdn1, sdn2) = sdn_of_dmy dmy in
      [ (Some sdn1, Some sdn2) ; (None, None) ]
  | About ->
      let (sdn1, sdn2) = sdn_of_dmy dmy in
      let delta = (sdn2 - sdn1 + 1) * 5 in
      [ (Some (sdn1 - delta), Some (sdn2 + delta)) ]
  | Before ->
      let (sdn1, sdn2) = sdn_of_dmy dmy in
      [ (None, Some sdn2) ]
  | After ->
      let (sdn1, sdn2) = sdn_of_dmy dmy in
      [ (Some sdn1, None) ]
  | OrYear y ->
      let dmy2 =
        { year = y + 1;
          month = 0;
          day = 0;
          prec = Sure;
          delta = 0 }
      in
      let (sdn11, sdn12) = sdn_of_dmy dmy in
      let (sdn21, sdn22) = sdn_of_dmy dmy2 in
      [ (Some sdn11, Some sdn12) ; (Some sdn21, Some sdn22) ]
  | YearInt y ->
      let dmy2 =
        { year = y + 1;
          month = 0;
          day = 0;
          prec = Sure;
          delta = 0 }
      in
      let (sdn11, sdn12) = sdn_of_dmy dmy in
      let (sdn21, sdn22) = sdn_of_dmy dmy2 in
      [ (Some sdn11, Some sdn22) ] ]
;

value compatible_sdn (sdn11, sdn12) (sdn21, sdn22) =
  if (sdn21, sdn22) = (None, None) then True
  else
    let bool1 =
      match (sdn11, sdn21) with
      [ (Some sdn1, Some sdn2) -> sdn1 <= sdn2
      | (None, _) -> True
      | (Some _, None) -> False ]
    in
    let bool2 =
      match (sdn12, sdn22) with
      [ (Some sdn1, Some sdn2) -> sdn1 >= sdn2
      | (None, _) -> True
      | (Some _, None) -> False ]
    in
    bool1 && bool2
;

value compatible_sdn_l sdn1_l sdn2 =
  List.fold_left (fun r sdn1 -> r || (compatible_sdn sdn1 sdn2))
    False sdn1_l
;

value compatible_sdn_ll sdn1_l sdn2_l =
  List.fold_left (fun r sdn2 -> r && (compatible_sdn_l sdn1_l sdn2))
    True sdn2_l
;

value compatible_dmys dmy1 dmy2 =
  compatible_sdn_ll (dmy_to_sdn_range_l dmy1) (dmy_to_sdn_range_l dmy2)
;

value compatible_dates date1 date2 =
  let compatible_cals cal1 cal2 =
    match (cal1, cal2) with
    [ (Dgregorian, Djulian)
    | (Dgregorian, Dfrench) -> True
    | _ -> cal1 = cal2 ]
  in
  if date1 = date2 then True
  else
    match (date1, date2) with
    [ (Dgreg dmy1 cal1, Dgreg dmy2 cal2) ->
        compatible_dmys dmy1 dmy2
        && compatible_cals cal1 cal2
    | (Dgreg _ _, Dtext _) -> False
    | (Dtext _, _) -> True ]
;

value compatible_codates codate1 codate2 =
  let od1 = Adef.od_of_codate codate1 in
  let od2 = Adef.od_of_codate codate2 in
  match (od1, od2) with
  [ (Some date1, Some date2) -> compatible_dates date1 date2
  | (Some _, None) -> False 
  | (None, _) -> True ]
;

value compatible_birth base1 base2 p1 p2 =
  let get_birth person =
    if person.birth = Adef.codate_None then person.baptism
    else person.birth
  in
  let birth1 = get_birth p1 in
  let birth2 = get_birth p2 in
  let res1 =
    if compatible_codates birth1 birth2 then []
    else [ MsgBirthDate ]
  in
  let res2 =
    if compatible_str_field p1.birth_place p2.birth_place then []
    else [ MsgBirthPlace ]
  in
  res1 @ res2
;

value compatible_death base1 base2 p1 p2 =
  let bool1 =
    p1.death = p2.death ||
    match (p1.death, p2.death) with
    [ (Death _ cdate1, Death _ cdate2) ->
        let date1 = Adef.date_of_cdate cdate1 in
        let date2 = Adef.date_of_cdate cdate2 in
        compatible_dates date1 date2
    | (NotDead, _)
    | (DeadYoung, Death _ _)
    | (DeadDontKnowWhen, Death _ _ | DeadYoung | DeadDontKnowWhen)
    | (DontKnowIfDead, _) -> True
    | _ -> False ]
  in
  let res1 =
    if bool1 then []
    else [ MsgDeathDate ]
  in
  let res2 =
    if compatible_str_field p1.death_place p2.death_place then []
    else [ MsgDeathPlace ]
  in
  res1 @ res2
;

value compatible_sexes base1 base2 p1 p2 =
  if p1.sex = p2.sex then []
  else [ MsgSex ]
;

value compatible_occupations base1 base2 p1 p2 =
  if compatible_str_field p1.occupation p2.occupation then []
  else [ MsgOccupation ]
;

value compatible_persons_ligth base1 base2 p1 p2 =
  let fn1 = sou base1 p1.first_name in
  let fn2 = sou base2 p2.first_name in
  let afn2 = [fn2 :: List.map (sou base2) p2.first_names_aliases] in
  let sn1 = sou base1 p1.surname in
  let sn2 = sou base2 p2.surname in
  let asn2 = [sn2 :: List.map (sou base2) p2.surnames_aliases] in
  let res1 =
    if compatible_names fn1 afn2 then []
    else [ MsgFirstName ]
  in
  let res2 =
    if compatible_names sn1 asn2 then []
    else [ MsgSurname ]
  in
  res1 @ res2
;

value compatible_persons base1 base2 p1 p2 =
  compatible_persons_ligth base1 base2 p1 p2
  @ compatible_sexes base1 base2 p1 p2 
  @ compatible_birth base1 base2 p1 p2
  @ compatible_death base1 base2 p1 p2
  @ compatible_occupations base1 base2 p1 p2
;

value rec find_compatible_persons_ligth base1 base2 iper1 iper2_list =
  match iper2_list with
  [ [] -> []
  | [ head :: rest ] ->
       let p1 = poi base1 iper1 in
       let p2 = poi base2 head in
       let c_rest = find_compatible_persons_ligth base1 base2 iper1 rest in
       if compatible_persons_ligth base1 base2 p1 p2 = [] then [ head :: c_rest ]
       else c_rest ]
;

value rec find_compatible_persons base1 base2 iper1 iper2_list =
  match iper2_list with
  [ [] -> []
  | [ head :: rest ] ->
       let p1 = poi base1 iper1 in
       let p2 = poi base2 head in
       let c_rest = find_compatible_persons base1 base2 iper1 rest in
       if compatible_persons base1 base2 p1 p2 = [] then [ head :: c_rest ]
       else c_rest ]
;

value compatible_unions base1 base2 iper1 iper2 ifam1 ifam2 =
  let get_spouse base iper ifam =
    let c = coi base ifam in
      if iper = c.father then poi base c.mother
      else poi base c.father
  in
  let spouse1 = get_spouse base1 iper1 ifam1 in
  let spouse2 = get_spouse base2 iper2 ifam2 in
  compatible_persons_ligth base1 base2 spouse1 spouse2
;

value rec find_compatible_unions base1 base2 iper1 iper2 ifam1 ifam2_list =
  match ifam2_list with
  [ [] -> []
  | [ head :: rest ] ->
       let c_rest = find_compatible_unions base1 base2 iper1 iper2 ifam1 rest in
       if compatible_unions base1 base2 iper1 iper2 ifam1 head = [] then
         [ head :: c_rest ]
       else c_rest ]
;

value compatible_divorces d1 d2 =
  match (d1, d2) with
  [ (Divorced codate1, Divorced codate2) ->
      compatible_codates codate1 codate2
  | (Divorced _, _) -> False
  | _ -> True ]
;

value compatible_marriages base1 base2 ifam1 ifam2 =
  let f1 = foi base1 ifam1 in
  let f2 = foi base2 ifam2 in
  let res1 =
    if compatible_codates f1.marriage f2.marriage then []
    else [ MsgMarriageDate ]
  in
  let res2 =
    if compatible_divorces f1.divorce f2.divorce then []
    else [ MsgDivorce ]
  in
  let res3 =
    if compatible_str_field f1.marriage_place f2.marriage_place then []
    else [ MsgMarriagePlace ]
  in
  let res = res1 @ res2 @ res3 in
  if res = [] then ()
  else print_f_messages base1 base2 ifam1 ifam2 res
;

value pdiff base1 base2 iper1 iper2 =
  let p1 = poi base1 iper1 in
  let p2 = poi base2 iper2 in
  let res = compatible_persons base1 base2 p1 p2 in
  if res = [] then ()
  else print_p_messages base1 base2 iper1 iper2 res
;

value compatible_parents base1 base2 iper1 iper2 =
  let a1 = (aoi base1 iper1).parents in
  let a2 = (aoi base2 iper2).parents in
  match (a1, a2) with
  [ (Some ifam1, Some ifam2) ->
       let c1 = coi base1 ifam1 in
       let c2 = coi base2 ifam2 in
       let _ = pdiff base1 base2 c1.father c2.father in
       let _ = pdiff base1 base2 c1.mother c2.mother in
       compatible_marriages base1 base2 ifam1 ifam2
  | (None, _) -> ()
  | (Some _, None) ->
       print_p_messages base1 base2 iper1 iper2 [ MsgParentsMissing ] ]
;

value rec ddiff base1 base2 iper1 iper2 d_tab =
  let d_check = d_tab.(Adef.int_of_iper iper1) in
  if List.mem iper2 d_check then ()
  else
    let _ = d_tab.(Adef.int_of_iper iper1) := [iper2 :: d_check ] in
  let spouse c iper =
    if iper = c.father then c.mother
    else c.father
  in
  let rec udiff base1 base2 iper1 iper2 r ifam1 ifam2 =
    let fd b1 b2 ip2_list ip1 =
      match find_compatible_persons_ligth b1 b2 ip1 ip2_list with
      [ [ip2] -> ddiff base1 base2 ip1 ip2 d_tab
      | [] -> 
          print_p_messages base1 base2 iper1 iper2 [ MsgChildMissing ip1 ] 
      | rest_list ->
          match find_compatible_persons b1 b2 ip1 rest_list with
          [ [best_ip2] -> ddiff base1 base2 ip1 best_ip2 d_tab
          | [] -> 
              print_p_messages base1 base2 iper1 iper2 [ MsgBadChild ip1 ]
          | _ ->
              print_p_messages base1 base2 iper1 iper2 [ MsgChildren ip1 ] ] ]
    in
    let c1 = coi base1 ifam1 in
    let c2 = coi base2 ifam2 in
    let p1 = spouse c1 iper1 in
    let p2 = spouse c2 iper2 in
    let d1 = Array.to_list (doi base1 ifam1).children in
    let d2 = Array.to_list (doi base2 ifam2).children in
    do {
      pdiff base1 base2 p1 p2;
      List.iter (fd base1 base2 d2) d1
    }
  in
  let fu b1 b2 ifam2_list ifam1 =
    match find_compatible_unions b1 b2 iper1 iper2 ifam1 ifam2_list with
    [ [ifam2] ->
        do {
          compatible_marriages b1 b2 ifam1 ifam2;
          compatible_parents b1 b2 (spouse (coi base1 ifam1) iper1)
            (spouse (coi base2 ifam2) iper2);
          udiff b1 b2 iper1 iper2 True ifam1 ifam2
        }
    | [] -> 
        print_p_messages base1 base2 iper1 iper2
          [ MsgSpouseMissing (spouse (coi base1 ifam1) iper1) ] 
    | _ -> 
        print_p_messages base1 base2 iper1 iper2
          [ MsgSpouses (spouse (coi base1 ifam1) iper1) ]  ]
  in
  let u1 = Array.to_list (uoi base1 iper1).family in
  let u2 = Array.to_list (uoi base2 iper2).family in
  do {
    pdiff base1 base2 iper1 iper2;
    List.iter (fu base1 base2 u2) u1
  }
;

value rec find_top base1 base2 iper1 iper2 =
  let p1 = poi base1 iper1 in
  let p2 = poi base2 iper2 in
  if compatible_persons_ligth base1 base2 p1 p2 = [] then
    let a1 = (aoi base1 iper1).parents in
    let a2 = (aoi base2 iper2).parents in
    match (a1, a2) with
    [ (Some ifam1, Some ifam2) ->
         let c1 = coi base1 ifam1 in
         let c2 = coi base2 ifam2 in
         let f_top_list = find_top base1 base2 c1.father c2.father in
         let m_top_list = find_top base1 base2 c1.mother c2.mother in
         f_top_list @ m_top_list
    | _ -> [(iper1, iper2)] ]
  else do {
    Printf.printf " Warning: %s doesn't match %s%s"
      (person_link in_file1.val base1 iper1 "base1")
      (person_link in_file2.val base2 iper2 "base2")
      cr.val;
    []
  }
;

value addiff base1 base2 iper1 iper2 d_tab =
  let topdiff (iper1, iper2) =
    do {
      Printf.printf "==> %s / %s%s"
        (person_link in_file1.val base1 iper1 "base1")
        (person_link in_file2.val base2 iper2 "base2")
        cr.val;
      ddiff base1 base2 iper1 iper2 d_tab
    }
  in
  do {
    Printf.printf "Building top list...%s" cr.val;
    let top_list = find_top base1 base2 iper1 iper2 in
    Printf.printf "Top list built.%s" cr.val;
    List.iter topdiff top_list
  }
;

(* Main *)

value gwdiff base1 base2 iper1 iper2 d_mode ad_mode =
  let desc_tab = Array.create base1.data.persons.len [] in
  match (d_mode, ad_mode) with
  [ (True, _)
  | (False, False) -> ddiff base1 base2 iper1 iper2 desc_tab
  | (False, True) -> addiff base1 base2 iper1 iper2 desc_tab ]
;

value p1_fn = ref "";
value p1_occ = ref 0;
value p1_sn = ref "";
value p2_fn = ref "";
value p2_occ = ref 0;
value p2_sn = ref "";

type arg_state =
  [ ASnone
  | ASwaitP1occ
  | ASwaitP1sn
  | ASwaitP2occ
  | ASwaitP2sn ]
;
value arg_state = ref ASnone;
value mem = ref False;
value d_mode = ref False;
value ad_mode = ref False;

value speclist =
  [("-1", Arg.String (fun s -> do { p1_fn.val := s; arg_state.val := ASwaitP1occ }),
    "<fn> <occ> <sn> : (mandatory) defines starting person in base1");
   ("-2", Arg.String (fun s -> do { p2_fn.val := s; arg_state.val := ASwaitP2occ }),
    "<fn> <occ> <sn> : (mandatory) defines starting person in base2");
   ("-ad", Arg.Set ad_mode,
    ": checks descendants of all ascendants ");
   ("-d", Arg.Set d_mode,
    ": checks descendants (default)");
   ("-html", Arg.String (fun s -> do { html.val := True; root.val := s }),
    "<root>: HTML format used for report");
   ("-mem", Arg.Set mem, ": save memory space, but slower") ]
;

value anonfun s =
  match arg_state.val with
  [ ASnone ->
      if in_file1.val = "" then in_file1.val := s
      else if in_file2.val = "" then in_file2.val := s
      else raise (Arg.Bad "Too much arguments")
  | ASwaitP1occ ->
      try
        do {
          p1_occ.val := int_of_string s;
          arg_state.val := ASwaitP1sn
        }
      with
      [ Failure _ -> raise (Arg.Bad "Numeric value for occ (-1)!") ]
  | ASwaitP1sn -> do { p1_sn.val := s; arg_state.val := ASnone }
  | ASwaitP2occ ->
      try
        do {
          p2_occ.val := int_of_string s;
          arg_state.val := ASwaitP2sn
        }
      with
      [ Failure _ -> raise (Arg.Bad "Numeric value for occ (-2)!") ]
  | ASwaitP2sn -> do { p2_sn.val := s; arg_state.val := ASnone }]
;

value errmsg =
  "Usage: " ^ Sys.argv.(0) ^ " \
[options] base1 base2
Options are: "
;

value check_args () =
  do {
    Argl.parse speclist anonfun errmsg;
    if in_file1.val = "" then
      do {
        Printf.printf "Missing reference data base\n";
        Printf.printf "Use option -help for usage\n";
        flush stdout;
        exit 2
      }
    else ();
    if in_file2.val = "" then
      do {
        Printf.printf "Missing destination data base\n";
        Printf.printf "Use option -help for usage\n";
        flush stdout;
        exit 2
      }
    else ();
    if p1_fn.val = "" then
      do {
        Printf.printf "-1 parameter is mandatory\n";
        Printf.printf "Use option -help for usage\n";
        flush stdout;
        exit 2
      }
    else ();
    if p1_sn.val = "" then
      do {
        Printf.printf "Incomplete -1 parameter\n";
        Printf.printf "Use option -help for usage\n";
        flush stdout;
        exit 2
      }
    else ();
    if p2_fn.val = "" then
      do {
        Printf.printf "-2 parameter is mandatory\n";
        Printf.printf "Use option -help for usage\n";
        flush stdout;
        exit 2
      }
    else ();
    if p2_sn.val = "" then
      do {
        Printf.printf "Incomplete -2 parameter\n";
        Printf.printf "Use option -help for usage\n";
        flush stdout;
        exit 2
      }
    else ()
  }
;

value main () =
  let _ = check_args () in
  let _ =
    if not html.val then cr.val := "\n"
    else cr.val := "<BR>\n"
  in
  (* Reference base *)
  let base1 = Iobase.input in_file1.val in
  let _ = base1.data.ascends.array () in
  let _ = base1.data.strings.array () in
  let _ =
    if not mem.val then
      let _ = base1.data.persons.array () in
      let _ = base1.data.families.array () in
      let _ = base1.data.couples.array () in
      let _ = base1.data.unions.array () in
      let _ = base1.data.descends.array () in
      ()
    else ()
  in
  (* Destination base *)
  let base2 =
     if in_file1.val != in_file2.val then
       let base2 = Iobase.input in_file2.val in
       let _ = base2.data.ascends.array () in
       let _ = base2.data.strings.array () in
       let _ =
         if not mem.val then
           let _ = base2.data.persons.array () in
           let _ = base2.data.families.array () in
           let _ = base2.data.couples.array () in
           let _ = base2.data.unions.array () in
           let _ = base2.data.descends.array () in
           ()
         else ()
       in
       base2
     else
       (* Reference = Destination *)
       base1
  in
  let iper1 = person_ht_find_unique base1 p1_fn.val p1_sn.val p1_occ.val in
  let iper2 = person_ht_find_unique base2 p2_fn.val p2_sn.val p2_occ.val in
  do {
    if html.val then
      Printf.printf "<BODY>\n"
    else ();
    gwdiff base1 base2 iper1 iper2 d_mode.val ad_mode.val;
    if html.val then
      Printf.printf "</BODY>\n"
    else ()
  }
;

Printexc.catch main ();
