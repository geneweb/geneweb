(* $Id: gwu.ml,v 2.18 1999-09-16 15:01:13 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Gutil;

type mfam =
  { m_fam : family;
    m_fath : person;
    m_moth : person;
    m_chil : array person }
;

value soy y = if y == 0 then "-0" else string_of_int y;

value print_date_dmy oc d =
  do match d.prec with
     [ About -> Printf.fprintf oc "~"
     | Maybe -> Printf.fprintf oc "?"
     | Before -> Printf.fprintf oc "<"
     | After -> Printf.fprintf oc ">"
     | _ -> () ];
     if d.day == 0 && d.month == 0 then
       Printf.fprintf oc "%s" (soy d.year)
     else if d.day == 0 then
       Printf.fprintf oc "%d/%s" d.month (soy d.year)
     else
       Printf.fprintf oc "%d/%d/%s" d.day d.month (soy d.year);
     match d.prec with
     [ OrYear y -> Printf.fprintf oc "|%s" (soy y)
     | YearInt y -> Printf.fprintf oc "..%s" (soy y)
     | _ -> () ];
  return ()
;

value spaces_to_underscore s =
  do for i = 0 to String.length s - 1 do
       if s.[i] = ' ' then s.[i] := '_' else ();
     done;
  return s
;

value print_date oc =
  fun
  [ Dgreg d Dgregorian -> print_date_dmy oc d
  | Dgreg d Djulian ->
      do print_date_dmy oc (Calendar.julian_of_gregorian d);
         Printf.fprintf oc "J";
      return ()
  | Dgreg d Dfrench ->
      do print_date_dmy oc (Calendar.french_of_gregorian d);
         Printf.fprintf oc "F";
      return ()
  | Dgreg d Dhebrew ->
      do print_date_dmy oc (Calendar.hebrew_of_gregorian d);
         Printf.fprintf oc "H";
      return ()
  | Dtext t -> Printf.printf "0(%s)" (spaces_to_underscore t) ]
;

value print_date_option oc =
  fun
  [ Some d -> print_date oc d
  | None -> () ]
;

value starting_char s =
  match s.[0] with
  [ 'a'..'z' | 'A'..'Z' | 'à'..'ý' | 'À'..'Ý' | ' ' -> True
  | '?' -> if s = "?" then True else False
  | _ -> False ]
;

value s_correct_string s =
  loop 0 0 where rec loop i len =
    if i == String.length s then Buff.get len
    else
      if i == 0 && not (starting_char s) then
        loop (i + 1) (Buff.store (Buff.store len '_') s.[0])
      else if s.[i] == ' ' then loop (i + 1) (Buff.store len '_')
      else if s.[i] == '_' || s.[i] == '\\' then
        loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      else loop (i + 1) (Buff.store len s.[i])
;

value correct_string base is = s_correct_string (sou base is);

value has_infos_not_dates base p =
  p.first_names_aliases <> [] || p.surnames_aliases <> [] ||
  sou base p.public_name <> "" || p.nick_names <> [] || p.aliases <> [] ||
  p.titles <> [] || sou base p.occupation <> "" ||
  sou base p.birth_place <> "" || sou base p.baptism_place <> "" ||
  sou base p.death_place <> "" || sou base p.psources <> ""
;

value has_infos base p =
  has_infos_not_dates base p || p.birth <> Adef.codate_None ||
  p.baptism <> Adef.codate_None ||  p.death <> NotDead
;

value print_if_no_empty oc base lab is =
  if sou base is = "" then ()
  else Printf.fprintf oc " %s %s" lab (correct_string base is)
;

value print_first_name_alias oc base is =
  Printf.fprintf oc " {%s}" (correct_string base is)
;

value print_surname_alias oc base is =
  Printf.fprintf oc " #salias %s" (correct_string base is)
;

value print_nick_name oc base is =
  Printf.fprintf oc " #nick %s" (correct_string base is)
;

value print_alias oc base is =
  Printf.fprintf oc " #alias %s" (correct_string base is)
;

value print_burial oc base b =
  match b with
  [ Buried cod ->
      do Printf.fprintf oc " #buri";
         match Adef.od_of_codate cod with
         [ Some d ->
             do Printf.fprintf oc " ";
                print_date oc d;
             return ()
         | _ -> () ];
      return ()
  | Cremated cod ->
      do Printf.fprintf oc " #crem";
         match Adef.od_of_codate cod with
         [ Some d ->
             do Printf.fprintf oc " ";
                print_date oc d;
             return ()
         | _ -> () ];
      return ()
  | UnknownBurial -> () ]
;

value print_title oc base t =
  let t_date_start = Adef.od_of_codate t.t_date_start in
  let t_date_end = Adef.od_of_codate t.t_date_end in
  do Printf.fprintf oc " [";
     match t.t_name with
     [ Tmain -> Printf.fprintf oc "*"
     | Tname s -> Printf.fprintf oc "%s" (correct_string base s)
     | Tnone -> () ];
     Printf.fprintf oc ":";
     Printf.fprintf oc "%s" (correct_string base t.t_ident);
     Printf.fprintf oc ":";
     Printf.fprintf oc "%s" (correct_string base t.t_place);
     if t.t_nth <> 0 then Printf.fprintf oc ":"
     else
       match (t_date_start, t_date_end) with
       [ (Some _, _) | (_, Some _) -> Printf.fprintf oc ":"
       | _ -> () ];
     print_date_option oc t_date_start;
     if t.t_nth <> 0 then Printf.fprintf oc ":"
     else
       match t_date_end with
       [ Some _ -> Printf.fprintf oc ":"
       | _ -> () ];
     print_date_option oc t_date_end;
     if t.t_nth <> 0 then Printf.fprintf oc ":%d" t.t_nth else ();
     Printf.fprintf oc "]";
  return ()
;

(* main_title = backward compatibility (installed version 2.01);
   should be removed one day... *)
value main_title =
  let val =
    fun
    [ "empereur" | "impératrice" -> 6
    | "roi" | "reine" -> 5
    | "prince" | "princesse" -> 4
    | "duc" | "duchesse" -> 3
    | "comte" | "comtesse" -> 2
    | "vicomte" | "vicomtesse" -> 1
    | _ -> 0 ]
  in
  fun base p ->
    let rec loop r =
      fun
      [ [] -> r
      | [x :: l] ->
          if x.t_name == Tmain then Some x
          else
            match r with
            [ Some t ->
                if val (sou base x.t_ident) > val (sou base t.t_ident) then
                  loop (Some x) l
                else loop r l
            | None -> loop (Some x) l ] ]
    in
    loop None p.titles
;

value print_infos oc base is_child print_sources p =
  do List.iter (print_first_name_alias oc base) p.first_names_aliases;
     List.iter (print_surname_alias oc base) p.surnames_aliases;
     match p.public_name with
     [ s when sou base s <> "" ->
         Printf.fprintf oc " (%s)" (correct_string base s)
     | _ -> () ];
     print_if_no_empty oc base "#image" p.image;
     List.iter (print_nick_name oc base) p.nick_names;
     List.iter (print_alias oc base) p.aliases;
(* backward compatibility (installed version 2.01) *)
     match p.titles with
     [ [_] | [] -> ()
     | [t0 :: _] ->
         match main_title base p with
         [ Some t -> if t = t0 then () else t.t_name := Tmain
         | _ -> () ] ];
(**)
     List.iter (print_title oc base) p.titles;
     match p.access with
     [ IfTitles -> ()
     | Public -> Printf.fprintf oc " #apubl"
     | Private -> Printf.fprintf oc " #apriv" ];
     print_if_no_empty oc base "#occu" p.occupation;
     if print_sources then print_if_no_empty oc base "#src" p.psources
     else ();
     match Adef.od_of_codate p.birth with
     [ Some d ->
         do Printf.fprintf oc " ";
            print_date oc d;
         return ()
     | _ ->
         if p.baptism <> Adef.codate_None then ()
         else
           match p.death with
           [ Death _ _ | DeadYoung | DeadDontKnowWhen -> Printf.fprintf oc " 0"
           | DontKnowIfDead
             when not is_child && not (has_infos_not_dates base p) &&
             p_first_name base p <> "?" && p_surname base p <> "?" ->
               Printf.fprintf oc " 0"
           | _ -> () ] ];
     print_if_no_empty oc base "#bp" p.birth_place;
     print_if_no_empty oc base "#bs" p.birth_src;
     match Adef.od_of_codate p.baptism with
     [ Some d ->
         do Printf.fprintf oc " !";
            print_date oc d;
         return ()
     | _ -> () ];
     print_if_no_empty oc base "#pp" p.baptism_place;
     print_if_no_empty oc base "#ps" p.baptism_src;
     match p.death with
     [ Death dr d ->
         do Printf.fprintf oc " ";
            match dr with
            [ Killed -> Printf.fprintf oc "k"
            | Murdered -> Printf.fprintf oc "m"
            | Executed -> Printf.fprintf oc "e"
            | Disappeared -> Printf.fprintf oc "s"
            | _ -> () ];
            print_date oc (Adef.date_of_cdate d);
         return ()
     | DeadYoung -> Printf.fprintf oc " mj"
     | DeadDontKnowWhen -> Printf.fprintf oc " 0"
     | DontKnowIfDead ->
         match (Adef.od_of_codate p.birth, Adef.od_of_codate p.baptism) with
         [ (Some _, _) | (_, Some _) -> Printf.fprintf oc " ?"
         | _ -> () ]
     | NotDead -> () ];
     print_if_no_empty oc base "#dp" p.death_place;
     print_if_no_empty oc base "#ds" p.death_src;
     print_burial oc base p.burial;
     print_if_no_empty oc base "#rp" p.burial_place;
     print_if_no_empty oc base "#rs" p.burial_src;
  return ()
;

value print_parent oc base mark fam_sel fam p =
  let a = aoi base p.cle_index in
  let has_printed_parents =
    match a.parents with
    [ Some ifam -> fam_sel ifam
    | None -> False ]
  in
  let first_parent_definition =
    if mark.(Adef.int_of_iper p.cle_index) then False
    else do mark.(Adef.int_of_iper p.cle_index) := True; return True
  in
  let pr = not has_printed_parents && first_parent_definition in
  let has_infos = if pr then has_infos base p else False in
  let first_name = sou base p.first_name in
  let surname = sou base p.surname in
  do Printf.fprintf oc "%s %s%s"
       (s_correct_string surname) (s_correct_string first_name)
       (if p.occ == 0 || first_name = "?" || surname = "?" then ""
        else "." ^ string_of_int p.occ);
     if pr then
       if has_infos then print_infos oc base False True p
       else if first_name <> "?" && surname <> "?" then
         Printf.fprintf oc " 0"
       else ()
     else ();
  return ()
;

value print_child oc base fam_surname print_sources p =
  do Printf.fprintf oc "-";
     match p.sex with
     [ Male -> Printf.fprintf oc " h"
     | Female -> Printf.fprintf oc " f"
     | _ -> () ];
     Printf.fprintf oc " %s" (s_correct_string (sou base p.first_name));
     if p.occ == 0 || p_first_name base p = "?" || p_surname base p = "?"
     then ()
     else Printf.fprintf oc ".%d" p.occ;
     if p.surname <> fam_surname then
       Printf.fprintf oc " %s" (s_correct_string (sou base p.surname))
     else ();
     print_infos oc base True print_sources p;
     Printf.fprintf oc "\n";
  return ()
;

value bogus_person base p =
  p_first_name base p = "?" && p_surname base p = "?"
;

value common_children_sources base children =
  if Array.length children <= 1 then None
  else
    loop 1 children.(0).psources where rec loop i src =
      if i == Array.length children then
        let s = sou base src  in
        if s = "" then None else Some src
      else
        let p = children.(i) in
        if p.psources == src then loop (i + 1) src else None
;

value array_forall f a =
  loop 0 where rec loop i =
    if i == Array.length a then True
    else if f a.(i) then loop (i + 1)
    else False
;

value empty_family base m =
  bogus_person base m.m_fath && bogus_person base m.m_moth &&
  array_forall (bogus_person base) m.m_chil
;

value print_family oc base ml (per_sel, fam_sel) fam_done m =
  let fam = m.m_fam in
  do Printf.fprintf oc "fam ";
     print_parent oc base ml fam_sel fam m.m_fath;
     Printf.fprintf oc " +";
     print_date_option oc (Adef.od_of_codate fam.marriage);
     if fam.not_married then Printf.fprintf oc " #nm" else ();
     print_if_no_empty oc base "#mp" fam.marriage_place;
     print_if_no_empty oc base "#ms" fam.marriage_src;
     match fam.divorce with
     [ NotDivorced -> ()
     | Divorced d ->
         let d = Adef.od_of_codate d in
         do Printf.fprintf oc " -"; print_date_option oc d; return () ];
     Printf.fprintf oc " ";
     print_parent oc base ml fam_sel fam m.m_moth;
     Printf.fprintf oc "\n";
     match sou base fam.fsources with
     [ "" -> ()
     | s -> Printf.fprintf oc "src %s\n" (correct_string base fam.fsources) ];
     let print_sources =
       match common_children_sources base m.m_chil with
       [ Some s ->
          do Printf.fprintf oc "csrc %s\n" (correct_string base s); return
          False
       | _ -> True ]
     in
     do match fam.comment with
        [ txt when sou base txt <> "" ->
            Printf.fprintf oc "comm %s\n" (sou base txt)
        | _ -> () ];
     return
     match Array.length m.m_chil with
     [ 0 -> ()
     | _ ->
         let fam_surname = m.m_fath.surname in
         do Printf.fprintf oc "beg\n";
            Array.iter
              (fun p ->
                 if per_sel p.cle_index then
                   print_child oc base fam_surname print_sources p
                 else ())
              m.m_chil;
            Printf.fprintf oc "end\n";
         return () ];
     fam_done.(Adef.int_of_ifam fam.fam_index) := True;
  return ()
;

value get_persons_with_notes base m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match (sou base fath.notes, (aoi base fath.cle_index).parents) with
    [ ("", _) | (_, Some _) -> list
    | _ -> [fath :: list] ]
  in
  let list =
    match (sou base moth.notes, (aoi base moth.cle_index).parents) with
    [ ("", _) | (_, Some _) -> list
    | _ -> [moth :: list] ]
  in
  List.fold_right
    (fun p list ->
       match sou base p.notes with
       [ "" -> list
       | _ -> [p :: list] ])
    (Array.to_list m.m_chil) list
;

value print_notes_for_person oc base p =
  let surn = s_correct_string (p_surname base p) in
  let fnam = s_correct_string (p_first_name base p) in
  if surn <> "?" || fnam <> "?" then
    do Printf.fprintf oc "\n";
       Printf.fprintf oc "notes %s %s%s\n" surn fnam
         (if p.occ == 0 then "" else "." ^ string_of_int p.occ);
       Printf.fprintf oc "beg\n";
       Printf.fprintf oc "%s\n" (sou base p.notes);
       Printf.fprintf oc "end notes\n";
    return ()
  else ()
;

value rec list_memf f x =
  fun
  [ [] -> False
  | [a :: l] -> f x a || list_memf f x l ]
;

value eq_key p1 p2 = p1.cle_index == p2.cle_index;

value print_notes oc base ml per_sel =
  let pl = List.fold_right (get_persons_with_notes base) ml [] in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else [p :: pl])
      pl []
  in
  List.iter
    (fun p ->
       if per_sel p.cle_index then print_notes_for_person oc base p else ())
    pl
;

value get_persons_with_relations base m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match (fath.rparents, (aoi base fath.cle_index).parents) with
    [ ([], _) | (_, Some _) -> list
    | _ -> [fath :: list] ]
  in
  let list =
    match (moth.rparents, (aoi base moth.cle_index).parents) with
    [ ([], _) | (_, Some _) -> list
    | _ -> [moth :: list] ]
  in
  List.fold_right
    (fun p list ->
       match p.rparents with
       [ [] -> list
       | _ -> [p :: list] ])
    (Array.to_list m.m_chil) list
;

value print_relation_parent oc base mark defined_p p =
  let a = aoi base p.cle_index in
  do Printf.fprintf oc "%s %s%s"
       (correct_string base p.surname)
       (correct_string base p.first_name)
       (if p.occ = 0 then "" else "." ^ string_of_int p.occ);
     if Array.length p.family = 0 && a.parents = None
     && not mark.(Adef.int_of_iper p.cle_index) then
       do mark.(Adef.int_of_iper p.cle_index) := True;
          if has_infos base p then print_infos oc base False True p
          else Printf.fprintf oc " 0";
          if p.rparents <> [] then defined_p.val := [p :: defined_p.val]
          else ();
       return ()
     else ();
  return ()
;

value print_relation_for_person oc base mark def_p p r =
  let fath =
    match r.r_fath with
    [ Some ip ->
        let p = poi base ip in
        if sou base p.first_name = "?" || sou base p.surname = "?" then None
        else Some p
    | None -> None ]
  in
  let moth =
    match r.r_moth with
    [ Some ip ->
        let p = poi base ip in
        if sou base p.first_name = "?" || sou base p.surname = "?" then None
        else Some p
    | None -> None ]
  in
  match (fath, moth) with
  [ (None, None) -> ()
  | _ ->
     do Printf.fprintf oc "- ";
        match r.r_type with
        [ Adoption -> Printf.fprintf oc "adop"
        | Recognition -> Printf.fprintf oc "reco"
        | CandidateParent -> Printf.fprintf oc "cand"
        | GodParent -> Printf.fprintf oc "godp" ];
        match (fath, moth) with
        [ (Some _, None) -> Printf.fprintf oc " fath"
        | (None, Some _) -> Printf.fprintf oc " moth"
        | _ -> () ];
        Printf.fprintf oc ": ";
        match (fath, moth) with
        [ (Some fath, None) -> print_relation_parent oc base mark def_p fath
        | (None, Some moth) -> print_relation_parent oc base mark def_p moth
        | (Some fath, Some moth) ->
            do print_relation_parent oc base mark def_p fath;
               Printf.fprintf oc " + ";
               print_relation_parent oc base mark def_p moth;
            return ()
        | _ -> () ];
        Printf.fprintf oc "\n";
     return () ]
;

value print_relations_for_person oc base mark def_p p =
  let surn = correct_string base p.surname in
  let fnam = correct_string base p.first_name in
  if surn <> "?" || fnam <> "?" then
    do Printf.fprintf oc "\n";
       Printf.fprintf oc "rel %s %s%s\n" surn fnam
         (if p.occ == 0 then "" else "." ^ string_of_int p.occ);
       Printf.fprintf oc "beg\n";
       List.iter (print_relation_for_person oc base mark def_p p) p.rparents;
       Printf.fprintf oc "end\n";
    return ()
  else ()
;

value print_relations oc base mark per_sel ml =
  let pl = List.fold_right (get_persons_with_relations base) ml [] in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else [p :: pl])
      pl []
  in
  loop pl where rec loop =
    fun
    [ [] -> ()
    | [p :: pl] ->
         let def_p = ref [] in
         do if per_sel p.cle_index then
              print_relations_for_person oc base mark def_p p
            else ();
         return loop (pl @ def_p.val) ]
;

value rec merge_families ifaml1f ifaml2f =
  match (ifaml1f, ifaml2f) with
  [ ([ifam1 :: ifaml1], [ifam2 :: ifaml2]) ->
      let m1 = List.memq ifam1 ifaml2 in
      let m2 = List.memq ifam2 ifaml1 in
      if m1 && m2 then merge_families ifaml1 ifaml2
      else if m1 then
        [ifam2 :: merge_families ifaml1f ifaml2]
      else if m2 then
        [ifam1 :: merge_families ifaml1 ifaml2f]
      else if ifam1 == ifam2 then [ifam1 :: merge_families ifaml1 ifaml2]
      else [ifam1; ifam2 :: merge_families ifaml1 ifaml2]
  | (ifaml1, []) -> ifaml1
  | ([], ifaml2) -> ifaml2 ]
;

value rec filter f =
  fun
  [ [x :: l] -> if f x then [x :: filter f l] else filter f l
  | [] -> [] ]
;

value connected_families base fam_sel fam cpl =
  loop [fam.fam_index] [] [cpl.father]
  where rec loop ifaml ipl_scanned =
    fun
    [ [ip :: ipl] ->
        if List.memq ip ipl_scanned then loop ifaml ipl_scanned ipl
        else
          let p = poi base ip in
          let ifaml1 = Array.to_list p.family in
          let ifaml1 = filter fam_sel ifaml1 in
          let ifaml = merge_families ifaml ifaml1 in
          let ipl =
            List.fold_right
              (fun ifam ipl ->
                 let cpl = coi base ifam in
                 [cpl.father; cpl.mother :: ipl])
              ifaml1 ipl
          in
          loop ifaml [ip :: ipl_scanned] ipl
    | [] -> ifaml ]
;

value find_person base p1 po p2 =
  try Gutil.person_ht_find_unique base p1 p2 po with
  [ Not_found ->
      do Printf.printf "Not found: %s%s %s\n"
           p1 (if po == 0 then "" else " " ^ string_of_int po) p2;
         flush stdout;
      return exit 2 ]
;

value gwu base out_dir out_oc src_oc_list anc desc =
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
  let ((per_sel, fam_sel) as sel) = Select.functions base anc desc in
  let fam_done = Array.create (base.data.families.len) False in
  let mark = Array.create base.data.persons.len False in
  let out_oc_first = ref True in
  let origin_file fname =
    if out_dir = "" then (out_oc, out_oc_first)
    else if fname = "" then (out_oc, out_oc_first)
    else
      try List.assoc fname src_oc_list.val with
      [ Not_found ->
          let oc = open_out (Filename.concat out_dir fname) in
          let x = (oc, ref True) in
          do src_oc_list.val := [(fname, x) :: src_oc_list.val];
          return x ]
  in
  do for i = 0 to base.data.families.len - 1 do
       let fam = base.data.families.get i in
       let cpl = base.data.couples.get i in
       if is_deleted_family fam then ()
       else
         do if fam_done.(i) then ()
            else if fam_sel fam.fam_index then
              let (oc, first) = origin_file (sou base fam.origin_file) in
              let ifaml = connected_families base fam_sel fam cpl in
              let ml =
                List.fold_right
                  (fun ifam ml ->
                     let fam = foi base ifam in
                     let cpl = coi base ifam in
                     let m =
                       {m_fam = fam;
                        m_fath = poi base cpl.father;
                        m_moth = poi base cpl.mother;
                        m_chil =
                          Array.map (fun ip -> poi base ip) fam.children}
                     in
                     if empty_family base m then ml else [m :: ml])
                  ifaml []
              in
              if ml <> [] then
                do if not first.val then Printf.fprintf oc "\n" else ();
                   first.val := False;
                   List.iter (print_family oc base mark sel fam_done) ml;
                   print_notes oc base ml per_sel;
                   print_relations oc base mark per_sel ml;
                return ()
              else ()
            else ();
         return ();
     done;
     let s = base.data.bnotes.nread 0 in
     if s = "" then ()
     else
       let (oc, first) = origin_file base.data.bnotes.norigin_file in
       do if not first.val then Printf.fprintf oc "\n" else ();
          Printf.fprintf oc "notes\n";
          Printf.fprintf oc "%s\n" s;
          Printf.fprintf oc "end notes\n";
       return ();
  return ()
;

value in_file = ref "";
value out_file = ref "";
value out_dir = ref "";
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
value mem = ref False;

value speclist =
  [("-o", Arg.String (fun s -> out_file.val := s),
   "<file>    output file name (else stdout)");
   ("-odir", Arg.String (fun s -> out_dir.val := s),
   "<dir>  create files from original name in directory (else on -o file)");
   ("-mem", Arg.Set mem,
   "        save memory space, but slower");
   ("-a",
    Arg.String
      (fun s -> do anc_1st.val := s; return arg_state.val := ASwaitAncOcc),
    "\"<1st_name>\" [num] \"<surname>\": select ancestors of...");
   ("-d",
    Arg.String
      (fun s -> do desc_1st.val := s; return arg_state.val := ASwaitDescOcc),
    "\"<1st_name>\" [num] \"<surname>\": select descendants of...")]
;

value anon_fun s =
  match arg_state.val with
  [ ASnone -> in_file.val := s
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

value errmsg = "Usage: " ^ Sys.argv.(0) ^ " [options] <base_file>
Options are:";

value main () =
  do Argl.parse speclist anon_fun errmsg;
     if in_file.val = "" then
       do Printf.printf "Missing base\n";
          Printf.printf "Use option -help for usage\n";
          flush stdout;
       return exit 2
     else ();
  return
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
  let base = Iobase.input in_file.val in
  let src_oc_list = ref [] in
  let _ = base.data.ascends.array () in
  let _ = base.data.strings.array () in
  do if not mem.val then
       let _ = base.data.persons.array () in
       let _ = base.data.families.array () in
       let _ = base.data.couples.array () in
       ()
     else ();
  return
  let oc_list = ref [] in
  let out_oc =
    if out_file.val = "" then stdout else open_out out_file.val
  in
  do gwu base out_dir.val out_oc src_oc_list anc desc;
     List.iter
       (fun (src, (oc, _)) -> do flush oc; close_out oc; return ())
       src_oc_list.val;
     flush out_oc;
     if out_file.val = "" then () else close_out out_oc;
  return ()
;

Printexc.catch main ();
