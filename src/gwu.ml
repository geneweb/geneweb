(* $Id: gwu.ml,v 3.35 2000-11-16 04:45:36 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Gutil;

type mfam =
  { m_fam : family; m_fath : person; m_moth : person; m_chil : array person }
;

value soy y = if y == 0 then "-0" else string_of_int y;

value print_date_dmy oc d =
  do match d.prec with
     [ About -> Printf.fprintf oc "~"
     | Maybe -> Printf.fprintf oc "?"
     | Before -> Printf.fprintf oc "<"
     | After -> Printf.fprintf oc ">"
     | _ -> () ];
     if d.day == 0 && d.month == 0 then Printf.fprintf oc "%s" (soy d.year)
     else if d.day == 0 then Printf.fprintf oc "%d/%s" d.month (soy d.year)
     else Printf.fprintf oc "%d/%d/%s" d.day d.month (soy d.year);
     match d.prec with
     [ OrYear y -> Printf.fprintf oc "|%s" (soy y)
     | YearInt y -> Printf.fprintf oc "..%s" (soy y)
     | _ -> () ];
  return ()
;

value is_printable =
  fun
  [ '\000'..'\031' -> False
  | _ -> True ]
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
  | Dtext t -> Printf.fprintf oc "0(%s)" (spaces_to_underscore t) ]
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

value gen_correct_string no_colon s =
  loop 0 0 where rec loop i len =
    if i == String.length s then Buff.get len
    else if len == 0 && not (starting_char s) then
      loop i (Buff.store len '_')
    else
      match s.[i] with
      [ ' ' | '\n' | '\t' ->
          if i == String.length s - 1 then Buff.get len
          else loop (i + 1) (Buff.store len '_')
      | '_' | '\\' -> loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | ':' when no_colon ->
          let len = Buff.store len '\\' in
          loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | c ->
          let c = if is_printable c then c else '_' in
          loop (i + 1) (Buff.store len c) ]
;

value s_correct_string = gen_correct_string False;
value correct_string base is = s_correct_string (sou base is);
value correct_string_no_colon base is = gen_correct_string True (sou base is);

value has_infos_not_dates base p =
  p.first_names_aliases <> [] || p.surnames_aliases <> [] ||
  sou base p.public_name <> "" || p.qualifiers <> [] || p.aliases <> [] ||
  p.titles <> [] || sou base p.occupation <> "" ||
  sou base p.birth_place <> "" || sou base p.baptism_place <> "" ||
  sou base p.death_place <> "" || sou base p.psources <> ""
;

value has_infos base p =
  has_infos_not_dates base p || p.birth <> Adef.codate_None ||
  p.baptism <> Adef.codate_None || p.death <> NotDead
;

value print_if_not_equal_to x oc base lab is =
  if sou base is = x then ()
  else Printf.fprintf oc " %s %s" lab (correct_string base is)
;

value print_if_no_empty = print_if_not_equal_to "";

value print_first_name_alias oc base is =
  Printf.fprintf oc " {%s}" (correct_string base is)
;

value print_surname_alias oc base is =
  Printf.fprintf oc " #salias %s" (correct_string base is)
;

value print_qualifier oc base is =
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
         [ Some d -> do Printf.fprintf oc " "; print_date oc d; return ()
         | _ -> () ];
      return ()
  | Cremated cod ->
      do Printf.fprintf oc " #crem";
         match Adef.od_of_codate cod with
         [ Some d -> do Printf.fprintf oc " "; print_date oc d; return ()
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
     Printf.fprintf oc "%s" (correct_string_no_colon base t.t_ident);
     Printf.fprintf oc ":";
     Printf.fprintf oc "%s" (correct_string_no_colon base t.t_place);
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

value print_infos oc base is_child csrc cbp p =
  do List.iter (print_first_name_alias oc base) p.first_names_aliases;
     List.iter (print_surname_alias oc base) p.surnames_aliases;
     match p.public_name with
     [ s when sou base s <> "" ->
         Printf.fprintf oc " (%s)" (correct_string base s)
     | _ -> () ];
     print_if_no_empty oc base "#image" p.image;
     List.iter (print_qualifier oc base) p.qualifiers;
     List.iter (print_alias oc base) p.aliases;
     List.iter (print_title oc base) p.titles;
     match p.access with
     [ IfTitles -> ()
     | Public -> Printf.fprintf oc " #apubl"
     | Private -> Printf.fprintf oc " #apriv" ];
     print_if_no_empty oc base "#occu" p.occupation;
     print_if_not_equal_to csrc oc base "#src" p.psources;
     match Adef.od_of_codate p.birth with
     [ Some d -> do Printf.fprintf oc " "; print_date oc d; return ()
     | _ ->
         if p.baptism <> Adef.codate_None then ()
         else
           match p.death with
           [ Death _ _ | DeadYoung | DeadDontKnowWhen ->
               Printf.fprintf oc " 0"
           | DontKnowIfDead
             when
               not is_child && not (has_infos_not_dates base p) &&
               p_first_name base p <> "?" && p_surname base p <> "?" ->
               Printf.fprintf oc " 0"
           | _ -> () ] ];
     print_if_not_equal_to cbp oc base "#bp" p.birth_place;
     print_if_no_empty oc base "#bs" p.birth_src;
     match Adef.od_of_codate p.baptism with
     [ Some d -> do Printf.fprintf oc " !"; print_date oc d; return ()
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
  do Printf.fprintf oc "%s %s%s" (s_correct_string surname)
       (s_correct_string first_name)
       (if p.occ == 0 || first_name = "?" || surname = "?" then ""
        else "." ^ string_of_int p.occ);
     if pr then
       if has_infos then print_infos oc base False "" "" p
       else if first_name <> "?" && surname <> "?" then Printf.fprintf oc " 0"
       else ()
     else ();
  return ()
;

value print_child oc base fam_surname csrc cbp p =
  do Printf.fprintf oc "-";
     match p.sex with
     [ Male -> Printf.fprintf oc " h"
     | Female -> Printf.fprintf oc " f"
     | _ -> () ];
     Printf.fprintf oc " %s" (s_correct_string (sou base p.first_name));
     if p.occ == 0 || p_first_name base p = "?" || p_surname base p = "?" then
       ()
     else Printf.fprintf oc ".%d" p.occ;
     if p.surname <> fam_surname then
       Printf.fprintf oc " %s" (s_correct_string (sou base p.surname))
     else ();
     print_infos oc base True csrc cbp p;
     Printf.fprintf oc "\n";
  return ()
;

value bogus_person base p =
  p_first_name base p = "?" && p_surname base p = "?"
;

value common_children proj base children =
  if Array.length children <= 1 then None
  else
    let list =
      List.map (fun p -> sou base (proj p)) (Array.to_list children)
    in
    if List.mem "" list then None
    else
      let list = Sort.list (fun s1 s2 -> s1 <= s2) list in
      let (src_max, n_max, _, _) =
        List.fold_left
          (fun (src_max, n_max, prev_src, n) src ->
             if src = prev_src then
               let n = n + 1 in
               if n > n_max then (src, n, src, n)
               else (src_max, n_max, src, n)
             else (src_max, n_max, src, 1))
          ("", 0, "", 0) list
      in
      if n_max > 1 then Some src_max else None
;

value common_children_sources = common_children (fun p -> p.psources);
value common_children_birth_place = common_children (fun p -> p.birth_place);

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

value print_witness oc base mark p notes_pl_p =
  let a = aoi base p.cle_index in
  let u = uoi base p.cle_index in
  do Printf.fprintf oc "%s %s%s" (correct_string base p.surname)
       (correct_string base p.first_name)
       (if p.occ = 0 then "" else "." ^ string_of_int p.occ);
     if Array.length u.family = 0 && a.parents = None &&
        not mark.(Adef.int_of_iper p.cle_index) then
       do mark.(Adef.int_of_iper p.cle_index) := True;
          if has_infos base p then print_infos oc base False "" "" p
          else Printf.fprintf oc " 0";
          match sou base p.notes with
          [ "" -> ()
          | _ -> notes_pl_p.val := [p :: notes_pl_p.val] ];
       return ()
     else ();
  return ()
;

value print_family oc base mark (per_sel, fam_sel) fam_done notes_pl_p m =
  let fam = m.m_fam in
  do Printf.fprintf oc "fam ";
     print_parent oc base mark fam_sel fam m.m_fath;
     Printf.fprintf oc " +";
     print_date_option oc (Adef.od_of_codate fam.marriage);
     match fam.relation with
     [ NotMarried -> Printf.fprintf oc " #nm"
     | Married -> ()
     | Engaged -> Printf.fprintf oc " #eng" ];
     print_if_no_empty oc base "#mp" fam.marriage_place;
     print_if_no_empty oc base "#ms" fam.marriage_src;
     match fam.divorce with
     [ NotDivorced -> ()
     | Separated -> Printf.fprintf oc " #sep"
     | Divorced d ->
         let d = Adef.od_of_codate d in
         do Printf.fprintf oc " -"; print_date_option oc d; return () ];
     Printf.fprintf oc " ";
     print_parent oc base mark fam_sel fam m.m_moth;
     Printf.fprintf oc "\n";
     Array.iter
       (fun ip ->
          if per_sel ip then
            let p = poi base ip in
            do Printf.fprintf oc "wit";
               match p.sex with
               [ Male -> Printf.fprintf oc " m"
               | Female -> Printf.fprintf oc " f"
               | _ -> () ];
               Printf.fprintf oc ": ";
               print_witness oc base mark p notes_pl_p;
               Printf.fprintf oc "\n";
            return ()
          else ())
       fam.witnesses;
     match sou base fam.fsources with
     [ "" -> ()
     | s -> Printf.fprintf oc "src %s\n" (correct_string base fam.fsources) ];
     let csrc =
       match common_children_sources base m.m_chil with
       [ Some s ->
           do Printf.fprintf oc "csrc %s\n" (s_correct_string s); return s
       | _ -> "" ]
     in
     let cbp =
       match common_children_birth_place base m.m_chil with
       [ Some s ->
           do Printf.fprintf oc "cbp %s\n" (s_correct_string s); return s
       | _ -> "" ]
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
                   print_child oc base fam_surname csrc cbp p
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
  let notes = sou base p.notes in
  let surn = s_correct_string (p_surname base p) in
  let fnam = s_correct_string (p_first_name base p) in
  if notes <> "" && surn <> "?" && fnam <> "?" then
    do Printf.fprintf oc "\n";
       Printf.fprintf oc "notes %s %s%s\n" surn fnam
         (if p.occ == 0 then "" else "." ^ string_of_int p.occ);
       Printf.fprintf oc "beg\n";
       Printf.fprintf oc "%s\n" notes;
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
value eq_key_fst (p1, _) (p2, _) = p1.cle_index == p2.cle_index;

value print_notes oc base ml per_sel pl =
  let pl = List.fold_right (get_persons_with_notes base) ml pl in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else [p :: pl]) pl []
  in
  List.iter
    (fun p ->
       if per_sel p.cle_index then print_notes_for_person oc base p else ())
    pl
;

value is_isolated base p =
  match (aoi base p.cle_index).parents with
  [ Some _ -> False
  | None -> Array.length (uoi base p.cle_index).family = 0 ]
;

value is_definition_for_parent base p =
  match (aoi base p.cle_index).parents with
  [ Some _ -> False
  | None -> True ]
;

value get_isolated_related base m list =
  let concat_isolated p_relation ip list =
    let p = poi base ip in
    if List.mem_assq p list then list
    else if is_isolated base p then
      match p.rparents with
      [ [{r_fath = Some x} :: _] when x = p_relation.cle_index ->
          [(p, True) :: list]
      | [{r_fath = None; r_moth = Some x} :: _]
        when x = p_relation.cle_index ->
          [(p, True) :: list]
      | _ -> list ]
    else list
  in
  let list =
    if is_definition_for_parent base m.m_fath then
      List.fold_right (concat_isolated m.m_fath) m.m_fath.related list
    else list
  in
  let list =
    if is_definition_for_parent base m.m_moth then
      List.fold_right (concat_isolated m.m_moth) m.m_moth.related list
    else list
  in
  let list =
    List.fold_right
      (fun p list -> List.fold_right (concat_isolated p) p.related list)
      (Array.to_list m.m_chil) list
  in
  list
;

value get_persons_with_relations base m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match (fath.rparents, (aoi base fath.cle_index).parents) with
    [ ([], _) | (_, Some _) -> list
    | _ -> [(fath, False) :: list] ]
  in
  let list =
    match (moth.rparents, (aoi base moth.cle_index).parents) with
    [ ([], _) | (_, Some _) -> list
    | _ -> [(moth, False) :: list] ]
  in
  let list =
    List.fold_right
      (fun ip list ->
         let p = poi base ip in
         match (p.rparents, (aoi base p.cle_index).parents) with
         [ ([], _) | (_, Some _) -> list
         | _ -> [(p, False) :: list] ])
      (Array.to_list m.m_fam.witnesses) list
  in
  List.fold_right
    (fun p list ->
       match p.rparents with
       [ [] -> list
       | _ -> [(p, False) :: list] ])
    (Array.to_list m.m_chil) list
;

value print_relation_parent oc base mark defined_p p =
  let a = aoi base p.cle_index in
  let u = uoi base p.cle_index in
  do Printf.fprintf oc "%s %s%s" (correct_string base p.surname)
       (correct_string base p.first_name)
       (if p.occ = 0 then "" else "." ^ string_of_int p.occ);
     if Array.length u.family = 0 && a.parents = None &&
        not mark.(Adef.int_of_iper p.cle_index) then
       do mark.(Adef.int_of_iper p.cle_index) := True;
          if has_infos base p then print_infos oc base False "" "" p
          else Printf.fprintf oc " 0";
          defined_p.val := [p :: defined_p.val];
       return ()
     else ();
  return ()
;

value print_relation_for_person oc base mark per_sel def_p p r =
  let fath =
    match r.r_fath with
    [ Some ip ->
        if per_sel ip then
          let p = poi base ip in
          if sou base p.first_name = "?" || sou base p.surname = "?" then None
          else Some p
        else None
    | None -> None ]
  in
  let moth =
    match r.r_moth with
    [ Some ip ->
        if per_sel ip then
          let p = poi base ip in
          if sou base p.first_name = "?" || sou base p.surname = "?" then None
          else Some p
        else None
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
         | GodParent -> Printf.fprintf oc "godp"
         | FosterParent -> Printf.fprintf oc "fost" ];
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

value print_relations_for_person oc base mark per_sel def_p is_definition p =
  let surn = correct_string base p.surname in
  let fnam = correct_string base p.first_name in
  let exist_relation =
    List.exists
      (fun r ->
         match (r.r_fath, r.r_moth) with
         [ (Some ip1, Some ip2) -> per_sel ip1 && per_sel ip2
         | (Some ip1, _) -> per_sel ip1
         | (_, Some ip2) -> per_sel ip2
         | _ -> False ])
      p.rparents
  in
  if surn <> "?" && fnam <> "?" && exist_relation then
    do Printf.fprintf oc "\n";
       Printf.fprintf oc "rel %s %s%s" surn fnam
         (if p.occ == 0 then "" else "." ^ string_of_int p.occ);
       if is_definition then
         do if has_infos base p then print_infos oc base False "" "" p
            else Printf.fprintf oc " 0";
            match p.sex with
            [ Male -> Printf.fprintf oc " #h"
            | Female -> Printf.fprintf oc " #f"
            | Neuter -> () ];
         return ()
       else ();
       Printf.fprintf oc "\n";
       Printf.fprintf oc "beg\n";
       List.iter (print_relation_for_person oc base mark per_sel def_p p)
         p.rparents;
       Printf.fprintf oc "end\n";
    return ()
  else ()
;

value print_relations oc base mark per_sel ml =
  let pl = List.fold_right (get_persons_with_relations base) ml [] in
  let pl = List.fold_right (get_isolated_related base) ml pl in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key_fst p pl then pl else [p :: pl]) pl []
  in
  let rec loop =
    fun
    [ [] -> ()
    | [(p, if_def) :: pl] ->
        let def_p = ref [] in
        do if p.rparents <> [] && per_sel p.cle_index then
             do print_relations_for_person oc base mark per_sel def_p if_def p;
                List.iter (print_notes_for_person oc base) def_p.val;
             return ()
           else ();
        return loop (pl @ List.map (fun p -> (p, False)) def_p.val) ]
  in
  loop pl
;

value rec merge_families ifaml1f ifaml2f =
  match (ifaml1f, ifaml2f) with
  [ ([ifam1 :: ifaml1], [ifam2 :: ifaml2]) ->
      let m1 = List.memq ifam1 ifaml2 in
      let m2 = List.memq ifam2 ifaml1 in
      if m1 && m2 then merge_families ifaml1 ifaml2
      else if m1 then [ifam2 :: merge_families ifaml1f ifaml2]
      else if m2 then [ifam1 :: merge_families ifaml1 ifaml2f]
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
  loop [fam.fam_index] [] [cpl.father] where rec loop ifaml ipl_scanned =
    fun
    [ [ip :: ipl] ->
        if List.memq ip ipl_scanned then loop ifaml ipl_scanned ipl
        else
          let u = uoi base ip in
          let ifaml1 = Array.to_list u.family in
          let ifaml1 = filter fam_sel ifaml1 in
          let ifaml = merge_families ifaml ifaml1 in
          let ipl =
            List.fold_right
              (fun ifam ipl ->
                 let cpl = coi base ifam in [cpl.father; cpl.mother :: ipl])
              ifaml1 ipl
          in
          loop ifaml [ip :: ipl_scanned] ipl
    | [] -> ifaml ]
;

value find_person base p1 po p2 =
  try Gutil.person_ht_find_unique base p1 p2 po with
  [ Not_found ->
      do Printf.printf "Not found: %s%s %s\n" p1
           (if po == 0 then "" else " " ^ string_of_int po) p2;
         flush stdout;
      return exit 2 ]
;

(* Separate option *)

type separate = [ ToSeparate | NotScanned | BeingScanned | Scanned ];

value rec find_ancestors base surn p list =
  match (aoi base p.cle_index).parents with
  [ Some ifam ->
      let cpl = coi base ifam in
      let fath = poi base cpl.father in
      let moth = poi base cpl.mother in
      if fath.surname <> surn && moth.surname <> surn then [p :: list]
      else
        let list =
          if fath.surname = surn then find_ancestors base surn fath list
          else list
        in
        let list =
          if moth.surname = surn then find_ancestors base surn moth list
          else list
        in
        list
  | None -> [p :: list] ]
;

value mark_branch base mark surn p =
  loop True p where rec loop top p =
    let u = uoi base p.cle_index in
    for i = 0 to Array.length u.family - 1 do
      let ifam = u.family.(i) in
      match mark.(Adef.int_of_ifam ifam) with
      [ NotScanned ->
          let ifaml =
            connected_families base (fun _ -> True) (foi base ifam)
              (coi base ifam)
          in
          let children =
            List.fold_left
              (fun list ifam ->
                 let desc = doi base ifam in
                 Array.fold_left (fun list ip -> [poi base ip :: list]) list
                   desc.children)
              [] ifaml
          in
          if top || List.exists (fun p -> p.surname = surn) children then
            do List.iter
                 (fun ifam -> mark.(Adef.int_of_ifam ifam) := ToSeparate)
                 ifaml;
               List.iter (loop False) children;
            return ()
          else ()
      | _ -> () ];
    done
;

value mark_someone base mark s =
  match Gutil.person_ht_find_all base s with
  [ [ip] ->
      let p = poi base ip in
      let plist = find_ancestors base p.surname p [] in
      List.iter (mark_branch base mark p.surname) plist
  | [] ->
      do Printf.eprintf "Error: \"%s\" is not found\n" s; flush stderr; return
      exit 2
  | _ ->
      do Printf.eprintf "Error: several answers for \"%s\"\n" s; flush stderr;
      return exit 2 ]
;

value sep_limit = ref 21;
value separate_list = ref [];

value scan_connex_component base test_action len ifam =
  loop len ifam where rec loop len ifam =
    let cpl = coi base ifam in
    let len =
      Array.fold_left
        (fun len ifam1 ->
           if ifam1 = ifam then len else test_action loop len ifam1)
        len (uoi base cpl.father).family
    in
    let len =
      Array.fold_left
        (fun len ifam1 ->
           if ifam1 = ifam then len else test_action loop len ifam1)
        len (uoi base cpl.mother).family
    in
    let len =
      match (aoi base cpl.father).parents with
      [ Some ifam -> test_action loop len ifam
      | _ -> len ]
    in
    let len =
      match (aoi base cpl.mother).parents with
      [ Some ifam -> test_action loop len ifam
      | _ -> len ]
    in
    let children = (doi base ifam).children in
    let len =
      Array.fold_left
        (fun len ip ->
           Array.fold_left (test_action loop) len (uoi base ip).family)
        len children
    in
    len
;

value mark_one_connex_component base mark ifam =
  let origin_file = sou base (foi base ifam).origin_file in
  let test_action loop len ifam =
    if mark.(Adef.int_of_ifam ifam) == NotScanned &&
       sou base (foi base ifam).origin_file = origin_file then
      do mark.(Adef.int_of_ifam ifam) := BeingScanned; return
      loop (len + 1) ifam
    else len
  in
  let _ = test_action (fun _ _ -> 1) 0 ifam in
  let len = 1 + scan_connex_component base test_action 0 ifam in
  let set_mark x =
    let test_action loop () ifam =
      if mark.(Adef.int_of_ifam ifam) == BeingScanned then
        do mark.(Adef.int_of_ifam ifam) := x; return loop () ifam
      else ()
    in
    do test_action (fun _ _ -> ()) () ifam;
       scan_connex_component base test_action () ifam;
    return ()
  in
  if len <= sep_limit.val then set_mark ToSeparate
  else
    do Printf.eprintf "group of size %d not included\n" len;
       let cpl = coi base ifam in
       Printf.eprintf "    %s + %s\n"
         (denomination base (poi base cpl.father))
         (denomination base (poi base cpl.mother));
       flush stderr;
    return set_mark Scanned
;

value mark_connex_components base mark fam =
  let test_action loop len ifam =
    if mark.(Adef.int_of_ifam ifam) == NotScanned then
      do mark_one_connex_component base mark ifam; return ()
    else ()
  in
  scan_connex_component base test_action () fam.fam_index
;

value add_small_connex_components base mark =
  for i = 0 to base.data.families.len - 1 do
    if mark.(i) = ToSeparate then
      let fam = base.data.families.get i in
      mark_connex_components base mark fam
    else ();
  done
;

value separate base =
  match List.rev separate_list.val with
  [ [] -> fun _ -> False
  | list ->
      let mark = Array.create base.data.families.len NotScanned in
      do if list <> [] then
           do List.iter (mark_someone base mark) list;
              add_small_connex_components base mark;
              let len =
                loop 0 0 where rec loop len i =
                  if i = base.data.families.len then len
                  else if mark.(i) = ToSeparate then loop (len + 1) (i + 1)
                  else loop len (i + 1)
              in
              Printf.eprintf "*** extracted %d families\n" len;
              flush stderr;
           return ()
         else ();
      return fun ifam -> mark.(Adef.int_of_ifam ifam) == ToSeparate ]
;

(* Main *)

value surnames = ref [];
value no_spouses_parents = ref False;
value no_notes = ref False;
value censor = ref 0;

value gwu base out_dir out_oc src_oc_list anc desc ancdesc =
  let to_separate = separate base in
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
  let ancdesc =
    match ancdesc with
    [ Some (p1, po, p2) -> Some (find_person base p1 po p2)
    | None -> None ]
  in
  let ((per_sel, fam_sel) as sel) =
    Select.functions base anc desc surnames.val ancdesc no_spouses_parents.val
      censor.val
  in
  let fam_done = Array.create base.data.families.len False in
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
          do src_oc_list.val := [(fname, x) :: src_oc_list.val]; return x ]
  in
  do for i = 0 to base.data.families.len - 1 do
       let fam = base.data.families.get i in
       let cpl = base.data.couples.get i in
       if is_deleted_family fam then ()
       else
         do if fam_done.(i) then ()
            else if fam_sel fam.fam_index then
              let ifaml = connected_families base fam_sel fam cpl in
              let (oc, first) =
                if to_separate fam.fam_index then (out_oc, out_oc_first)
                else origin_file (sou base fam.origin_file)
              in
              let ml =
                List.fold_right
                  (fun ifam ml ->
                     let fam = foi base ifam in
                     let cpl = coi base ifam in
                     let des = doi base ifam in
                     let m =
                       {m_fam = fam; m_fath = poi base cpl.father;
                        m_moth = poi base cpl.mother;
                        m_chil =
                          Array.map (fun ip -> poi base ip) des.children}
                     in
                     if empty_family base m then
                       do fam_done.(Adef.int_of_ifam m.m_fam.fam_index) :=
                            True;
                       return ml
                     else [m :: ml])
                  ifaml []
              in
              if ml <> [] then
                let notes_pl_p = ref [] in
                do if not first.val then Printf.fprintf oc "\n" else ();
                   first.val := False;
                   List.iter
                     (print_family oc base mark sel fam_done notes_pl_p) ml;
                   print_notes oc base ml per_sel notes_pl_p.val;
                   print_relations oc base mark per_sel ml;
                return ()
              else ()
            else ();
         return ();
     done;
     let s = base.data.bnotes.nread 0 in
     if s = "" then ()
     else if not no_notes.val then
       let (oc, first) = origin_file base.data.bnotes.norigin_file in
       do if not first.val then Printf.fprintf oc "\n" else ();
          Printf.fprintf oc "notes\n";
          Printf.fprintf oc "%s\n" s;
          Printf.fprintf oc "end notes\n";
       return ()
     else ();
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
value ancdesc_1st = ref "";
value ancdesc_occ = ref 0;
value ancdesc_2nd = ref "";

type arg_state =
  [ ASnone
  | ASwaitAncOcc
  | ASwaitAncSurn
  | ASwaitDescOcc
  | ASwaitDescSurn
  | ASwaitAncdescOcc
  | ASwaitAncdescSurn ]
;
value arg_state = ref ASnone;
value mem = ref False;

value speclist =
  [("-o", Arg.String (fun s -> out_file.val := s),
    "<file>    output file name (else stdout)");
   ("-odir", Arg.String (fun s -> out_dir.val := s),
    "<dir>  create files from original name in directory (else on -o file)");
   ("-mem", Arg.Set mem, "        save memory space, but slower");
   ("-a",
    Arg.String
      (fun s -> do anc_1st.val := s; return arg_state.val := ASwaitAncOcc),
    "\"<1st_name>\" [num] \"<surname>\" : select ancestors of...");
   ("-d",
    Arg.String
      (fun s -> do desc_1st.val := s; return arg_state.val := ASwaitDescOcc),
    "\"<1st_name>\" [num] \"<surname>\" : select descendants of...");
   ("-ad",
    Arg.String
      (fun s -> do ancdesc_1st.val := s; return arg_state.val := ASwaitAncdescOcc),
    "\"<1st_name>\" [num] \"<surname>\" : select ancestors of...
    and all their descendants (has no effect if -a and/or -d used,
    option -nsp is forced).");
   ("-s", Arg.String (fun x -> surnames.val := [x :: surnames.val]),
    "\"<surname>\" : select this surname (option usable several times)");
   ("-nsp", Arg.Set no_spouses_parents,
    ": no spouses' parents (for options -s and -d)");
   ("-nn", Arg.Set no_notes, ": no (data base) notes");
   ("-c", Arg.Int (fun i -> censor.val := i), "\
<num> :
     When a person is born less than <num> years ago, it is not exported unless
     it is Public. All the spouses and descendants are also censored.");
   ("-sep",
    Arg.String (fun s -> separate_list.val := [s :: separate_list.val]), "\
\"1st_name.num surname\" :
     To use together with the option \"-odir\": separate this person and
     all his ancestors and descendants sharing the same surname. All the
     concerned families are displayed on standard output instead of their
     associated files. This option can be used several times.");
   ("-sep_limit", Arg.Int (fun i -> sep_limit.val := i), "\
<num> :
     When using the option \"-sep\", groups of families can become isolated
     in the files. Gwu reconnects them to the separated families (i.e.
     displays them to standard output) if the size of these groups is less
     than " ^ string_of_int sep_limit.val ^ "\
. The present option changes this limit.")]
;

value anonfun s =
  match arg_state.val with
  [ ASnone ->
      if in_file.val = "" then in_file.val := s
      else raise (Arg.Bad "Cannot treat several data bases")
  | ASwaitAncOcc ->
      try
        do anc_occ.val := int_of_string s; return
        arg_state.val := ASwaitAncSurn
      with
      [ Failure _ ->
          do anc_occ.val := 0; anc_2nd.val := s; return
          arg_state.val := ASnone ]
  | ASwaitAncSurn -> do anc_2nd.val := s; return arg_state.val := ASnone
  | ASwaitDescOcc ->
      try
        do desc_occ.val := int_of_string s; return
        arg_state.val := ASwaitDescSurn
      with
      [ Failure _ ->
          do desc_occ.val := 0; desc_2nd.val := s; return
          arg_state.val := ASnone ]
  | ASwaitDescSurn -> do desc_2nd.val := s; return arg_state.val := ASnone
  | ASwaitAncdescOcc ->
      try
        do ancdesc_occ.val := int_of_string s; return
        arg_state.val := ASwaitAncdescSurn
      with
      [ Failure _ ->
          do ancdesc_occ.val := 0; ancdesc_2nd.val := s; return
          arg_state.val := ASnone ]
  | ASwaitAncdescSurn -> do ancdesc_2nd.val := s; return arg_state.val := ASnone ]
;

value errmsg =
  "Usage: " ^ Sys.argv.(0) ^ " \
[options] <base_file>
If both options -a and -d are used, intersection is assumed.
If several options -s are used, union is assumed.
When option -s is used, the options -a and -d are ignored.
Options are:"
;

value main () =
  do ifdef MAC then
       do Printf.eprintf "args? "; flush stderr;
          let line = input_line stdin in
          let list = Gutil.arg_list_of_string line in
          Argl.parse_list speclist anonfun errmsg list;
       return ()
     else ();
     Argl.parse speclist anonfun errmsg;
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
  let ancdesc =
    if ancdesc_1st.val <> "" then
      if anc_1st.val <> "" || desc_1st.val <> "" then
	do Printf.printf "Option -ad skipped since -a and/or -d used\n";
	return None
      else if ancdesc_2nd.val = "" then
        do Printf.printf "Misused option -ad\n";
           Printf.printf "Use option -help for usage\n";
           flush stdout;
        return exit 2
      else
        do no_spouses_parents.val := True; return
        Some (ancdesc_1st.val, ancdesc_occ.val, ancdesc_2nd.val)
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
       let _ = base.data.unions.array () in
       let _ = base.data.descends.array () in
       ()
     else ();
  return
  let oc_list = ref [] in
  let out_oc =
    if out_file.val = "" then stdout else open_out out_file.val
  in
  do gwu base out_dir.val out_oc src_oc_list anc desc ancdesc;
     List.iter
       (fun (src, (oc, _)) -> do flush oc; close_out oc; return ())
       src_oc_list.val;
     flush out_oc;
     if out_file.val = "" then () else close_out out_oc;
  return ()
;

Printexc.catch main ();
