(* camlp4r ./pa_html.cmo ./pa_lock.cmo *)
(* $Id: mergeInd.ml,v 5.19 2006-09-22 23:47:14 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Util;

value print_differences conf base branches p1 p2 =
  let gen_string_field chk1 chk2 str_orig title name proj =
    let x1 = proj p1 in
    let x2 = proj p2 in
    if x1 <> "" && x1 <> "?" && x2 <> "" && x2 <> "?" && x1 <> x2 then do {
      Wserver.wprint "<h4>%s</h4>\n" (capitale title);
      tag "ul" begin
        tag "li" begin
          xtag "input" "type=\"radio\" name=\"%s\" value=\"1\"%s" name chk1;
          Wserver.wprint "%s\n" x1;
        end;
        tag "li" begin
          xtag "input" "type=\"radio\" name=\"%s\" value=\"2\"%s" name chk2;
          Wserver.wprint "%s\n" x2;
        end;
      end;
    }
    else ()
  in
  let string_field = gen_string_field " checked" "" in
  tag "form" "method=\"post\" action=\"%s\"" conf.command begin
    tag "p" begin
      Util.hidden_env conf;
      xtag "input" "type=\"hidden\" name=\"m\" value=\"MRG_IND_OK\"";
      xtag "input" "type=\"hidden\" name=\"i1\" value=\"%d\""
        (Adef.int_of_iper (get_key_index p1));
      xtag "input" "type=\"hidden\" name=\"i2\" value=\"%d\""
        (Adef.int_of_iper (get_key_index p2));
      loop branches where rec loop =
        fun
        [ [(ip1, ip2)] ->
            do {
              xtag "input" "type=\"hidden\" name=\"ini1\" value=\"%d\""
                (Adef.int_of_iper ip1);
              xtag "input" "type=\"hidden\" name=\"ini2\" value=\"%d\""
                (Adef.int_of_iper ip2);
            }
        | [_ :: branches] -> loop branches
        | _ -> () ];
    end;
    html_p conf;
    string_field True (transl_nth conf "first name/first names" 0)
      "first_name" (fun p -> p_first_name base p);
    string_field True (transl_nth conf "surname/surnames" 0) "surname"
      (fun p -> p_surname base p);
    let select_smallest_num = p_first_name base p1 = p_first_name base p2 in
    gen_string_field
      (if get_occ p1 < get_occ p2 || not select_smallest_num then " checked"
       else "")
      (if get_occ p1 > get_occ p2 && select_smallest_num then " checked"
       else "")
      False (transl conf "number") "number"
      (fun p -> string_of_int (get_occ p));
    string_field True (transl_nth conf "image/images" 0) "image"
      (fun p -> sou base (get_image p));
    string_field True (transl conf "public name") "public_name"
      (fun p -> sou base (get_public_name p));
    string_field True (transl conf "occupation") "occupation"
      (fun p -> sou base (get_occupation p));
    string_field False (transl conf "sex") "sex"
      (fun p ->
         match get_sex p with
         [ Male -> "M"
         | Female -> "F"
         | Neuter -> "" ]);
(*
    string_field False (transl conf "access") "access"
      (fun p ->
         match p.access with
         [ IfTitles -> transl conf "if titles"
         | Private -> "private"
         | Public -> "public" ]);
*)
    string_field False (transl conf "birth") "birth"
      (fun p ->
         match Adef.od_of_codate (get_birth p) with
         [ None -> ""
         | Some d -> Date.string_of_ondate conf d ]);
    string_field True (transl conf "birth" ^ " / " ^ transl conf "place")
      "birth_place" (fun p -> sou base (get_birth_place p));
    string_field False (transl conf "baptism") "baptism"
      (fun p ->
         match Adef.od_of_codate (get_baptism p) with
         [ None -> ""
         | Some d -> Date.string_of_ondate conf d ]);
    string_field True (transl conf "baptism" ^ " / " ^ transl conf "place")
      "baptism_place" (fun p -> sou base (get_baptism_place p));
    string_field False (transl conf "death") "death"
      (fun p ->
         let is = 2 in
         match get_death p with
         [ NotDead -> transl_nth conf "alive" is
         | Death dr cd ->
             let s =
               match dr with
               [ Killed -> transl_nth conf "killed (in action)" is
               | Murdered -> transl_nth conf "murdered" is
               | Executed -> transl_nth conf "executed (legally killed)" is
               | Disappeared -> transl_nth conf "disappeared" is
               | Unspecified -> transl_nth conf "died" is ]
             in
             s ^ " " ^ Date.string_of_ondate conf (Adef.date_of_cdate cd)
         | DeadYoung -> transl_nth conf "died young" is
         | DeadDontKnowWhen -> transl_nth conf "died" is
         | DontKnowIfDead -> "" ]);
    string_field True (transl conf "death" ^ " / " ^ transl conf "place")
      "death_place" (fun p -> sou base (get_death_place p));
    string_field False (transl conf "burial") "burial"
      (fun p ->
         let is = 2 in
         match get_burial p with
         [ UnknownBurial -> ""
         | Buried cod ->
             transl_nth conf "buried" is ^
               (match Adef.od_of_codate cod with
                [ None -> ""
                | Some d -> " " ^ Date.string_of_ondate conf d ])
         | Cremated cod ->
             transl_nth conf "cremated" is ^
               (match Adef.od_of_codate cod with
                [ None -> ""
                | Some d -> " " ^ Date.string_of_ondate conf d ]) ]);
    string_field True (transl conf "burial" ^ " / " ^ transl conf "place")
      "burial_place" (fun p -> sou base (get_burial_place p));
    html_p conf;
    Wserver.wprint "<input type=\"submit\" value=\"Ok\">\n";
  end
;

value compatible_codates cd1 cd2 =
  cd1 = cd2 || cd2 = Adef.codate_None || cd1 = Adef.codate_None;

value compatible_cdates cd1 cd2 = cd1 = cd2;

value compatible_death_reasons dr1 dr2 = dr1 = dr2 || dr2 = Unspecified;

value compatible_deaths d1 d2 =
  if d1 = d2 then True
  else
    match (d1, d2) with
    [ (Death dr1 cd1, Death dr2 cd2) ->
        compatible_death_reasons dr1 dr2 && compatible_cdates cd1 cd2
    | (Death _ _, NotDead) -> False
    | (Death _ _, _) -> True
    | (_, DontKnowIfDead) -> True
    | (DontKnowIfDead, _) -> True
    | _ -> False ]
;

value compatible_burials b1 b2 =
  if b1 = b2 then True
  else
    match (b1, b2) with
    [ (_, UnknownBurial) -> True
    | (UnknownBurial, _) -> True
    | (Buried cd1, Buried cd2) -> compatible_codates cd1 cd2
    | (Cremated cd1, Cremated cd2) -> compatible_codates cd1 cd2
    | _ -> False ]
;

value compatible_strings s1 s2 =
  s1 = s2 || s2 = Adef.istr_of_int 0 || s1 = Adef.istr_of_int 0;

value compatible_divorces d1 d2 = d1 = d2;

value compatible_relation_kinds rk1 rk2 = rk1 = rk2;

value compatible_accesses a1 a2 = (*a1 = a2*)True;

value compatible_titles t1 t2 = t1 = t2 || t2 = [];

value compatible_strings_lists sl1 sl2 = sl2 = [] || sl1 = sl2;

value compatible_ind base p1 p2 =
  get_first_name p1 = get_first_name p2 && get_surname p1 = get_surname p2 &&
  compatible_strings (get_image p1) (get_image p2) &&
  compatible_strings (get_public_name p1) (get_public_name p2) &&
  compatible_strings_lists (get_qualifiers p1) (get_qualifiers p2) &&
  compatible_strings_lists (get_aliases p1) (get_aliases p2) &&
  compatible_strings_lists (get_first_names_aliases p1)
    (get_first_names_aliases p2) &&
  compatible_strings_lists (get_surnames_aliases p1)
    (get_surnames_aliases p2) &&
  compatible_titles (get_titles p1) (get_titles p2) &&
  get_rparents p2 = [] && get_related p2 = [] &&
  compatible_strings (get_occupation p1) (get_occupation p2) &&
  compatible_accesses (get_access p1) (get_access p2) &&
  compatible_codates (get_birth p1) (get_birth p2) &&
  compatible_strings (get_birth_place p1) (get_birth_place p2) &&
  compatible_codates (get_baptism p1) (get_baptism p2) &&
  compatible_strings (get_baptism_place p1) (get_baptism_place p2) &&
  compatible_deaths (get_death p1) (get_death p2) &&
  compatible_strings (get_death_place p1) (get_death_place p2) &&
  compatible_burials (get_burial p1) (get_burial p2) &&
  compatible_strings (get_burial_place p1) (get_burial_place p2) &&
  compatible_strings (get_notes p1) (get_notes p2) 
;

value compatible_fam base fam1 fam2 =
  compatible_codates (get_marriage fam1) (get_marriage fam2) &&
  compatible_strings (get_marriage_place fam1) (get_marriage_place fam2) &&
  Array.length (get_witnesses fam2) = 0 &&
  compatible_relation_kinds (get_relation fam1) (get_relation fam2) &&
  compatible_divorces (get_divorce fam1) (get_divorce fam2) &&
  compatible_strings (get_fsources fam1) (get_fsources fam2)
;

value propose_merge_ind conf base branches p1 p2 =
  let title h =
    let s = transl_nth conf "person/persons" 1 in
    Wserver.wprint "%s" (capitale (transl_decline conf "merge" s))
  in
  do {
    header conf title;
    if branches <> [] then do {
      Wserver.wprint "%s:\n" (capitale (transl conf "you must first merge"));
      tag "ul" begin
        html_li conf;
        stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p1) begin
          Merge.print_someone conf base p1;
        end;
        Wserver.wprint "\n%s\n" (transl_nth conf "and" 0);
        stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p2) begin
          Merge.print_someone conf base p2;
        end;
        Wserver.wprint "\n";
      end;
      html_p conf;
    }
    else ();
    print_differences conf base branches p1 p2;
    if branches <> [] then do {
      html_p conf;
      Wserver.wprint "<hr>";
      html_p conf;
      Wserver.wprint "%s:\n" (capitale (transl_nth conf "branch/branches" 1));
      html_p conf;
      tag "table" begin
        List.iter
          (fun (ip1, ip2) ->
             let p1 = poi base ip1 in
             let p2 = poi base ip2 in
             do {
               tag "tr" "align=\"%s\"" conf.left begin
                 tag "td" begin
                   Wserver.wprint "\n%s" (referenced_person_text conf base p1);
                   Wserver.wprint "%s" (Date.short_dates_text conf base p1);
                 end;
                 tag "td" begin
                   Wserver.wprint "\n%s" (referenced_person_text conf base p2);
                   Wserver.wprint "%s" (Date.short_dates_text conf base p2);
                 end;
               end;
             })
          [(get_key_index p1, get_key_index p2) :: branches];
      end;
    }
    else ();
    trailer conf;
  }
;

value reparent_ind base ip1 ip2 =
  let a1 = aoi base ip1 in
  let a2 = aoi base ip2 in
  match (get_parents a1, get_parents a2) with
  [ (None, Some ifam) ->
      let des = doi base ifam in
      do {
        let rec replace i =
          if (get_children des).(i) = ip2 then (get_children des).(i) := ip1
          else replace (i + 1)
        in
        replace 0;
        let a1 =
          ascend_of_gen_ascend {parents = Some ifam; consang = Adef.fix (-1)}
        in
        base.func.patch_ascend ip1 a1;
        base.func.patch_descend ifam des;
      }
  | _ -> () ]
;

value effective_merge_ind conf base p1 p2 =
  do {
    reparent_ind base (get_key_index p1) (get_key_index p2);
    let u2 = uoi base (get_key_index p2) in
    if Array.length (get_family u2) <> 0 then do {
      for i = 0 to Array.length (get_family u2) - 1 do {
        let ifam = (get_family u2).(i) in
        let cpl = coi base ifam in
        let cpl =
          if get_key_index p2 = get_father cpl then
            couple False (get_key_index p1) (get_mother cpl)
          else if get_key_index p2 = get_mother cpl then
            couple False (get_father cpl) (get_key_index p1)
          else assert False
        in
        base.func.patch_couple ifam (couple_of_gen_couple cpl);
      };
      let u1 = uoi base (get_key_index p1) in
      let u1 =
        union_of_gen_union
          {family = Array.append (get_family u1) (get_family u2)}
      in
      base.func.patch_union (get_key_index p1) u1;
      let u2 = union_of_gen_union {family = [| |]} in
      base.func.patch_union (get_key_index p2) u2;
    }
    else ();
    let p1 =
      person_of_gen_person
        {(gen_person_of_person p1) with
         sex = if get_sex p2 <> Neuter then get_sex p2 else get_sex p1;
         birth =
           if get_birth p1 = Adef.codate_None then get_birth p2
           else get_birth p1;
         birth_place =
           if get_birth_place p1 = Adef.istr_of_int 0 then get_birth_place p2
           else get_birth_place p1;
         birth_src =
           if get_birth_src p1 = Adef.istr_of_int 0 then get_birth_src p2
           else get_birth_src p1;
         baptism =
           if get_baptism p1 = Adef.codate_None then get_baptism p2
           else get_baptism p1;
         baptism_place =
           if get_baptism_place p1 = Adef.istr_of_int 0 then
             get_baptism_place p2
           else get_baptism_place p1;
         baptism_src =
           if get_baptism_src p1 = Adef.istr_of_int 0 then get_baptism_src p2
           else get_baptism_src p1;
         death =
           if get_death p1 = DontKnowIfDead then get_death p2
           else get_death p1;
         death_place =
           if get_death_place p1 = Adef.istr_of_int 0 then get_death_place p2
           else get_death_place p1;
         death_src =
           if get_death_src p1 = Adef.istr_of_int 0 then get_death_src p2
           else get_death_src p1;
         burial =
           if get_burial p1 = UnknownBurial then get_burial p2
           else get_burial p1;
         burial_place =
           if get_burial_place p1 = Adef.istr_of_int 0 then
             get_burial_place p2
           else get_burial_place p1;
         burial_src =
           if get_burial_src p1 = Adef.istr_of_int 0 then get_burial_src p2
           else get_burial_src p1;
         occupation =
           if get_occupation p1 = Adef.istr_of_int 0 then get_occupation p2
           else get_occupation p1;
         notes =
           if get_notes p1 = Adef.istr_of_int 0 then get_notes p2
           else get_notes p1}
    in
    let p2 = UpdateIndOk.effective_del conf base p2 in
    base.func.patch_person (get_key_index p1) p1;
    base.func.patch_person (get_key_index p2) p2;
    Notes.update_notes_links_db conf (NotesLinks.PgInd (get_key_index p1))
      (sou base (get_notes p1)) True;
  }
;

value merge_ind conf base branches ip1 ip2 changes_done =
  let p1 = poi base ip1 in
  let p2 = poi base ip2 in
  if compatible_ind base p1 p2 then do {
    effective_merge_ind conf base p1 p2; (True, True)
  }
  else do {
    propose_merge_ind conf base branches p1 p2; (False, changes_done)
  }
;

value propose_merge_fam conf base branches fam1 fam2 p1 p2 =
  let title h =
    let s = transl_nth conf "family/families" 1 in
    Wserver.wprint "%s" (capitale (transl_decline conf "merge" s))
  in
  do {
    header conf title;
    Wserver.wprint "%s:\n"
      (capitale (transl conf "you must first merge the 2 families"));
    tag "ul" begin
      html_li conf;
      stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p1) begin
        Merge.print_someone conf base p1;
      end;
      Wserver.wprint "\n%s\n" (transl conf "with");
      stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p2) begin
        Merge.print_someone conf base p2;
      end;
      Wserver.wprint "\n";
    end;
    html_p conf;
    MergeFam.print_differences conf base branches fam1 fam2;
    trailer conf;
  }
;

value effective_merge_fam conf base fam1 fam2 p1 p2 =
  let des1 = doi base (get_fam_index fam1) in
  let des2 = doi base (get_fam_index fam2) in
  let fam1 =
    family_of_gen_family
      {(gen_family_of_family fam1) with
       marriage =
         if get_marriage fam1 = Adef.codate_None then get_marriage fam2
         else get_marriage fam1;
       marriage_place =
         if get_marriage_place fam1 = Adef.istr_of_int 0 then
           get_marriage_place fam2
         else get_marriage_place fam1;
       marriage_src =
         if get_marriage_src fam1 = Adef.istr_of_int 0 then
           get_marriage_src fam2
         else get_marriage_src fam1;
       fsources =
         if get_fsources fam1 = Adef.istr_of_int 0 then get_fsources fam2
         else get_fsources fam1}
  in
  do {
    base.func.patch_family (get_fam_index fam1) fam1;
    let des1 =
      descend_of_gen_descend
        {children = Array.append (get_children des1) (get_children des2)}
    in
    base.func.patch_descend (get_fam_index fam1) des1;
    for i = 0 to Array.length (get_children des2) - 1 do {
      let ip = (get_children des2).(i) in
      let a =
        ascend_of_gen_ascend
          {parents = Some (get_fam_index fam1); consang = Adef.fix (-1)}
      in
      base.func.patch_ascend ip a;
    };
    let des2 = descend_of_gen_descend {children = [| |]} in
    base.func.patch_descend (get_fam_index fam2) des2;
    UpdateFamOk.effective_del conf base fam2;
  }
;

value merge_fam conf base branches ifam1 ifam2 ip1 ip2 changes_done =
  let p1 = poi base ip1 in
  let p2 = poi base ip2 in
  let fam1 = foi base ifam1 in
  let fam2 = foi base ifam2 in
  if compatible_fam base fam1 fam2 then do {
    effective_merge_fam conf base fam1 fam2 p1 p2; (True, True)
  }
  else do {
    propose_merge_fam conf base branches fam1 fam2 p1 p2;
    (False, changes_done)
  }
;

value not_found_or_incorrect conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Wserver.wprint "%s %s %s %s %s\n" (capitale (transl conf "not found"))
      (transl conf "or") (transl conf "several answers") (transl conf "or")
      (transl conf "incorrect request");
    trailer conf;
  }
;

value same_person conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Wserver.wprint "%s\n" (capitale (transl conf "it is the same person!"));
    trailer conf;
  }
;

value different_sexes conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Wserver.wprint "%s.\n" (capitale (transl conf "incompatible sexes"));
    trailer conf;
  }
;

value rec try_merge conf base branches ip1 ip2 changes_done =
  let a1 = aoi base ip1 in
  let a2 = aoi base ip2 in
  let ok_so_far = True in
  let (ok_so_far, changes_done) =
    match (get_parents a1, get_parents a2) with
    [ (Some ifam1, Some ifam2) when ifam1 <> ifam2 ->
        let branches = [(ip1, ip2) :: branches] in
        let cpl1 = coi base ifam1 in
        let cpl2 = coi base ifam2 in
        let (ok_so_far, changes_done) =
          if ok_so_far then
            if get_father cpl1 = get_father cpl2 then (True, changes_done)
            else
              try_merge conf base branches (get_father cpl1) (get_father cpl2)
                changes_done
          else (False, changes_done)
        in
        let (ok_so_far, changes_done) =
          if ok_so_far then
            if get_mother cpl1 = get_mother cpl2 then (True, changes_done)
            else
              try_merge conf base branches (get_mother cpl1) (get_mother cpl2)
                changes_done
          else (False, changes_done)
        in
        let (ok_so_far, changes_done) =
          if ok_so_far then
            merge_fam conf base branches ifam1 ifam2 (get_father cpl1)
              (get_mother cpl1) changes_done
          else (False, changes_done)
        in
        (ok_so_far, changes_done)
    | _ -> (ok_so_far, changes_done) ]
  in
  if ok_so_far then merge_ind conf base branches ip1 ip2 changes_done
  else (False, changes_done)
;

value print_merged conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "merge done")) in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "\n%s" (referenced_person_text conf base p);
    Wserver.wprint "\n";
    trailer conf;
  }
;

value is_ancestor base ip1 ip2 =
  let visited = Array.create base.data.persons.len False in
  let rec loop ip =
    if visited.(Adef.int_of_iper ip) then False
    else if ip = ip1 then True
    else do {
      visited.(Adef.int_of_iper ip) := True;
      match get_parents (aoi base ip) with
      [ Some ifam ->
          let cpl = coi base ifam in
          loop (get_father cpl) || loop (get_mother cpl)
      | None -> False ]
    }
  in
  loop ip2
;

value error_loop conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<strong>%s%s %s</strong>" (p_first_name base p)
      (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
      (p_surname base p);
    Wserver.wprint "\n%s\n" (transl conf "would be his/her own ancestor");
    Wserver.wprint "\n";
    trailer conf;
  }
;

value print conf base =
  let p1 =
    match p_getint conf.env "i" with
    [ Some i1 -> Some (poi base (Adef.iper_of_int i1))
    | None -> None ]
  in
  let p2 =
    match p_getint conf.env "i2" with
    [ Some i2 -> Some (poi base (Adef.iper_of_int i2))
    | None ->
        match (p_getenv conf.env "select", p_getenv conf.env "n") with
        [ (Some "input" | None, Some n) ->
            let ipl = Gutil.person_ht_find_all base n in
            match ipl with
            [ [ip2] -> Some (poi base ip2)
            | _ -> None ]
        | (Some x, Some "" | None) ->
            Some (poi base (Adef.iper_of_int (int_of_string x)))
        | _ -> None ] ]
  in
  match (p1, p2) with
  [ (Some p1, Some p2) ->
      if get_key_index p1 = get_key_index p2 then same_person conf
      else if
        get_sex p1 <> get_sex p2 && get_sex p1 <> Neuter &&
        get_sex p2 <> Neuter
      then
        different_sexes conf
      else if is_ancestor base (get_key_index p1) (get_key_index p2) then
        error_loop conf base p2
      else if is_ancestor base (get_key_index p2) (get_key_index p1) then
        error_loop conf base p1
      else
        let (ok, changes_done) =
          try_merge conf base [] (get_key_index p1) (get_key_index p2) False
        in
        do {
          if changes_done then Util.commit_patches conf base else ();
          if ok then do {
            let key =
              (sou base (get_first_name p1), sou base (get_surname p1),
               get_occ p1, get_key_index p1)
            in
            History.record conf base key "fp";
            print_merged conf base p1;
          }
          else ();
        }
  | _ -> not_found_or_incorrect conf ]
;

(* Undocumented feature... Kill someone's ancestors *)

value rec kill_ancestors conf base included_self p nb_ind nb_fam =
  do {
    match get_parents (aoi base (get_key_index p)) with
    [ Some ifam ->
        let cpl = coi base ifam in
        do {
          kill_ancestors conf base True (poi base (get_father cpl)) nb_ind
            nb_fam;
          kill_ancestors conf base True (poi base (get_mother cpl)) nb_ind
            nb_fam;
          UpdateFamOk.effective_del conf base (foi base ifam);
          incr nb_fam;
        }
    | None -> () ];
    if included_self then do {
      let ip = get_key_index p in
      let p = UpdateIndOk.effective_del conf base p in
      base.func.patch_person ip p;
      incr nb_ind;
    }
    else ();
  }
;

value print_killed conf base p nb_ind nb_fam =
  let title _ = Wserver.wprint "Ancestors killed" in
  do {
    Util.header conf title;
    Wserver.wprint "%s's ancestors killed.<br>\n"
      (referenced_person_title_text conf base p);
    Wserver.wprint "%d persons and %d families deleted<p>\n" nb_ind nb_fam;
    Util.trailer conf;
  }
;

value print_kill_ancestors conf base =
  match p_getenv conf.base_env "can_kill_ancestors" with
  [ Some "yes" ->
      match find_person_in_env conf base "" with
      [ Some p ->
          let key =
            (sou base (get_first_name p), sou base (get_surname p),
             get_occ p, get_key_index p)
          in
          let nb_ind = ref 0 in
          let nb_fam = ref 0 in
          do {
            kill_ancestors conf base False p nb_ind nb_fam;
            Util.commit_patches conf base;
            History.record conf base key "ka";
            print_killed conf base p nb_ind.val nb_fam.val;
          }
      | None -> incorrect_request conf ]
  | _ -> incorrect_request conf ]
;
