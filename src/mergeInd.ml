(* camlp5r ./pa_html.cmo ./pa_lock.cmo *)
(* $Id: mergeInd.ml,v 5.55 2008-01-21 14:02:36 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
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
        match (p_getenv conf.env "m", p_getint conf.env "ip") with
        [ (Some "MRG_DUP_IND_Y_N", Some ip) -> do {
            xtag "input" "type=\"hidden\" name=\"ip\" value=\"%d\"" ip;
            List.iter
              (fun excl_name ->
                 match p_getenv conf.env excl_name with
                 [ Some "" | None -> ()
                 | Some s ->
                     xtag "input" "type=\"hidden\" name=\"%s\" value=\"%s\""
                       excl_name s ])
              ["iexcl"; "fexcl"]
          }
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
    string_field True (transl_nth conf "occupation/occupations" 0) "occupation"
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
         | DontKnowIfDead | OfCourseDead -> "" ]);
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
  eq_istr s1 s2 || is_empty_string s2 || is_empty_string s1;

value compatible_divorces d1 d2 = d1 = d2;

value compatible_relation_kinds rk1 rk2 = rk1 = rk2;

value compatible_accesses a1 a2 = (*a1 = a2*)True;

value compatible_titles t1 t2 =
  Futil.eq_lists (Futil.eq_titles eq_istr) t1 t2 || t2 = [];

value compatible_strings_lists sl1 sl2 =
  sl2 = [] || Futil.eq_lists eq_istr sl1 sl2;

value compatible_notes base s1 s2 =
  compatible_strings s1 s2 || sou base s1 = sou base s2;

value compatible_ind base p1 p2 =
  eq_istr (get_first_name p1) (get_first_name p2) &&
  eq_istr (get_surname p1) (get_surname p2) &&
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
  compatible_notes base (get_notes p1) (get_notes p2)
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
      xtag "hr";
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

value reparent_ind base warning ip1 ip2 =
  let a1 = poi base ip1 in
  let a2 = poi base ip2 in
  match (get_parents a1, get_parents a2) with
  [ (None, Some ifam) -> do {
      let des = gen_descend_of_descend (foi base ifam) in
      let rec replace i =
        if des.children.(i) = ip2 then des.children.(i) := ip1
        else replace (i + 1)
      in
      replace 0;
      let a1 = {parents = Some ifam; consang = Adef.fix (-1)} in
      patch_ascend base ip1 a1;
      patch_descend base ifam des;
    }
  | (Some ifam, None) -> do {
      let fam = foi base ifam in
      let children = get_children fam in
      match CheckItem.sort_children base children with
      [ Some (b, a) -> do {
          let des = gen_descend_of_descend fam in
          patch_descend base ifam des;
          warning (ChangedOrderOfChildren ifam fam b a);
        }
      | None -> () ]
    }
  | _ -> () ]
;

value effective_merge_ind conf base warning p1 p2 =
  do {
    let u2 = poi base (get_key_index p2) in
    if Array.length (get_family u2) <> 0 then do {
      for i = 0 to Array.length (get_family u2) - 1 do {
        let ifam = (get_family u2).(i) in
        let cpl = foi base ifam in
        let cpl =
          if get_key_index p2 = get_father cpl then
            couple False (get_key_index p1) (get_mother cpl)
          else if get_key_index p2 = get_mother cpl then
            couple False (get_father cpl) (get_key_index p1)
          else assert False
        in
        patch_couple base ifam cpl;
      };
      let u1 = {family = Array.append (get_family p1) (get_family u2)} in
      patch_union base (get_key_index p1) u1;
      let u2 = {family = [| |]} in
      patch_union base (get_key_index p2) u2;
    }
    else ();
    let p1 =
      {(gen_person_of_person p1) with
       sex = if get_sex p2 <> Neuter then get_sex p2 else get_sex p1;
       birth =
         if get_birth p1 = Adef.codate_None then get_birth p2
         else get_birth p1;
       birth_place =
         if is_empty_string (get_birth_place p1) then get_birth_place p2
         else get_birth_place p1;
       birth_src =
         if is_empty_string (get_birth_src p1) then get_birth_src p2
         else get_birth_src p1;
       baptism =
         if get_baptism p1 = Adef.codate_None then get_baptism p2
         else get_baptism p1;
       baptism_place =
         if is_empty_string (get_baptism_place p1) then get_baptism_place p2
         else get_baptism_place p1;
       baptism_src =
         if is_empty_string (get_baptism_src p1) then get_baptism_src p2
         else get_baptism_src p1;
       death =
         if get_death p1 = DontKnowIfDead then get_death p2
         else get_death p1;
       death_place =
         if is_empty_string (get_death_place p1) then get_death_place p2
         else get_death_place p1;
       death_src =
         if is_empty_string (get_death_src p1) then get_death_src p2
         else get_death_src p1;
       burial =
         if get_burial p1 = UnknownBurial then get_burial p2
         else get_burial p1;
       burial_place =
         if is_empty_string (get_burial_place p1) then get_burial_place p2
         else get_burial_place p1;
       burial_src =
         if is_empty_string (get_burial_src p1) then get_burial_src p2
         else get_burial_src p1;
       occupation =
         if is_empty_string (get_occupation p1) then get_occupation p2
         else get_occupation p1;
       notes =
         if is_empty_string (get_notes p1) then get_notes p2
         else get_notes p1}
    in
    patch_person base p1.key_index p1;
    reparent_ind base warning p1.key_index (get_key_index p2);
    delete_key base (sou base (get_first_name p2)) (sou base (get_surname p2))
      (get_occ p2);
    let p2 = UpdateIndOk.effective_del conf base warning p2 in
    patch_person base p2.key_index p2;
    let s =
      let sl =
        [p1.notes; p1.occupation; p1.birth_src; p1.baptism_src; p1.death_src;
         p1.burial_src; p1.psources]
      in
      String.concat " " (List.map (sou base) sl)
    in
    Notes.update_notes_links_db conf (NotesLinks.PgInd p1.key_index) s;
  }
;

value is_ancestor base ip1 ip2 =
  let visited = Array.create (nb_of_persons base) False in
  let rec loop ip =
    if visited.(Adef.int_of_iper ip) then False
    else if ip = ip1 then True
    else do {
      visited.(Adef.int_of_iper ip) := True;
      match get_parents (poi base ip) with
      [ Some ifam ->
          let cpl = foi base ifam in
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

value merge_ind conf base warning branches ip1 ip2 changes_done =
  let p1 = poi base ip1 in
  let p2 = poi base ip2 in
  if is_ancestor base ip1 ip2 then do {
    error_loop conf base p2;
    (False, False)
  }
  else if is_ancestor base ip2 ip1 then do {
    error_loop conf base p1;
    (False, False)
  }
  else if compatible_ind base p1 p2 then do {
    effective_merge_ind conf base warning p1 p2;
    (True, True)
  }
  else do {
    propose_merge_ind conf base branches p1 p2;
    (False, changes_done)
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

value effective_merge_fam conf base (ifam1, fam1) (ifam2, fam2) p1 p2 = do {
  let des1 = fam1 in
  let des2 = fam2 in
  let fam1 =
    {(gen_family_of_family fam1) with
     marriage =
       if get_marriage fam1 = Adef.codate_None then get_marriage fam2
       else get_marriage fam1;
     marriage_place =
       if is_empty_string (get_marriage_place fam1) then
         get_marriage_place fam2
       else get_marriage_place fam1;
     marriage_src =
       if is_empty_string (get_marriage_src fam1) then
         get_marriage_src fam2
       else get_marriage_src fam1;
     fsources =
       if is_empty_string (get_fsources fam1) then get_fsources fam2
       else get_fsources fam1}
  in
  let des1 =
    let children = Array.append (get_children des1) (get_children des2) in
    let _ : option _ = CheckItem.sort_children base children in
    {children = children}
  in
  UpdateFamOk.effective_del conf base (ifam2, fam2);
  for i = 0 to Array.length (get_children des2) - 1 do {
    let ip = (get_children des2).(i) in
    let a = {parents = Some ifam1; consang = Adef.fix (-1)} in
    patch_ascend base ip a;
  };
  patch_family base ifam1 fam1;
  patch_descend base ifam1 des1;
};

value merge_fam conf base branches ifam1 ifam2 ip1 ip2 changes_done =
  let p1 = poi base ip1 in
  let p2 = poi base ip2 in
  let fam1 = foi base ifam1 in
  let fam2 = foi base ifam2 in
  if compatible_fam base fam1 fam2 then do {
    effective_merge_fam conf base (ifam1, fam1) (ifam2, fam2) p1 p2;
    (True, True)
  }
  else do {
    propose_merge_fam conf base branches (ifam1, fam1) (ifam2, fam2) p1 p2;
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

value rec try_merge conf base warning branches ip1 ip2 changes_done =
  let a1 = poi base ip1 in
  let a2 = poi base ip2 in
  let ok_so_far = True in
  let (ok_so_far, changes_done) =
    match (get_parents a1, get_parents a2) with
    [ (Some ifam1, Some ifam2) when ifam1 <> ifam2 ->
        let branches = [(ip1, ip2) :: branches] in
        let cpl1 = foi base ifam1 in
        let cpl2 = foi base ifam2 in
        let (ok_so_far, changes_done) =
          if ok_so_far then
            if get_father cpl1 = get_father cpl2 then (True, changes_done)
            else
              let warning _ = () in
              try_merge conf base warning branches (get_father cpl1)
                (get_father cpl2) changes_done
          else (False, changes_done)
        in
        let (ok_so_far, changes_done) =
          if ok_so_far then
            if get_mother cpl1 = get_mother cpl2 then (True, changes_done)
            else
              let warning _ = () in
              try_merge conf base warning branches (get_mother cpl1)
                (get_mother cpl2) changes_done
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
  if ok_so_far then merge_ind conf base warning branches ip1 ip2 changes_done
  else (False, changes_done)
;

value print_merged conf base wl p = do {
  let title _ = Wserver.wprint "%s" (capitale (transl conf "merge done")) in
  Wserver.wrap_string.val := Util.xml_pretty_print;
  header conf title;
  print_link_to_welcome conf True;
  tag "ul" begin
    tag "li" begin
      Wserver.wprint "%s\n" (referenced_person_text conf base p);
    end;
  end;
  match (p_getenv conf.env "m", p_getint conf.env "ip") with
  [ (Some "MRG_DUP_IND_Y_N", Some ip) ->
      let s1 =
        match p_getenv conf.env "iexcl" with
        [ Some "" | None -> ""
        | Some s -> ";iexcl=" ^ s ]
      in
      let s2 =
        match p_getenv conf.env "fexcl" with
        [ Some "" | None -> ""
        | Some s -> ";fexcl=" ^ s ]
      in
      tag "p" begin
        stag "a" "href=%sm=MRG_DUP;ip=%d%s%s" (commd conf) ip s1 s2 begin
          Wserver.wprint "%s" (capitale (transl conf "continue merging"));
        end;
        Wserver.wprint "\n(%s)\n"
          (Util.transl_a_of_b conf (transl conf "possible duplications")
             (referenced_person_text conf base
                (poi base (Adef.iper_of_int ip))));
      end
  | _ -> () ];
  Update.print_warnings conf base wl;
  trailer conf;
};

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
        let rev_wl = ref [] in
        let warning w = rev_wl.val := [w :: rev_wl.val] in
        let (ok, changes_done) =
          try_merge conf base warning [] (get_key_index p1) (get_key_index p2)
            False
        in
        do {
          if changes_done then Util.commit_patches conf base else ();
          if ok then do {
            let changed =
              let p1 = Util.string_gen_person base (gen_person_of_person p1) in
              let p2 = Util.string_gen_person base (gen_person_of_person p2) in
              U_Merge_person p2 p1 p1
            in
            History.record conf base changed "fp";
            Update.delete_topological_sort conf base;
            print_merged conf base (List.rev rev_wl.val) p1;
          }
          else ();
        }
  | _ -> not_found_or_incorrect conf ]
;

(* Undocumented feature... Kill someone's ancestors *)

value rec kill_ancestors conf base included_self p nb_ind nb_fam =
  do {
    match get_parents p with
    [ Some ifam ->
        let cpl = foi base ifam in
        do {
          kill_ancestors conf base True (poi base (get_father cpl)) nb_ind
            nb_fam;
          kill_ancestors conf base True (poi base (get_mother cpl)) nb_ind
            nb_fam;
          UpdateFamOk.effective_del conf base (ifam, foi base ifam);
          incr nb_fam;
        }
    | None -> () ];
    if included_self then do {
      let ip = get_key_index p in
      let warning _ = () in
      let p = UpdateIndOk.effective_del conf base warning p in
      patch_person base ip p;
      incr nb_ind;
    }
    else ();
  }
;

value print_killed conf base p nb_ind nb_fam =
  let title _ = Wserver.wprint "Ancestors killed" in
  do {
    Hutil.header conf title;
    Wserver.wprint "%s's ancestors killed.<br>\n"
      (referenced_person_title_text conf base p);
    Wserver.wprint "%d persons and %d families deleted<p>\n" nb_ind nb_fam;
    Hutil.trailer conf;
  }
;

value print_kill_ancestors conf base =
  match p_getenv conf.base_env "can_kill_ancestors" with
  [ Some "yes" ->
      match find_person_in_env conf base "" with
      [ Some p ->
          let nb_ind = ref 0 in
          let nb_fam = ref 0 in
          do {
            kill_ancestors conf base False p nb_ind nb_fam;
            Util.commit_patches conf base;
            let changed = 
              U_Kill_ancestors 
                (Util.string_gen_person base (gen_person_of_person p))
            in
            History.record conf base changed "ka";
            print_killed conf base p nb_ind.val nb_fam.val;
          }
      | None -> incorrect_request conf ]
  | _ -> incorrect_request conf ]
;
