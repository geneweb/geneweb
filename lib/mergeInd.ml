(* Copyright (c) 1998-2007 INRIA *)

type merge_ind_job = { p1 : Gwdb.person; p2 : Gwdb.person }
type merge_fam_job = { f1 : Gwdb.family; f2 : Gwdb.family }
type merge_job = IndJob of merge_ind_job | FamJob of merge_fam_job
type stuck_job = merge_job
type merge_jobs = merge_job Stack.t
type remaining_merge_jobs = merge_jobs

type merge_result =
  | Stuck of stuck_job * remaining_merge_jobs * Warning.base_warning list
  | Finished of Warning.base_warning list

type job_progression = {
  stuck_job : merge_job option;
  jobs : merge_jobs;
  changes : bool;
  warnings : Warning.base_warning list;
}

let empty_job_progression ?(jobs = Stack.create ()) () =
  { stuck_job = None; changes = false; jobs; warnings = [] }

let add_warning w job_progression =
  { job_progression with warnings = w :: job_progression.warnings }

let compatible_cdates cd1 cd2 =
  cd1 = cd2 || cd2 = Date.cdate_None || cd1 = Date.cdate_None

let compatible_death_reasons dr1 dr2 = dr1 = dr2 || dr2 = Def.Unspecified

let compatible_deaths d1 d2 =
  if d1 = d2 then true
  else
    match (d1, d2) with
    | Def.Death (dr1, cd1), Death (dr2, cd2) ->
        compatible_death_reasons dr1 dr2 && compatible_cdates cd1 cd2
    | Death (_, _), NotDead -> false
    | Death (_, _), _ -> true
    | _, DontKnowIfDead -> true
    | DontKnowIfDead, _ -> true
    | _ -> false

let compatible_burials b1 b2 =
  if b1 = b2 then true
  else
    match (b1, b2) with
    | _, Def.UnknownBurial -> true
    | UnknownBurial, _ -> true
    | Buried cd1, Buried cd2 -> compatible_cdates cd1 cd2
    | Cremated cd1, Cremated cd2 -> compatible_cdates cd1 cd2
    | _ -> false

let compatible_strings s1 s2 =
  Gwdb.eq_istr s1 s2 || Gwdb.is_empty_string s2 || Gwdb.is_empty_string s1

let compatible_divorces d1 d2 = d1 = d2
let compatible_relation_kinds rk1 rk2 = rk1 = rk2

let compatible_titles t1 t2 =
  List.equal (Futil.eq_titles Gwdb.eq_istr) t1 t2 || t2 = []

let compatible_pevents pevt1 pevt2 = pevt1 = [] && pevt2 = []
let compatible_fevents fevt1 fevt2 = fevt1 = [] && fevt2 = []

let compatible_strings_lists sl1 sl2 =
  sl2 = [] || List.equal Gwdb.eq_istr sl1 sl2

let compatible_notes base s1 s2 =
  compatible_strings s1 s2 || Gwdb.sou base s1 = Gwdb.sou base s2

let compatible_ind base p1 p2 =
  Gwdb.eq_istr (Gwdb.get_first_name p1) (Gwdb.get_first_name p2)
  && Gwdb.eq_istr (Gwdb.get_surname p1) (Gwdb.get_surname p2)
  && compatible_strings (Gwdb.get_image p1) (Gwdb.get_image p2)
  && compatible_strings (Gwdb.get_public_name p1) (Gwdb.get_public_name p2)
  && compatible_strings_lists (Gwdb.get_qualifiers p1) (Gwdb.get_qualifiers p2)
  && compatible_strings_lists (Gwdb.get_aliases p1) (Gwdb.get_aliases p2)
  && compatible_strings_lists
       (Gwdb.get_first_names_aliases p1)
       (Gwdb.get_first_names_aliases p2)
  && compatible_strings_lists
       (Gwdb.get_surnames_aliases p1)
       (Gwdb.get_surnames_aliases p2)
  && compatible_titles (Gwdb.get_titles p1) (Gwdb.get_titles p2)
  && compatible_pevents (Gwdb.get_pevents p1) (Gwdb.get_pevents p2)
  && Gwdb.get_rparents p2 = []
  && Gwdb.get_related p2 = []
  && compatible_strings (Gwdb.get_occupation p1) (Gwdb.get_occupation p2)
  && compatible_cdates (Gwdb.get_birth p1) (Gwdb.get_birth p2)
  && compatible_strings (Gwdb.get_birth_place p1) (Gwdb.get_birth_place p2)
  && compatible_cdates (Gwdb.get_baptism p1) (Gwdb.get_baptism p2)
  && compatible_strings (Gwdb.get_baptism_place p1) (Gwdb.get_baptism_place p2)
  && compatible_deaths (Gwdb.get_death p1) (Gwdb.get_death p2)
  && compatible_strings (Gwdb.get_death_place p1) (Gwdb.get_death_place p2)
  && compatible_burials (Gwdb.get_burial p1) (Gwdb.get_burial p2)
  && compatible_strings (Gwdb.get_burial_place p1) (Gwdb.get_burial_place p2)
  && compatible_notes base (Gwdb.get_notes p1) (Gwdb.get_notes p2)

let compatible_fam fam1 fam2 =
  compatible_cdates (Gwdb.get_marriage fam1) (Gwdb.get_marriage fam2)
  && compatible_strings
       (Gwdb.get_marriage_place fam1)
       (Gwdb.get_marriage_place fam2)
  && Array.length (Gwdb.get_witnesses fam2) = 0
  && compatible_fevents (Gwdb.get_fevents fam1) (Gwdb.get_fevents fam2)
  && compatible_relation_kinds (Gwdb.get_relation fam1) (Gwdb.get_relation fam2)
  && compatible_divorces (Gwdb.get_divorce fam1) (Gwdb.get_divorce fam2)
  && compatible_strings (Gwdb.get_fsources fam1) (Gwdb.get_fsources fam2)

let reparent_ind' base job_progression ip1 ip2 =
  let a1 = Gwdb.poi base ip1 in
  let a2 = Gwdb.poi base ip2 in
  match (Gwdb.get_parents a1, Gwdb.get_parents a2) with
  | None, Some ifam ->
      let des = Gwdb.gen_descend_of_family (Gwdb.foi base ifam) in
      let rec replace i =
        if des.children.(i) = ip2 then des.children.(i) <- ip1
        else replace (i + 1)
      in
      replace 0;
      let a1 = { Def.parents = Some ifam; consang = Adef.fix (-1) } in
      Gwdb.patch_ascend base ip1 a1;
      Gwdb.patch_descend base ifam des;
      job_progression
  | Some ifam, None -> (
      let fam = Gwdb.foi base ifam in
      let children = Gwdb.get_children fam in
      match CheckItem.sort_children base children with
      | Some (b, a) ->
          let des = Gwdb.gen_descend_of_family fam in
          Gwdb.patch_descend base ifam des;
          add_warning (ChangedOrderOfChildren (ifam, fam, b, a)) job_progression
      | None -> job_progression)
  | _ -> job_progression

let reparent_ind base ip1 ip2 =
  let jp = reparent_ind' base (empty_job_progression ()) ip1 ip2 in
  List.rev jp.warnings

let effective_merge_ind conf base job_progression p1 p2 =
  let u2 = Gwdb.poi base (Gwdb.get_iper p2) in
  if Array.length (Gwdb.get_family u2) <> 0 then (
    for i = 0 to Array.length (Gwdb.get_family u2) - 1 do
      let ifam = (Gwdb.get_family u2).(i) in
      let cpl = Gwdb.foi base ifam in
      let cpl =
        if Gwdb.get_iper p2 = Gwdb.get_father cpl then
          Adef.couple (Gwdb.get_iper p1) (Gwdb.get_mother cpl)
        else if Gwdb.get_iper p2 = Gwdb.get_mother cpl then
          Adef.couple (Gwdb.get_father cpl) (Gwdb.get_iper p1)
        else assert false
      in
      Gwdb.patch_couple base ifam cpl
    done;
    let family = Array.append (Gwdb.get_family p1) (Gwdb.get_family u2) in
    Update_util.sort_families_array_by_date base family;
    let u1 = { Def.family } in
    Gwdb.patch_union base (Gwdb.get_iper p1) u1;
    let u2 = { Def.family = [||] } in
    Gwdb.patch_union base (Gwdb.get_iper p2) u2);
  let p1 =
    let get_string fn = if Gwdb.is_empty_string (fn p1) then fn p2 else fn p1 in
    {
      (Gwdb.gen_person_of_person p1) with
      sex =
        (if Gwdb.get_sex p2 <> Neuter then Gwdb.get_sex p2 else Gwdb.get_sex p1);
      birth =
        (if Gwdb.get_birth p1 = Date.cdate_None then Gwdb.get_birth p2
        else Gwdb.get_birth p1);
      birth_place = get_string Gwdb.get_birth_place;
      birth_src = get_string Gwdb.get_birth_src;
      baptism =
        (if Gwdb.get_baptism p1 = Date.cdate_None then Gwdb.get_baptism p2
        else Gwdb.get_baptism p1);
      baptism_place = get_string Gwdb.get_baptism_place;
      baptism_src = get_string Gwdb.get_baptism_src;
      death =
        (if Gwdb.get_death p1 = DontKnowIfDead then Gwdb.get_death p2
        else Gwdb.get_death p1);
      death_place = get_string Gwdb.get_death_place;
      death_src = get_string Gwdb.get_death_src;
      burial =
        (if Gwdb.get_burial p1 = UnknownBurial then Gwdb.get_burial p2
        else Gwdb.get_burial p1);
      burial_place = get_string Gwdb.get_burial_place;
      burial_src = get_string Gwdb.get_burial_src;
      occupation = get_string Gwdb.get_occupation;
      notes = get_string Gwdb.get_notes;
    }
  in
  Gwdb.patch_person base p1.key_index p1;
  let job_progression =
    reparent_ind' base job_progression p1.key_index (Gwdb.get_iper p2)
  in
  UpdateIndOk.effective_del conf base p2;
  let s =
    let sl =
      [
        p1.notes;
        p1.occupation;
        p1.birth_note;
        p1.birth_src;
        p1.baptism_note;
        p1.baptism_src;
        p1.death_note;
        p1.death_src;
        p1.burial_note;
        p1.burial_src;
        p1.psources;
      ]
    in
    let sl =
      let rec loop l accu =
        match l with
        | [] -> accu
        | evt :: l -> loop l (evt.Def.epers_note :: evt.epers_src :: accu)
      in
      loop p1.pevents sl
    in
    String.concat " " (List.map (Gwdb.sou base) sl)
  in
  Notes.update_notes_links_db base (Def.NLDB.PgInd p1.key_index) s;
  job_progression

exception Error_loop of Gwdb.person
exception Different_sexes of Gwdb.person * Gwdb.person

let check_ind base p1 p2 =
  if
    Gwdb.get_sex p1 <> Gwdb.get_sex p2
    && Gwdb.get_sex p1 <> Neuter
    && Gwdb.get_sex p2 <> Neuter
  then raise @@ Different_sexes (p1, p2)
  else if Person.is_ancestor base p1 p2 then raise (Error_loop p2)
  else if Person.is_ancestor base p2 p1 then raise (Error_loop p1)
  else compatible_ind base p1 p2

let effective_merge_fam conf base ifam1 fam1 fam2 =
  let des1 = fam1 in
  let des2 = fam2 in
  let fam1 =
    {
      (Gwdb.gen_family_of_family fam1) with
      marriage =
        (if Gwdb.get_marriage fam1 = Date.cdate_None then Gwdb.get_marriage fam2
        else Gwdb.get_marriage fam1);
      marriage_place =
        (if Gwdb.is_empty_string (Gwdb.get_marriage_place fam1) then
         Gwdb.get_marriage_place fam2
        else Gwdb.get_marriage_place fam1);
      marriage_src =
        (if Gwdb.is_empty_string (Gwdb.get_marriage_src fam1) then
         Gwdb.get_marriage_src fam2
        else Gwdb.get_marriage_src fam1);
      fsources =
        (if Gwdb.is_empty_string (Gwdb.get_fsources fam1) then
         Gwdb.get_fsources fam2
        else Gwdb.get_fsources fam1);
    }
  in
  let des1 =
    let children =
      Array.append (Gwdb.get_children des1) (Gwdb.get_children des2)
    in
    let _ = (CheckItem.sort_children base children : _ option) in
    { Def.children }
  in
  UpdateFamOk.effective_del conf base Gwdb.dummy_iper fam2;
  for i = 0 to Array.length (Gwdb.get_children des2) - 1 do
    let ip = (Gwdb.get_children des2).(i) in
    let a = { Def.parents = Some ifam1; consang = Adef.fix (-1) } in
    Gwdb.patch_ascend base ip a
  done;
  Gwdb.patch_family base ifam1 fam1;
  Gwdb.patch_descend base ifam1 des1

let push_fam_job base jobs ifam1 ifam2 =
  let f1 = Gwdb.foi base ifam1 in
  let f2 = Gwdb.foi base ifam2 in
  Stack.push (FamJob { f1; f2 }) jobs;
  let mother1 = Gwdb.poi base @@ Gwdb.get_mother f1 in
  let mother2 = Gwdb.poi base @@ Gwdb.get_mother f2 in
  Stack.push (IndJob { p1 = mother1; p2 = mother2 }) jobs;
  let father1 = Gwdb.poi base @@ Gwdb.get_father f1 in
  let father2 = Gwdb.poi base @@ Gwdb.get_father f2 in
  Stack.push (IndJob { p1 = father1; p2 = father2 }) jobs

let try_merge_fam conf base job_progression f1 f2 =
  if compatible_fam f1 f2 then (
    effective_merge_fam conf base (Gwdb.get_ifam f1) f1 f2;
    { job_progression with changes = true })
  else { job_progression with stuck_job = Some (FamJob { f1; f2 }) }

let try_merge conf base job_progression p1 p2 =
  match (Gwdb.get_parents p1, Gwdb.get_parents p2) with
  | Some ifam1, Some ifam2 when not (Gwdb.eq_ifam ifam1 ifam2) ->
      Stack.push (IndJob { p1; p2 }) job_progression.jobs;
      push_fam_job base job_progression.jobs ifam1 ifam2;
      job_progression
  | _ when check_ind base p1 p2 ->
      let job_progression =
        effective_merge_ind conf base job_progression p1 p2
      in
      { job_progression with changes = true }
  | _ -> { job_progression with stuck_job = Some (IndJob { p1; p2 }) }

let same_person p1 p2 = Gwdb.eq_iper (Gwdb.get_iper p1) (Gwdb.get_iper p2)
let same_family f1 f2 = Gwdb.eq_ifam (Gwdb.get_ifam f1) (Gwdb.get_ifam f2)

let rec perform_jobs conf base job_progression =
  match Stack.pop_opt job_progression.jobs with
  | Some (IndJob { p1; p2 }) when not (same_person p1 p2) ->
      let job_progression = try_merge conf base job_progression p1 p2 in
      if Option.is_some job_progression.stuck_job then job_progression
      else perform_jobs conf base job_progression
  | Some (FamJob { f1; f2 }) when not (same_family f1 f2) ->
      let job_progression = try_merge_fam conf base job_progression f1 f2 in
      perform_jobs conf base job_progression
  | Some (FamJob _) | Some (IndJob _) -> perform_jobs conf base job_progression
  | None -> job_progression

let perform_merge_job conf base p1 p2 =
  let jobs = Stack.create () in
  Stack.push (IndJob { p1; p2 }) jobs;
  let job_progression = empty_job_progression ~jobs () in
  perform_jobs conf base job_progression

let merge_result_of_job_progression job_progression =
  match job_progression with
  | { stuck_job = Some job; jobs; warnings; _ } ->
      Stuck (job, jobs, List.rev warnings)
  | { stuck_job = None; warnings; _ } -> Finished (List.rev warnings)

let merge conf base p1 p2 =
  let job_progression = perform_merge_job conf base p1 p2 in
  if job_progression.changes then Util.commit_patches conf base;
  (if Option.is_none job_progression.stuck_job then
   let changed =
     let p1 = Gwdb.gen_person_of_person p1 in
     let p2 = Gwdb.gen_person_of_person p2 in
     Def.U_Merge_person (p2, p1, p1)
   in
   History.record conf base changed "fp");
  Update.delete_topological_sort conf base;
  merge_result_of_job_progression job_progression

(* Undocumented feature... Kill someone's ancestors *)

let rec kill_ancestors conf base included_self p nb_ind nb_fam =
  (match Gwdb.get_parents p with
  | Some ifam ->
      let cpl = Gwdb.foi base ifam in
      kill_ancestors conf base true
        (Gwdb.poi base (Gwdb.get_father cpl))
        nb_ind nb_fam;
      kill_ancestors conf base true
        (Gwdb.poi base (Gwdb.get_mother cpl))
        nb_ind nb_fam;
      UpdateFamOk.effective_del conf base Gwdb.dummy_iper cpl;
      incr nb_fam
  | None -> ());
  if included_self then (
    UpdateIndOk.effective_del conf base p;
    incr nb_ind)

let person_pairs_of_jobs jobs =
  List.rev
  @@ Stack.fold
       (fun acc -> function
         | IndJob { p1; p2 } -> (Gwdb.get_iper p1, Gwdb.get_iper p2) :: acc
         | FamJob _ -> acc)
       [] jobs
