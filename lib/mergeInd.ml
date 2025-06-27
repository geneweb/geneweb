(* Copyright (c) 1998-2007 INRIA *)

open Def
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let compatible_cdates cd1 cd2 =
  cd1 = cd2 || cd2 = Date.cdate_None || cd1 = Date.cdate_None

let compatible_death_reasons dr1 dr2 = dr1 = dr2 || dr2 = Unspecified

let compatible_deaths d1 d2 =
  if d1 = d2 then true
  else
    match (d1, d2) with
    | Death (dr1, cd1), Death (dr2, cd2) ->
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
    | _, UnknownBurial -> true
    | UnknownBurial, _ -> true
    | Buried cd1, Buried cd2 -> compatible_cdates cd1 cd2
    | Cremated cd1, Cremated cd2 -> compatible_cdates cd1 cd2
    | _ -> false

let compatible_strings s1 s2 =
  Driver.Istr.equal s1 s2 || Driver.Istr.is_empty s2 || Driver.Istr.is_empty s1

let compatible_divorces d1 d2 = d1 = d2
let compatible_separations d1 d2 = d1 = d2
let compatible_relation_kinds rk1 rk2 = rk1 = rk2

let compatible_titles t1 t2 =
  Futil.eq_lists (Futil.eq_titles Driver.Istr.equal) t1 t2 || t2 = []

let compatible_pevents pevt1 pevt2 = pevt1 = [] && pevt2 = []
let compatible_fevents fevt1 fevt2 = fevt1 = [] && fevt2 = []

let compatible_strings_lists sl1 sl2 =
  sl2 = [] || Futil.eq_lists Driver.Istr.equal sl1 sl2

let compatible_notes base s1 s2 =
  compatible_strings s1 s2 || Driver.sou base s1 = Driver.sou base s2

let compatible_ind base p1 p2 =
  Driver.Istr.equal (Driver.get_first_name p1) (Driver.get_first_name p2)
  && Driver.Istr.equal (Driver.get_surname p1) (Driver.get_surname p2)
  && compatible_strings (Driver.get_image p1) (Driver.get_image p2)
  && compatible_strings (Driver.get_public_name p1) (Driver.get_public_name p2)
  && compatible_strings_lists (Driver.get_qualifiers p1)
       (Driver.get_qualifiers p2)
  && compatible_strings_lists (Driver.get_aliases p1) (Driver.get_aliases p2)
  && compatible_strings_lists
       (Driver.get_first_names_aliases p1)
       (Driver.get_first_names_aliases p2)
  && compatible_strings_lists
       (Driver.get_surnames_aliases p1)
       (Driver.get_surnames_aliases p2)
  && compatible_titles (Driver.get_titles p1) (Driver.get_titles p2)
  && compatible_pevents (Driver.get_pevents p1) (Driver.get_pevents p2)
  && Driver.get_rparents p2 = []
  && Driver.get_related p2 = []
  && compatible_strings (Driver.get_occupation p1) (Driver.get_occupation p2)
  && compatible_cdates (Driver.get_birth p1) (Driver.get_birth p2)
  && compatible_strings (Driver.get_birth_place p1) (Driver.get_birth_place p2)
  && compatible_cdates (Driver.get_baptism p1) (Driver.get_baptism p2)
  && compatible_strings
       (Driver.get_baptism_place p1)
       (Driver.get_baptism_place p2)
  && compatible_deaths (Driver.get_death p1) (Driver.get_death p2)
  && compatible_strings (Driver.get_death_place p1) (Driver.get_death_place p2)
  && compatible_burials (Driver.get_burial p1) (Driver.get_burial p2)
  && compatible_strings
       (Driver.get_burial_place p1)
       (Driver.get_burial_place p2)
  && compatible_notes base (Driver.get_notes p1) (Driver.get_notes p2)

let compatible_fam fam1 fam2 =
  compatible_cdates (Driver.get_marriage fam1) (Driver.get_marriage fam2)
  && compatible_strings
       (Driver.get_marriage_place fam1)
       (Driver.get_marriage_place fam2)
  && Array.length (Driver.get_witnesses fam2) = 0
  && compatible_fevents (Driver.get_fevents fam1) (Driver.get_fevents fam2)
  && compatible_relation_kinds (Driver.get_relation fam1)
       (Driver.get_relation fam2)
  (* TODO can we skip Driver.get_divorce and Driver.get_separation! treated as fevents *)
  && compatible_divorces (Driver.get_divorce fam1) (Driver.get_divorce fam2)
  && compatible_separations
       (Driver.get_separation fam1)
       (Driver.get_separation fam2)
  && compatible_strings (Driver.get_fsources fam1) (Driver.get_fsources fam2)

let reparent_ind base (warning : CheckItem.base_warning -> unit) ip1 ip2 =
  let a1 = Driver.poi base ip1 in
  let a2 = Driver.poi base ip2 in
  match (Driver.get_parents a1, Driver.get_parents a2) with
  | None, Some ifam ->
      let des = Driver.gen_descend_of_family (Driver.foi base ifam) in
      let rec replace i =
        if des.children.(i) = ip2 then des.children.(i) <- ip1
        else replace (i + 1)
      in
      replace 0;
      let a1 = { parents = Some ifam; consang = Adef.fix (-1) } in
      Driver.patch_ascend base ip1 a1;
      Driver.patch_descend base ifam des
  | Some ifam, None -> (
      let fam = Driver.foi base ifam in
      let children = Driver.get_children fam in
      match CheckItem.sort_children base children with
      | Some (b, a) ->
          let des = Driver.gen_descend_of_family fam in
          Driver.patch_descend base ifam des;
          warning (ChangedOrderOfChildren (ifam, fam, b, a))
      | None -> ())
  | _ -> ()

let effective_merge_ind conf base (warning : CheckItem.base_warning -> unit) p1
    p2 =
  let u2 = Driver.poi base (Driver.get_iper p2) in
  if Array.length (Driver.get_family u2) <> 0 then (
    for i = 0 to Array.length (Driver.get_family u2) - 1 do
      let ifam = (Driver.get_family u2).(i) in
      let cpl = Driver.foi base ifam in
      let cpl =
        if Driver.get_iper p2 = Driver.get_father cpl then
          Gutil.couple false (Driver.get_iper p1) (Driver.get_mother cpl)
        else if Driver.get_iper p2 = Driver.get_mother cpl then
          Gutil.couple false (Driver.get_father cpl) (Driver.get_iper p1)
        else assert false
      in
      Driver.patch_couple base ifam cpl
    done;
    let u1 =
      { family = Array.append (Driver.get_family p1) (Driver.get_family u2) }
    in
    Driver.patch_union base (Driver.get_iper p1) u1;
    let u2 = { family = [||] } in
    Driver.patch_union base (Driver.get_iper p2) u2);
  let p1 =
    let get_string fn = if Driver.Istr.is_empty (fn p1) then fn p2 else fn p1 in
    {
      (Driver.gen_person_of_person p1) with
      sex =
        (if Driver.get_sex p2 <> Neuter then Driver.get_sex p2
         else Driver.get_sex p1);
      birth =
        (if Driver.get_birth p1 = Date.cdate_None then Driver.get_birth p2
         else Driver.get_birth p1);
      birth_place = get_string Driver.get_birth_place;
      birth_src = get_string Driver.get_birth_src;
      baptism =
        (if Driver.get_baptism p1 = Date.cdate_None then Driver.get_baptism p2
         else Driver.get_baptism p1);
      baptism_place = get_string Driver.get_baptism_place;
      baptism_src = get_string Driver.get_baptism_src;
      death =
        (if Driver.get_death p1 = DontKnowIfDead then Driver.get_death p2
         else Driver.get_death p1);
      death_place = get_string Driver.get_death_place;
      death_src = get_string Driver.get_death_src;
      burial =
        (if Driver.get_burial p1 = UnknownBurial then Driver.get_burial p2
         else Driver.get_burial p1);
      burial_place = get_string Driver.get_burial_place;
      burial_src = get_string Driver.get_burial_src;
      occupation = get_string Driver.get_occupation;
      notes = get_string Driver.get_notes;
    }
  in
  Driver.patch_person base p1.key_index p1;
  reparent_ind base warning p1.key_index (Driver.get_iper p2);
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
        | evt :: l -> loop l (evt.epers_note :: evt.epers_src :: accu)
      in
      loop p1.pevents sl
    in
    String.concat " " (List.map (Driver.sou base) sl)
  in
  Notes.update_notes_links_db base (Def.NLDB.PgInd p1.key_index) s;
  let key = Util.make_key base p1 in
  let pgl =
    let db = Driver.read_nldb base in
    let db = Notes.merge_possible_aliases conf db in
    Notes.links_to_cache_entries conf base db key
  in
  Notes.update_cache_linked_pages conf Notes.Merge key key (List.length pgl)

exception Error_loop of Driver.person
exception Same_person
exception Different_sexes of Driver.person * Driver.person

module IperSet = Driver.Iper.Set

let is_ancestor base p1 p2 =
  let ip1 = Driver.get_iper p1 in
  let ip2 = Driver.get_iper p2 in
  if ip1 = ip2 then raise Same_person
  else
    let rec loop set = function
      | [] -> false
      | ip :: tl -> (
          if IperSet.mem ip set then loop set tl
          else if ip = ip1 then true
          else
            let set = IperSet.add ip set in
            match Driver.get_parents (Driver.poi base ip) with
            | Some ifam ->
                let cpl = Driver.foi base ifam in
                loop set (Driver.get_father cpl :: Driver.get_mother cpl :: tl)
            | None -> loop set tl)
    in
    loop IperSet.empty [ ip2 ]

let check_ind base p1 p2 =
  if
    Driver.get_sex p1 <> Driver.get_sex p2
    && Driver.get_sex p1 <> Neuter
    && Driver.get_sex p2 <> Neuter
  then raise @@ Different_sexes (p1, p2)
  else if is_ancestor base p1 p2 then raise (Error_loop p2)
  else if is_ancestor base p2 p1 then raise (Error_loop p1)
  else compatible_ind base p1 p2

let merge_ind conf base warning branches p1 p2 changes_done propose_merge_ind =
  if check_ind base p1 p2 then (
    effective_merge_ind conf base warning p1 p2;
    (true, true))
  else (
    propose_merge_ind conf base branches p1 p2;
    (false, changes_done))

let effective_merge_fam conf base ifam1 fam1 fam2 =
  let des1 = fam1 in
  let des2 = fam2 in
  let fam1 =
    {
      (Driver.gen_family_of_family fam1) with
      marriage =
        (if Driver.get_marriage fam1 = Date.cdate_None then
           Driver.get_marriage fam2
         else Driver.get_marriage fam1);
      marriage_place =
        (if Driver.Istr.is_empty (Driver.get_marriage_place fam1) then
           Driver.get_marriage_place fam2
         else Driver.get_marriage_place fam1);
      marriage_src =
        (if Driver.Istr.is_empty (Driver.get_marriage_src fam1) then
           Driver.get_marriage_src fam2
         else Driver.get_marriage_src fam1);
      fsources =
        (if Driver.Istr.is_empty (Driver.get_fsources fam1) then
           Driver.get_fsources fam2
         else Driver.get_fsources fam1);
    }
  in
  let des1 =
    let children =
      Array.append (Driver.get_children des1) (Driver.get_children des2)
    in
    let _ = (CheckItem.sort_children base children : _ option) in
    { children }
  in
  UpdateFamOk.effective_del conf base Driver.Iper.dummy fam2;
  for i = 0 to Array.length (Driver.get_children des2) - 1 do
    let ip = (Driver.get_children des2).(i) in
    let a = { parents = Some ifam1; consang = Adef.fix (-1) } in
    Driver.patch_ascend base ip a
  done;
  Driver.patch_family base ifam1 fam1;
  Driver.patch_descend base ifam1 des1

let merge_fam conf base branches ifam1 ifam2 fam1 fam2 ip1 ip2 changes_done
    propose_merge_fam =
  let p1 = Driver.poi base ip1 in
  let p2 = Driver.poi base ip2 in
  if compatible_fam fam1 fam2 then (
    effective_merge_fam conf base ifam1 fam1 fam2;
    (true, true))
  else (
    propose_merge_fam conf base branches (ifam1, fam1) (ifam2, fam2) p1 p2;
    (false, changes_done))

let rec try_merge conf base warning branches ip1 ip2 changes_done
    propose_merge_ind propose_merge_fam =
  let p1 = Driver.poi base ip1 in
  let p2 = Driver.poi base ip2 in
  let ok_so_far = true in
  let ok_so_far, changes_done =
    match (Driver.get_parents p1, Driver.get_parents p2) with
    | Some ifam1, Some ifam2 when ifam1 <> ifam2 ->
        let branches = (ip1, ip2) :: branches in
        let fam1 = Driver.foi base ifam1 in
        let fam2 = Driver.foi base ifam2 in
        let f1 = Driver.get_father fam1 in
        let f2 = Driver.get_father fam2 in
        let m1 = Driver.get_mother fam1 in
        let m2 = Driver.get_mother fam2 in
        let ok_so_far, changes_done =
          if ok_so_far then
            if f1 = f2 then (true, changes_done)
            else
              try_merge conf base ignore branches f1 f2 changes_done
                propose_merge_ind propose_merge_fam
          else (false, changes_done)
        in
        let ok_so_far, changes_done =
          if ok_so_far then
            if m1 = m2 then (true, changes_done)
            else
              try_merge conf base ignore branches m1 m2 changes_done
                propose_merge_ind propose_merge_fam
          else (false, changes_done)
        in
        let ok_so_far, changes_done =
          if ok_so_far then
            merge_fam conf base branches ifam1 ifam2 fam1 fam2 f1 m1
              changes_done propose_merge_fam
          else (false, changes_done)
        in
        (ok_so_far, changes_done)
    | _ -> (ok_so_far, changes_done)
  in
  if ok_so_far then
    merge_ind conf base warning branches p1 p2 changes_done propose_merge_ind
  else (false, changes_done)

let merge conf base p1 p2 propose_merge_ind propose_merge_fam =
  let rev_wl = ref [] in
  let warning w = rev_wl := w :: !rev_wl in
  let ok, changes_done =
    try_merge conf base warning [] (Driver.get_iper p1) (Driver.get_iper p2)
      false propose_merge_ind propose_merge_fam
  in
  if changes_done then Util.commit_patches conf base;
  (if ok then
     let changed =
       let p1 = Util.string_gen_person base (Driver.gen_person_of_person p1) in
       let p2 = Util.string_gen_person base (Driver.gen_person_of_person p2) in
       U_Merge_person (p2, p1, p1)
     in
     History.record conf base changed "fp");
  Update.delete_topological_sort conf base;
  (ok, List.rev !rev_wl)

(* Undocumented feature... Kill someone's ancestors *)

let rec kill_ancestors conf base included_self p nb_ind nb_fam =
  (match Driver.get_parents p with
  | Some ifam ->
      let cpl = Driver.foi base ifam in
      kill_ancestors conf base true
        (Driver.poi base (Driver.get_father cpl))
        nb_ind nb_fam;
      kill_ancestors conf base true
        (Driver.poi base (Driver.get_mother cpl))
        nb_ind nb_fam;
      UpdateFamOk.effective_del conf base Driver.Iper.dummy cpl;
      incr nb_fam
  | None -> ());
  if included_self then (
    UpdateIndOk.effective_del conf base p;
    incr nb_ind)
