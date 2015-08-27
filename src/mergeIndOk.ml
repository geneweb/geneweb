(* camlp5r ./pa_html.cmo *)
(* $Id: mergeIndOk.ml,v 5.44 2008-01-21 14:02:36 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Futil;
open Gutil;
open Gwdb;
open Hutil;
open Mutil;
open Util;

value rec merge_lists l1 =
  fun
  [ [x2 :: l2] ->
      if List.mem x2 l1 then merge_lists l1 l2 else merge_lists (l1 @ [x2]) l2
  | [] -> l1 ]
;

value cat_strings base is1 sep is2 =
  let n1 = sou base is1 in
  let n2 = sou base is2 in
  if n1 = "" then n2 else if n2 = "" then n1 else n1 ^ sep ^ n2
;

value merge_strings base is1 sep is2 =
  let n1 = sou base is1 in
  let n2 = sou base is2 in
  if n1 = n2 then n1
  else if n1 = "" then n2
  else if n2 = "" then n1
  else n1 ^ sep ^ n2
;

value sorp base ip =
  let p = poi base ip in
  (sou base (get_first_name p), sou base (get_surname p), get_occ p,
   Update.Link, "")
;

value merge_event_witnesses base wit1 wit2 =
  let list =
    List.fold_right
      (fun wit list -> if List.mem wit list then list else [wit :: list])
      (Array.to_list wit1)
      (Array.to_list wit2)
  in
  Array.of_list list
;

value merge_events base l1 l2 p =
  let list_mem e l =
    let found_birth = ref False in
    let found_baptism = ref False in
    let found_death = ref False in
    let found_burial = ref False in
    match e.epers_name with
    [ Epers_Birth | Epers_Baptism | Epers_Death |
      Epers_Burial | Epers_Cremation ->
        List.fold_right
          (fun e1 (mem, l1) ->
            match e1.epers_name with
            [ Epers_Birth ->
                if found_birth.val then (mem, [e1 :: l1])
                else
                  if e.epers_name = e1.epers_name then
                    let witnesses =
                      merge_event_witnesses base e1.epers_witnesses e.epers_witnesses
                    in
                    let e1 =
                      {(e1) with epers_date = p.birth;
                        epers_place = p.birth_place;
                        epers_note = p.birth_note; epers_src = p.birth_src;
                        epers_witnesses = witnesses}
                    in
                    let _ = found_birth.val := True in
                    (True, [e1 :: l1])
                  else
                    (mem, [e1 :: l1])
            | Epers_Baptism ->
                if found_baptism.val then (mem, [e1 :: l1])
                else
                  if e.epers_name = e1.epers_name then
                    let witnesses =
                      merge_event_witnesses base e1.epers_witnesses e.epers_witnesses
                    in
                    let e1 =
                      {(e1) with epers_date = p.baptism;
                        epers_place = p.baptism_place;
                        epers_note = p.baptism_note; epers_src = p.baptism_src;
                        epers_witnesses = witnesses}
                    in
                    let _ = found_baptism.val := True in
                    (True, [e1 :: l1])
                  else
                    (mem, [e1 :: l1])
            | Epers_Death ->
                if found_death.val then (mem, [e1 :: l1])
                else
                  if e.epers_name = e1.epers_name then
                    let (is_dead, date) =
                      match p.death with
                      [ NotDead | DontKnowIfDead -> (False, Adef.codate_None)
                      | Death _ cd ->
                          (True, Adef.codate_of_od (Some (Adef.date_of_cdate cd)))
                      | DeadYoung | DeadDontKnowWhen | OfCourseDead ->
                          (True, Adef.codate_None) ]
                    in
                    let witnesses =
                      merge_event_witnesses base e1.epers_witnesses e.epers_witnesses
                    in
                    let e1 =
                      {(e1) with epers_date = date;
                        epers_place = p.death_place;
                        epers_note = p.death_note; epers_src = p.death_src;
                        epers_witnesses = witnesses}
                    in
                    let _ = found_death.val := True in
                    if is_dead then (True, [e1 :: l1])
                    else (True, l1)
                  else
                    (mem, [e1 :: l1])
            | Epers_Burial ->
                if found_burial.val then (mem, [e1 :: l1])
                else
                  if e.epers_name = e1.epers_name then
                    match p.burial with
                    [ UnknownBurial ->
                        let _ = found_burial.val := True in
                        (True, l1)
                    | Buried cd ->
                        let witnesses =
                          merge_event_witnesses base e1.epers_witnesses e.epers_witnesses
                        in
                        let e1 =
                          {(e1) with epers_date = cd;
                            epers_place = p.burial_place;
                            epers_note = p.burial_note; epers_src = p.burial_src;
                            epers_witnesses = witnesses}
                        in
                        let _ = found_burial.val := True in
                        (True, [e1 :: l1])
                    | _ ->
                        let _ = found_burial.val := True in
                        (mem, [e1 :: l1]) ]
                  else
                    (mem, [e1 :: l1])
            | Epers_Cremation ->
                if found_burial.val then (mem, [e1 :: l1])
                else
                  if e.epers_name = e1.epers_name then
                    match p.burial with
                    [ UnknownBurial ->
                        let _ = found_burial.val := True in
                        (True, l1)
                    | Cremated cd ->
                        let witnesses =
                          merge_event_witnesses base e1.epers_witnesses e.epers_witnesses
                        in
                        let e1 =
                          {(e1) with epers_date = cd;
                            epers_place = p.burial_place;
                            epers_note = p.burial_note; epers_src = p.burial_src;
                            epers_witnesses = witnesses}
                        in
                        let _ = found_burial.val := True in
                        (True, [e1 :: l1])
                    | _ ->
                        let _ = found_burial.val := True in
                        (mem, [e1 :: l1]) ]
                  else
                    (mem, [e1 :: l1])
            | _ -> (mem, [e1 :: l1]) ])
          l (False, [])
    | _ -> (False, l) ]
  in
  let rec merge_events_aux l1 l2 =
    match l2 with
    [ [] -> l1
    | [e2 :: l2] ->
        let (mem, l1) = list_mem e2 l1 in
        if mem then merge_events_aux l1 l2
        else merge_events_aux (l1 @ [e2]) l2 ]
  in
  merge_events_aux l1 l2
;

value reconstitute conf base p1 p2 =
  let field name proj null =
    let x1 = proj p1 in
    let x2 = proj p2 in
    match p_getenv conf.env name with
    [ Some "1" -> x1
    | Some "2" -> x2
    | _ -> if null x1 then x2 else x1 ]
  in
  let list conv proj =
    let l1 = List.map conv (proj p1) in
    let l2 = List.map conv (proj p2) in merge_lists l1 l2
  in
  let merge_primary_events conv proj p =
    let l1 = List.map conv (proj p1) in
    let l2 = List.map conv (proj p2) in
    merge_events base l1 l2 p
  in
  let p =
    {first_name =
       field "first_name" (fun p -> p_first_name base p)
         (fun x -> x = "" || x = "?");
     surname =
       field "surname" (fun p -> p_surname base p) (fun x -> x = "" || x = "?");
     occ = field "number" get_occ ( \= 0);
     image = field "image" (fun p -> sou base (get_image p)) ( \= "");
     public_name =
       field "public_name" (fun p -> sou base (get_public_name p)) ( \= "");
     qualifiers = list (sou base) get_qualifiers;
     aliases = list (sou base) get_aliases;
     first_names_aliases = list (sou base) get_first_names_aliases;
     surnames_aliases = list (sou base) get_surnames_aliases;
     titles = list (map_title_strings (sou base)) get_titles;
     rparents = list (map_relation_ps (sorp base) (sou base)) get_rparents;
     related = [];
     occupation =
       field "occupation" (fun p -> sou base (get_occupation p)) ( \= "");
     sex = field "sex" get_sex ( \= Neuter);
     access = field "access" get_access ( \= IfTitles);
     birth = field "birth" get_birth ( \= Adef.codate_None);
     birth_place =
       field "birth_place" (fun p -> sou base (get_birth_place p)) ( \= "");
     birth_note =
       merge_strings base (get_birth_note p1) "<br>\n" (get_birth_note p2);
     birth_src = merge_strings base (get_birth_src p1) ", " (get_birth_src p2);
     baptism = field "baptism" get_baptism ( \= Adef.codate_None);
     baptism_place =
       field "baptism_place" (fun p -> sou base (get_baptism_place p)) ( \= "");
     baptism_note =
       merge_strings base (get_baptism_note p1) "<br>\n" (get_baptism_note p2);
     baptism_src =
       merge_strings base (get_baptism_src p1) ", " (get_baptism_src p2);
     death = field "death" get_death
       (fun x ->
         match x with
         [ DontKnowIfDead | OfCourseDead -> True
         | _ -> False]);
     death_place =
       field "death_place" (fun p -> sou base (get_death_place p)) ( \= "");
     death_note =
       merge_strings base (get_death_note p1) "<br>\n" (get_death_note p2);
     death_src = merge_strings base (get_death_src p1) ", " (get_death_src p2);
     burial = field "burial" get_burial ( \= UnknownBurial);
     burial_place =
       field "burial_place" (fun p -> sou base (get_burial_place p)) ( \= "");
     burial_note =
       merge_strings base (get_burial_note p1) "<br>\n" (get_burial_note p2);
     burial_src =
       merge_strings base (get_burial_src p1) ", " (get_burial_src p2);
       pevents = list (map_pers_event (sorp base) (sou base)) get_pevents;
     notes = merge_strings base (get_notes p1) "<br>\n" (get_notes p2);
     psources = merge_strings base (get_psources p1) ", " (get_psources p2);
     key_index = get_key_index p1}
  in
  (* On fait la fusion des évènements à partir *)
  (* de la fusion des évènements principaux.   *)
  let pevents =
    merge_primary_events (map_pers_event (sorp base) (sou base)) get_pevents p
  in
  {(p) with pevents = pevents}
;

value print_merge conf base =
  match (p_getint conf.env "i1", p_getint conf.env "i2") with
  [ (Some i1, Some i2) ->
      let p1 = poi base (Adef.iper_of_int i1) in
      let p2 = poi base (Adef.iper_of_int i2) in
      let p = reconstitute conf base p1 p2 in
      let sp = UpdateInd.string_person_of base p1 in
      let digest = Update.digest_person sp in
      (* TODO: check that marking all events as new is OK *)
      UpdateInd.print_update_ind conf base ([],p) digest
  | _ -> incorrect_request conf ]
;

value print_mod_merge_ok conf base wl p = do {
  let title _ = Wserver.wprint "%s" (capitale (transl conf "merge done")) in
  header conf title;
  print_link_to_welcome conf True;
  Wserver.wprint "\n%s\n"
    (referenced_person_text conf base (poi base p.key_index));
  Update.print_warnings conf base wl;
  Merge.print_possible_continue_merging conf base;
  trailer conf;
};

value redirect_relations_of_added_related base p ip2 rel_chil =
  let (p_related, mod_p) =
    List.fold_right
      (fun ipc (p_related, mod_p) -> do {
         let pc = poi base ipc in
         let (pc_rparents, mod_pc, p_related, mod_p) =
           List.fold_right
             (fun r (pc_rparents, mod_pc, p_related, mod_p) ->
                let (r, mod_pc, p_related, mod_p) =
                  match r.r_fath with
                  [ Some ip when ip = ip2 ->
                      let (p_related, mod_p) =
                        if List.mem ipc p_related then
                          (p_related, mod_p)
                        else ([ipc :: p_related], True)
                      in
                      let r = {(r) with r_fath = Some p.key_index} in
                      (r, True, p_related, mod_p)
                  | _ -> (r, mod_pc, p_related, mod_p) ]
                in
                let (r, mod_pc, p_related, mod_p) =
                  match r.r_moth with
                  [ Some ip when ip = ip2 ->
                      let (p_related, mod_p) =
                        if List.mem ipc p_related then
                          (p_related, mod_p)
                        else ([ipc :: p_related], True)
                      in
                      let r = {(r) with r_moth = Some p.key_index} in
                      (r, True, p_related, mod_p)
                  | _ -> (r, mod_pc, p_related, mod_p) ]
                in
                ([r :: pc_rparents], mod_pc, p_related, mod_p))
             (get_rparents pc) ([], False, p_related, mod_p)
         in
         let (pc_pevents, mod_pc, p_related, mod_p) =
           List.fold_right
             (fun e (pc_pevents, mod_pc, p_related, mod_p) ->
                let (e, mod_pc, p_related, mod_p) =
                  let (witnesses, mod_p, p_related) =
                    List.fold_right
                      (fun (ip, k) (witnesses, mod_p, p_related) ->
                         if ip = ip2 then do {
                           let (p_related, mod_p) =
                             if List.mem ipc p_related then (p_related, mod_p)
                             else ([ipc :: p_related], True)
                           in
                           ([(p.key_index, k) :: witnesses], mod_p, p_related)
                         }
                         else ([(ip, k) :: witnesses], mod_p, p_related))
                      (Array.to_list e.epers_witnesses) ([], mod_pc, p_related)
                  in
                  let e =
                    {(e) with epers_witnesses = Array.of_list witnesses}
                  in
                  (e, True, p_related, mod_p)
                in
                ([e :: pc_pevents], mod_pc, p_related, mod_p))
             (get_pevents pc) ([], False, p_related, mod_p)
         in
         (* TODO mod_pc = True tout le temps *)
         if mod_pc then
           let pc = gen_person_of_person pc in
           let pc = {(pc) with rparents = pc_rparents; pevents = pc_pevents} in
           patch_person base ipc pc
         else ();
         let (p_related, mod_p) =
           loop (p_related, mod_p) 0
           where rec loop (p_related, mod_p) i =
             if i = Array.length (get_family pc) then
               (p_related, mod_p)
             else
               let ifam = (get_family pc).(i) in
               let fam = gen_family_of_family (foi base ifam) in
               let (p_related, mod_p) =
                 if array_mem ip2 fam.witnesses
                 then do {
                   let (p_related, mod_p) =
                     loop (p_related, mod_p) 0
                     where rec loop (p_related, mod_p) j =
                       if j = Array.length fam.witnesses then
                         (p_related, mod_p)
                       else
                       let (p_related, mod_p) =
                         if fam.witnesses.(j) = ip2 then do {
                           fam.witnesses.(j) := p.key_index;
                           if List.mem ipc p_related then
                             (p_related, mod_p)
                           else ([ipc :: p_related], True)
                         }
                         else (p_related, mod_p)
                       in
                       loop (p_related, mod_p) (j + 1)
                   in
                   patch_family base ifam fam;
                   (p_related, mod_p)
                 }
                 else (p_related, mod_p)
               in
               let (pc_fevents, mod_pc, p_related, mod_p) =
                 List.fold_right
                   (fun e (pc_fevents, mod_pc, p_related, mod_p) ->
                      let (e, mod_pc, p_related, mod_p) =
                        let (p_related, mod_p) =
                          loop (p_related, mod_p) 0
                          where rec loop (p_related, mod_p) j =
                            if j = Array.length e.efam_witnesses then
                              (p_related, mod_p)
                            else
                              let (p_related, mod_p) =
                                if fst e.efam_witnesses.(j) = ip2 then do {
                                  let (_, wk) = e.efam_witnesses.(j) in
                                  e.efam_witnesses.(j) := (p.key_index, wk);
                                  if List.mem ipc p_related then
                                    (p_related, mod_p)
                                  else ([ipc :: p_related], True)
                                }
                                else (p_related, mod_p)
                              in
                              loop (p_related, mod_p) (j + 1)
                        in
                        (e, True, p_related, mod_p)
                      in
                      ([e :: pc_fevents], mod_pc, p_related, mod_p))
                   (fam.fevents) ([], False, p_related, mod_p)
               in
               let () =
                 (* TODO mod_pc = True tout le temps *)
                 if mod_pc then
                   let fam = {(fam) with fevents = pc_fevents} in
                   patch_family base ifam fam
                 else ()
               in
               loop (p_related, mod_p) (i + 1)
         in
         (p_related, mod_p)
       })
      rel_chil (p.related, False)
  in
  if mod_p then {(p) with related = p_related} else p
;

value redirect_added_families base p ip2 p2_family =
  for i = 0 to Array.length p2_family - 1 do {
    let ifam = p2_family.(i) in
    let fam = foi base ifam in
    let cpl =
      if ip2 = get_father fam then do {
        Array.iter
          (fun ip ->
             let w = poi base ip in
             if not (List.mem p.key_index (get_related w)) then
               let w = gen_person_of_person w in
               let w =
                 {(w) with related = [p.key_index :: w.related]}
               in
               patch_person base ip w
             else ())
          (get_witnesses fam);
        List.iter
          (fun evt ->
             Array.iter
               (fun (ip, _) ->
                 let w = poi base ip in
                 if not (List.mem p.key_index (get_related w)) then
                   let w = gen_person_of_person w in
                   let w =
                     {(w) with related = [p.key_index :: w.related]}
                   in
                   patch_person base ip w
                 else ())
               evt.efam_witnesses)
          (get_fevents fam);
        couple False p.key_index (get_mother fam)
      }
      else if ip2 = get_mother fam then
        couple False (get_father fam) p.key_index
      else assert False
    in
    patch_couple base ifam cpl;
  }
;

value effective_mod_merge conf base o_p1 o_p2 _ sp =
  match p_getint conf.env "i2" with
  [ Some i2 -> do {
      let ip2 = Adef.iper_of_int i2 in
      let p2 = poi base ip2 in
      let rel_chil = get_related p2 in
      let p_family = get_family (poi base sp.key_index) in
      let p2_family = get_family p2 in
      let warning _ = () in
      MergeInd.reparent_ind base warning sp.key_index ip2;
      delete_key base (sou base (get_first_name p2))
        (sou base (get_surname p2)) (get_occ p2);
      let warning _ = () in
      let p2 = UpdateIndOk.effective_del conf base warning p2 in
      patch_person base p2.key_index p2;
      let u2 = {family = [| |]} in
      patch_union base p2.key_index u2;
      let p = UpdateIndOk.effective_mod conf base sp in
      let p = redirect_relations_of_added_related base p ip2 rel_chil in
      redirect_added_families base p ip2 p2_family;
      Update.update_misc_names_of_family base p.sex {family = p_family};
      patch_person base p.key_index p;
      let u = {family = Array.append p_family p2_family} in
      if p2_family <> [| |] then patch_union base p.key_index u else ();
      Consang.check_noloop_for_person_list base (Update.error conf base)
        [p.key_index];
      let wl =
        let a = poi base p.key_index in
        let a = {parents = get_parents a; consang = get_consang a} in
        UpdateIndOk.all_checks_person conf base p a u
      in
      Util.commit_patches conf base;
      let changed = U_Merge_person o_p1 o_p2 (Util.string_gen_person base p) in
      History.record conf base changed "fp";
      Update.delete_topological_sort conf base;
      print_mod_merge_ok conf base wl p;
    }
  | _ -> incorrect_request conf ]
;

value print_mod_merge o_conf base =
  let get_gen_person i =
    match p_getint o_conf.env i with
    [ Some i ->
        Util.string_gen_person
          base
          (gen_person_of_person (poi base (Adef.iper_of_int i)))
    | None ->
        Util.string_gen_person
          base
          (gen_person_of_person (poi base (Adef.iper_of_int (-1)))) ]
  in
  let o_p1 = get_gen_person "i" in
  let o_p2 = get_gen_person "i2" in
  let conf = Update.update_conf o_conf in
  UpdateIndOk.print_mod_aux conf base (effective_mod_merge conf base o_p1 o_p2)
;
