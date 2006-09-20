(* camlp4r ./pa_html.cmo *)
(* $Id: mergeIndOk.ml,v 5.8 2006-09-20 12:35:43 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
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
  if is1 = is2 then sou base is1 else cat_strings base is1 sep is2
;

value sorp base ip =
  let p = poi base ip in
  (sou base (get_first_name p), sou base (get_surname p), get_occ p,
   Update.Link, "")
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
   birth_src = merge_strings base (get_birth_src p1) ", " (get_birth_src p2);
   baptism = field "baptism" get_baptism ( \= Adef.codate_None);
   baptism_place =
     field "baptism_place" (fun p -> sou base (get_baptism_place p)) ( \= "");
   baptism_src =
     merge_strings base (get_baptism_src p1) ", " (get_baptism_src p2);
   death = field "death" get_death ( \= DontKnowIfDead);
   death_place =
     field "death_place" (fun p -> sou base (get_death_place p)) ( \= "");
   death_src = merge_strings base (get_death_src p1) ", " (get_death_src p2);
   burial = field "burial" get_burial ( \= UnknownBurial);
   burial_place =
     field "burial_place" (fun p -> sou base (get_burial_place p)) ( \= "");
   burial_src =
     merge_strings base (get_burial_src p1) ", " (get_burial_src p2);
   notes = cat_strings base (get_notes p1) "<br>\n" (get_notes p2);
   psources = merge_strings base (get_psources p1) ", " (get_psources p2);
   cle_index = get_cle_index p1}
;

value print_merge conf base =
  match (p_getint conf.env "i1", p_getint conf.env "i2") with
  [ (Some i1, Some i2) ->
      let p1 = base.data.persons.get i1 in
      let p2 = base.data.persons.get i2 in
      let p = reconstitute conf base p1 p2 in
      let digest = Update.digest_person p1 in
      UpdateInd.print_update_ind conf base p digest
  | _ -> incorrect_request conf ]
;

value print_mod_merge_ok conf base wl p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "merge done")) in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "\n%s" (referenced_person_text conf base p);
    Wserver.wprint "\n";
    Update.print_warnings conf base wl;
    match (p_getint conf.env "ini1", p_getint conf.env "ini2") with
    [ (Some ini1, Some ini2) ->
        let p1 = base.data.persons.get ini1 in
        let p2 = base.data.persons.get ini2 in
        do {
          Wserver.wprint "\n";
          html_p conf;
          stag "a" "href=%sm=MRG_IND;i=%d;i2=%d" (commd conf) ini1 ini2 begin
            Wserver.wprint "%s" (capitale (transl conf "continue merging"));
          end;
          Wserver.wprint "\n";
          Merge.print_someone conf base p1;
          Wserver.wprint "\n%s\n" (transl_nth conf "and" 0);
          Merge.print_someone conf base p2;
          Wserver.wprint "\n";
        }
    | _ -> () ];
    trailer conf;
  }
;

value effective_mod_merge conf base sp =
  match p_getint conf.env "i2" with
  [ Some i2 ->
      let p2 = base.data.persons.get i2 in
      let u2 = base.data.unions.get i2 in
      let rel_chil = get_related p2 in
      let p2_family = get_family u2 in
      do {
        MergeInd.reparent_ind base sp.cle_index (get_cle_index p2);
        let p2 = UpdateIndOk.effective_del conf base p2 in
        base.func.patch_person (get_cle_index p2) p2;
        let u2 = union_of_gen_union {family = [| |]} in
        base.func.patch_union (get_cle_index p2) u2;
        let p = UpdateIndOk.effective_mod conf base sp in
        let u = uoi base (get_cle_index p) in
        let (p_related, mod_p) =
          List.fold_right
            (fun ipc (p_related, mod_p) ->
               let pc = poi base ipc in
               let uc = uoi base ipc in
               let (pc_rparents, mod_pc, p_related, mod_p) =
                 List.fold_right
                   (fun r (pc_rparents, mod_pc, p_related, mod_p) ->
                      let (r, mod_pc, p_related, mod_p) =
                        match r.r_fath with
                        [ Some ip when ip = get_cle_index p2 ->
                            let (p_related, mod_p) =
                              if List.memq ipc p_related then
                                (p_related, mod_p)
                              else ([ipc :: p_related], True)
                            in
                            let r =
                              {(r) with r_fath = Some (get_cle_index p)}
                            in
                            (r, True, p_related, mod_p)
                        | _ -> (r, mod_pc, p_related, mod_p) ]
                      in
                      let (r, mod_pc, p_related, mod_p) =
                        match r.r_moth with
                        [ Some ip when ip = get_cle_index p2 ->
                            let (p_related, mod_p) =
                              if List.memq ipc p_related then
                                (p_related, mod_p)
                              else ([ipc :: p_related], True)
                            in
                            let r =
                              {(r) with r_moth = Some (get_cle_index p)}
                            in
                            (r, True, p_related, mod_p)
                        | _ -> (r, mod_pc, p_related, mod_p) ]
                      in
                      ([r :: pc_rparents], mod_pc, p_related, mod_p))
                   (get_rparents pc) ([], False, p_related, mod_p)
               in
               do {
                 if mod_pc then
                   let pc =
                     person_of_gen_person
                       {(gen_person_of_person pc) with rparents = pc_rparents}
                   in
                   base.func.patch_person ipc pc
                 else ();
                 let (p_related, mod_p) =
                   loop (p_related, mod_p) 0
                   where rec loop (p_related, mod_p) i =
                     if i = Array.length (get_family uc) then
                       (p_related, mod_p)
                     else
                       let fam = foi base (get_family uc).(i) in
                       let (p_related, mod_p) =
                         if array_memq (get_cle_index p2) fam.witnesses
                         then do {
                           let (p_related, mod_p) =
                             loop (p_related, mod_p) 0
                             where rec loop (p_related, mod_p) j =
                               if j = Array.length fam.witnesses then
                                 (p_related, mod_p)
                               else
                               let (p_related, mod_p) =
                                 if fam.witnesses.(j) == get_cle_index p2
                                 then do {
                                   fam.witnesses.(j) := get_cle_index p;
                                   if List.memq ipc p_related then
                                     (p_related, mod_p)
                                   else ([ipc :: p_related], True)
                                 }
                                 else (p_related, mod_p)
                               in
                               loop (p_related, mod_p) (j + 1)
                           in
                           base.func.patch_family fam.fam_index fam;
                           (p_related, mod_p)
                         }
                         else (p_related, mod_p)
                       in
                       loop (p_related, mod_p) (i + 1)
                 in
                 (p_related, mod_p)
               })
            rel_chil (get_related p, False)
        in
        let p =
          if mod_p then
            person_of_gen_person
              {(gen_person_of_person p) with related = p_related}
          else p
        in
        for i = 0 to Array.length p2_family - 1 do {
          let ifam = p2_family.(i) in
          let fam = foi base ifam in
          let cpl = coi base ifam in
          if get_cle_index p2 = father cpl then do {
            set_father cpl (get_cle_index p);
            Array.iter
              (fun ip ->
                 let w = poi base ip in
                 if not (List.memq (get_cle_index p) (get_related w)) then
                   let w =
                     person_of_gen_person
                       {(gen_person_of_person w) with
                        related = [get_cle_index p :: get_related w]}
                   in
                   base.func.patch_person ip w
                 else ())
              fam.witnesses;
          }
          else if get_cle_index p2 = mother cpl then
            set_mother cpl (get_cle_index p)
          else assert False;
          base.func.patch_couple ifam cpl;
        };
        Update.update_misc_names_of_family conf base p u;
        base.func.patch_person (get_cle_index p) p;
        if p2_family <> [| |] then do {
          let u =
            union_of_gen_union
              {family = Array.append (get_family u) p2_family}
          in
          base.func.patch_union (get_cle_index p) u;
        }
        else ();
        Gutil.check_noloop_for_person_list base (Update.error conf base)
          [get_cle_index p];
        let wl =
          UpdateIndOk.all_checks_person conf base p
            (aoi base (get_cle_index p)) u
        in
        let key = (sp.first_name, sp.surname, sp.occ, sp.cle_index) in
        Util.commit_patches conf base;
        History.record conf base key "fp";
        Update.delete_topological_sort conf base;
        print_mod_merge_ok conf base wl p;
      }
  | _ -> incorrect_request conf ]
;

value print_mod_merge o_conf base =
  let conf = Update.update_conf o_conf in
  UpdateIndOk.print_mod_aux conf base (effective_mod_merge conf base)
;
