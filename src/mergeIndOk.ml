(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: mergeIndOk.ml,v 3.10 2001-02-13 00:23:44 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Config;
open Def;
open Util;
open Gutil;

value rec merge_lists l1 =
  fun
  [ [x2 :: l2] ->
      if List.mem x2 l1 then merge_lists l1 l2 else merge_lists (l1 @ [x2]) l2
  | [] -> l1 ]
;

value cat_strings base is1 sep is2 =
  let n1 = sou base is1 in
  let n2 = sou base is2 in
  if n1 = "" then n2
  else if n2 = "" then n1
  else n1 ^ sep ^ n2
;

value merge_strings base is1 sep is2 =
  if is1 = is2 then sou base is1 else cat_strings base is1 sep is2
;

value sorp base ip =
  let p = poi base ip in
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  let occ =
    if first_name = "?" || surname = "?" then Adef.int_of_iper p.cle_index
    else p.occ
  in
  (sou base p.first_name, sou base p.surname, p.occ, Update.Link, "")
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
    let l2 = List.map conv (proj p2) in
    merge_lists l1 l2
  in
  {first_name =
      field "first_name" (fun p -> p_first_name base p)
        (fun x -> x = "" || x = "?");
   surname = field "surname" (fun p -> p_surname base p)
        (fun x -> x = "" || x = "?");
   occ = field "number" (fun p -> p.occ) (\= 0);
   image = field "image" (fun p -> sou base p.image) (\= "");
   public_name = field "public_name" (fun p -> sou base p.public_name) (\= "");
   qualifiers = list (sou base) (fun p -> p.qualifiers);
   aliases = list (sou base) (fun p -> p.aliases);
   first_names_aliases = list (sou base) (fun p -> p.first_names_aliases);
   surnames_aliases = list (sou base) (fun p -> p.surnames_aliases);
   titles = list (map_title_strings (sou base)) (fun p -> p.titles);
   rparents =
     list (map_relation_ps (sorp base) (sou base)) (fun p -> p.rparents);
   related = [];
   occupation = field "occupation" (fun p -> sou base p.occupation) (\= "");
   sex = field "sex" (fun p -> p.sex) (\= Neuter);
   access = field "access" (fun p -> p.access) (\= IfTitles);
   birth = field "birth" (fun p -> p.birth) (\= Adef.codate_None);
   birth_place = field "birth_place" (fun p -> sou base p.birth_place) (\= "");
   birth_src = merge_strings base p1.birth_src ", " p2.birth_src;
   baptism = field "baptism" (fun p -> p.baptism) (\= Adef.codate_None);
   baptism_place =
     field "baptism_place" (fun p -> sou base p.baptism_place) (\= "");
   baptism_src = merge_strings base p1.baptism_src ", " p2.baptism_src;
   death = field "death" (fun p -> p.death) (\= DontKnowIfDead);
   death_place = field "death_place" (fun p -> sou base p.death_place) (\= "");
   death_src = merge_strings base p1.death_src ", " p2.death_src;
   burial = field "burial" (fun p -> p.burial) (\= UnknownBurial);
   burial_place =
     field "burial_place" (fun p -> sou base p.burial_place) (\= "");
   burial_src = merge_strings base p1.burial_src ", " p2.burial_src;
   notes = cat_strings base p1.notes "<br>\n" p2.notes;
   psources = merge_strings base p1.psources ", " p2.psources;
   cle_index = p1.cle_index}
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
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "merge done"))
  in
  do header conf title;
     print_link_to_welcome conf True;
     afficher_personne_referencee conf base p;
     Wserver.wprint "\n";
     Update.print_warnings conf base wl;
     match (p_getint conf.env "ini1", p_getint conf.env "ini2") with
     [ (Some ini1, Some ini2) ->
         let p1 = base.data.persons.get ini1 in
         let p2 = base.data.persons.get ini2 in
         do Wserver.wprint "\n";
            html_p conf;
            stag "a" "href=%sm=MRG_IND;i=%d;i2=%d" (commd conf) ini1 ini2
            begin
              Wserver.wprint "%s" (capitale (transl conf "continue merging"));
            end;
            Wserver.wprint "\n";
            Merge.print_someone conf base p1;
            Wserver.wprint "\n%s\n" (transl conf "and");
            Merge.print_someone conf base p2;
            Wserver.wprint "\n";
         return ()
     | _ -> () ];
     trailer conf;
  return ()
;

value effective_mod_merge conf base sp =
  match p_getint conf.env "i2" with
  [ Some i2 ->
      let p2 = base.data.persons.get i2 in
      let u2 = base.data.unions.get i2 in
      let rel_chil = p2.related in
      let p2_family = u2.family in
      let p2_sexe = p2.sex in
      do MergeInd.reparent_ind base sp p2;
         UpdateIndOk.effective_del conf base p2;
         base.func.patch_person p2.cle_index p2;
         u2.family := [| |];
         base.func.patch_union p2.cle_index u2;
      return
      let p = UpdateIndOk.effective_mod conf base sp in
      let u = uoi base p.cle_index in
      do List.iter
           (fun ipc ->
              let pc = poi base ipc in
              let uc = uoi base ipc in
              let mod_p = ref False in
              do List.iter
                   (fun r ->
                      do match r.r_fath with
                         [ Some ip when ip = p2.cle_index ->
                             do r.r_fath := Some p.cle_index;
                                mod_p.val := True;
                                if List.memq ipc p.related then ()
                                else p.related := [ipc :: p.related];
                             return ()
                         | _ -> () ];
                         match r.r_moth with
                         [ Some ip when ip = p2.cle_index ->
                             do r.r_moth := Some p.cle_index;
                                mod_p.val := True;
                                if List.memq ipc p.related then ()
                                else p.related := [ipc :: p.related];
                             return ()
                         | _ -> () ];
                      return ())
                   pc.rparents;
                 for i = 0 to Array.length uc.family - 1 do
                   let fam = foi base uc.family.(i) in
                   if array_memq p2.cle_index fam.witnesses then
                     do for j = 0 to Array.length fam.witnesses - 1 do
                          if fam.witnesses.(j) == p2.cle_index then
                            do fam.witnesses.(j) := p.cle_index;
                               if List.memq ipc p.related then ()
                               else p.related := [ipc :: p.related];
                            return ()
                          else ();
                        done;
                        base.func.patch_family fam.fam_index fam;
                     return ()
                   else ();
                 done;
                 if mod_p.val then base.func.patch_person ipc pc else ();
              return ())
           rel_chil;
         for i = 0 to Array.length p2_family - 1 do
           let ifam = p2_family.(i) in
           let cpl = coi base ifam in
           do match p2_sexe with
              [ Male -> cpl.father := p.cle_index
              | Female -> cpl.mother := p.cle_index
              | Neuter -> assert False ];
              base.func.patch_couple ifam cpl;
           return ();
         done;
         Update.update_misc_names_of_family base p u;
         base.func.patch_person p.cle_index p;
         if p2_family <> [| |] then
           do u.family := Array.append u.family p2_family;
              base.func.patch_union p.cle_index u;
           return ()
         else ();
         Gutil.check_noloop_for_person_list base (Update.error conf base)
           [p.cle_index];
      return
      let wl =
        UpdateIndOk.all_checks_person conf base p (aoi base p.cle_index) u
      in
      let key = (sp.first_name, sp.surname, sp.occ) in
      do base.func.commit_patches ();
         History.record conf base key "fp";
         print_mod_merge_ok conf base wl p;
      return ()
  | _ -> incorrect_request conf ]
;

value print_mod_merge conf base =
  UpdateIndOk.print_mod_aux conf base (effective_mod_merge conf base)
;
