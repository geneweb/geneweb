(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: mergeIndOk.ml,v 1.5 1998-12-16 06:04:58 ddr Exp $ *)

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
      field "first_name" (fun p -> sou base p.first_name)
        (fun x -> x = "" || x = "?");
   surname = field "surname" (fun p -> sou base p.surname)
        (fun x -> x = "" || x = "?");
   occ = field "number" (fun p -> p.occ) (\= 0);
   photo = field "photo" (fun p -> sou base p.photo) (\= "");
   public_name = field "public_name" (fun p -> sou base p.public_name) (\= "");
   nick_names = list (sou base) (fun p -> p.nick_names);
   aliases = list (sou base) (fun p -> p.aliases);
   first_names_aliases = list (sou base) (fun p -> p.first_names_aliases);
   surnames_aliases = list (sou base) (fun p -> p.surnames_aliases);
   titles = list (map_title_strings (sou base)) (fun p -> p.titles);
   occupation = field "occupation" (fun p -> sou base p.occupation) (\= "");
   sex = field "sex" (fun p -> p.sex) (\= Neuter);
   access = field "access" (fun p -> p.access) (\= IfTitles);
   birth = field "birth" (fun p -> p.birth) (\= Adef.codate_None);
   birth_place = field "birth_place" (fun p -> sou base p.birth_place) (\= "");
   birth_src = cat_strings base p1.birth_src ", " p2.birth_src;
   baptism = field "baptism" (fun p -> p.baptism) (\= Adef.codate_None);
   baptism_place =
     field "baptism_place" (fun p -> sou base p.baptism_place) (\= "");
   baptism_src = cat_strings base p1.baptism_src ", " p2.baptism_src;
   death = field "death" (fun p -> p.death) (\= DontKnowIfDead);
   death_place = field "death_place" (fun p -> sou base p.death_place) (\= "");
   death_src = cat_strings base p1.death_src ", " p2.death_src;
   burial = field "burial" (fun p -> p.burial) (\= UnknownBurial);
   burial_place =
     field "burial_place" (fun p -> sou base p.burial_place) (\= "");
   burial_src = cat_strings base p1.burial_src ", " p2.burial_src;
   family = [| |];
   notes = cat_strings base p1.notes "<br>\n" p2.notes;
   psources = cat_strings base p1.psources ", " p2.psources;
   cle_index = p1.cle_index}
;

value print_merge1 conf base p p2 digest =
  let title _ =
    Wserver.wprint "%s / %s # %d" (capitale (transl conf "merge"))
      (capitale (transl_nth conf "person/persons" 0))
      (Adef.int_of_iper p.cle_index)
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       UpdateInd.merge_call conf;
       Wserver.wprint "<input type=hidden name=i value=%d>\n"
         (Adef.int_of_iper p.cle_index);
       Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n" digest;
       Wserver.wprint "<input type=hidden name=i2 value=%d>\n"
         (Adef.int_of_iper p2.cle_index);
       Wserver.wprint "\n";
       UpdateInd.print_person conf base p;
       Wserver.wprint "\n<p>\n";
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_merge conf base =
  match (p_getint conf.env "i1", p_getint conf.env "i2") with
  [ (Some i1, Some i2) ->
      let p1 = base.persons.get i1 in
      let p2 = base.persons.get i2 in
      let p = reconstitute conf base p1 p2 in
      let digest = Update.digest_person p1 in
      print_merge1 conf base p p2 digest
  | _ -> incorrect_request conf ]
;

value print_mod_merge_ok conf base wl p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "merge done"))
  in
  do header conf title;
     afficher_personne_referencee conf base p;
     Wserver.wprint "\n";
     Update.print_warnings conf base wl;
     match (p_getint conf.env "ini1", p_getint conf.env "ini2") with
     [ (Some ini1, Some ini2) ->
         let p1 = base.persons.get ini1 in
         let p2 = base.persons.get ini2 in
         do Wserver.wprint "\n<p>\n";
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

value effective_mod_merge conf base p =
  match p_getint conf.env "i2" with
  [ Some i2 ->
      let p2 = base.persons.get i2 in
      let a1 = aoi base p.cle_index in
      let a2 = aoi base p2.cle_index in
      do match (a1.parents, a2.parents) with
         [ (None, Some ifam) ->
             let fam = foi base ifam in
             replace 0 where rec replace i =
               if fam.children.(i) = p2.cle_index then
                 do fam.children.(i) := p.cle_index;
                    a1.parents := Some ifam;
                    a2.parents := None;
                    base.patch_ascend p.cle_index a1;
                    base.patch_ascend p2.cle_index a2;
                    base.patch_family ifam fam;
                 return ()
               else replace (i + 1)
         | _ -> () ];
      return
      let p2_family = p2.family in
      let p2_sexe = p2.sex in
      do UpdateIndOk.effective_del conf base p2;
         p2.family := [| |];
         Update.update_misc_names_of_family base p2;
         base.patch_person p2.cle_index p2;
      return
      let p = UpdateIndOk.effective_mod conf base p in
      do for i = 0 to Array.length p2_family - 1 do
           let ifam = p2_family.(i) in
           let cpl = coi base ifam in
           do match p2_sexe with
              [ Masculine -> cpl.father := p.cle_index
              | Feminine -> cpl.mother := p.cle_index
              | Neuter -> assert False ];
              base.patch_couple ifam cpl;
           return ();
         done;
         p.family := Array.append p.family p2_family;
         base.patch_ascend p.cle_index a1;
         Update.update_misc_names_of_family base p;
         base.patch_person p.cle_index p;
         Gutil.check_noloop_for_person_list base (Update.error conf base)
           [p.cle_index];
      return
      let wl =
        UpdateIndOk.all_checks_person conf base p (aoi base p.cle_index)
      in
      do base.commit_patches ();
         print_mod_merge_ok conf base wl p;
      return ()
  | _ -> incorrect_request conf ]
;

value print_mod_merge conf base =
  UpdateIndOk.print_mod_aux conf base (effective_mod_merge conf base)
;
