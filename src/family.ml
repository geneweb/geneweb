(* camlp4r ./def.syn.cmo *)
(* $Id: family.ml,v 1.1.1.1 1998-09-01 14:32:08 ddr Exp $ *)

open Def;
open Gutil;
open Config;
open Util;

value person_is_std_key base p k =
  let k = Name.strip_lower k in
  if k = Name.strip_lower (sou base p.first_name ^ " " ^ sou base p.surname)
  then True
  else if
    List.exists (fun n -> Name.strip n = k) (person_misc_names base p)
  then True
  else False
;

value select_std_eq base pl k =
  List.fold_right
    (fun p pl -> if person_is_std_key base p k then [p :: pl] else pl)
    pl []
;

value inconnu_au_bataillon conf =
  match (p_getenv conf.env "n", p_getenv conf.env "p") with
  [ (Some nom, Some prenom) ->
      let title _ =
        Wserver.wprint "%s: \"%s %s\"" (capitale (transl conf "not found"))
          prenom nom
      in
      do header conf title; trailer conf; return ()
  | _ -> incorrect_request conf ]
;

value inconnu conf n =
  let title _ =
    Wserver.wprint "%s: \"%s\"" (capitale (transl conf "not found")) n
  in
  do header conf title; trailer conf; return ()
;

value person_selected conf base senv p =
  match p_getenv senv "m" with
  [ Some "R" -> Relation.print conf senv base p
  | Some mode -> incorrect_request conf
  | None -> Perso.print conf base p ]
;

value compact_list conf base xl =
  let pl =
    Sort.list
      (fun p1 p2 ->
         match
          (Adef.od_of_codate p1.birth, p1.death,
           Adef.od_of_codate p2.birth, p2.death)
         with
         [ (Some d1, _, Some d2, _) -> d1 strictement_avant d2
         | (Some d1, _, _, Death _ d2) ->
             d1 strictement_avant Adef.date_of_cdate d2
         | (_, Death _ d1, Some d2, _) ->
              Adef.date_of_cdate d1 strictement_avant d2
         | (_, Death _ d1, _, Death _ d2) ->
              Adef.date_of_cdate d1 strictement_avant Adef.date_of_cdate d2
         | (Some _, _, _, _) -> True
         | (_, Death _ _, _, _) -> True
         | _ ->
             let c =
               alphabetique (sou base p1.surname) (sou base p2.surname)
             in
             if c == 0 then
               let c =
                 alphabetique (sou base p1.first_name) (sou base p2.first_name)
               in
               if c == 0 then p1.occ > p2.occ else c > 0
             else c > 0 ])
     (List.map (poi base) xl)
  in
  let pl =
    List.fold_right
      (fun p pl ->
         match pl with
         [ [p1 :: _] when p == p1 -> pl
         | _ -> [p :: pl] ])
      pl []
  in
  pl
;

value precisez conf base n pl =
  let title _ = Wserver.wprint "%s : %s" n (transl conf "specify") in
  let n = Name.crush_lower n in
  let ptll =
    List.map
      (fun p ->
         let tl = ref [] in
         let add_tl t =
           tl.val :=
             let rec add_rec =
               fun
               [ [t1 :: tl1] ->
                   if t1.t_title = t.t_title && t1.t_place = t.t_place then
                     [t1 :: tl1]
                   else [t1 :: add_rec tl1]
               | [] -> [t] ]
             in
             add_rec tl.val
         in
         let compare_and_add t pn =
           let pn = sou base pn in
           if Name.crush_lower pn = n then add_tl t
           else
             match p.nick_names with
             [ [nn :: _] ->
                 let nn = sou base nn in
                 if Name.crush_lower (pn ^ " " ^ nn) = n then add_tl t
                 else ()
             | _ -> () ]
         in
         do List.iter
              (fun t ->
                 match (t.t_name, p.public_name) with
                 [ (Tname s, _) -> compare_and_add t s
                 | (_,  pn) when sou base pn <> "" -> compare_and_add t pn
                 | _ -> () ])
              p.titles;
         return (p, tl.val))
      pl
  in
  do header conf title;
     Wserver.wprint "<ul>\n";
     List.iter
       (fun (p, tl) ->
          do Wserver.wprint "<li>\n";
             match tl with
             [ [] -> afficher_personne_titre_referencee conf base p
             | [t :: tl] ->
                 do afficher_personne_un_titre_referencee conf base p t;
                    List.iter (afficher_un_titre conf base p) tl;
                 return () ];
             Date.afficher_dates_courtes conf base p;
             match p.sexe with
             [ Feminin ->
                 let husbands =
                   List.fold_right
                     (fun ifam husbands ->
                        let cpl = coi base ifam in
                        let husband = poi base cpl.father in
                        if sou base husband.surname <> "?" then
                          [husband :: husbands]
                        else husbands)
                     (Array.to_list p.family) []
                 in
                 match husbands with
                 [ [] -> ()
                 | [h :: hl] ->
                     do Wserver.wprint ", <em>%s "
                          (transl_nth conf "spouse" 1);
                        afficher_personne_titre conf base h;
                        List.iter
                          (fun h ->
                             do Wserver.wprint ", %s\n" (transl conf "and");
                                afficher_personne_titre conf base h;
                             return ())
                          hl;
                        Wserver.wprint "</em>\n";
                     return () ]
             | _ -> () ];
          return ())
       ptll;
     Wserver.wprint "</ul>\n";
     trailer conf;
  return ()
;

value family_m conf base =
  do conf.senv :=
       match try Some (List.assoc "e" conf.env) with _ -> None with
       [ Some s -> s
       | _ -> "" ];
  return
  let senv = Util.create_env (decode_varenv conf.senv) in
  match p_getenv conf.env "m" with
  [ Some "A" ->
      match find_person_in_env conf base "" with
      [ Some p -> Ascend.print conf base p
      | _ -> inconnu_au_bataillon conf ]
  | Some "ADD_FAM" when conf.wizard ->
      UpdateFam.print_add conf base
  | Some "ADD_FAM_OK" when conf.wizard ->
      UpdateFamOk.print_add conf base
  | Some "ADD_IND" when conf.wizard ->
      UpdateInd.print_add conf base
  | Some "ADD_IND_OK" when conf.wizard ->
      UpdateIndOk.print_add conf base
  | Some "ADD_PAR" when conf.wizard ->
      UpdateFam.print_add_parents conf base
  | Some "AN" ->
      match p_getenv conf.env "v" with
      [ Some x -> Birthday.print conf base (int_of_string x)
      | _ -> Birthday.menu_print conf base ]
  | Some "AD" ->
      match p_getenv conf.env "v" with
      [ Some x -> Birthday.print_dead conf base (int_of_string x)
      | _ -> Birthday.menu_print_dead conf base ]
  | Some "AM" ->
      match p_getenv conf.env "v" with
      [ Some x -> Birthday.print_marriage conf base (int_of_string x)
      | _ -> Birthday.print_menu_marriage conf base ]
  | Some "AS_OK" ->
      AdvSearchOk.print conf base
  | Some "B" when conf.wizard || conf.friend -> Birth.print conf base
  | Some "D" ->
      match find_person_in_env conf base "" with
      [ Some p -> Descend.print conf base p
      | _ -> inconnu_au_bataillon conf ]
  | Some "DEL_FAM" when conf.wizard ->
      UpdateFam.print_del conf base
  | Some "DEL_FAM_OK" when conf.wizard ->
      UpdateFamOk.print_del conf base
  | Some "DEL_IND" when conf.wizard ->
      UpdateInd.print_del conf base
  | Some "DEL_IND_OK" when conf.wizard ->
      UpdateIndOk.print_del conf base
  | Some "H" ->
      match p_getenv conf.env "v" with
      [ Some f -> Srcfile.print conf base f
      | None -> () ]
  | Some "LEX" -> Srcfile.print_lexicon conf base
  | Some "MRG" when conf.wizard ->
      match find_person_in_env conf base "" with
      [ Some p -> Merge.print conf base p
      | _ -> inconnu_au_bataillon conf ]
  | Some "MRG_FAM" when conf.wizard ->
      MergeFam.print conf base
  | Some "MRG_FAM_OK" when conf.wizard ->
      MergeFamOk.print_merge conf base
  | Some "MRG_MOD_FAM_OK" when conf.wizard ->
      MergeFamOk.print_mod_merge conf base
  | Some "MRG_IND" when conf.wizard ->
      MergeInd.print conf base
  | Some "MRG_IND_OK" when conf.wizard ->
      MergeIndOk.print_merge conf base
  | Some "MRG_MOD_IND_OK" when conf.wizard ->
      MergeIndOk.print_mod_merge conf base
  | Some "MOD_FAM" when conf.wizard ->
      UpdateFam.print_mod conf base
  | Some "MOD_FAM_OK" when conf.wizard ->
      UpdateFamOk.print_mod conf base
  | Some "MOD_IND" when conf.wizard ->
      UpdateInd.print_mod conf base
  | Some "MOD_IND_OK" when conf.wizard ->
      UpdateIndOk.print_mod conf base
  | Some "N" ->
      match p_getenv conf.env "v" with
      [ Some v -> Some.surname_print conf base v
      | _ -> Alln.family_names_print conf base ]
  | Some "NG" ->
      match p_getenv conf.env "n" with
      [ Some n ->
          match p_getenv conf.env "t" with
          [ Some "P" -> Some.first_name_print conf base n
          | Some "N" -> Some.surname_print conf base n
          | _ ->
              let pl = person_ht_find_all base n in
              let pl = compact_list conf base pl in
              let pl =
                let spl = select_std_eq base pl n in
                if spl = [] then pl else spl
              in
              match pl with
              [ [] -> inconnu conf n
              | [p] -> person_selected conf base senv p
              | pl -> precisez conf base n pl ] ]
      | None -> () ]
  | Some "P" ->
      match p_getenv conf.env "v" with
      [ Some v -> Some.first_name_print conf base v
      | None -> Alln.first_names_print conf base ]
  | Some "R" ->
      match find_person_in_env conf base "" with
      [ Some p -> Relation.print conf senv base p
      | _ -> inconnu_au_bataillon conf ]
  | Some "REQUEST" when conf.wizard ->
      let title _ = () in
      do header conf title;
         Wserver.wprint "<pre>\n";
         List.iter (Wserver.wprint "%s\n") conf.request;
         Wserver.wprint "</pre>\n";
         trailer conf;
      return ()
  | Some "RL" -> RelationLink.print conf base
  | Some "SWI_FAM" when conf.wizard ->
      UpdateFam.print_swi conf base
  | Some "SWI_FAM_OK" when conf.wizard ->
      UpdateFamOk.print_swi conf base
  | Some "TT" -> Title.print conf base
  | Some "U" when conf.wizard ->
      match find_person_in_env conf base "" with
      [ Some p -> Update.print conf base p
      | _ -> inconnu_au_bataillon conf ]
  | Some mode -> incorrect_request conf
  | None ->
      match find_person_in_env conf base "" with
      [ Some p -> person_selected conf base senv p
      | _ -> inconnu_au_bataillon conf ] ]
;

value family conf base =
  do if conf.env = [] then
       do Srcfile.incr_welcome_counter conf; return
       Srcfile.print_start conf base
     else
       do Srcfile.incr_request_counter conf; return
       family_m conf base;
  return Wserver.wflush ()
;
