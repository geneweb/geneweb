(* $Id: request.ml,v 5.61 2008-11-03 15:40:10 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gutil
open Gwdb
open Hutil
open Util

let person_is_std_key conf base p k =
  let k = Name.strip_lower k in
  if k = Name.strip_lower (p_first_name base p ^ " " ^ p_surname base p) then
    true
  else if
    List.exists (fun n -> Name.strip n = k)
      (person_misc_names base p (nobtit conf base))
  then
    true
  else false

let select_std_eq conf base pl k =
  List.fold_right
    (fun p pl -> if person_is_std_key conf base p k then p :: pl else pl) pl
    []

let very_unknown conf =
  match p_getenv conf.env "n", p_getenv conf.env "p" with
    Some nom, Some prenom ->
      let title _ =
        Wserver.printf "%s%s \"%s %s\"" (capitale (transl conf "not found"))
          (Util.transl conf ":") prenom nom
      in
      Wserver.http HttpStatus.Not_Found;
      rheader conf title;
      print_link_to_welcome conf false;
      trailer conf
  | _ -> incorrect_request conf

let unknown conf n =
  let title _ =
    Wserver.printf "%s%s \"%s\"" (capitale (transl conf "not found"))
      (Util.transl conf ":") n
  in
  Wserver.http HttpStatus.Not_Found;
  rheader conf title;
  print_link_to_welcome conf false;
  trailer conf

let relation_print conf base p =
  let p1 =
    match p_getint conf.senv "ei" with
      Some i ->
        conf.senv <- [];
        if i >= 0 && i < nb_of_persons base then
          Some (pget conf base (Adef.iper_of_int i))
        else None
    | None ->
        match find_person_in_env conf base "1" with
          Some p1 -> conf.senv <- []; Some p1
        | None -> None
  in
  Relation.print conf base p p1

let person_selected conf base p =
  match p_getenv conf.senv "em" with
    Some "R" -> relation_print conf base p
  | Some mode -> incorrect_request conf
  | None -> record_visited conf (get_key_index p); Perso.print conf base p

let person_selected_with_redirect conf base p =
  match p_getenv conf.senv "em" with
    Some "R" -> relation_print conf base p
  | Some mode -> incorrect_request conf
  | None ->
      Wserver.http_redirect_temporarily (commd conf ^ Util.acces conf base p)

let compact_list conf base xl =
  let pl = sort_person_list base xl in
  let pl =
    List.fold_right
      (fun p pl ->
         match pl with
           p1 :: _ when get_key_index p = get_key_index p1 -> pl
         | _ -> p :: pl)
      pl []
  in
  pl

let cut_words str =
  let rec loop beg i =
    if i < String.length str then
      match str.[i] with
        ' ' ->
          if beg = i then loop (succ beg) (succ i)
          else String.sub str beg (i - beg) :: loop (succ i) (succ i)
      | _ -> loop beg (succ i)
    else if beg = i then []
    else [String.sub str beg (i - beg)]
  in
  loop 0 0

let try_find_with_one_first_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match Mutil.lindex n1 ' ' with
    Some i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let (list, _) =
        Some.persons_of_fsname conf base base_strings_of_surname
          (spi_find (persons_of_surname base)) get_surname sn
      in
      let pl =
        List.fold_left
          (fun pl (_, _, ipl) ->
             List.fold_left
               (fun pl ip ->
                  let p = pget conf base ip in
                  let fn1 =
                    Name.abbrev (Name.lower (sou base (get_first_name p)))
                  in
                  if List.mem fn (cut_words fn1) then p :: pl else pl)
               pl ipl)
          [] list
      in
      pl
  | None -> []

let name_with_roman_number str =
  let rec loop found len i =
    if i = String.length str then if found then Some (Buff.get len) else None
    else
      match str.[i] with
        '0'..'9' as c ->
          let (n, i) =
            let rec loop n i =
              if i = String.length str then n, i
              else
                match str.[i] with
                  '0'..'9' as c ->
                    loop (10 * n + Char.code c - Char.code '0') (i + 1)
                | _ -> n, i
            in
            loop (Char.code c - Char.code '0') (i + 1)
          in
          loop true (Buff.mstore len (Mutil.roman_of_arabian n)) i
      | c -> loop found (Buff.store len c) (i + 1)
  in
  loop false 0 0

let find_all conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with Failure _ -> None in
  match sosa_ref, sosa_nb with
    Some p, Some n ->
      if n <> Sosa.zero then
        match Util.branch_of_sosa conf base (get_key_index p) n with
          Some ((ip, _) :: _) -> [pget conf base ip], true
        | _ -> [], false
      else [], false
  | _ ->
      match person_of_string_key base an with
        Some ip ->
          let pl =
            let p = pget conf base ip in if is_hidden p then [] else [p]
          in
          let pl =
            if not conf.wizard && not conf.friend then
              List.fold_right
                (fun p pl ->
                   if not (is_hide_names conf p) ||
                      Util.authorized_age conf base p
                   then
                     p :: pl
                   else pl)
                pl []
            else pl
          in
          pl, false
      | None ->
          let ipl = person_not_a_key_find_all base an in
          let (an, ipl) =
            if ipl = [] then
              match name_with_roman_number an with
                Some an1 ->
                  let ipl = person_ht_find_all base an1 in
                  if ipl = [] then an, [] else an1, ipl
              | None -> an, ipl
            else an, ipl
          in
          let pl =
            List.fold_left
              (fun l ip ->
                 let p = pget conf base ip in
                 if is_hidden p then l else p :: l)
              [] ipl
          in
          let spl = select_std_eq conf base pl an in
          let pl =
            if spl = [] then
              if pl = [] then try_find_with_one_first_name conf base an
              else pl
            else spl
          in
          let pl =
            if not conf.wizard && not conf.friend then
              List.fold_right
                (fun p pl ->
                   if not (is_hide_names conf p) ||
                      Util.authorized_age conf base p
                   then
                     p :: pl
                   else pl)
                pl []
            else pl
          in
          compact_list conf base pl, false

let specify conf base n pl =
  let title _ = Wserver.printf "%s : %s" n (transl conf "specify") in
  let n = Name.crush_lower n in
  let ptll =
    List.map
      (fun p ->
         let tl = ref [] in
         let add_tl t =
           tl :=
             let rec add_rec =
               function
                 t1 :: tl1 ->
                   if eq_istr t1.t_ident t.t_ident &&
                      eq_istr t1.t_place t.t_place
                   then
                     t1 :: tl1
                   else t1 :: add_rec tl1
               | [] -> [t]
             in
             add_rec !tl
         in
         let compare_and_add t pn =
           let pn = sou base pn in
           if Name.crush_lower pn = n then add_tl t
           else
             match get_qualifiers p with
               nn :: _ ->
                 let nn = sou base nn in
                 if Name.crush_lower (pn ^ " " ^ nn) = n then add_tl t
             | _ -> ()
         in
         List.iter
           (fun t ->
              match t.t_name, get_public_name p with
                Tname s, _ -> compare_and_add t s
              | _, pn when sou base pn <> "" -> compare_and_add t pn
              | _ -> ())
           (nobtit conf base p);
         p, !tl)
      pl
  in
  header conf title;
  conf.cancel_links <- false;
  print_link_to_welcome conf true;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  Wserver.printf "<ul>\n";
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  List.iter
    (fun (p, tl) ->
       Wserver.printf "<li>\n";
       Perso.print_sosa conf base p true;
       begin match tl with
         [] ->
           Wserver.printf "\n%s" (referenced_person_title_text conf base p)
       | t :: _ ->
           Wserver.printf "<a href=\"%s%s\">\n" (commd conf)
             (acces conf base p);
           Wserver.printf "%s" (titled_person_text conf base p t);
           Wserver.printf "</a>\n";
           List.iter
             (fun t -> Wserver.printf "%s" (one_title_text conf base p t)) tl
       end;
       Wserver.printf "%s" (Date.short_dates_text conf base p);
       if authorized_age conf base p then
         begin match get_first_names_aliases p with
           [] -> ()
         | fnal ->
             Wserver.printf "\n<em>(";
             Mutil.list_iter_first
               (fun first fna ->
                  if not first then Wserver.printf ", ";
                  Wserver.printf "%s" (sou base fna))
               fnal;
             Wserver.printf ")</em>"
         end;
       begin let spouses =
         List.fold_right
           (fun ifam spouses ->
              let cpl = foi base ifam in
              let spouse = pget conf base (spouse (get_key_index p) cpl) in
              if p_surname base spouse <> "?" then spouse :: spouses
              else spouses)
           (Array.to_list (get_family p)) []
       in
         match spouses with
           [] -> ()
         | h :: hl ->
             let s =
               List.fold_left
                 (fun s h -> s ^ ",\n" ^ person_title_text conf base h)
                 (person_title_text conf base h) hl
             in
             Wserver.printf ", <em>&amp; %s</em>\n" s
       end;
       Wserver.printf "</li>\n")
    ptll;
  Wserver.printf "</ul>\n";
  trailer conf

(* Make the "special" environement; "em=mode;ei=n" *)

let set_senv conf vm vi =
  conf.senv <- ["em", vm; "ei", vi];
  begin match p_getenv conf.env "image" with
    Some "off" -> conf.senv <- conf.senv @ ["image", "off"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "long" with
    Some "on" -> conf.senv <- conf.senv @ ["long", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "spouse" with
    Some "on" -> conf.senv <- conf.senv @ ["spouse", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "et" with
    Some x -> conf.senv <- conf.senv @ ["et", x]
  | _ -> ()
  end;
  begin match p_getenv conf.env "cgl" with
    Some "on" -> conf.senv <- conf.senv @ ["cgl", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "bd" with
    None | Some ("0" | "") -> ()
  | Some x -> conf.senv <- conf.senv @ ["bd", x]
  end;
  match p_getenv conf.env "color" with
    Some x -> conf.senv <- conf.senv @ ["color", code_varenv x]
  | _ -> ()

let make_senv conf base =
  let get x = Util.p_getenv conf.env x in
  match get "em", get "ei", get "ep", get "en", get "eoc" with
    Some vm, Some vi, _, _, _ -> set_senv conf vm vi
  | Some vm, None, Some vp, Some vn, voco ->
      let voc =
        match voco with
          Some voc -> (try int_of_string voc with Failure _ -> 0)
        | None -> 0
      in
      let ip =
        match person_of_key base vp vn voc with
          Some ip -> ip
        | None -> incorrect_request conf; raise Exit
      in
      let vi = string_of_int (Adef.int_of_iper ip) in set_senv conf vm vi
  | _ -> ()

let updmenu_print = Perso.interp_templ "updmenu"

let family_m conf base =
  match p_getenv conf.env "m" with
    Some "A" ->
      begin match find_person_in_env conf base "" with
        Some p -> Perso.print_ascend conf base p
      | _ -> very_unknown conf
      end
  | Some "ADD_FAM" when conf.wizard -> UpdateFam.print_add conf base
  | Some "ADD_FAM_OK" when conf.wizard -> UpdateFamOk.print_add conf base
  | Some "ADD_IND" when conf.wizard -> UpdateInd.print_add conf base
  | Some "ADD_IND_OK" when conf.wizard -> UpdateIndOk.print_add conf base
  | Some "ADD_PAR" when conf.wizard -> UpdateFam.print_add_parents conf base
  | Some "ANM" -> Birthday.print_anniversaries conf
  | Some "AN" ->
      begin match p_getenv conf.env "v" with
        Some x -> Birthday.print_birth conf base (int_of_string x)
      | _ -> Birthday.print_menu_birth conf base
      end
  | Some "AD" ->
      begin match p_getenv conf.env "v" with
        Some x -> Birthday.print_dead conf base (int_of_string x)
      | _ -> Birthday.print_menu_dead conf base
      end
  | Some "AM" ->
      begin match p_getenv conf.env "v" with
        Some x -> Birthday.print_marriage conf base (int_of_string x)
      | _ -> Birthday.print_menu_marriage conf base
      end
  | Some "AS_OK" -> AdvSearchOk.print conf base
  | Some "B" when conf.wizard || conf.friend ->
      BirthDeath.print_birth conf base
  | Some "C" ->
      begin match find_person_in_env conf base "" with
        Some p -> Cousins.print conf base p
      | _ -> very_unknown conf
      end
  | Some "CAL" -> Date.print_calendar conf
  | Some "CHG_CHN" when conf.wizard -> ChangeChildren.print conf base
  | Some "CHG_CHN_OK" when conf.wizard -> ChangeChildren.print_ok conf base
  | Some "CHG_EVT_IND_ORD" when conf.wizard ->
      UpdateInd.print_change_event_order conf base
  | Some "CHG_EVT_IND_ORD_OK" when conf.wizard ->
      UpdateIndOk.print_change_event_order conf base
  | Some "CHG_EVT_FAM_ORD" when conf.wizard ->
      UpdateFam.print_change_event_order conf base
  | Some "CHG_EVT_FAM_ORD_OK" when conf.wizard ->
      UpdateFamOk.print_change_event_order conf base
  | Some "CHG_FAM_ORD" when conf.wizard ->
      UpdateFam.print_change_order conf base
  | Some "CHG_FAM_ORD_OK" when conf.wizard ->
      UpdateFamOk.print_change_order_ok conf base
  | Some "CONN_WIZ" when conf.wizard -> Wiznotes.connected_wizards conf base
  | Some "D" ->
      begin match find_person_in_env conf base "" with
        Some p -> Descend.print conf base p
      | _ -> very_unknown conf
      end
  | Some "DAG" -> Dag.print conf base
  | Some "DEL_FAM" when conf.wizard -> UpdateFam.print_del conf base
  | Some "DEL_FAM_OK" when conf.wizard -> UpdateFamOk.print_del conf base
  | Some "DEL_IMAGE" when conf.wizard && conf.can_send_image ->
      SendImage.print_del conf base
  | Some "DEL_IMAGE_OK" when conf.wizard && conf.can_send_image ->
      SendImage.print_del_ok conf base
  | Some "DEL_IND" when conf.wizard -> UpdateInd.print_del conf base
  | Some "DEL_IND_OK" when conf.wizard -> UpdateIndOk.print_del conf base
  | Some "F" ->
      match find_person_in_env conf base "" with
      [ Some p ->   Perso.interp_templ "family" conf base p
      | _ -> very_unknown conf ]
  | Some "FORUM" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print conf base
      end
  | Some "FORUM_ADD" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print_add conf base
      end
  | Some "FORUM_ADD_OK" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print_add_ok conf base
      end
  | Some "FORUM_DEL" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print_del conf base
      end
  | Some "FORUM_P_P" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print_access_switch conf base
      end
  | Some "FORUM_SEARCH" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print_search conf base
      end
  | Some "FORUM_VAL" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print_valid conf base
      end
  | Some "FORUM_VIEW" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print conf base
      end
  | Some "H" ->
      begin match p_getenv conf.env "v" with
        Some f -> Srcfile.print conf base f
      | None -> Hutil.incorrect_request conf
      end
  | Some "HIST" -> History.print conf base
  | Some "HIST_CLEAN" when conf.wizard -> History_diff.print_clean conf base
  | Some "HIST_CLEAN_OK" when conf.wizard ->
      History_diff.print_clean_ok conf base
  | Some "HIST_DIFF" -> History_diff.print conf base
  | Some "HIST_SEARCH" -> History.print_search conf base
  | Some "IMH" -> Image.print_html conf base
  | Some "INV_FAM" when conf.wizard -> UpdateFam.print_inv conf base
  | Some "INV_FAM_OK" when conf.wizard -> UpdateFamOk.print_inv conf base
  | Some "KILL_ANC" when conf.wizard ->
      MergeInd.print_kill_ancestors conf base
  | Some "LB" when conf.wizard || conf.friend ->
      BirthDeath.print_birth conf base
  | Some "LD" when conf.wizard || conf.friend ->
      BirthDeath.print_death conf base
  | Some "LINKED" ->
      begin match find_person_in_env conf base "" with
        Some p -> Perso.print_what_links conf base p
      | _ -> very_unknown conf
      end
  | Some "LL" -> BirthDeath.print_longest_lived conf base
  | Some "LM" when conf.wizard || conf.friend ->
      BirthDeath.print_marriage conf base
  | Some "LEX" -> Srcfile.print_lexicon conf base
  | Some "MISC_NOTES" -> Notes.print_misc_notes conf base
  | Some "MISC_NOTES_SEARCH" -> Notes.print_misc_notes_search conf base
  | Some "MOD_DATA" when conf.wizard -> UpdateData.print_mod conf base
  | Some "MOD_DATA_OK" when conf.wizard -> UpdateData.print_mod_ok conf base
  | Some "MOD_FAM" when conf.wizard -> UpdateFam.print_mod conf base
  | Some "MOD_FAM_OK" when conf.wizard -> UpdateFamOk.print_mod conf base
  | Some "MOD_IND" when conf.wizard -> UpdateInd.print_mod conf base
  | Some "MOD_IND_OK" when conf.wizard -> UpdateIndOk.print_mod conf base
  | Some "MOD_NOTES" when conf.wizard -> Notes.print_mod conf base
  | Some "MOD_NOTES_OK" when conf.wizard -> Notes.print_mod_ok conf base
  | Some "MOD_WIZNOTES" when conf.authorized_wizards_notes ->
      Wiznotes.print_mod conf base
  | Some "MOD_WIZNOTES_OK" when conf.authorized_wizards_notes ->
      Wiznotes.print_mod_ok conf base
  | Some "MRG" when conf.wizard ->
      begin match find_person_in_env conf base "" with
        Some p -> Merge.print conf base p
      | _ -> very_unknown conf
      end
  | Some "MRG_DUP" when conf.wizard -> MergeDup.main_page conf base
  | Some "MRG_DUP_IND_Y_N" when conf.wizard -> MergeDup.answ_ind_y_n conf base
  | Some "MRG_DUP_FAM_Y_N" when conf.wizard -> MergeDup.answ_fam_y_n conf base
  | Some "MRG_FAM" when conf.wizard -> MergeFam.print conf base
  | Some "MRG_FAM_OK" when conf.wizard -> MergeFamOk.print_merge conf base
  | Some "MRG_MOD_FAM_OK" when conf.wizard ->
      MergeFamOk.print_mod_merge conf base
  | Some "MRG_IND" when conf.wizard -> MergeInd.print conf base
  | Some "MRG_IND_OK" when conf.wizard -> MergeIndOk.print_merge conf base
  | Some "MRG_MOD_IND_OK" when conf.wizard ->
      MergeIndOk.print_mod_merge conf base
  | Some "N" ->
      begin match p_getenv conf.env "v" with
        Some v -> Some.surname_print conf base Some.surname_not_found v
      | _ -> Alln.print_surnames conf base
      end
  | Some "NG" ->
      (* Rétro-compatibilité <= 6.06 *)
      let env =
        match p_getenv conf.env "n" with
          Some n ->
            begin match p_getenv conf.env "t" with
              Some "P" -> ("fn", n) :: conf.env
            | Some "N" -> ("sn", n) :: conf.env
            | _ -> ("v", n) :: conf.env
            end
        | None -> conf.env
      in
      let conf = {conf with env = env} in
      (* Nouveau mode de recherche. *)
      begin match p_getenv conf.env "select" with
        Some "input" | None ->
          (* Récupère le contenu non vide de la recherche. *)
          let real_input label =
            match p_getenv conf.env label with
              Some s -> if s = "" then None else Some s
            | None -> None
          in
          (* Recherche par clé, sosa, alias ... *)
          let search n =
            let (pl, sosa_acc) = find_all conf base n in
            match pl with
              [] ->
                conf.cancel_links <- false;
                Some.surname_print conf base unknown n
            | [p] ->
                if sosa_acc || Gutil.person_of_string_key base n <> None ||
                   person_is_std_key conf base p n
                then
                  person_selected_with_redirect conf base p
                else specify conf base n pl
            | pl -> specify conf base n pl
          in
          begin match real_input "v" with
            Some n -> search n
          | None ->
              match real_input "fn", real_input "sn" with
                Some fn, Some sn -> search (fn ^ " " ^ sn)
              | Some fn, None ->
                  conf.cancel_links <- false;
                  Some.first_name_print conf base fn
              | None, Some sn ->
                  conf.cancel_links <- false;
                  Some.surname_print conf base unknown sn
              | None, None -> incorrect_request conf
          end
      | Some i ->
          relation_print conf base
            (pget conf base (Adef.iper_of_int (int_of_string i)))
      end
  | Some "NOTES" -> Notes.print conf base
  | Some "OA" when conf.wizard || conf.friend ->
      BirthDeath.print_oldest_alive conf base
  | Some "OE" when conf.wizard || conf.friend ->
      BirthDeath.print_oldest_engagements conf base
  | Some "P" ->
      begin match p_getenv conf.env "v" with
        Some v -> Some.first_name_print conf base v
      | None -> Alln.print_first_names conf base
      end
  | Some "POP_PYR" when conf.wizard || conf.friend ->
      BirthDeath.print_population_pyramid conf base
  | Some "PS" -> Place.print_all_places_surnames conf base
  | Some "R" ->
      begin match find_person_in_env conf base "" with
        Some p -> relation_print conf base p
      | _ -> very_unknown conf
      end
  | Some "REQUEST" when conf.wizard ->
      let title _ = () in
      header conf title;
      Wserver.printf "<pre>\n";
      List.iter (Wserver.printf "%s\n") conf.request;
      Wserver.printf "</pre>\n";
      trailer conf
  | Some "RL" -> RelationLink.print conf base
  | Some "RLM" -> Relation.print_multi conf base
  | Some "S" -> SearchName.print conf base specify unknown
  | Some "SND_IMAGE" when conf.wizard && conf.can_send_image ->
      SendImage.print conf base
  | Some "SND_IMAGE_OK" when conf.wizard && conf.can_send_image ->
      SendImage.print_send_ok conf base
  | Some "SRC" ->
      begin match p_getenv conf.env "v" with
        Some f -> Srcfile.print_source conf base f
      | _ -> Hutil.incorrect_request conf
      end
  | Some "STAT" -> BirthDeath.print_statistics conf base
  | Some "CHANGE_WIZ_VIS" when conf.wizard ->
      Wiznotes.change_wizard_visibility conf base
  | Some "TT" -> Title.print conf base
  | Some "U" when conf.wizard ->
      begin match find_person_in_env conf base "" with
        Some p -> updmenu_print conf base p
      | _ -> very_unknown conf
      end
  | Some "VIEW_WIZNOTES" when conf.wizard && conf.authorized_wizards_notes ->
      Wiznotes.print_view conf base
  | Some "WIZNOTES" when conf.authorized_wizards_notes ->
      Wiznotes.print conf base
  | Some "WIZNOTES_SEARCH" when conf.authorized_wizards_notes ->
      Wiznotes.print_search conf base
  | Some mode
    when
      try
        String.get mode 0 = 'A' && String.get mode 1 = 'P' &&
        String.get mode 2 = 'I' && String.get mode 3 = '_'
      with _ -> false ->
      (* On passe en mode API, i.e. que les exceptions API sont levées. *)
      let () = Api_conf.set_mode_api () in
      begin match Some mode with
        Some "API_ALL_PERSONS" -> Api.print_all_persons conf base
      | Some "API_ALL_FAMILIES" -> Api.print_all_families conf base
      | Some "API_ANNIVERSARY" -> Api.print_birthday conf base
      | Some "API_BASE_WARNINGS" when conf.wizard || conf.friend ->
          (* Pour les flex, on autorise en mode friend. *)
          Api.print_base_warnings conf base
      | Some "API_CLOSE_PERSONS" ->
          Api_graph.print_close_person_relations conf base
      | Some "API_CPL_REL" -> Api_graph.print_cpl_relation conf base
      | Some "API_GRAPH_ASC" -> Api_graph.print_graph_asc conf base
      | Some "API_GRAPH_ASC_LIA" -> Api_graph.print_graph_asc_lia conf base
      | Some "API_GRAPH_DESC" -> Api_graph.print_graph_desc conf base
      | Some "API_GRAPH_REL" -> Api_graph.print_graph_rel conf base
      | Some "API_FIRST_AVAILABLE_PERSON" ->
          Api.print_first_available_person conf base
      | Some "API_FIND_SOSA" -> Api.print_find_sosa conf base
      | Some "API_INFO_BASE" -> Api.print_info_base conf base
      | Some "API_INFO_IND" -> Api.print_info_ind conf base
      | Some "API_IMAGE" -> Api.print_img conf base
      | Some "API_IMAGE_EXT" -> Api.print_img_ext conf base
      | Some "API_IMAGE_ALL" -> Api.print_img_all conf base
      | Some "API_IMAGE_PERSON" -> Api.print_img_person conf base
      | Some "API_IMAGE_UPDATE" when conf.wizard ->
          Api.print_updt_image conf base
      | Some "API_LAST_MODIFIED_PERSONS" ->
          Api.print_last_modified_persons conf base
      | Some "API_LAST_VISITED_PERSONS" ->
          Api.print_last_visited_persons conf base
      | Some "API_LIST_PERSONS" -> Api.print_list_ref_person conf base
      | Some "API_LOOP_BASE" -> Api.print_loop conf base
      | Some "API_MAX_ANCESTORS" when conf.wizard ->
          Api.print_max_ancestors conf base
      | Some "API_NB_ANCESTORS" ->
          Api_saisie_read.print_nb_ancestors conf base
      | Some "API_NOTIFICATION_BIRTHDAY" ->
          Api.print_notification_birthday conf base
      | Some "API_PRINT_INDEX" -> Api.print_all_full_person conf base
      | Some "API_PRINT_EXPORT" -> Api.print_export conf base
      | Some "API_PRINT_EXPORT_SEARCH" -> Api.print_export_search conf base
      | Some "API_PRINT_SYNCHRO" -> Api.print_synchro_patch_mobile conf base
      | Some "API_REF_PERSON_FROM_ID" ->
          Api.print_ref_person_from_ip conf base
      | Some "API_REMOVE_IMAGE_EXT" when conf.wizard ->
          Api.print_remove_image_ext conf base
      | Some "API_REMOVE_IMAGE_EXT_ALL" when conf.wizard ->
          Api.print_remove_image_ext_all conf base
      | Some "API_SEARCH" -> Api_search.print_search conf base
      | Some "API_GRAPH_TREE_V2" ->
          Api_saisie_read.print_graph_tree_v2 conf base
      | Some "API_PERSON_TREE" -> Api_saisie_read.print_person_tree conf base
      | Some "API_FICHE_PERSON" ->
          Api_saisie_read.print_fiche_person conf base
      | Some "API_AUTO_COMPLETE" when conf.wizard ->
          Api_saisie_write.print_auto_complete conf base
      | Some "API_GET_CONFIG" when conf.wizard ->
          Api_saisie_write.print_config conf base
      | Some "API_PERSON_SEARCH_LIST" when conf.wizard ->
          Api_saisie_write.print_person_search_list conf base
      | Some "API_GET_PERSON_SEARCH_INFO" when conf.wizard ->
          Api_saisie_write.print_person_search_info conf base
      | Some "API_ADD_CHILD" when conf.wizard ->
          Api_saisie_write.print_add_child conf base
      | Some "API_ADD_CHILD_OK" when conf.wizard ->
          Api_saisie_write.print_add_child_ok conf base
      | Some "API_ADD_FAMILY" when conf.wizard ->
          Api_saisie_write.print_add_family conf base
      | Some "API_ADD_FAMILY_OK" when conf.wizard ->
          Api_saisie_write.print_add_family_ok conf base
      | Some "API_ADD_FIRST_FAM_OK" when conf.wizard ->
          Api_saisie_write.print_add_first_fam_ok conf base
      | Some "API_ADD_PARENTS" when conf.wizard ->
          Api_saisie_write.print_add_parents conf base
      | Some "API_ADD_PARENTS_OK" when conf.wizard ->
          Api_saisie_write.print_add_parents_ok conf base
      | Some "API_ADD_PERSON_OK" when conf.wizard ->
          Api_saisie_write.print_add_ind_ok conf base
      | Some "API_ADD_PERSON_START_OK" when conf.wizard ->
          Api_saisie_write.print_add_ind_start_ok conf base
      | Some "API_ADD_SIBLING" when conf.wizard ->
          Api_saisie_write.print_add_sibling conf base
      | Some "API_ADD_SIBLING_OK" when conf.wizard ->
          Api_saisie_write.print_add_sibling_ok conf base
      | Some "API_EDIT_FAMILY_REQUEST" when conf.wizard ->
          Api_saisie_write.print_mod_family_request conf base
      | Some "API_EDIT_FAMILY" when conf.wizard ->
          Api_saisie_write.print_mod_family conf base
      | Some "API_EDIT_FAMILY_OK" when conf.wizard ->
          Api_saisie_write.print_mod_family_ok conf base
      | Some "API_EDIT_PERSON" when conf.wizard ->
          Api_saisie_write.print_mod_ind conf base
      | Some "API_EDIT_PERSON_OK" when conf.wizard ->
          Api_saisie_write.print_mod_ind_ok conf base
      | Some "API_DEL_FAMILY_OK" when conf.wizard ->
          Api_saisie_write.print_del_fam_ok conf base
      | Some "API_DEL_PERSON_OK" when conf.wizard ->
          Api_saisie_write.print_del_ind_ok conf base
      | Some "API_LINK_TREE" -> Api_link.print_link_tree conf base
      | Some "API_STATS" -> Api_stats.print_stats conf base
      | Some mode -> ()
      | None -> ()
      end
  | Some mode -> incorrect_request conf
  | None ->
      match find_person_in_env conf base "" with
        Some p -> person_selected conf base p
      | _ -> very_unknown conf

let print_no_index conf base =
  let title _ =
    Wserver.printf "%s" (Util.capitale (transl conf "link to use"))
  in
  let link = url_no_index conf base in
  header conf title;
  Wserver.printf "<ul>\n";
  html_li conf;
  Wserver.printf "<a href=\"http://%s\">\n" link;
  Wserver.printf "%s" link;
  Wserver.printf "</a>\n";
  Wserver.printf "</ul>\n";
  print_link_to_welcome conf false;
  trailer conf

let family_m_nobase conf =
  (* On passe en mode API, i.e. que les exceptions API sont levées. *)
  let () = Api_conf.set_mode_api () in
  match p_getenv conf.env "m" with
    Some "API_ADD_FIRST_FAM" -> Api_saisie_write.print_add_first_fam conf
  | Some mode -> ()
  | None -> ()

let special_vars =
  ["alwsurn"; "cgl"; "dsrc"; "em"; "ei"; "ep"; "en"; "eoc"; "escache"; "et";
   "iz"; "log_cnl"; "log_pwd"; "log_uid"; "long"; "manitou"; "nz"; "ocz";
   "pz"; "pure_xhtml"; "size"; "spouse"; "templ"; "p_mod"; "wide"]

let only_special_env = List.for_all (fun (x, _) -> List.mem x special_vars)

let extract_henv conf base =
  begin match find_sosa_ref conf base with
    Some p ->
      let x =
        let first_name = p_first_name base p in
        let surname = p_surname base p in
        if Util.accessible_by_key conf base p first_name surname then
          ["pz", code_varenv (Name.lower first_name);
           "nz", code_varenv (Name.lower surname);
           "ocz", string_of_int (get_occ p)]
        else ["iz", string_of_int (Adef.int_of_iper (get_key_index p))]
      in
      conf.henv <- conf.henv @ x
  | None -> ()
  end;
  begin match p_getenv conf.env "dsrc" with
    Some "" | None -> ()
  | Some s -> conf.henv <- conf.henv @ ["dsrc", code_varenv s]
  end;
  begin match p_getenv conf.env "templ" with
    None -> ()
  | Some s -> conf.henv <- conf.henv @ ["templ", code_varenv s]
  end;
  begin match p_getenv conf.env "escache" with
    Some _ ->
      let v = escache_value base in conf.henv <- conf.henv @ ["escache", v]
  | None -> ()
  end;
  begin match p_getenv conf.env "alwsurn" with
    Some x -> conf.henv <- conf.henv @ ["alwsurn", x]
  | None -> ()
  end;
  begin match p_getenv conf.env "pure_xhtml" with
    Some x -> conf.henv <- conf.henv @ ["pure_xhtml", x]
  | None -> ()
  end;
  begin match p_getenv conf.env "size" with
    Some x -> conf.henv <- conf.henv @ ["size", x]
  | None -> ()
  end;
  begin match p_getenv conf.env "manitou" with
    Some "off" -> conf.henv <- conf.henv @ ["manitou", "off"]
  | Some _ | None -> ()
  end;
  begin match p_getenv conf.env "p_mod" with
    Some x -> conf.henv <- conf.henv @ ["p_mod", x]
  | None -> ()
  end;
  begin match p_getenv conf.env "wide" with
      Some x -> conf.henv <- conf.henv @ [("wide", x)]
    | None -> ()
  end

let set_owner conf =
  if Sys.unix then
    let s = Unix.stat (Util.base_path [] (conf.bname ^ ".gwb")) in
    try Unix.setgid s.Unix.st_gid; Unix.setuid s.Unix.st_uid with
      Unix.Unix_error (_, _, _) -> ()

let thousand oc x = Sosa.print (output_string oc) "," (Sosa.of_int x)

let log_count conf r =
  match r with
    Some (welcome_cnt, request_cnt, start_date) ->
      Log.with_log
        (fun oc ->
           Printf.fprintf oc "  #accesses %a (#welcome %a) since %s\n"
             thousand (welcome_cnt + request_cnt) thousand welcome_cnt
             start_date)
  | None -> ()

let print_moved conf base s =
  match Util.open_etc_file "moved" with
    Some ic ->
      let env = ["bname", conf.bname] in
      let conf = {conf with is_printed_by_template = false} in
      Util.html conf; Templ.copy_from_templ conf env ic
  | None ->
      let title _ = Wserver.printf "%s -&gt; %s" conf.bname s in
      Hutil.header_no_page_title conf title;
      Wserver.printf "The database %s has moved to:\n<dl><dt><dd>\n"
        conf.bname;
      Wserver.printf "<a href=\"%s\">" s;
      Wserver.printf "%s" s;
      Wserver.printf "</a>";
      Wserver.printf "\n</dd></dt></dl>\n";
      Hutil.trailer conf

let cnt_trace = ref 50
let trace_keys base (fn, sn, occ) ipo =
  if !cnt_trace < 0 then ()
  else
    match ipo with
      None ->
        Printf.eprintf "(\"%s\", \"%s\", \"%d\") deleted\n" fn sn occ;
        flush stderr
    | Some ip ->
        decr cnt_trace;
        let p = poi base ip in
        let fn1 = sou base (get_first_name p) in
        let sn1 = sou base (get_surname p) in
        let occ1 = get_occ p in
        if Name.lower (Mutil.nominative fn1) = fn &&
           Name.lower (Mutil.nominative sn1) = sn && occ1 = occ
        then
          begin
            Printf.eprintf "(\"%s\", \"%s\", \"%d\") ok\n" fn sn occ;
            flush stderr
          end
        else
          begin
            Printf.eprintf
              "Error %s.%d %s with key = (\"%s\", \"%s\", \"%d\")\n" fn1 occ1
              sn1 fn sn occ;
            flush stderr
          end

let treat_request conf base =
  begin match
    p_getenv conf.base_env "moved", p_getenv conf.env "opt",
    p_getenv conf.env "m"
  with
    Some s, _, _ -> print_moved conf base s
  | _, Some "no_index", _ -> print_no_index conf base
  | _, _, Some "IM" -> Image.print conf base
  | _, _, Some "DOC" ->
      begin match p_getenv conf.env "s" with
        Some f ->
          if Filename.check_suffix f ".txt" then
            let f = Filename.chop_suffix f ".txt" in
            Srcfile.print_source conf base f
          else Image.print conf base
      | None -> incorrect_request conf
      end
  | _ ->
      set_owner conf;
      extract_henv conf base;
      make_senv conf base;
      let conf =
        match Util.default_sosa_ref conf base with
          Some p -> {conf with default_sosa_ref = get_key_index p, Some p}
        | None -> conf
      in
      if only_special_env conf.env then
        begin
          begin match p_getenv conf.base_env "counter" with
            Some "no" -> ()
          | _ -> let r = Srcfile.incr_welcome_counter conf in log_count conf r
          end;
          Srcfile.print_start conf base
        end
      else
        begin
          begin match p_getenv conf.base_env "counter" with
            Some "no" -> ()
          | _ -> let r = Srcfile.incr_request_counter conf in log_count conf r
          end;
          match p_getenv conf.env "ptempl" with
            Some tname when p_getenv conf.base_env "ptempl" = Some "yes" ->
              begin match find_person_in_env conf base "" with
                Some p -> Perso.interp_templ tname conf base p
              | None -> family_m conf base
              end
          | _ -> family_m conf base
        end
  end;
  Wserver.wflush ()

let treat_request_on_possibly_locked_base conf bfile =
  match try Left (Gwdb.open_base bfile) with e -> Right e with
    Left base ->
      if !(Mutil.utf_8_db) then ()
      else
        begin
          Hashtbl.clear conf.lexicon;
          let fname = Filename.concat "lang" "lexicon.txt" in
          Mutil.input_lexicon conf.lang conf.lexicon
            (fun () -> Secure.open_in (Util.search_in_lang_path fname));
          conf.charset <-
            try Hashtbl.find conf.lexicon " !charset" with
              Not_found -> "iso-8859-1"
        end;
      (try treat_request conf base with exc -> close_base base; raise exc);
      close_base base
  | Right e ->
      let transl conf w =
        try Hashtbl.find conf.lexicon w with Not_found -> "[" ^ w ^ "]"
      in
      let title _ =
        Wserver.printf "%s" (Util.capitale (transl conf "error"))
      in
      Hutil.rheader conf title;
      Wserver.printf "<ul>";
      Util.html_li conf;
      Wserver.printf "%s" (Util.capitale (transl conf "cannot access base"));
      Wserver.printf " \"%s\".</ul>\n" conf.bname;
      begin match e with
        Sys_error _ -> ()
      | _ ->
          Wserver.printf
            "<em><font size=\"-1\">Internal message: %s</font></em>\n"
            (Printexc.to_string e)
      end;
      Hutil.trailer conf

let this_request_updates_database conf =
  match p_getenv conf.env "m" with
    Some ("FORUM_ADD_OK" | "FORUM_DEL" | "FORUM_VAL") -> true
  | Some "API_PRINT_SYNCHRO" -> true
  | Some x when conf.wizard ->
      begin match x with
        "ADD_FAM_OK" | "ADD_IND_OK" | "CHANGE_WIZ_VIS" | "CHG_CHN_OK" |
        "CHG_EVT_IND_ORD_OK" | "CHG_EVT_FAM_ORD_OK" | "CHG_FAM_ORD_OK" |
        "DEL_FAM_OK" | "DEL_IMAGE_OK" | "DEL_IND_OK" | "INV_FAM_OK" |
        "KILL_ANC" | "MOD_FAM_OK" | "MOD_IND_OK" | "MOD_NOTES_OK" |
        "MOD_WIZNOTES_OK" | "MRG_DUP_IND_Y_N" | "MRG_DUP_FAM_Y_N" |
        "MRG_IND" | "MRG_MOD_FAM_OK" | "MRG_MOD_IND_OK" | "MOD_DATA_OK" |
        "SND_IMAGE_OK" ->
          true
      | "API_BASE_WARNINGS" | "API_IMAGE_UPDATE" | "API_REMOVE_IMAGE_EXT" |
        "API_REMOVE_IMAGE_EXT_ALL" | "API_DEL_PERSON_OK" |
        "API_EDIT_PERSON_OK" | "API_ADD_CHILD_OK" | "API_ADD_PERSON_OK" |
        "API_ADD_PARENTS_OK" | "API_ADD_FAMILY_OK" | "API_ADD_FIRST_FAM_OK" |
        "API_EDIT_FAMILY_OK" | "API_DEL_FAMILY_OK" | "API_ADD_SIBLING_OK" |
        "API_ADD_PERSON_START_OK" | "API_PRINT_SYNCHRO" ->
          true
      | _ -> false
      end
  | _ -> false

let treat_request_on_base conf =
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  if this_request_updates_database conf then
    match
      Lock.control (Mutil.lock_file bfile) false
        (fun () -> treat_request_on_possibly_locked_base conf bfile)
    with
      Some x -> x
    | None -> Update.error_locked conf
  else treat_request_on_possibly_locked_base conf bfile

let treat_request_on_nobase conf =
  if !(Mutil.utf_8_db) then ()
  else
    begin
      Hashtbl.clear conf.lexicon;
      let fname = Filename.concat "lang" "lexicon.txt" in
      Mutil.input_lexicon conf.lang conf.lexicon
        (fun () -> Secure.open_in (Util.search_in_lang_path fname));
      conf.charset <-
        try Hashtbl.find conf.lexicon " !charset" with
          Not_found -> "iso-8859-1"
    end;
  try family_m_nobase conf; Wserver.wflush () with exc -> raise exc
