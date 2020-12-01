(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Config
open Def
open Gwdb
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

let name_with_roman_number str =
  let rec loop found len i =
    if i = String.length str then if found then Some (Buff.get len) else None
    else
      match str.[i] with
      |  '0'..'9' as c ->
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

let compact_list base xl =
  let pl = Gutil.sort_person_list base xl in
  List.fold_right
    (fun p pl ->
       match pl with
         p1 :: _ when get_iper p = get_iper p1 -> pl
       | _ -> p :: pl)
    pl []

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
  match String.index_opt n1 ' ' with
    Some i ->
    let fn = String.sub n1 0 i in
    let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
    let (list, _) =
      Some.persons_of_fsname conf base base_strings_of_surname
        (spi_find (persons_of_surname base)) get_surname sn
    in
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
  | None -> []

let find_all conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match sosa_ref, sosa_nb with
    Some p, Some n ->
    if n <> Sosa.zero then
      match Util.branch_of_sosa conf base n p with
        Some (p :: _) -> [p], true
      | _ -> [], false
    else [], false
  | _ ->
    match Gutil.person_of_string_key base an with
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
      let ipl = Gutil.person_not_a_key_find_all base an in
      let (an, ipl) =
        if ipl = [] then
          match name_with_roman_number an with
            Some an1 ->
            let ipl = Gutil.person_ht_find_all base an1 in
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
      compact_list base pl, false

let relation_print conf base p =
  let p1 =
    match p_getenv conf.senv "ei" with
    | Some i ->
      conf.senv <- [];
      (* if i >= 0 && i < nb_of_persons base then *)
      Some (pget conf base (iper_of_string i))
    (* else None *)
    | None ->
      match find_person_in_env conf base "1" with
        Some p1 -> conf.senv <- []; Some p1
      | None -> None
  in
  RelationDisplay.print conf base p p1

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
  Hutil.header conf title;
  conf.cancel_links <- false;
  Hutil.print_link_to_welcome conf true;
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
           Wserver.print_string (titled_person_text conf base p t);
           Wserver.printf "</a>\n";
           List.iter
             (fun t -> Wserver.print_string (one_title_text base t)) tl
       end;
       Wserver.print_string (DateDisplay.short_dates_text conf base p);
       if authorized_age conf base p then
         begin match get_first_names_aliases p with
             [] -> ()
           | fnal ->
             Wserver.printf "\n<em>(";
             Mutil.list_iter_first
               (fun first fna ->
                  if not first then Wserver.printf ", ";
                  Wserver.print_string (sou base fna))
               fnal;
             Wserver.printf ")</em>"
         end;
       begin let spouses =
               Array.fold_right
                 (fun ifam spouses ->
                    let cpl = foi base ifam in
                    let spouse = pget conf base (Gutil.spouse (get_iper p) cpl) in
                    if p_surname base spouse <> "?" then spouse :: spouses
                    else spouses)
                 (get_family p) []
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
  Hutil.trailer conf

let incorrect_request conf = Hutil.incorrect_request conf

let person_selected conf base p =
  match p_getenv conf.senv "em" with
    Some "R" -> relation_print conf base p
  | Some _ -> incorrect_request conf
  | None -> record_visited conf (get_iper p); Perso.print conf base p

let person_selected_with_redirect conf base p =
  match p_getenv conf.senv "em" with
    Some "R" -> relation_print conf base p
  | Some _ -> incorrect_request conf
  | None ->
    Wserver.http_redirect_temporarily (commd conf ^ Util.acces conf base p)

let updmenu_print = Perso.interp_templ "updmenu"

let enabled_forum conf = p_getenv conf.base_env "disable_forum" <> Some "yes"

let very_unknown conf base =
  match p_getenv conf.env "n", p_getenv conf.env "p" with
  | Some sname, Some fname ->
    let title _ =
      Wserver.printf "%s: \"%s %s\"" (Utf8.capitalize (transl conf "not found"))
        (Util.escape_html fname) (Util.escape_html sname)
    in
    Wserver.http Wserver.Not_Found;
    Hutil.rheader conf title;
    Hutil.print_link_to_welcome conf false;
    Hutil.trailer conf
  | _ -> incorrect_request conf

let unknown = begin fun conf n ->
      let title _ =
        Wserver.printf "%s: \"%s\"" (Utf8.capitalize (transl conf "not found"))
          (Util.escape_html n)
      in
      Wserver.http Wserver.Not_Found;
      Hutil.rheader conf title;
      Hutil.print_link_to_welcome conf false;
      Hutil.trailer conf
    end

(* Make the "special" environement em ei *)
let set_senv conf vm vi =
  let aux k v =
    if p_getenv conf.env k = Some v
    then conf.senv <- conf.senv @ [k,v]
  in
  conf.senv <- ["em", vm; "ei", vi];
  aux "image" "off";
  aux "long" "on";
  aux "spouse" "on";
  begin match p_getenv conf.env "et" with
    | Some x -> conf.senv <- conf.senv @ ["et", x]
    | _ -> ()
  end;
  aux "cgl" "on";
  begin match p_getenv conf.env "bd" with
    | None | Some ("0" | "") -> ()
    | Some x -> conf.senv <- conf.senv @ ["bd", x]
  end;
  match p_getenv conf.env "color" with
  | Some x -> conf.senv <- conf.senv @ ["color", code_varenv x]
  | _ -> ()

let make_senv conf base =
  let get x = Util.p_getenv conf.env x in
  match get "em", get "ei", get "ep", get "en", get "eoc" with
  | Some vm, Some vi, _, _, _ -> set_senv conf vm vi
  | Some vm, None, Some vp, Some vn, voco ->
    let voc =
      match voco with
      | Some voc -> (try int_of_string voc with Failure _ -> 0)
      | None -> 0
    in
    let ip =
      match person_of_key base vp vn voc with
      | Some ip -> ip
      | None -> Hutil.incorrect_request conf; raise Exit
    in
    let vi = string_of_iper ip in set_senv conf vm vi
  | _ -> ()

let try_plugin conf base m =
  match List.assoc_opt "plugins" conf.Config.base_env with
  | None -> false
  | Some list ->
    let list = String.split_on_char ',' list in
    List.exists (fun (ns, fn) -> List.mem ns list && fn conf base) (GwdPlugin.get m)

let w_person conf base fn =
  match find_person_in_env conf base "" with
  | Some p -> fn p
  | _ -> very_unknown conf base

[@@@ocaml.warning "-45"]
let family_m conf base =
#ifdef DEBUG
  Mutil.bench __LOC__ @@ fun () ->
#endif
  if conf.wizard
  || conf.friend
  || List.assoc_opt "visitor_access" conf.base_env <> Some "no"
  then begin
    let m = Opt.default "" @@ p_getenv conf.env "m" in
    if not @@ try_plugin conf base m
    then match m with
      | "" -> w_person conf base @@ person_selected conf base
      | "A" -> w_person conf base @@ Perso.print_ascend conf base
      | "ADD_FAM" when conf.wizard -> UpdateFam.print_add conf base
      | "ADD_FAM_OK" when conf.wizard -> UpdateFamOk.print_add conf base
      | "ADD_IND" when conf.wizard -> UpdateInd.print_add conf base
      | "ADD_IND_OK" when conf.wizard -> UpdateIndOk.print_add conf base
      | "ADD_PAR" when conf.wizard -> UpdateFam.print_add_parents conf base
      | "ADD_PAR_OK" when conf.wizard -> UpdateFamOk.print_add_parents conf base
      | "ANM" -> BirthdayDisplay.print_anniversaries conf
      | "AN" -> begin match p_getenv conf.env "v" with
          | Some x -> BirthdayDisplay.print_birth conf base (int_of_string x)
          | _ -> BirthdayDisplay.print_menu_birth conf base
        end
      | "AD" -> begin match p_getenv conf.env "v" with
          | Some x -> BirthdayDisplay.print_dead conf base (int_of_string x)
          | _ -> BirthdayDisplay.print_menu_dead conf base
        end
      | "AM" -> begin match p_getenv conf.env "v" with
          | Some x -> BirthdayDisplay.print_marriage conf base (int_of_string x)
          | _ -> BirthdayDisplay.print_menu_marriage conf base
        end
      | "AS_OK" -> AdvSearchOkDisplay.print conf base
      | "C" -> w_person conf base @@ CousinsDisplay.print conf base
      | "CAL" -> Hutil.print_calendar conf
      | "CHG_CHN" when conf.wizard -> ChangeChildrenDisplay.print conf base
      | "CHG_CHN_OK" when conf.wizard -> ChangeChildrenDisplay.print_ok conf base
      | "CHG_EVT_IND_ORD" when conf.wizard -> UpdateInd.print_change_event_order conf base
      | "CHG_EVT_IND_ORD_OK" when conf.wizard -> UpdateIndOk.print_change_event_order conf base
      | "CHG_EVT_FAM_ORD" when conf.wizard -> UpdateFam.print_change_event_order conf base
      | "CHG_EVT_FAM_ORD_OK" when conf.wizard -> UpdateFamOk.print_change_event_order conf base
      | "CHG_FAM_ORD" when conf.wizard -> UpdateFam.print_change_order conf base
      | "CHG_FAM_ORD_OK" when conf.wizard -> UpdateFamOk.print_change_order_ok conf base
      | "CONN_WIZ" when conf.wizard -> WiznotesDisplay.connected_wizards conf base
      | "D" -> w_person conf base @@ DescendDisplay.print conf base
      | "DAG" -> DagDisplay.print conf base
      | "DEL_FAM" when conf.wizard -> UpdateFam.print_del conf base
      | "DEL_FAM_OK" when conf.wizard -> UpdateFamOk.print_del conf base
      | "DEL_IMAGE" when conf.wizard && conf.can_send_image -> SendImage.print_del conf base
      | "DEL_IMAGE_OK" when conf.wizard && conf.can_send_image -> SendImage.print_del_ok conf base
      | "DEL_IND" when conf.wizard -> UpdateInd.print_del conf base
      | "DEL_IND_OK" when conf.wizard -> UpdateIndOk.print_del conf base
      | "F" -> w_person conf base @@ Perso.interp_templ "family" conf base
      | "FORUM" when enabled_forum conf -> ForumDisplay.print conf base
      | "FORUM_ADD" when enabled_forum conf -> ForumDisplay.print_add conf base
      | "FORUM_ADD_OK" when enabled_forum conf -> ForumDisplay.print_add_ok conf base
      | "FORUM_DEL" when enabled_forum conf -> ForumDisplay.print_del conf base
      | "FORUM_P_P" when enabled_forum conf -> ForumDisplay.print_access_switch conf base
      | "FORUM_SEARCH" when enabled_forum conf -> ForumDisplay.print_search conf base
      | "FORUM_VAL" when enabled_forum conf -> ForumDisplay.print_valid conf base
      | "FORUM_VIEW" when enabled_forum conf -> ForumDisplay.print conf base
      | "H" -> begin match p_getenv conf.env "v" with
          | Some f -> SrcfileDisplay.print conf base f
          | None -> Hutil.incorrect_request conf
        end
      | "HIST" -> History.print conf base
      | "HIST_CLEAN" when conf.wizard -> HistoryDiffDisplay.print_clean conf
      | "HIST_CLEAN_OK" when conf.wizard -> HistoryDiffDisplay.print_clean_ok conf
      | "HIST_DIFF" -> HistoryDiffDisplay.print conf base
      | "HIST_SEARCH" -> History.print_search conf base
      | "IMH" -> ImageDisplay.print_html conf
      | "INV_FAM" when conf.wizard -> UpdateFam.print_inv conf base
      | "INV_FAM_OK" when conf.wizard -> UpdateFamOk.print_inv conf base
      | "KILL_ANC" when conf.wizard -> MergeIndDisplay.print_kill_ancestors conf base
      | "LB" when conf.wizard || conf.friend -> BirthDeathDisplay.print_birth conf base
      | "LD" when conf.wizard || conf.friend -> BirthDeathDisplay.print_death conf base
      | "LINKED" -> w_person conf base @@ Perso.print_what_links conf base
      | "LL" -> BirthDeathDisplay.print_longest_lived conf base
      | "LM" when conf.wizard || conf.friend -> BirthDeathDisplay.print_marriage conf base
      | "MISC_NOTES" -> NotesDisplay.print_misc_notes conf base
      | "MISC_NOTES_SEARCH" -> NotesDisplay.print_misc_notes_search conf base
      | "MOD_DATA" when conf.wizard -> UpdateDataDisplay.print_mod conf base
      | "MOD_DATA_OK" when conf.wizard -> UpdateDataDisplay.print_mod_ok conf base
      | "MOD_FAM" when conf.wizard -> UpdateFam.print_mod conf base
      | "MOD_FAM_OK" when conf.wizard -> UpdateFamOk.print_mod conf base
      | "MOD_IND" when conf.wizard -> UpdateInd.print_mod conf base
      | "MOD_IND_OK" when conf.wizard -> UpdateIndOk.print_mod conf base
      | "MOD_NOTES" when conf.wizard -> NotesDisplay.print_mod conf base
      | "MOD_NOTES_OK" when conf.wizard -> NotesDisplay.print_mod_ok conf base
      | "MOD_WIZNOTES" when conf.authorized_wizards_notes -> WiznotesDisplay.print_mod conf base
      | "MOD_WIZNOTES_OK" when conf.authorized_wizards_notes -> WiznotesDisplay.print_mod_ok conf base
      | "MRG" when conf.wizard -> w_person conf base @@ MergeDisplay.print conf base
      | "MRG_DUP" when conf.wizard -> MergeDupDisplay.main_page conf base
      | "MRG_DUP_IND_Y_N" when conf.wizard -> MergeDupDisplay.answ_ind_y_n conf base
      | "MRG_DUP_FAM_Y_N" when conf.wizard -> MergeDupDisplay.answ_fam_y_n conf base
      | "MRG_FAM" when conf.wizard -> MergeFamDisplay.print conf base
      | "MRG_FAM_OK" when conf.wizard -> MergeFamOk.print_merge conf base
      | "MRG_MOD_FAM_OK" when conf.wizard -> MergeFamOk.print_mod_merge conf base
      | "MRG_IND" when conf.wizard -> MergeIndDisplay.print conf base
      | "MRG_IND_OK" when conf.wizard -> MergeIndOkDisplay.print_merge conf base
      | "MRG_MOD_IND_OK" when conf.wizard -> MergeIndOkDisplay.print_mod_merge conf base
      | "N" -> begin match p_getenv conf.env "v" with
          | Some v -> Some.surname_print conf base Some.surname_not_found v
          | _ -> AllnDisplay.print_surnames conf base
        end
      | "NG" -> begin
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
          match p_getenv conf.env "select" with
          | Some "input" | None ->
            (* Récupère le contenu non vide de la recherche. *)
            let real_input label =
              match p_getenv conf.env label with
              | Some s -> if s = "" then None else Some s
              | None -> None
            in
            (* Recherche par clé, sosa, alias ... *)
            let search n =
              let (pl, sosa_acc) = find_all conf base n in
              match pl with
              | [] ->
                conf.cancel_links <- false ;
                Some.surname_print conf base unknown n
              | [p] ->
                if sosa_acc
                || Gutil.person_of_string_key base n <> None
                || person_is_std_key conf base p n
                then person_selected_with_redirect conf base p
                else specify conf base n pl
              | pl -> specify conf base n pl
            in
            begin match real_input "v" with
              | Some n -> search n
              | None ->
                match real_input "fn", real_input "sn" with
                  Some fn, Some sn -> search (fn ^ " " ^ sn)
                | Some fn, None ->
                  conf.cancel_links <- false ;
                  Some.first_name_print conf base fn
                | None, Some sn ->
                  conf.cancel_links <- false ;
                  Some.surname_print conf base unknown sn
                | None, None -> incorrect_request conf
            end
          | Some i ->
            relation_print conf base
              (pget conf base (iper_of_string i))
        end
      | "NOTES" -> NotesDisplay.print conf base
      | "OA" when conf.wizard || conf.friend -> BirthDeathDisplay.print_oldest_alive conf base
      | "OE" when conf.wizard || conf.friend -> BirthDeathDisplay.print_oldest_engagements conf base
      | "P" -> begin match p_getenv conf.env "v" with
          | Some v -> Some.first_name_print conf base v
          | None -> AllnDisplay.print_first_names conf base
        end
      | "POP_PYR" when conf.wizard || conf.friend -> BirthDeathDisplay.print_population_pyramid conf base
      | "PS" -> PlaceDisplay.print_all_places_surnames conf base
      | "R" -> w_person conf base @@ relation_print conf base
      | "REQUEST" when conf.wizard ->
        Wserver.http Wserver.OK;
        Wserver.header "Content-type: text";
        List.iter (fun s -> Wserver.print_string @@ s ^ "\n") conf.Config.request ;
      | "RL" -> RelationLink.print conf base
      | "RLM" -> RelationDisplay.print_multi conf base
      | "S" -> SearchName.print conf base specify unknown
      | "SND_IMAGE" when conf.wizard && conf.can_send_image -> SendImage.print conf base
      | "SND_IMAGE_OK" when conf.wizard && conf.can_send_image -> SendImage.print_send_ok conf base
      | "SRC" -> begin match p_getenv conf.env "v" with
          | Some f -> SrcfileDisplay.print_source conf base f
          | _ -> Hutil.incorrect_request conf
        end
      | "STAT" -> BirthDeathDisplay.print_statistics conf
      | "CHANGE_WIZ_VIS" when conf.wizard -> WiznotesDisplay.change_wizard_visibility conf base
      | "TT" -> TitleDisplay.print conf base
      | "U" when conf.wizard -> w_person conf base @@ updmenu_print conf base
      | "VIEW_WIZNOTES" when conf.wizard && conf.authorized_wizards_notes -> WiznotesDisplay.print_view conf base
      | "WIZNOTES" when conf.authorized_wizards_notes -> WiznotesDisplay.print conf base
      | "WIZNOTES_SEARCH" when conf.authorized_wizards_notes -> WiznotesDisplay.print_search conf base
#ifdef API
      | mode
        when
          try
            String.get mode 0 = 'A' && String.get mode 1 = 'P' &&
            String.get mode 2 = 'I' && String.get mode 3 = '_'
          with _ -> false ->
        (* On passe en mode API, i.e. que les exceptions API sont levées. *)
        let () = Api_conf.set_mode_api () in
        begin match mode with
          | "API_ALL_PERSONS" -> Api.print_all_persons conf base
          | "API_ALL_FAMILIES" -> Api.print_all_families conf base
          | "API_BASE_WARNINGS" when conf.wizard -> Api.print_base_warnings conf base
          | "API_CLOSE_PERSONS" -> Api_graph.print_close_person_relations conf base
          | "API_CPL_REL" -> Api_graph.print_cpl_relation conf base
          | "API_GRAPH_ASC" -> Api_graph.print_graph_asc conf base
          | "API_GRAPH_ASC_LIA" -> Api_graph.print_graph_asc_lia conf base
          | "API_GRAPH_DESC" -> Api_graph.print_graph_desc conf base
          | "API_GRAPH_REL" -> Api_graph.print_graph_rel conf base
          | "API_FIRST_AVAILABLE_PERSON" -> Api.print_first_available_person conf base
          | "API_FIND_SOSA" -> Api.print_find_sosa conf base
          | "API_INFO_BASE" -> Api.print_info_base conf base
          | "API_INFO_IND" -> Api.print_info_ind conf base
          | "API_IMAGE" -> Api.print_img conf base
          | "API_IMAGE_EXT" -> Api.print_img_ext conf base
          | "API_IMAGE_ALL" -> Api.print_img_all conf base
          | "API_IMAGE_PERSON" -> Api.print_img_person conf base
          | "API_IMAGE_UPDATE" when conf.wizard -> Api.print_updt_image conf base
          | "API_LAST_MODIFIED_PERSONS" -> Api.print_last_modified_persons conf base
          | "API_LAST_VISITED_PERSONS" -> Api.print_last_visited_persons conf base
          | "API_LIST_PERSONS" -> Api.print_list_ref_person conf base
          | "API_LOOP_BASE" -> Api.print_loop conf base
          | "API_MAX_ANCESTORS" when conf.wizard -> Api.print_max_ancestors conf base
          | "API_NB_ANCESTORS" -> Api_saisie_read.print_nb_ancestors conf base
          | "API_NOTIFICATION_BIRTHDAY" -> Api.print_notification_birthday conf base
          | "API_REF_PERSON_FROM_ID" -> Api.print_ref_person_from_ip conf base
          | "API_REMOVE_IMAGE_EXT" when conf.wizard -> Api.print_remove_image_ext base
          | "API_REMOVE_IMAGE_EXT_ALL" when conf.wizard -> Api.print_remove_image_ext_all base
          | "API_SEARCH" -> Api_search.print_search conf base
          | "API_GRAPH_TREE_V2" -> Api_saisie_read.print_graph_tree_v2 conf base
          | "API_PERSON_TREE" -> Api_saisie_read.print_person_tree conf base
          | "API_FICHE_PERSON" -> Api_saisie_read.print_fiche_person conf base
          | "API_AUTO_COMPLETE" when conf.wizard -> Api_saisie_write.print_auto_complete conf base
          | "API_GET_CONFIG" when conf.wizard -> Api_saisie_write.print_config conf base
          | "API_PERSON_SEARCH_LIST" when conf.wizard -> Api_saisie_write.print_person_search_list conf base
          | "API_GET_PERSON_SEARCH_INFO" when conf.wizard -> Api_saisie_write.print_person_search_info conf base
          | "API_ADD_CHILD" when conf.wizard -> Api_saisie_write.print_add_child conf base
          | "API_ADD_CHILD_OK" when conf.wizard -> Api_saisie_write.print_add_child_ok conf base
          | "API_ADD_FAMILY" when conf.wizard -> Api_saisie_write.print_add_family conf base
          | "API_ADD_FAMILY_OK" when conf.wizard -> Api_saisie_write.print_add_family_ok conf base
          | "API_ADD_FIRST_FAM_OK" when conf.wizard -> Api_saisie_write.print_add_first_fam_ok conf base
          | "API_ADD_PARENTS" when conf.wizard -> Api_saisie_write.print_add_parents conf base
          | "API_ADD_PARENTS_OK" when conf.wizard -> Api_saisie_write.print_add_parents_ok conf base
          | "API_ADD_PERSON_OK" when conf.wizard -> Api_saisie_write.print_add_ind_ok conf base
          | "API_ADD_PERSON_START_OK" when conf.wizard -> Api_saisie_write.print_add_ind_start_ok conf base
          | "API_ADD_SIBLING" when conf.wizard -> Api_saisie_write.print_add_sibling conf base
          | "API_ADD_SIBLING_OK" when conf.wizard -> Api_saisie_write.print_add_sibling_ok conf base
          | "API_EDIT_FAMILY_REQUEST" when conf.wizard -> Api_saisie_write.print_mod_family_request conf base
          | "API_EDIT_FAMILY" when conf.wizard -> Api_saisie_write.print_mod_family conf base
          | "API_EDIT_FAMILY_OK" when conf.wizard -> Api_saisie_write.print_mod_family_ok conf base
          | "API_EDIT_PERSON" when conf.wizard -> Api_saisie_write.print_mod_ind conf base
          | "API_EDIT_PERSON_OK" when conf.wizard -> Api_saisie_write.print_mod_ind_ok conf base
          | "API_DEL_FAMILY_OK" when conf.wizard -> Api_saisie_write.print_del_fam_ok conf base
          | "API_DEL_PERSON_OK" when conf.wizard -> Api_saisie_write.print_del_ind_ok conf base
          | "API_LINK_TREE" -> Api_link.print_link_tree conf base
          | "API_STATS" -> Api_stats.print_stats conf base
          | "API_SELECT_EVENTS" -> Api_graph.print_select_events conf base
          | _ -> incorrect_request conf
        end
#endif
      | _ -> incorrect_request conf
  end else
    begin
      let title _ = Wserver.print_string (Utf8.capitalize (transl conf "error")) in
      Hutil.rheader conf title;
      Wserver.printf "<ul>\n<li>\n%s \"%s\" %s.</li>\n</ul>"
        (Utf8.capitalize (transl conf "base")) conf.bname
        (Utf8.capitalize (transl conf "reserved to friends or wizards"));
      Hutil.trailer conf
    end

let family_m_nobase conf =
#ifdef API
  (* On passe en mode API, i.e. que les exceptions API sont levées. *)
  let () = Api_conf.set_mode_api () in
  match p_getenv conf.env "m" with
  | Some "API_ADD_FIRST_FAM" -> Api_saisie_write.print_add_first_fam conf
  | Some _ | None -> ()
#else
  Hutil.incorrect_request conf
#endif

[@@@ocaml.warning "+45"]

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
        else ["iz", string_of_iper (get_iper p)]
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
    try
      let s = Unix.stat (Util.base_path [] (conf.bname ^ ".gwb")) in
      Unix.setgid s.Unix.st_gid ;
      Unix.setuid s.Unix.st_uid
    with
      Unix.Unix_error (_, _, _) -> ()

let log_count r =
  let thousand oc x = output_string oc @@ Mutil.string_of_int_sep ","  x in
  match r with
    Some (welcome_cnt, request_cnt, start_date) ->
      Log.with_log
        (fun oc ->
           Printf.fprintf oc "  #accesses %a (#welcome %a) since %s\n"
             thousand (welcome_cnt + request_cnt) thousand welcome_cnt
             start_date)
  | None -> ()

let print_moved conf s =
  Util.include_template conf ["bname", conf.bname] "moved"
    (fun () ->
      let title _ = Wserver.printf "%s -&gt; %s" conf.bname s in
      Hutil.header_no_page_title conf title;
      Wserver.printf "The database %s has moved to:\n<dl><dt><dd>\n"
        conf.bname;
      Wserver.printf "<a href=\"%s\">" s;
      Wserver.print_string s;
      Wserver.printf "</a>";
      Wserver.printf "\n</dd></dt></dl>\n";
      Hutil.trailer conf)

let print_no_index conf base =
  let title _ =
    Wserver.print_string (Utf8.capitalize (transl conf "link to use"))
  in
  let link = url_no_index conf base in
  Hutil.header conf title;
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>" ;
  Wserver.printf "<a href=\"http://%s\">\n" link;
  Wserver.print_string link;
  Wserver.printf "</a>\n";
  Wserver.printf "</ul>\n";
  Hutil.print_link_to_welcome conf false;
  Hutil.trailer conf

let treat_request conf base =
  begin match
    p_getenv conf.base_env "moved", p_getenv conf.env "opt",
    p_getenv conf.env "m"
  with
    Some s, _, _ -> print_moved conf s
  | _, Some "no_index", _ -> print_no_index conf base
  | _, _, Some "IM" -> ImageDisplay.print conf base
  | _, _, Some "DOC" ->
      begin match p_getenv conf.env "s" with
        Some f ->
          if Filename.check_suffix f ".txt" then
            let f = Filename.chop_suffix f ".txt" in
            SrcfileDisplay.print_source conf base f
          else ImageDisplay.print conf base
      | None -> Hutil.incorrect_request conf
      end
  | _ ->
      set_owner conf;
      extract_henv conf base;
      make_senv conf base;
      let conf =
        match Util.default_sosa_ref conf base with
          Some p -> {conf with default_sosa_ref = get_iper p, Some p}
        | None -> conf
      in
      if only_special_env conf.env then
        begin
          begin match p_getenv conf.base_env "counter" with
            Some "no" -> ()
          | _ -> let r = SrcfileDisplay.incr_welcome_counter conf in log_count r
          end;
          if not @@ try_plugin conf base "" then SrcfileDisplay.print_start conf base
        end
      else
        begin
          begin match p_getenv conf.base_env "counter" with
            Some "no" -> ()
          | _ -> let r = SrcfileDisplay.incr_request_counter conf in log_count r
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
      (try treat_request conf base with exc -> close_base base; raise exc);
      close_base base
  | Right e ->
      let transl conf w =
        try Hashtbl.find conf.lexicon w with Not_found -> "[" ^ w ^ "]"
      in
      let title _ =
        Wserver.print_string (Utf8.capitalize (transl conf "error"))
      in
      Hutil.rheader conf title;
      Wserver.printf "<ul>";
      Wserver.printf "<li>" ;
      Wserver.print_string (Utf8.capitalize (transl conf "cannot access base"));
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
    Lock.control (Mutil.lock_file bfile) false
      ~onerror:(fun () -> Update.error_locked conf)
      (fun () -> treat_request_on_possibly_locked_base conf bfile)
  else treat_request_on_possibly_locked_base conf bfile

let treat_request_on_nobase conf =
  try family_m_nobase conf; Wserver.wflush () with exc -> raise exc
