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
  let title _ = Output.printf conf "%s : %s" n (transl conf "specify") in
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
  Hutil.print_link_to_welcome conf true;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  Output.print_string conf "<ul>\n";
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  List.iter
    (fun (p, tl) ->
       Output.print_string conf "<li>\n";
       Perso.print_sosa conf base p true;
       begin match tl with
           [] ->
           Output.printf conf "\n%s" (referenced_person_title_text conf base p)
         | t :: _ ->
           Output.printf conf "<a href=\"%s%s\">\n" (commd conf)
             (acces conf base p);
           Output.print_string conf (titled_person_text conf base p t);
           Output.print_string conf "</a>\n";
           List.iter
             (fun t -> Output.print_string conf (one_title_text base t)) tl
       end;
       Output.print_string conf (DateDisplay.short_dates_text conf base p);
       if authorized_age conf base p then
         begin match get_first_names_aliases p with
             [] -> ()
           | fnal ->
             Output.print_string conf "\n<em>(";
             Mutil.list_iter_first
               (fun first fna ->
                  if not first then Output.print_string conf ", ";
                  Output.print_string conf (sou base fna))
               fnal;
             Output.print_string conf ")</em>"
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
           Output.printf conf ", <em>&amp; %s</em>\n" s
       end;
       Output.print_string conf "</li>\n")
    ptll;
  Output.print_string conf "</ul>\n";
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

let very_unknown conf base =
  match p_getenv conf.env "n", p_getenv conf.env "p" with
  | Some sname, Some fname ->
    let title _ =
      Output.printf conf "%s: \"%s %s\"" (Utf8.capitalize (transl conf "not found"))
        (Util.escape_html fname) (Util.escape_html sname)
    in
    Output.status conf Def.Not_Found;
    Hutil.rheader conf title;
    Hutil.print_link_to_welcome conf false;
    Hutil.trailer conf
  | _ -> incorrect_request conf

let unknown = begin fun conf n ->
      let title _ =
        Output.printf conf "%s: \"%s\"" (Utf8.capitalize (transl conf "not found"))
          (Util.escape_html n)
      in
      Output.status conf Def.Not_Found;
      Hutil.rheader conf title;
      Hutil.print_link_to_welcome conf false;
      Hutil.trailer conf
    end

let make_henv conf base =
  Opt.iter begin fun p ->
    let x =
      let first_name = p_first_name base p in
      let surname = p_surname base p in
      if Util.accessible_by_key conf base p first_name surname then
        [ "pz", Mutil.encode (Name.lower first_name)
        ; "nz", Mutil.encode (Name.lower surname)
        ; "ocz", string_of_int (get_occ p) ]
      else [ "iz", string_of_iper (get_iper p) ]
    in
    conf.henv <- conf.henv @ x
  end (Util.find_sosa_ref conf base) ;
  begin match p_getenv conf.env "dsrc" with
    Some "" | None -> ()
  | Some s -> conf.henv <- conf.henv @ ["dsrc", Mutil.encode s]
  end;
  begin match p_getenv conf.env "templ" with
    None -> ()
  | Some s -> conf.henv <- conf.henv @ ["templ", Mutil.encode s]
  end;
  Opt.iter
    (fun _ -> conf.henv <- conf.henv @ ["escache", escache_value base])
    (Util.p_getenv conf.env "escache") ;
  if Util.p_getenv conf.env "manitou" = Some "off"
  then conf.henv <- conf.henv @ ["manitou", "off"] ;
  let aux param =
    Opt.iter
      (fun s -> conf.henv <- conf.henv @ [ param, s ])
      (Util.p_getenv conf.env param)
  in
  aux "alwsurn";
  aux "pure_xhtml";
  aux "size";
  aux "p_mod";
  aux "wide"

let special_vars =
  [ "alwsurn"; "cgl"; "dsrc"; "em"; "ei"; "ep"; "en"; "eoc"; "escache"; "et";
    "iz"; "log_cnl"; "log_pwd"; "log_uid"; "long"; "manitou"; "nz"; "ocz";
    "p_mod"; "pure_xhtml"; "pz"; "size"; "spouse"; "templ"; "wide" ]

let only_special_env = List.for_all (fun (x, _) -> List.mem x special_vars)

let make_senv conf base =
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
    | Some x -> conf.senv <- conf.senv @ ["color", Mutil.encode x]
    | _ -> ()
  in
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
      | None -> incorrect_request conf; raise Exit
    in
    let vi = string_of_iper ip in set_senv conf vm vi
  | _ -> ()

let propose_base conf =
  let title _ = Output.print_string conf "Base" in
  Hutil.header conf title;
  Output.print_string conf "<ul><li>";
  Output.printf conf "<form method=\"get\" action=\"%s\">\n" conf.indep_command;
  Output.print_string conf "<input name=\"b\" size=\"40\"> =&gt;\n";
  Output.print_string conf
    "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
  Output.print_string conf (Utf8.capitalize (transl_nth conf "validate/delete" 0));
  Output.print_string conf "</button>\n";
  Output.print_string conf "</li></ul>";
  Hutil.trailer conf

let try_plugin list conf base m =
  List.exists
    (fun (ns, fn) -> List.mem ns list && fn conf base)
    (Hashtbl.find_all GwdPlugin.ht m)

let w_lock ~onerror fn conf base =
  let bfile = Util.bpath (conf.bname ^ ".gwb") in
  Lock.control
    (Mutil.lock_file bfile) false
    ~onerror:(fun () -> onerror conf base)
    (fun () -> fn conf base)

let w_base ~none fn conf base =
  match base with
  | None -> none conf
  | Some base ->
    make_henv conf base;
    make_senv conf base;
    let conf =
      match Util.default_sosa_ref conf base with
      | Some p -> { conf with default_sosa_ref = get_iper p, Some p }
      | None -> conf
    in
    fn conf base

let w_person ~none fn conf base =
  match find_person_in_env conf base "" with
  | Some p -> fn conf base p
  | _ -> none conf base

let treat_request =
  let w_lock = w_lock ~onerror:(fun conf _ -> Update.error_locked conf) in
  let w_base = w_base ~none:incorrect_request in
  let w_person = w_person ~none:very_unknown in
  fun conf ->
  let bfile =
    if conf.bname = "" then None
    else Some (Util.bpath (conf.bname ^ ".gwb"))
  in
  let base =
    match bfile with
    | None -> None
    | Some bfile -> try Some (Gwdb.open_base bfile) with _ -> None
  in
#ifdef DEBUG
  Mutil.bench (__FILE__ ^ " " ^ string_of_int __LINE__) @@ fun () ->
#endif
  if conf.wizard
  || conf.friend
  || List.assoc_opt "visitor_access" conf.base_env <> Some "no"
  then begin
#ifdef UNIX
    begin match bfile with
      | None -> ()
      | Some bfile ->
        let stat = Unix.stat bfile in
        Unix.setgid stat.Unix.st_gid ;
        Unix.setuid stat.Unix.st_uid ;
    end ;
#endif
    let plugins =
      match List.assoc_opt "plugins" conf.Config.base_env with
      | None -> []
      | Some list -> String.split_on_char ',' list
    in
    List.iter (fun (ns, fn) -> if List.mem ns plugins then fn conf base) !GwdPlugin.se ;
    let m = Opt.default "" @@ p_getenv conf.env "m" in
    if not @@ try_plugin plugins conf base m
    then begin
        if p_getenv conf.base_env "counter" <> Some "no"
        then begin
          match SrcfileDisplay.incr_welcome_counter conf with
          | Some (welcome_cnt, request_cnt, start_date) ->
            GwdLog.log begin fun oc ->
              let thousand oc x = output_string oc @@ Mutil.string_of_int_sep ","  x in
              Printf.fprintf oc "  #accesses %a (#welcome %a) since %s\n"
                thousand (welcome_cnt + request_cnt) thousand welcome_cnt
                start_date
            end ;
          | None -> ()
        end ;
        let incorrect_request conf _ = incorrect_request conf in
        match m with
        | "" ->
          if base <> None then
            w_base @@
            if only_special_env conf.env then SrcfileDisplay.print_start
            else w_person @@ fun conf base p ->
              match p_getenv conf.env "ptempl" with
              | Some t when p_getenv conf.base_env "ptempl" = Some "yes" ->
                Perso.interp_templ t conf base p
              | _ -> person_selected conf base p
          else if conf.bname = ""
          then fun conf _ -> include_template conf [] "index" (fun () -> propose_base conf)
          else incorrect_request
        | "A" ->
          Perso.print_ascend |> w_person |> w_base
        | "ADD_FAM" when conf.wizard ->
          w_base @@ UpdateFam.print_add
        | "ADD_FAM_OK" when conf.wizard ->
          w_base @@ w_lock @@ UpdateFamOk.print_add
        | "ADD_IND" when conf.wizard ->
          w_base @@ UpdateInd.print_add
        | "ADD_IND_OK" when conf.wizard ->
          w_base @@ w_lock @@ UpdateIndOk.print_add
        | "ADD_PAR" when conf.wizard ->
          w_base @@ UpdateFam.print_add_parents
        | "ADD_PAR_OK" when conf.wizard ->
          w_base @@ w_lock @@ UpdateFamOk.print_add_parents
        | "ANM" ->
          w_base @@ fun conf _ -> BirthdayDisplay.print_anniversaries conf
        | "AN" ->
          w_base @@ fun conf base -> begin match p_getenv conf.env "v" with
            | Some x -> BirthdayDisplay.print_birth conf base (int_of_string x)
            | _ -> BirthdayDisplay.print_menu_birth conf base
          end
        | "AD" ->
          w_base @@ fun conf base -> begin match p_getenv conf.env "v" with
            | Some x -> BirthdayDisplay.print_dead conf base (int_of_string x)
            | _ -> BirthdayDisplay.print_menu_dead conf base
          end
        | "AM" ->
          w_base @@ fun conf base -> begin match p_getenv conf.env "v" with
            | Some x -> BirthdayDisplay.print_marriage conf base (int_of_string x)
            | _ -> BirthdayDisplay.print_menu_marriage conf base
          end
        | "AS_OK" ->
          w_base @@ AdvSearchOkDisplay.print
        | "C" ->
          w_base @@ w_person @@ CousinsDisplay.print
        | "CAL" ->
          fun conf _ -> Hutil.print_calendar conf
        | "CHG_CHN" when conf.wizard ->
          w_base @@ ChangeChildrenDisplay.print
        | "CHG_CHN_OK" when conf.wizard ->
          w_base @@ w_lock @@ ChangeChildrenDisplay.print_ok
        | "CHG_EVT_IND_ORD" when conf.wizard ->
          w_base @@ UpdateInd.print_change_event_order
        | "CHG_EVT_IND_ORD_OK" when conf.wizard ->
          w_base @@ w_lock @@ UpdateIndOk.print_change_event_order
        | "CHG_EVT_FAM_ORD" when conf.wizard ->
          w_base @@ UpdateFam.print_change_event_order
        | "CHG_EVT_FAM_ORD_OK" when conf.wizard ->
          w_base @@ w_lock @@ UpdateFamOk.print_change_event_order
        | "CHG_FAM_ORD" when conf.wizard ->
          w_base @@ UpdateFam.print_change_order
        | "CHG_FAM_ORD_OK" when conf.wizard ->
          w_base @@ w_lock @@ UpdateFamOk.print_change_order_ok
        | "CONN_WIZ" when conf.wizard ->
          w_base @@ WiznotesDisplay.connected_wizards
        | "D" ->
          w_base @@ w_person @@ DescendDisplay.print
        | "DAG" ->
          w_base @@ DagDisplay.print
        | "DEL_FAM" when conf.wizard ->
          w_base @@ UpdateFam.print_del
        | "DEL_FAM_OK" when conf.wizard ->
          w_base @@ w_lock @@ UpdateFamOk.print_del
        | "DEL_IMAGE" when conf.wizard && conf.can_send_image ->
          w_base @@ SendImage.print_del
        | "DEL_IMAGE_OK" when conf.wizard && conf.can_send_image ->
          w_base @@ w_lock @@ SendImage.print_del_ok
        | "DEL_IND" when conf.wizard ->
          w_base @@ UpdateInd.print_del
        | "DEL_IND_OK" when conf.wizard ->
          w_base @@ w_lock @@ UpdateIndOk.print_del
        | "F" ->
          w_base @@ w_person @@ Perso.interp_templ "family"
        | "H" ->
          w_base @@ fun conf base -> begin match p_getenv conf.env "v" with
            | Some f -> SrcfileDisplay.print conf base f
            | None -> incorrect_request conf base
          end
        | "HIST" ->
          w_base @@ History.print
        | "HIST_CLEAN" when conf.wizard ->
          w_base @@ fun conf _ -> HistoryDiffDisplay.print_clean conf
        | "HIST_CLEAN_OK" when conf.wizard ->
          w_base @@ fun conf _ -> HistoryDiffDisplay.print_clean_ok conf
        | "HIST_DIFF" ->
          w_base @@ HistoryDiffDisplay.print
        | "HIST_SEARCH" ->
          w_base @@ History.print_search
        | "IM" ->
          w_base @@ ImageDisplay.print
        | "IMH" ->
          w_base @@ fun conf _ -> ImageDisplay.print_html conf
        | "INV_FAM" when conf.wizard ->
          w_base @@ UpdateFam.print_inv
        | "INV_FAM_OK" when conf.wizard ->
          w_base @@ w_lock @@ UpdateFamOk.print_inv
        | "KILL_ANC" when conf.wizard ->
          w_base @@ w_lock @@ MergeIndDisplay.print_kill_ancestors
        | "LB" when conf.wizard || conf.friend ->
          w_base @@ BirthDeathDisplay.print_birth
        | "LD" when conf.wizard || conf.friend ->
          w_base @@ BirthDeathDisplay.print_death
        | "LINKED" ->
          w_base @@ w_person @@ Perso.print_what_links
        | "LL" ->
          w_base @@ BirthDeathDisplay.print_longest_lived
        | "LM" when conf.wizard || conf.friend ->
          w_base @@ BirthDeathDisplay.print_marriage
        | "MISC_NOTES" ->
          w_base @@ NotesDisplay.print_misc_notes
        | "MISC_NOTES_SEARCH" ->
          w_base @@ NotesDisplay.print_misc_notes_search
        | "MOD_DATA" when conf.wizard ->
          w_base @@ UpdateDataDisplay.print_mod
        | "MOD_DATA_OK" when conf.wizard ->
          w_base @@ w_lock @@ UpdateDataDisplay.print_mod_ok
        | "MOD_FAM" when conf.wizard ->
          w_base @@ UpdateFam.print_mod
        | "MOD_FAM_OK" when conf.wizard ->
          w_base @@ w_lock @@ UpdateFamOk.print_mod
        | "MOD_IND" when conf.wizard ->
          w_base @@ UpdateInd.print_mod
        | "MOD_IND_OK" when conf.wizard ->
          w_base @@ w_lock @@ UpdateIndOk.print_mod
        | "MOD_NOTES" when conf.wizard ->
          w_base @@ NotesDisplay.print_mod
        | "MOD_NOTES_OK" when conf.wizard ->
          w_base @@ w_lock @@ NotesDisplay.print_mod_ok
        | "MOD_WIZNOTES" when conf.authorized_wizards_notes ->
          w_base @@ WiznotesDisplay.print_mod
        | "MOD_WIZNOTES_OK" when conf.authorized_wizards_notes ->
          w_base @@ w_lock @@ WiznotesDisplay.print_mod_ok
        | "MRG" when conf.wizard ->
          w_base @@ w_person @@ MergeDisplay.print
        | "MRG_DUP" when conf.wizard ->
          w_base @@ MergeDupDisplay.main_page
        | "MRG_DUP_IND_Y_N" when conf.wizard ->
          w_base @@ w_lock @@ MergeDupDisplay.answ_ind_y_n
        | "MRG_DUP_FAM_Y_N" when conf.wizard ->
          w_base @@ w_lock @@ MergeDupDisplay.answ_fam_y_n
        | "MRG_FAM" when conf.wizard ->
          w_base @@ MergeFamDisplay.print
        | "MRG_FAM_OK" when conf.wizard ->
          w_base @@ w_lock @@ MergeFamOk.print_merge
        | "MRG_MOD_FAM_OK" when conf.wizard ->
          w_base @@ w_lock @@ MergeFamOk.print_mod_merge
        | "MRG_IND" when conf.wizard ->
          w_base @@ w_lock @@ MergeIndDisplay.print
        | "MRG_IND_OK" when conf.wizard ->
          w_base @@ w_lock @@ MergeIndOkDisplay.print_merge
        | "MRG_MOD_IND_OK" when conf.wizard ->
          w_base @@ w_lock @@ MergeIndOkDisplay.print_mod_merge
        | "N" ->
          w_base @@ fun conf base -> begin match p_getenv conf.env "v" with
            | Some v -> Some.surname_print conf base Some.surname_not_found v
            | _ -> AllnDisplay.print_surnames conf base
          end
        | "NG" -> w_base @@ begin fun conf base ->
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
                    Some.first_name_print conf base fn
                  | None, Some sn ->
                    Some.surname_print conf base unknown sn
                  | None, None -> incorrect_request conf base
              end
            | Some i ->
              relation_print conf base
                (pget conf base (iper_of_string i))
          end
        | "NOTES" ->
          w_base @@ NotesDisplay.print
        | "OA" when conf.wizard || conf.friend ->
          w_base @@ BirthDeathDisplay.print_oldest_alive
        | "OE" when conf.wizard || conf.friend ->
          w_base @@ BirthDeathDisplay.print_oldest_engagements
        | "P" ->
          w_base @@ fun conf base -> begin match p_getenv conf.env "v" with
            | Some v -> Some.first_name_print conf base v
            | None -> AllnDisplay.print_first_names conf base
          end
        | "POP_PYR" when conf.wizard || conf.friend ->
          w_base @@ BirthDeathDisplay.print_population_pyramid
        | "PS" ->
          w_base @@ PlaceDisplay.print_all_places_surnames
        | "R" ->
          w_base @@ w_person @@ relation_print
        | "REQUEST" when conf.wizard ->
          fun _ _ ->
            Output.status conf Def.OK;
            Output.header conf "Content-type: text";
            List.iter (fun s -> Output.print_string conf @@ s ^ "\n") conf.Config.request ;
        | "RL" ->
          w_base @@ RelationLink.print
        | "RLM" ->
          w_base @@ RelationDisplay.print_multi
        | "S" ->
          w_base @@ fun conf base -> SearchName.print conf base specify unknown
        | "SND_IMAGE" when conf.wizard && conf.can_send_image ->
          w_base @@ SendImage.print
        | "SND_IMAGE_OK" when conf.wizard && conf.can_send_image ->
          w_base @@ w_lock @@ SendImage.print_send_ok
        | "SRC" ->
          w_base @@ fun conf base -> begin match p_getenv conf.env "v" with
            | Some f -> SrcfileDisplay.print_source conf base f
            | _ -> incorrect_request conf base
          end
        | "STAT" ->
          w_base @@ fun conf _ -> BirthDeathDisplay.print_statistics conf
        | "CHANGE_WIZ_VIS" when conf.wizard ->
          w_base @@ w_lock @@ WiznotesDisplay.change_wizard_visibility
        | "TT" ->
          w_base @@ TitleDisplay.print
        | "U" when conf.wizard ->
          w_base @@ w_person @@ updmenu_print
        | "VIEW_WIZNOTES" when conf.wizard && conf.authorized_wizards_notes ->
          w_base @@ WiznotesDisplay.print_view
        | "WIZNOTES" when conf.authorized_wizards_notes ->
          w_base @@ WiznotesDisplay.print
        | "WIZNOTES_SEARCH" when conf.authorized_wizards_notes ->
          w_base @@ WiznotesDisplay.print_search
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
            | "API_ADD_FIRST_FAM" ->
              w_base @@ fun conf _ -> Api_saisie_write.print_add_first_fam conf
            | "API_ALL_PERSONS" ->
              w_base @@ Api.print_all_persons
            | "API_ALL_FAMILIES" ->
              w_base @@ Api.print_all_families
            | "API_BASE_WARNINGS" when conf.wizard ->
              w_base @@ w_lock @@ Api.print_base_warnings
            | "API_CLOSE_PERSONS" ->
              w_base @@ Api_graph.print_close_person_relations
            | "API_CPL_REL" ->
              w_base @@ Api_graph.print_cpl_relation
            | "API_GRAPH_ASC" ->
              w_base @@ Api_graph.print_graph_asc
            | "API_GRAPH_ASC_LIA" ->
              w_base @@ Api_graph.print_graph_asc_lia
            | "API_GRAPH_DESC" ->
              w_base @@ Api_graph.print_graph_desc
            | "API_GRAPH_REL" ->
              w_base @@ Api_graph.print_graph_rel
            | "API_FIRST_AVAILABLE_PERSON" ->
              w_base @@ Api.print_first_available_person
            | "API_FIND_SOSA" ->
              w_base @@ Api.print_find_sosa
            | "API_INFO_BASE" ->
              w_base @@ Api.print_info_base
            | "API_INFO_IND" ->
              w_base @@ Api.print_info_ind
            | "API_IMAGE" ->
              w_base @@ Api.print_img
            | "API_IMAGE_EXT" ->
              w_base @@ Api.print_img_ext
            | "API_IMAGE_ALL" ->
              w_base @@ Api.print_img_all
            | "API_IMAGE_PERSON" ->
              w_base @@ Api.print_img_person
            | "API_IMAGE_UPDATE" when conf.wizard ->
              w_base @@ w_lock @@ Api.print_updt_image
            | "API_LAST_MODIFIED_PERSONS" ->
              w_base @@ Api.print_last_modified_persons
            | "API_LAST_VISITED_PERSONS" ->
              w_base @@ Api.print_last_visited_persons
            | "API_LIST_PERSONS" ->
              w_base @@ Api.print_list_ref_person
            | "API_LOOP_BASE" ->
              w_base @@ Api.print_loop
            | "API_MAX_ANCESTORS" when conf.wizard ->
              w_base @@ Api.print_max_ancestors
            | "API_NB_ANCESTORS" ->
              w_base @@ Api_saisie_read.print_nb_ancestors
            | "API_NOTIFICATION_BIRTHDAY" ->
              w_base @@ Api.print_notification_birthday
            | "API_REF_PERSON_FROM_ID" ->
              w_base @@ Api.print_ref_person_from_ip
            | "API_REMOVE_IMAGE_EXT" when conf.wizard ->
              w_base @@ w_lock @@ fun _ -> Api.print_remove_image_ext
            | "API_REMOVE_IMAGE_EXT_ALL" when conf.wizard ->
              w_base @@ w_lock @@ fun _ -> Api.print_remove_image_ext_all
            | "API_SEARCH" ->
              w_base @@ Api_search.print_search
            | "API_GRAPH_TREE_V2" ->
              w_base @@ Api_saisie_read.print_graph_tree_v2
            | "API_PERSON_TREE" ->
              w_base @@ Api_saisie_read.print_person_tree
            | "API_FICHE_PERSON" ->
              w_base @@ Api_saisie_read.print_fiche_person
            | "API_AUTO_COMPLETE" when conf.wizard ->
              w_base @@ Api_saisie_write.print_auto_complete
            | "API_GET_CONFIG" when conf.wizard ->
              w_base @@ Api_saisie_write.print_config
            | "API_PERSON_SEARCH_LIST" when conf.wizard ->
              w_base @@ Api_saisie_write.print_person_search_list
            | "API_GET_PERSON_SEARCH_INFO" when conf.wizard ->
              w_base @@ Api_saisie_write.print_person_search_info
            | "API_ADD_CHILD" when conf.wizard ->
              w_base @@ Api_saisie_write.print_add_child
            | "API_ADD_CHILD_OK" when conf.wizard ->
              w_base @@ w_lock @@ Api_saisie_write.print_add_child_ok
            | "API_ADD_FAMILY" when conf.wizard ->
              w_base @@ Api_saisie_write.print_add_family
            | "API_ADD_FAMILY_OK" when conf.wizard ->
              w_base @@ w_lock @@ Api_saisie_write.print_add_family_ok
            | "API_ADD_FIRST_FAM_OK" when conf.wizard ->
              w_base @@ w_lock @@ Api_saisie_write.print_add_first_fam_ok
            | "API_ADD_PARENTS" when conf.wizard ->
              w_base @@ Api_saisie_write.print_add_parents
            | "API_ADD_PARENTS_OK" when conf.wizard ->
              w_base @@ w_lock @@ Api_saisie_write.print_add_parents_ok
            | "API_ADD_PERSON_OK" when conf.wizard ->
              w_base @@ w_lock @@ Api_saisie_write.print_add_ind_ok
            | "API_ADD_PERSON_START_OK" when conf.wizard ->
              w_base @@ w_lock @@ Api_saisie_write.print_add_ind_start_ok
            | "API_ADD_SIBLING" when conf.wizard ->
              w_base @@ Api_saisie_write.print_add_sibling
            | "API_ADD_SIBLING_OK" when conf.wizard ->
              w_base @@ w_lock @@ Api_saisie_write.print_add_sibling_ok
            | "API_EDIT_FAMILY_REQUEST" when conf.wizard ->
              w_base @@ Api_saisie_write.print_mod_family_request
            | "API_EDIT_FAMILY" when conf.wizard ->
              w_base @@ Api_saisie_write.print_mod_family
            | "API_EDIT_FAMILY_OK" when conf.wizard ->
              w_base @@ w_lock @@ Api_saisie_write.print_mod_family_ok
            | "API_EDIT_PERSON" when conf.wizard ->
              w_base @@ Api_saisie_write.print_mod_ind
            | "API_EDIT_PERSON_OK" when conf.wizard ->
              w_base @@ w_lock @@ Api_saisie_write.print_mod_ind_ok
            | "API_DEL_FAMILY_OK" when conf.wizard ->
              w_base @@ w_lock @@ Api_saisie_write.print_del_fam_ok
            | "API_DEL_PERSON_OK" when conf.wizard ->
              w_base @@ w_lock @@ Api_saisie_write.print_del_ind_ok
            | "API_LINK_TREE" ->
              w_base @@ Api_link.print_link_tree
            | "API_STATS" ->
              w_base @@ Api_stats.print_stats
            | "API_SELECT_EVENTS" ->
              w_base @@ Api_graph.print_select_events
            | _ -> incorrect_request
          end
#endif
        | _ -> incorrect_request
      end conf base ;
    Output.flush conf ;
  end else
    begin
      let title _ = Output.print_string conf (Utf8.capitalize (transl conf "error")) in
      Hutil.rheader conf title;
      Output.printf conf "<ul>\n<li>\n%s \"%s\" %s.</li>\n</ul>"
        (Utf8.capitalize (transl conf "base")) conf.bname
        (Utf8.capitalize (transl conf "reserved to friends or wizards"));
      Hutil.trailer conf
    end

let treat_request conf =
  try treat_request conf with
  | Update.ModErr _ -> Output.flush conf ; ()
  | e -> GwdLog.syslog `LOG_CRIT (Printexc.to_string e)
