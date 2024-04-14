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

let find_all conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match sosa_ref, sosa_nb with
  | Some p, Some n ->
    if n <> Sosa.zero then
      match Util.branch_of_sosa conf base n p with
        Some (p :: _) -> [p], true
      | _ -> [], false
    else [], false
  | _ ->
    let acc = SearchName.search_by_key conf base an in
    if acc <> [] then acc, false
    else
      ( SearchName.search_key_aux begin fun conf base acc an ->
            let spl = select_std_eq conf base acc an in
            if spl = [] then
              if acc = [] then SearchName.search_by_name conf base an
              else acc
            else spl
          end conf base an
      , false )

let relation_print conf base p =
  let p1 =
    match p_getenv conf.senv "ei" with
    | Some i ->
      conf.senv <- [] ;
      let i = iper_of_string i in
      if Gwdb.iper_exists base i
      then Some (pget conf base i)
      else None
    | None ->
      match find_person_in_env conf base "1" with
      | Some p1 ->
        conf.senv <- [];
        Some p1
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
  Output.print_sstring conf "<ul>\n";
  (* Construction de la table des sosa de la base *)
  let () = SosaCache.build_sosa_ht conf base in
  List.iter
    (fun (p, _tl) ->
       Output.print_sstring conf "<li>\n";
       SosaCache.print_sosa conf base p true;
       Update.print_person_parents_and_spouses conf base p;
       Output.print_sstring conf "</li>\n"
    ) ptll;
  Output.print_sstring conf "</ul>\n";
  Hutil.trailer conf

let incorrect_request ?(comment = "") conf =
  Hutil.incorrect_request ~comment:comment conf

let person_selected conf base p =
  match p_getenv conf.senv "em" with
    Some "R" -> relation_print conf base p
  | Some _ -> incorrect_request conf ~comment:"error #9"
  | None -> record_visited conf (get_iper p); Perso.print conf base p

let person_selected_with_redirect conf base p =
  match p_getenv conf.senv "em" with
  | Some "R" -> relation_print conf base p
  | Some _ -> incorrect_request conf ~comment:"error #8"
  | None ->
    Wserver.http_redirect_temporarily
      (commd conf ^^^ Util.acces conf base p :> string)

let updmenu_print = Perso.interp_templ "updmenu"

let very_unknown conf _ =
  match p_getenv conf.env "n", p_getenv conf.env "p" with
  | Some sname, Some fname ->
    let title _ =
      transl conf "not found"
      |> Utf8.capitalize_fst
      |> Output.print_sstring conf ;
      Output.print_sstring conf (transl conf ":") ;
      Output.print_sstring conf {| "|} ;
      Output.print_string conf (Util.escape_html fname) ;
      Output.print_sstring conf {| |} ;
      Output.print_string conf (Util.escape_html sname) ;
      Output.print_sstring conf {|"|} ;
    in
    Output.status conf Def.Not_Found;
    Hutil.rheader conf title;
    Hutil.print_link_to_welcome conf false;
    Hutil.trailer conf
  | _ ->
    match p_getenv conf.env "i" with
    | Some i ->
      let title _ =
        Output.print_sstring conf "<kbd>" ;
        Output.print_string conf (Util.escape_html i) ;
        Output.print_sstring conf "</kbd>" ;
        Output.print_sstring conf (transl conf ":") ;
        Output.print_sstring conf " " ;
        transl conf "not found"
        |> Utf8.capitalize_fst
        |> Output.print_sstring conf ;
      in
      Output.status conf Def.Not_Found;
      Hutil.rheader conf title;
      Hutil.print_link_to_welcome conf false;
      Hutil.trailer conf
    | None -> Hutil.incorrect_request conf ~comment:"error #1"

(* Print Not found page *)
let unknown conf n =
  let title _ =
    transl conf "not found"
    |> Utf8.capitalize_fst
    |> Output.print_sstring conf ;
    Output.print_sstring conf (transl conf ":") ;
    Output.print_sstring conf {| "|} ;
    Output.print_string conf (Util.escape_html n) ;
    Output.print_sstring conf {|"|} ;
  in
  Output.status conf Def.Not_Found;
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf false;
  Hutil.trailer conf

let make_henv conf base =
  let conf =
    match Util.find_sosa_ref conf base with
    | Some p ->
      let x =
        let first_name = p_first_name base p in
        let surname = p_surname base p in
        if Util.accessible_by_key conf base p first_name surname then
          [ "pz", Name.lower first_name |> Mutil.encode
          ; "nz", Name.lower surname |> Mutil.encode
          ; "ocz", get_occ p |> string_of_int |> Mutil.encode
          ]
        else [ "iz", get_iper p |> string_of_iper |> Mutil.encode ]
      in
      { conf with henv = conf.henv @ x }
    | None -> conf
  in
  let conf =
    match p_getenv conf.env "dsrc" with
    | Some "" | None -> conf
    | Some s -> { conf with henv = conf.henv @ ["dsrc", Mutil.encode s] }
  in
  let conf =
    match p_getenv conf.env "templ" with
    | None -> conf
    | Some s -> { conf with henv = conf.henv @ ["templ", Mutil.encode s] }
  in
  let conf =
    match Util.p_getenv conf.env "escache" with
    | Some _ -> { conf with henv = conf.henv @ ["escache", escache_value base] }
    | None -> conf
  in
  let conf =
    if Util.p_getenv conf.env "manitou" = Some "off"
    then { conf with henv = conf.henv @ ["manitou", Adef.encoded "off"] }
    else conf
  in
  let aux param conf =
    match Util.p_getenv conf.env param with
    | Some s -> { conf with henv = conf.henv @ [param, Mutil.encode s] }
    | None -> conf
  in
  aux "alwsurn" conf
  |> aux "pure_xhtml"
  |> aux "size"
  |> aux "p_mod"
  |> aux "wide"

let special_vars =
  [ "alwsurn"; "cgl"; "dsrc"; "em"; "ei"; "ep"; "en"; "eoc"; "escache"; "et";
    "iz"; "long"; "manitou"; "nz"; "ocz";
    "p_mod"; "pure_xhtml"; "pz"; "size"; "templ"; "wide" ]

let only_special_env env = List.for_all (fun (x, _) -> List.mem x special_vars) env

let make_senv conf base =
  let set_senv conf vm vi =
    let aux k v conf =
      if p_getenv conf.env k = Some v
      then { conf with senv = conf.senv @ [ k, Mutil.encode v ] }
      else conf
    in
    let conf =
      { conf with senv = ["em", vm; "ei", vi] }
      |> aux "long" "on"
    in
    let conf =
      match p_getenv conf.env "et" with
      | Some x -> { conf with senv = conf.senv @ ["et", Mutil.encode x] }
      | _ -> conf
    in
    let conf = aux "cgl" "on" conf in
    let conf =
      match p_getenv conf.env "bd" with
      | None | Some ("0" | "") -> conf
      | Some x -> { conf with senv = conf.senv @ ["bd", Mutil.encode x] }
    in
    match p_getenv conf.env "color" with
    | Some x -> { conf with senv = conf.senv @ ["color", Mutil.encode x] }
    | _ -> conf
  in
  let get x = Util.p_getenv conf.env x in
  match get "em", get "ei", get "ep", get "en", get "eoc" with
  | Some vm, Some vi, _, _, _ -> set_senv conf (Mutil.encode vm) (Mutil.encode vi)
  | Some vm, None, Some vp, Some vn, voco ->
    let voc =
      match voco with
      | Some voc -> (try int_of_string voc with Failure _ -> 0)
      | None -> 0
    in
    let ip =
      match person_of_key base vp vn voc with
      | Some ip -> ip
      | None -> Hutil.incorrect_request conf ~comment:"error #2"; raise Exit
    in
    let vi = string_of_iper ip in
    set_senv conf (Mutil.encode vm) (Mutil.encode vi)
  | _ -> conf

let propose_base conf =
  let title _ = Output.print_sstring conf "Base" in
  Hutil.header conf title;
  Output.print_sstring conf {|<ul><li><form method="GET" action="|} ;
  Output.print_sstring conf conf.indep_command ;
  Output.print_sstring conf {|">|} ;
  Output.print_sstring conf {|<input name="b" size="40"> =&gt; |} ;
  Output.print_sstring conf {|<button type="submit" class="btn btn-secondary btn-lg">|} ;
  transl_nth conf "validate/delete" 0
  |> Utf8.capitalize_fst
  |> Output.print_sstring conf ;
  Output.print_sstring conf "</button></li></ul>";
  Hutil.trailer conf

let try_plugin list conf base_name m =
  let fn =
    if List.mem "*" list
    then (fun ( _, fn) -> fn conf base_name)
    else (fun (ns, fn) -> (List.mem ns conf.forced_plugins || List.mem ns list) && fn conf base_name)
  in
  List.exists fn (Hashtbl.find_all GwdPlugin.ht m)

let w_lock ~onerror fn conf (base_name : string option) =
  let bfile = Util.bpath (conf.bname ^ ".gwb") in
  Lock.control
    (Mutil.lock_file bfile) true
    ~onerror:(fun () -> onerror conf base_name)
    (fun () -> fn conf base_name)

let w_base ~none fn conf (bfile : string option) =
  match bfile with
  | None -> none conf
  | Some bfile ->
     let base = try Some (Gwdb.open_base bfile) with _ -> None in
     match base with
     | None -> none conf
     | Some base ->
        let conf = make_henv conf base in
        let conf = make_senv conf base in
        let conf = match Util.default_sosa_ref conf base with
          | Some p -> { conf with default_sosa_ref = get_iper p, Some p;
              nb_of_persons = Gwdb.nb_of_persons base;
              nb_of_families = Gwdb.nb_of_families base}
          | None -> { conf with
              nb_of_persons = Gwdb.nb_of_persons base;
              nb_of_families = Gwdb.nb_of_families base}
        in
        fn conf base

let w_person ~none fn conf base =
  match find_person_in_env conf base "" with
  | Some p -> fn conf base p
  | _ -> none conf base

let output_error ?headers ?content conf code =
  !GWPARAM.output_error ?headers ?content conf code

let w_wizard fn conf base =
  if conf.wizard then
    fn conf base
  else if conf.just_friend_wizard then
    output_error conf Def.Forbidden
  else
    (* FIXME: send authentification headers *)
    output_error conf Def.Unauthorized

let treat_request =
  let w_lock = w_lock ~onerror:(fun conf _ -> Update.error_locked conf) in
  let w_base =
    let none conf =
      if conf.bname = "" then output_error conf Def.Bad_Request
      else output_error conf Def.Not_Found
    in
    w_base ~none
  in
  let w_person = w_person ~none:very_unknown in
  fun conf ->
  let bfile =
    if conf.bname = "" then None
    else
      let bfile = Util.bpath (conf.bname ^ ".gwb") in
      if Sys.file_exists bfile
      then Some bfile
      else None
  in
  let process () =
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
    if List.mem "*" plugins then
      List.iter (fun (_ , fn) -> fn conf bfile) !GwdPlugin.se
    else
      List.iter (fun (ns, fn) -> if List.mem ns plugins then fn conf bfile) !GwdPlugin.se ;
    let m = Option.value ~default:"" (p_getenv conf.env "m") in
    if not @@ try_plugin plugins conf bfile m
    then begin
        if List.assoc_opt "counter" conf.base_env <> Some "no" &&
          m <> "IM" && m <> "IM_C" && m <> "SRC" && m <> "DOC"
        then begin
          match
            if only_special_env conf.env
            then SrcfileDisplay.incr_welcome_counter conf
            else SrcfileDisplay.incr_request_counter conf
          with
          | Some (welcome_cnt, request_cnt, start_date) ->
            GwdLog.log begin fun oc ->
              let thousand oc x = output_string oc @@ Mutil.string_of_int_sep ","  x in
              Printf.fprintf oc "  #accesses %a (#welcome %a) since %s\n"
                thousand (welcome_cnt + request_cnt) thousand welcome_cnt
                start_date
            end ;
          | None -> ()
        end ;
        let incorrect_request ?(comment = "") conf _ =
          incorrect_request ~comment:comment conf
        in
        let doc_aux conf base print =
          match Util.p_getenv conf.env "s" with
          | Some f ->
                if Filename.check_suffix f ".txt" then
                  let f = Filename.chop_suffix f ".txt" in
                  SrcfileDisplay.print_source conf base f
                else print conf f
          | _ -> incorrect_request conf ~comment:"error #3" base
        in
        match m with
        | "" ->
          let base =
            match bfile with
            | None -> None
            | Some bfile -> try Some (Gwdb.open_base bfile) with _ -> None
          in
          if base <> None then
            w_base @@
            if only_special_env conf.env then SrcfileDisplay.print_start
            else w_person @@ fun conf base p ->
              match p_getenv conf.env "ptempl" with
              | Some t when List.assoc_opt "ptempl" conf.base_env = Some "yes" ->
                Perso.interp_templ t conf base p
              | _ -> person_selected conf base p
          else if conf.bname = ""
          then fun conf _ -> include_template conf [] "index" (fun () -> propose_base conf)
          else
            w_base begin (* print_start -> welcome.txt *)
              if only_special_env conf.env then SrcfileDisplay.print_start
              else w_person @@ fun conf base p ->
                match p_getenv conf.env "ptempl" with
                | Some t when List.assoc_opt "ptempl" conf.base_env = Some "yes" ->
                  Perso.interp_templ t conf base p
                | _ -> person_selected conf base p
            end

        | "A" ->
          AscendDisplay.print |> w_person |> w_base
        | "ADD_FAM" ->
          w_wizard @@ w_base @@ UpdateFam.print_add
        | "ADD_FAM_OK" ->
          w_wizard @@ w_lock @@ w_base @@ UpdateFamOk.print_add
        | "ADD_IND" ->
          w_wizard @@ w_base @@ UpdateInd.print_add
        | "ADD_IND_OK" ->
          w_wizard @@ w_lock @@ w_base @@ UpdateIndOk.print_add
        | "ADD_PAR" ->
          w_wizard @@ w_base @@ UpdateFam.print_add_parents
        | "ADD_PAR_OK" ->
          w_wizard @@ w_lock @@ w_base @@ UpdateFamOk.print_add_parents
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
        | "AS" ->
          w_base @@ fun conf base ->
            SrcfileDisplay.print conf base "advanced"
        | "AS_OK" ->
          w_base @@ AdvSearchOkDisplay.print
        | "C" ->
          w_base @@ w_person @@ CousinsDisplay.print
        | "CAL" ->
          fun conf _ -> Hutil.print_calendar conf
        | "CHG_CHN" when conf.wizard ->
          w_wizard @@ w_base @@ ChangeChildrenDisplay.print
        | "CHG_CHN_OK" ->
          w_wizard @@ w_lock @@ w_base @@ ChangeChildrenDisplay.print_ok
        | "CHG_EVT_IND_ORD" ->
          w_wizard @@ w_base @@ UpdateInd.print_change_event_order
        | "CHG_EVT_IND_ORD_OK" ->
          w_wizard @@ w_lock @@ w_base @@ UpdateIndOk.print_change_event_order
        | "CHG_EVT_FAM_ORD" ->
          w_wizard @@ w_base @@ UpdateFam.print_change_event_order
        | "CHG_EVT_FAM_ORD_OK" ->
          w_wizard @@ w_lock @@ w_base @@ UpdateFamOk.print_change_event_order
        | "CHG_FAM_ORD" ->
          w_wizard @@ w_base @@ UpdateFam.print_change_order
        | "CHG_FAM_ORD_OK" ->
          w_wizard @@ w_lock @@ w_base @@ UpdateFamOk.print_change_order_ok
        | "CONN_WIZ" ->
          w_wizard @@ w_base @@ WiznotesDisplay.connected_wizards
        | "D" ->
          w_base @@ w_person @@ DescendDisplay.print
        | "DAG" ->
          w_base @@ DagDisplay.print
        | "DEL_FAM" ->
          w_wizard @@ w_base @@ UpdateFam.print_del
        | "DEL_FAM_OK" ->
          w_wizard @@ w_lock @@ w_base @@ UpdateFamOk.print_del

        | "DEL_IMAGE" ->
          w_wizard @@ w_lock @@ w_base @@ ImageCarrousel.print_del
        | "DEL_IMAGE_OK" ->
          w_wizard @@ w_lock @@ w_base @@ ImageCarrousel.print_del_ok
        | "DEL_IMAGE_C_OK" ->
          w_wizard @@ w_lock @@ w_base @@ ImageCarrousel.print_main_c

        | "DEL_IND" ->
          w_wizard @@ w_base @@ UpdateInd.print_del
        | "DEL_IND_OK" ->
          w_wizard @@ w_lock @@ w_base @@ UpdateIndOk.print_del
        | "DOC" ->
          w_base @@ fun conf base -> doc_aux conf base
            ImageDisplay.print_source
        | "DOCH" ->
          w_base @@ fun conf base -> doc_aux conf base
            (fun conf _base -> ImageDisplay.print_html conf)
        | "F" ->
          w_base @@ w_person @@ Perso.interp_templ "family"
        | "H" ->
          w_wizard @@ w_base @@ fun conf base ->
            ( match p_getenv conf.env "v" with
            | Some f -> SrcfileDisplay.print conf base f
            | None -> incorrect_request conf base ~comment:"error #4")
        | "HIST" ->
          w_base @@ History.print
        | "HIST_CLEAN" ->
          w_wizard @@ w_base @@ fun conf _ -> HistoryDiffDisplay.print_clean conf
        | "HIST_CLEAN_OK" ->
          w_wizard @@ w_base @@ fun conf _ -> HistoryDiffDisplay.print_clean_ok conf
        | "HIST_DIFF" ->
          w_base @@ HistoryDiffDisplay.print
        | "HIST_SEARCH" ->
          w_base @@ History.print_search

        | "IM_C" ->
          w_base @@ ImageCarrousel.print_c ~saved:false
        | "IM_C_S" ->
          w_base @@ ImageCarrousel.print_c ~saved:true


        | "IM" ->
          w_base @@ ImageDisplay.print
        | "IMH" ->
          w_base @@ fun conf _ -> ImageDisplay.print_html conf
        | "INV_FAM" ->
          w_wizard @@ w_base @@ UpdateFam.print_inv
        | "INV_FAM_OK" ->
          w_wizard @@ w_lock @@ w_base @@ UpdateFamOk.print_inv
        | "KILL_ANC" ->
          w_wizard @@ w_lock @@ w_base @@ MergeIndDisplay.print_kill_ancestors
        | "L" -> w_base @@ fun conf base -> Perso.interp_templ "list" conf base 
              (Gwdb.empty_person base Gwdb.dummy_iper) 
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
        | "MOD_DATA" ->
          w_wizard @@ w_base @@ UpdateDataDisplay.print_mod
        | "MOD_DATA_OK" ->
          w_wizard @@ w_lock @@ w_base @@ UpdateDataDisplay.print_mod_ok
        | "MOD_FAM" ->
          w_wizard @@ w_base @@ UpdateFam.print_mod
        | "MOD_FAM_OK" when conf.wizard ->
          w_wizard @@ w_lock @@ w_base @@ UpdateFamOk.print_mod
        | "MOD_IND" ->
          w_wizard @@ w_base @@ UpdateInd.print_mod
        | "MOD_IND_OK" ->
          w_wizard @@ w_lock @@ w_base @@ UpdateIndOk.print_mod
        | "MOD_NOTES" ->
          w_wizard @@ w_base @@ NotesDisplay.print_mod
        | "MOD_NOTES_OK" ->
          w_wizard @@ w_lock @@ w_base @@ NotesDisplay.print_mod_ok
        | "MOD_WIZNOTES" when conf.authorized_wizards_notes ->
          w_base @@ WiznotesDisplay.print_mod
        | "MOD_WIZNOTES_OK" when conf.authorized_wizards_notes ->
          w_lock @@ w_base @@ WiznotesDisplay.print_mod_ok
        | "MRG" ->
          w_wizard @@ w_base @@ w_person @@ MergeDisplay.print
        | "MRG_DUP" ->
          w_wizard @@ w_base @@ MergeDupDisplay.main_page
        | "MRG_DUP_IND_Y_N" ->
          w_wizard @@ w_lock @@ w_base @@ MergeDupDisplay.answ_ind_y_n
        | "MRG_DUP_FAM_Y_N" ->
          w_wizard @@ w_lock @@ w_base @@ MergeDupDisplay.answ_fam_y_n
        | "MRG_FAM" ->
          w_wizard @@ w_base @@ MergeFamDisplay.print
        | "MRG_FAM_OK" ->
          w_wizard @@ w_lock @@ w_base @@ MergeFamOk.print_merge
        | "MRG_MOD_FAM_OK" ->
          w_wizard @@ w_lock @@ w_base @@ MergeFamOk.print_mod_merge
        | "MRG_IND" ->
          w_wizard @@ w_lock @@ w_base @@ MergeIndDisplay.print
        | "MRG_IND_OK" -> (* despite the _OK suffix, this one does not actually update databse *)
          w_wizard @@ w_base @@ MergeIndOkDisplay.print_merge
        | "MRG_MOD_IND_OK" ->
          w_wizard @@ w_lock @@ w_base @@ MergeIndOkDisplay.print_mod_merge
        | "N" ->
          w_base @@ fun conf base -> begin match p_getenv conf.env "v" with
            | Some v -> Some.search_surname_print conf base Some.surname_not_found v
            | _ -> AllnDisplay.print_surnames conf base
          end
        | "NG" -> w_base @@ begin fun conf base ->
            (* Rétro-compatibilité <= 6.06 *)
            let env =
              match p_getenv conf.env "n" with
                Some n ->
                begin match p_getenv conf.env "t" with
                    Some "P" -> ("fn", Mutil.encode n) :: conf.env
                  | Some "N" -> ("sn", Mutil.encode n) :: conf.env
                  | _ -> ("v", Mutil.encode n) :: conf.env
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
                  Some.search_surname_print conf base unknown n
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
                    Some.search_first_name_print conf base fn
                  | None, Some sn ->
                    Some.search_surname_print conf base unknown sn
                  | None, None -> incorrect_request conf base ~comment:"error #5"
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
            | Some v -> Some.search_first_name_print conf base v
            | None -> AllnDisplay.print_first_names conf base
          end


        | "PERSO" ->
          w_base @@ w_person @@ Geneweb.Perso.interp_templ "perso"

        | "POP_PYR" when conf.wizard || conf.friend ->
          w_base @@ BirthDeathDisplay.print_population_pyramid
        | "PS" ->
          w_base @@ PlaceDisplay.print_all_places_surnames
        | "PPS" ->
          w_base @@ Place.print_all_places_surnames
        | "R" ->
          w_base @@ w_person @@ relation_print
        | "REFRESH" ->
          w_base @@ w_person @@ Perso.interp_templ "carrousel"
        | "REQUEST" ->
          w_wizard @@ fun _ _ ->
            Output.status conf Def.OK;
            Output.header conf "Content-type: text";
            List.iter begin fun s ->
              Output.print_sstring conf s ;
              Output.print_sstring conf "\n"
            end conf.Config.request ;

        | "RESET_IMAGE_C_OK" ->
          w_base @@ ImageCarrousel.print_main_c

        | "RL" ->
          w_base @@ RelationLink.print
        | "RLM" ->
          w_base @@ RelationDisplay.print_multi
        | "S" ->
          w_base @@ fun conf base -> SearchName.print conf base specify unknown

        | "SND_IMAGE" -> w_wizard @@w_lock @@ w_base @@ ImageCarrousel.print
        | "SND_IMAGE_OK" ->
           w_wizard @@ w_lock @@ w_base @@ ImageCarrousel.print_send_ok
        | "SND_IMAGE_C" ->
          w_base @@ w_person @@ Perso.interp_templ "carrousel"
        | "SND_IMAGE_C_OK" ->
          w_wizard @@ w_lock @@ w_base @@ ImageCarrousel.print_main_c

        | "SRC" ->
          w_base @@ fun conf base -> begin match p_getenv conf.env "v" with
            | Some f -> SrcfileDisplay.print_source conf base f
            | _ -> incorrect_request conf base ~comment:"error #6"
          end
        | "STAT" ->
          w_base @@ fun conf _ -> BirthDeathDisplay.print_statistics conf
        | "CHANGE_WIZ_VIS" ->
          w_wizard @@ w_lock @@ w_base @@ WiznotesDisplay.change_wizard_visibility
        | "TP" ->
          w_base @@ fun conf base ->
            begin match Util.p_getenv conf.env "v" with
            | Some f ->
              begin match Util.find_person_in_env conf base "" with
              | Some p -> Perso.interp_templ ("tp_" ^ f) conf base p
              | _ -> Perso.interp_templ ("tp0_" ^ f) conf base
                       (Gwdb.empty_person base Gwdb.dummy_iper)
              end
            | None -> incorrect_request conf base ~comment:"error #7"
            end
        | "TT" ->
          w_base @@ TitleDisplay.print
        | "U" ->
          w_wizard @@ w_base @@ w_person @@ updmenu_print
        | "VIEW_WIZNOTES" when conf.authorized_wizards_notes ->
          w_wizard @@ w_base @@ WiznotesDisplay.print_view
        | "WIZNOTES" when conf.authorized_wizards_notes ->
          w_base @@ WiznotesDisplay.print
        | "WIZNOTES_SEARCH" when conf.authorized_wizards_notes ->
          w_base @@ WiznotesDisplay.print_search
        | _ ->
            w_base @@ fun conf base ->
            incorrect_request conf base ~comment:"error #10"
      end conf bfile ;
  end else begin
    let title _ =
      Printf.sprintf "%s %s %s"
      (transl conf "base" |> Utf8.capitalize_fst)
      conf.bname
      (transl conf "reserved to friends or wizards")
      |> Output.print_sstring conf
    in
    Hutil.rheader conf title ;
    let base_name =
      if conf.cgi then (Printf.sprintf "b=%s&" conf.bname) else ""
    in
    let user = transl_nth conf "user/password/cancel" 0 in
    let passwd = transl_nth conf "user/password/cancel" 1 in
    let body =
      if conf.cgi then
        Printf.sprintf {|
            <input type="text" class="form-control" name="w"
              title="%s/%s %s" placeholder="%s:%s"
              aria-label="password input"
              aria-describedby="username:password" autofocus>
            <label for="w" class="sr-only">%s:%s</label>
            <div class="input-group-append">
              <button type="submit" class="btn btn-primary">OK</button>
            </div>|}
            (transl_nth conf "wizard/wizards/friend/friends/exterior" 2)
            (transl_nth conf "wizard/wizards/friend/friends/exterior" 0)
            passwd user passwd user passwd
      else
        Printf.sprintf {|
            <div>
              <ul>
              <li>%s%s <a href="%s?%sw=f"> %s</a></li>
              <li>%s%s <a href="%s?%sw=w"> %s</a></li>
              </ul>
            </div> |}
            (transl conf "access" |> Utf8.capitalize_fst) (transl conf ":")
            (conf.command :> string) base_name
            (transl_nth conf "wizard/wizards/friend/friends/exterior" 2)
            (transl conf "access" |> Utf8.capitalize_fst) (transl conf ":")
            (conf.command :> string) base_name
            (transl_nth conf "wizard/wizards/friend/friends/exterior" 0)
    in
    Output.print_sstring conf 
      (Printf.sprintf {|
        <form class="form-inline" method="post" action="%s">
          <div class="input-group mt-1">
            <input type="hidden" name="b" value="%s">
            %s
          </div>
        </form>
      |} (conf.command :> string) (conf.bname) body
      );
    Hutil.trailer conf
  end
  in
  if conf.debug then Mutil.bench (__FILE__ ^ " " ^ string_of_int __LINE__) process
  else process ()

let treat_request conf =
  try treat_request conf with Update.ModErr _ -> Output.flush conf
