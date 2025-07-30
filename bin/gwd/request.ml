(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Config
open Def
open Util
module Logs = Geneweb_logs.Logs
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let person_is_std_key conf base p k =
  let k = Name.strip_lower k in
  if
    k
    = Name.strip_lower
        (Driver.p_first_name base p ^ " " ^ Driver.p_surname base p)
  then true
  else if
    List.exists
      (fun n -> Name.strip n = k)
      (Driver.person_misc_names base p (nobtit conf base))
  then true
  else false

let select_std_eq conf base pl k =
  List.fold_right
    (fun p pl -> if person_is_std_key conf base p k then p :: pl else pl)
    pl []

let find_all conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match (sosa_ref, sosa_nb) with
  | Some p, Some n ->
      if n <> Sosa.zero then
        match Util.branch_of_sosa conf base n p with
        | Some (p :: _) -> ([ p ], true)
        | _ -> ([], false)
      else ([], false)
  | _ ->
      let acc = Option.to_list @@ SearchName.search_by_key conf base an in
      if acc <> [] then (acc, false)
      else
        ( SearchName.search_key_aux
            (fun conf base acc an ->
              let spl = select_std_eq conf base acc an in
              if spl = [] then
                if acc = [] then SearchName.search_by_name conf base an else acc
              else spl)
            conf base an,
          false )

let relation_print conf base p =
  let p1 =
    match p_getenv conf.senv "ei" with
    | Some i ->
        conf.senv <- [];
        let i = Driver.Iper.of_string i in
        if Geneweb_db.Driver.iper_exists base i then Some (pget conf base i)
        else None
    | None -> (
        match find_person_in_env conf base "1" with
        | Some p1 ->
            conf.senv <- [];
            Some p1
        | None -> None)
  in
  RelationDisplay.print conf base p p1

let specify conf base n pl1 pl2 pl3 =
  let title _ = Output.printf conf "%s : %s" n (transl conf "specify") in
  let n = Name.crush_lower n in
  let ptll pl =
    List.map
      (fun p ->
        let tl = ref [] in
        let add_tl t =
          tl :=
            let rec add_rec = function
              | t1 :: tl1 ->
                  if
                    Driver.Istr.equal t1.t_ident t.t_ident
                    && Driver.Istr.equal t1.t_place t.t_place
                  then t1 :: tl1
                  else t1 :: add_rec tl1
              | [] -> [ t ]
            in
            add_rec !tl
        in
        let compare_and_add t pn =
          let pn = Driver.sou base pn in
          if Name.crush_lower pn = n then add_tl t
          else
            match Driver.get_qualifiers p with
            | nn :: _ ->
                let nn = Driver.sou base nn in
                if Name.crush_lower (pn ^ " " ^ nn) = n then add_tl t
            | _ -> ()
        in
        List.iter
          (fun t ->
            match (t.t_name, Driver.get_public_name p) with
            | Tname s, _ -> compare_and_add t s
            | _, pn when Driver.sou base pn <> "" -> compare_and_add t pn
            | _ -> ())
          (nobtit conf base p);
        (p, !tl))
      pl
  in

  let sort_ptll ptll =
    let l =
      List.rev_map
        (fun (p, v) ->
          let bi = Driver.get_birth p in
          let bi = if bi = Date.cdate_None then Driver.get_baptism p else bi in
          (p, v, Date.cdate_to_dmy_opt bi))
        ptll
    in
    List.sort
      (fun (_, _, dmy1) (_, _, dmy2) ->
        Option.compare Date.compare_dmy dmy2 dmy1)
      l
    |> List.rev_map (fun (p, v, _) -> (p, v))
  in
  let ptll1 = ptll pl1 |> sort_ptll in
  let ptll2 = ptll pl2 |> sort_ptll in
  let ptll3 = ptll pl3 |> sort_ptll in
  Hutil.header conf title;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  (* TODO set possible limit to number of persons displayed (ptll) *)
  (* Construction de la table des sosa de la base *)
  let () = SosaCache.build_sosa_ht conf base in
  Output.print_sstring conf "<ul>\n";
  List.iter
    (fun (p, _tl) ->
      Output.print_sstring conf "<li>\n";
      SosaCache.print_sosa conf base p true;
      Update.print_person_parents_and_spouses conf base p;
      Output.print_sstring conf "</li>\n")
    ptll1;
  Output.print_sstring conf "</ul>\n";
  if ptll2 <> [] then (
    Output.print_sstring conf
      (transl conf "other possibilities" |> Utf8.capitalize_fst);
    Output.print_sstring conf "<ul>\n";
    List.iter
      (fun (p, _tl) ->
        Output.print_sstring conf "<li>\n";
        SosaCache.print_sosa conf base p true;
        Update.print_person_parents_and_spouses conf base p;
        Output.print_sstring conf "</li>\n")
      ptll2;
    Output.print_sstring conf "</ul>\n");
  if ptll3 <> [] then (
    Output.print_sstring conf
      (transl conf "with spouse name" |> Utf8.capitalize_fst);
    Output.print_sstring conf "<ul>\n";
    List.iter
      (fun (p, _tl) ->
        Output.print_sstring conf "<li>\n";
        SosaCache.print_sosa conf base p true;
        Update.print_person_parents_and_spouses conf base p;
        Output.print_sstring conf "</li>\n")
      ptll3;
    Output.print_sstring conf "</ul>\n");

  Hutil.trailer conf

let this_request_updates_database conf =
  match p_getenv conf.env "m" with
  | Some
      ( "ADD_FAM" | "ADD_IND" | "CHANGE_WIZ_VIS" | "CHG_CHN" | "CHG_FAM_ORD"
      | "DEL_FAM" | "DEL_IMAGE" | "DEL_IND" | "INV_FAM" | "KILL_ANC" | "MOD_FAM"
      | "MOD_IND" | "MOD_NOTES" | "MOD_WIZNOTES" | "MRG_DUP_IND_Y_N"
      | "MRG_DUP_FAM_Y_N" | "MRG_IND" | "MRG_MOD_FAM" | "MRG_MOD_IND"
      | "MOD_DATA" | "SND_IMAGE" ) ->
      true
  | _ -> false

let incorrect_request ?(comment = "") conf =
  Hutil.incorrect_request ~comment conf

let person_selected conf base p =
  match p_getenv conf.senv "em" with
  | Some "R" -> relation_print conf base p
  | Some _ -> incorrect_request conf ~comment:"incorrect em= value"
  | None ->
      record_visited conf (Driver.get_iper p);
      Perso.print conf base p

let person_selected_with_redirect conf base p =
  match p_getenv conf.senv "em" with
  | Some "R" -> relation_print conf base p
  | Some _ -> incorrect_request conf ~comment:"Incorrect em= value"
  | None ->
      Wserver.http_redirect_temporarily
        (commd conf ^^^ Util.acces conf base p :> string)

let updmenu_print = Perso.interp_templ "updmenu"

(* Print Not found page *)
let unknown conf n =
  let title _ =
    transl conf "not found" |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf (transl conf ":");
    Output.print_sstring conf {| "|};
    Output.print_string conf (Util.escape_html n);
    Output.print_sstring conf {|"|}
  in
  Output.status conf Def.Not_Found;
  Hutil.header ~error:true conf title;
  Hutil.trailer conf

let make_henv conf base =
  let conf =
    match Util.find_sosa_ref conf base with
    | Some p ->
        let x =
          let first_name = Driver.p_first_name base p in
          let surname = Driver.p_surname base p in
          if Util.accessible_by_key conf base p first_name surname then
            [
              ("pz", Name.lower first_name |> Mutil.encode);
              ("nz", Name.lower surname |> Mutil.encode);
              ("ocz", Driver.get_occ p |> string_of_int |> Mutil.encode);
            ]
          else
            [
              ("iz", Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode);
            ]
        in
        { conf with henv = conf.henv @ x }
    | None -> conf
  in
  let conf =
    match p_getenv conf.env "dsrc" with
    | Some "" | None -> conf
    | Some s -> { conf with henv = conf.henv @ [ ("dsrc", Mutil.encode s) ] }
  in
  let conf =
    match p_getenv conf.env "templ" with
    | None -> conf
    | Some s -> { conf with henv = conf.henv @ [ ("templ", Mutil.encode s) ] }
  in
  let conf =
    match Util.p_getenv conf.env "escache" with
    | Some _ ->
        { conf with henv = conf.henv @ [ ("escache", escache_value base) ] }
    | None -> conf
  in
  let conf =
    if Util.p_getenv conf.env "manitou" = Some "off" then
      { conf with henv = conf.henv @ [ ("manitou", Adef.encoded "off") ] }
    else conf
  in
  let conf =
    if Util.p_getenv conf.env "fmode" = Some "on" then
      { conf with henv = conf.henv @ [ ("fmode", Adef.encoded "on") ] }
    else conf
  in
  let conf =
    let fn, oc, sn = GWPARAM.split_key conf.userkey in
    match
      Geneweb_db.Driver.person_of_key base fn sn
        (if oc = "" then 0 else int_of_string oc)
    with
    | Some ip ->
        {
          conf with
          semi_public =
            (if conf.semi_public then
               Driver.get_access (Driver.poi base ip) = SemiPublic
             else true);
          user_iper = Some ip;
        }
    | None -> conf
  in
  let aux param conf =
    match Util.p_getenv conf.env param with
    | Some s -> { conf with henv = conf.henv @ [ (param, Mutil.encode s) ] }
    | None -> conf
  in
  aux "alwsurn" conf |> aux "pure_xhtml" |> aux "size" |> aux "p_mod"
  |> aux "wide"

let special_vars =
  [
    "alwsurn";
    "cgl";
    "dsrc";
    "em";
    "ei";
    "ep";
    "en";
    "eoc";
    "escache";
    "et";
    "iz";
    "long";
    "manitou";
    "nz";
    "ocz";
    "fmode";
    "p_mod";
    "pure_xhtml";
    "pz";
    "size";
    "templ";
    "wide";
  ]

let only_special_env env =
  List.for_all (fun (x, _) -> List.mem x special_vars) env

let make_senv conf base =
  let set_senv conf vm vi =
    let aux k v conf =
      if p_getenv conf.env k = Some v then
        { conf with senv = conf.senv @ [ (k, Mutil.encode v) ] }
      else conf
    in
    let conf =
      { conf with senv = [ ("em", vm); ("ei", vi) ] } |> aux "long" "on"
    in
    let conf =
      match p_getenv conf.env "et" with
      | Some x -> { conf with senv = conf.senv @ [ ("et", Mutil.encode x) ] }
      | _ -> conf
    in
    let conf = aux "cgl" "on" conf in
    let conf =
      match p_getenv conf.env "bd" with
      | None | Some ("0" | "") -> conf
      | Some x -> { conf with senv = conf.senv @ [ ("bd", Mutil.encode x) ] }
    in
    match p_getenv conf.env "color" with
    | Some x -> { conf with senv = conf.senv @ [ ("color", Mutil.encode x) ] }
    | _ -> conf
  in
  let get x = Util.p_getenv conf.env x in
  match (get "em", get "ei", get "ep", get "en", get "eoc") with
  | Some vm, Some vi, _, _, _ ->
      set_senv conf (Mutil.encode vm) (Mutil.encode vi)
  | Some vm, None, Some vp, Some vn, voco ->
      let voc =
        match voco with
        | Some voc -> ( try int_of_string voc with Failure _ -> 0)
        | None -> 0
      in
      let ip =
        match Driver.person_of_key base vp vn voc with
        | Some ip -> ip
        | None ->
            Hutil.incorrect_request conf
              ~comment:"Incorrect em=, ei=, ep=, en=, eoc= configuration";
            raise Exit
      in
      let vi = Driver.Iper.to_string ip in
      set_senv conf (Mutil.encode vm) (Mutil.encode vi)
  | _ -> conf

let propose_base conf =
  let title _ = Output.print_sstring conf "Base" in
  Hutil.header conf title;
  Output.print_sstring conf {|<ul><li><form method="GET" action="|};
  Output.print_sstring conf conf.indep_command;
  Output.print_sstring conf {|">|};
  Output.print_sstring conf {|<input name="b" size="40"> =&gt; |};
  Output.print_sstring conf
    {|<button type="submit" class="btn btn-secondary btn-lg">|};
  transl_nth conf "validate/delete" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "</button></li></ul>";
  Hutil.trailer conf

let try_plugin list conf base_name m =
  let fn =
    if List.mem "*" list then fun (_, fn) -> fn conf base_name
    else fun (ns, fn) ->
      (List.mem ns conf.forced_plugins || List.mem ns list) && fn conf base_name
  in
  List.exists fn (Hashtbl.find_all GwdPlugin.ht m)

let w_lock ~onerror fn conf (base_name : string option) =
  let bfile = !GWPARAM.bpath conf.bname in
  (* FIXME: we lost the backtrace because onerror does not handle it. *)
  Lock.control
    ~on_exn:(fun _exn _bt -> onerror conf base_name)
    ~wait:true ~lock_file:(Mutil.lock_file bfile)
  @@ fun () -> fn conf base_name

let w_base ~none fn conf (bfile : string option) =
  match bfile with
  | None -> none conf
  | Some bfile ->
      Driver.with_database bfile (fun base ->
          let conf = make_henv conf base in
          let conf = make_senv conf base in
          let conf =
            match Util.default_sosa_ref conf base with
            | Some p ->
                {
                  conf with
                  default_sosa_ref = (Driver.get_iper p, Some p);
                  nb_of_persons = Driver.nb_of_persons base;
                  nb_of_families = Driver.nb_of_families base;
                }
            | None ->
                {
                  conf with
                  nb_of_persons = Driver.nb_of_persons base;
                  nb_of_families = Driver.nb_of_families base;
                }
          in
          fn conf base)

let w_person ~none fn conf base =
  match find_person_in_env conf base "" with
  | Some p -> fn conf base p
  | _ -> none conf base

let w_wizard fn conf base =
  if conf.wizard then fn conf base
  else if conf.just_friend_wizard then GWPARAM.output_error conf Def.Forbidden
  else
    (* FIXME: send authentification headers *)
    GWPARAM.output_error conf Def.Unauthorized

let treat_request =
  let w_lock = w_lock ~onerror:(fun conf _ -> Update.error_locked conf) in
  let w_base =
    let none conf =
      if conf.bname = "" then GWPARAM.output_error conf Def.Bad_Request
      else GWPARAM.output_error conf Def.Not_Found
    in
    w_base ~none
  in
  let w_person = w_person ~none:SrcfileDisplay.print_welcome in
  let print_page conf l =
    w_base
      (if only_special_env conf.env then SrcfileDisplay.print_welcome
       else
         w_person @@ fun conf base p ->
         match p_getenv conf.env "ptempl" with
         | Some t when List.assoc_opt "ptempl" conf.base_env = Some "yes" ->
             Perso.interp_templ t conf base p
         | _ -> person_selected conf base p)
      conf l
  in
  let handle_no_bfile conf l =
    if conf.bname = "" then
      try Templ.output_simple conf Templ.Env.empty "index"
      with _ -> propose_base conf
    else print_page conf l
  in
  fun conf ->
    let bfile =
      if conf.bname = "" then None
      else
        let bfile =
          Filename.concat (Secure.base_dir ()) (conf.bname ^ ".gwb")
        in
        if Sys.file_exists bfile then Some bfile else None
    in
    let process () =
      if
        conf.wizard || conf.friend
        || List.assoc_opt "visitor_access" conf.base_env <> Some "no"
      then
        if
          List.assoc_opt "wizards_cant_write" conf.base_env = Some "yes"
          && this_request_updates_database conf
        then
          Hutil.incorrect_request conf
            ~comment:"Wizard actions not allowed on this base"
        else (
          (if Sys.unix then
             match bfile with
             | None -> ()
             | Some bfile ->
                 let stat = Unix.stat bfile in
                 let fuid = stat.Unix.st_uid in
                 let fgid = stat.Unix.st_gid in
                 let pgid = Unix.getgid () in
                 let puid = Unix.getuid () in
                 (* FIXME possible issue with effective uid/gid! *)
                 (* see setuid, setgid man pages *)
                 if puid <> fuid then Unix.setuid fuid;
                 if pgid <> fgid then Unix.setgid fgid);

          let plugins =
            match List.assoc_opt "plugins" conf.Config.base_env with
            | None -> []
            | Some list -> String.split_on_char ',' list |> List.map String.trim
          in
          if List.mem "*" plugins then
            List.iter (fun (_, fn) -> fn conf bfile) !GwdPlugin.se
          else
            List.iter
              (fun (ns, fn) -> if List.mem ns plugins then fn conf bfile)
              !GwdPlugin.se;
          let m = Option.value ~default:"" (p_getenv conf.env "m") in
          if not @@ try_plugin plugins conf bfile m then
            ((if
                List.assoc_opt "counter" conf.base_env <> Some "no"
                && m <> "IM" && m <> "IM_C" && m <> "SRC" && m <> "DOC"
              then
                match
                  if only_special_env conf.env then
                    SrcfileDisplay.incr_welcome_counter conf
                  else SrcfileDisplay.incr_request_counter conf
                with
                | Some (welcome_cnt, request_cnt, start_date) ->
                    let thousand ppf x =
                      Fmt.string ppf (Mutil.string_of_int_sep "," x)
                    in
                    Logs.info (fun k ->
                        k "#accesses %a (#welcome %a) since %s" thousand
                          (welcome_cnt + request_cnt)
                          thousand welcome_cnt start_date)
                | None -> ());
             let incorrect_request ?(comment = "") conf _ =
               incorrect_request ~comment conf
             in
             let doc_aux conf base print =
               match Util.p_getenv conf.env "s" with
               | Some f ->
                   if Filename.check_suffix f ".txt" then
                     let f = Filename.chop_suffix f ".txt" in
                     SrcfileDisplay.print_source conf base f
                   else print conf f
               | _ ->
                   incorrect_request conf ~comment:"Missing s= for m=DOC" base
             in
             match m with
             | "" -> (
                 match bfile with
                 | Some bfile -> (
                     (* We attempt to load the database in order to detect issues. *)
                     try
                       Driver.with_database bfile ignore;
                       print_page
                     with _ -> handle_no_bfile)
                 | None -> handle_no_bfile)
             | "A" -> AscendDisplay.print |> w_person |> w_base
             | "ADD_FAM" -> w_wizard @@ w_base @@ UpdateFam.print_add
             | "ADD_FAM_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ UpdateFamOk.print_add
             | "ADD_IND" -> w_wizard @@ w_base @@ UpdateInd.print_add
             | "ADD_IND_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ UpdateIndOk.print_add
             | "ADD_PAR" -> w_wizard @@ w_base @@ UpdateFam.print_add_parents
             | "ADD_PAR_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ UpdateFamOk.print_add_parents
             | "ANM" ->
                 w_base @@ fun conf _ ->
                 BirthdayDisplay.print_anniversaries conf
             | "AN" -> (
                 w_base @@ fun conf base ->
                 match p_getenv conf.env "v" with
                 | Some x ->
                     BirthdayDisplay.print_birth conf base (int_of_string x)
                 | _ -> BirthdayDisplay.print_menu_birth conf base)
             | "AD" -> (
                 w_base @@ fun conf base ->
                 match p_getenv conf.env "v" with
                 | Some x ->
                     BirthdayDisplay.print_dead conf base (int_of_string x)
                 | _ -> BirthdayDisplay.print_menu_dead conf base)
             | "AM" -> (
                 w_base @@ fun conf base ->
                 match p_getenv conf.env "v" with
                 | Some x ->
                     BirthdayDisplay.print_marriage conf base (int_of_string x)
                 | _ -> BirthdayDisplay.print_menu_marriage conf base)
             | "AS" ->
                 w_base @@ fun conf base ->
                 SrcfileDisplay.print conf base "advanced"
             | "AS_OK" -> w_base @@ AdvSearchOkDisplay.print
             | "BLASON_MOVE_TO_ANC" -> w_base @@ ImageCarrousel.print_main_c
             | "BLASON_STOP" -> w_base @@ ImageCarrousel.print_main_c
             | "C" -> w_base @@ w_person @@ CousinsDisplay.print
             | "CHK_DATA" -> w_base @@ CheckDataDisplay.print
             | "CAL" -> w_base @@ Hutil.print_calendar
             | "CHANGE_WIZ_VIS" ->
                 w_wizard @@ w_lock @@ w_base
                 @@ WiznotesDisplay.change_wizard_visibility
             | "CHG_CHN" when conf.wizard ->
                 w_wizard @@ w_base @@ ChangeChildrenDisplay.print
             | "CHG_CHN_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ ChangeChildrenDisplay.print_ok
             | "CHG_EVT_IND_ORD" ->
                 w_wizard @@ w_base @@ UpdateInd.print_change_event_order
             | "CHG_EVT_IND_ORD_OK" ->
                 w_wizard @@ w_lock @@ w_base
                 @@ UpdateIndOk.print_change_event_order
             | "CHG_EVT_FAM_ORD" ->
                 w_wizard @@ w_base @@ UpdateFam.print_change_event_order
             | "CHG_EVT_FAM_ORD_OK" ->
                 w_wizard @@ w_lock @@ w_base
                 @@ UpdateFamOk.print_change_event_order
             | "CHG_FAM_ORD" ->
                 w_wizard @@ w_base @@ UpdateFam.print_change_order
             | "CHG_FAM_ORD_OK" ->
                 w_wizard @@ w_lock @@ w_base
                 @@ UpdateFamOk.print_change_order_ok
             | "CONN_WIZ" ->
                 w_wizard @@ w_base @@ WiznotesDisplay.connected_wizards
             | "D" -> w_base @@ w_person @@ DescendDisplay.print
             | "DAG" -> w_base @@ DagDisplay.print
             | "DEL_FAM" -> w_wizard @@ w_base @@ UpdateFam.print_del
             | "DEL_FAM_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ UpdateFamOk.print_del
             | "DEL_IMAGE" ->
                 w_wizard @@ w_lock @@ w_base @@ ImageCarrousel.print_del
             | "DEL_IMAGE_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ ImageCarrousel.print_del_ok
             | "DEL_IMAGE_C_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ ImageCarrousel.print_main_c
             | "DEL_IND" -> w_wizard @@ w_base @@ UpdateInd.print_del
             | "DEL_IND_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ UpdateIndOk.print_del
             | "DOC" ->
                 w_base @@ fun conf base ->
                 doc_aux conf base ImageDisplay.print_source
             | "DOCH" ->
                 w_base @@ fun conf base ->
                 doc_aux conf base (fun conf _base ->
                     ImageDisplay.print_html conf)
             | "F" -> w_base @@ w_person @@ Perso.interp_templ "family"
             | "FIM" -> w_base @@ ImageDisplay.print_blason
             | "H" -> (
                 w_base @@ fun conf base ->
                 match p_getenv conf.env "v" with
                 | Some f -> SrcfileDisplay.print conf base f
                 | None ->
                     incorrect_request conf base ~comment:"Missing v= for m=H")
             | "HIST" -> w_base @@ History.print
             | "HIST_CLEAN" ->
                 w_wizard @@ w_base
                 @@ fun conf _ -> HistoryDiffDisplay.print_clean conf
             | "HIST_CLEAN_OK" ->
                 w_wizard @@ w_base
                 @@ fun conf _ -> HistoryDiffDisplay.print_clean_ok conf
             | "HIST_DIFF" -> w_base @@ HistoryDiffDisplay.print
             | "HIST_SEARCH" -> w_base @@ History.print_search
             | "IM_C" -> w_base @@ ImageCarrousel.print_c ~saved:false
             | "IM_C_S" -> w_base @@ ImageCarrousel.print_c ~saved:true
             | "IM" -> w_base @@ ImageDisplay.print
             | "IMAGE_TO_BLASON" -> w_base @@ ImageCarrousel.print_main_c
             | "IMH" -> w_base @@ fun conf _ -> ImageDisplay.print_html conf
             | "INV_FAM" -> w_wizard @@ w_base @@ UpdateFam.print_inv
             | "INV_FAM_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ UpdateFamOk.print_inv
             | "KILL_ANC" ->
                 w_wizard @@ w_lock @@ w_base
                 @@ MergeIndDisplay.print_kill_ancestors
             | "L" ->
                 w_base @@ fun conf base ->
                 Perso.interp_templ "list" conf base
                   (Driver.empty_person base Driver.Iper.dummy)
             | "LB" when conf.wizard || conf.friend ->
                 w_base @@ BirthDeathDisplay.print_birth
             | "LD" when conf.wizard || conf.friend ->
                 w_base @@ BirthDeathDisplay.print_death
             | "LINKED" -> w_base @@ w_person @@ NotesDisplay.print_what_links_p
             | "LL" -> w_base @@ BirthDeathDisplay.print_longest_lived
             | "LM" when conf.wizard || conf.friend ->
                 w_base @@ BirthDeathDisplay.print_marriage
             | "MISC_NOTES" -> w_base @@ NotesDisplay.print_misc_notes
             | "MISC_NOTES_SEARCH" ->
                 w_base @@ NotesDisplay.print_misc_notes_search
             | "MOD_DATA" -> w_wizard @@ w_base @@ UpdateDataDisplay.print_mod
             | "MOD_DATA_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ UpdateDataDisplay.print_mod_ok
             | "MOD_FAM" -> w_wizard @@ w_base @@ UpdateFam.print_mod
             | "MOD_FAM_OK" when conf.wizard ->
                 w_wizard @@ w_lock @@ w_base @@ UpdateFamOk.print_mod
             | "MOD_IND" -> w_wizard @@ w_base @@ UpdateInd.print_mod
             | "MOD_IND_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ UpdateIndOk.print_mod
             | "MOD_NOTES" ->
                 w_wizard
                 @@ w_base (fun conf base ->
                        match p_getenv conf.env "ajax" with
                        | Some "on" ->
                            let charset =
                              if conf.charset = "" then "utf-8"
                              else conf.charset
                            in
                            Output.header conf
                              "Content-type: application/json; charset=%s"
                              charset;
                            NotesDisplay.print_mod_json conf base
                        | _ -> NotesDisplay.print_mod conf base)
             | "MOD_NOTES_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ NotesDisplay.print_mod_ok
             | "MOD_WIZNOTES" when conf.authorized_wizards_notes ->
                 w_base @@ WiznotesDisplay.print_mod
             | "MOD_WIZNOTES_OK" when conf.authorized_wizards_notes ->
                 w_lock @@ w_base @@ WiznotesDisplay.print_mod_ok
             | "MRG" -> w_wizard @@ w_base @@ w_person @@ MergeDisplay.print
             | "MRG_DUP" -> w_wizard @@ w_base @@ MergeDupDisplay.main_page
             | "MRG_DUP_IND_Y_N" ->
                 w_wizard @@ w_lock @@ w_base @@ MergeDupDisplay.answ_ind_y_n
             | "MRG_DUP_FAM_Y_N" ->
                 w_wizard @@ w_lock @@ w_base @@ MergeDupDisplay.answ_fam_y_n
             | "MRG_FAM" -> w_wizard @@ w_base @@ MergeFamDisplay.print
             | "MRG_FAM_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ MergeFamOk.print_merge
             | "MRG_MOD_FAM_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ MergeFamOk.print_mod_merge
             | "MRG_IND" ->
                 w_wizard @@ w_lock @@ w_base @@ MergeIndDisplay.print
             | "MRG_IND_OK" ->
                 (* despite the _OK suffix, this one does not actually update databse *)
                 w_wizard @@ w_base @@ MergeIndOkDisplay.print_merge
             | "MRG_MOD_IND_OK" ->
                 w_wizard @@ w_lock @@ w_base
                 @@ MergeIndOkDisplay.print_mod_merge
             | "N" -> (
                 w_base @@ fun conf base ->
                 match p_getenv conf.env "v" with
                 | Some v ->
                     Some.search_surname_print conf base Some.surname_not_found
                       v
                 | _ -> AllnDisplay.print_surnames conf base)
             | "NG" -> (
                 w_base @@ fun conf base ->
                 (* Rétro-compatibilité <= 6.06 *)
                 let env =
                   match p_getenv conf.env "n" with
                   | Some n -> (
                       match p_getenv conf.env "t" with
                       | Some "P" -> ("fn", Mutil.encode n) :: conf.env
                       | Some "N" -> ("sn", Mutil.encode n) :: conf.env
                       | _ -> ("v", Mutil.encode n) :: conf.env)
                   | None -> conf.env
                 in
                 let conf = { conf with env } in
                 (* Nouveau mode de recherche. *)
                 match p_getenv conf.env "select" with
                 | Some "input" | None -> (
                     (* Récupère le contenu non vide de la recherche. *)
                     let real_input label =
                       match p_getenv conf.env label with
                       | Some s -> if s = "" then None else Some s
                       | None -> None
                     in
                     (* Recherche par clé, sosa, alias ... *)
                     let search n =
                       let pl, sosa_acc = find_all conf base n in
                       match pl with
                       | [] -> Some.search_surname_print conf base unknown n
                       | [ p ] ->
                           if
                             sosa_acc
                             || Gutil.person_of_string_key base n <> None
                             || person_is_std_key conf base p n
                           then person_selected_with_redirect conf base p
                           else specify conf base n pl [] []
                       | pl -> specify conf base n pl [] []
                     in
                     match real_input "v" with
                     | Some n -> search n
                     | None -> (
                         match (real_input "fn", real_input "sn") with
                         | Some fn, Some sn -> search (fn ^ " " ^ sn)
                         | Some fn, None ->
                             Some.search_first_name_print conf base fn
                         | None, Some sn ->
                             Some.search_surname_print conf base unknown sn
                         | None, None ->
                             incorrect_request conf base
                               ~comment:"Missing fn= and sn= for m=NG"))
                 | Some i ->
                     relation_print conf base
                       (pget conf base (Driver.Iper.of_string i)))
             | "NOTES" ->
                 w_base (fun conf base ->
                     match
                       (p_getenv conf.env "ref", p_getenv conf.env "ajax")
                     with
                     | Some "on", _ ->
                         let fnotes =
                           match p_getenv conf.env "f" with
                           | Some f ->
                               if NotesLinks.check_file_name f <> None then f
                               else ""
                           | None -> ""
                         in
                         NotesDisplay.print_what_links conf base fnotes
                     | _, Some "on" ->
                         let charset =
                           if conf.charset = "" then "utf-8" else conf.charset
                         in
                         Output.header conf
                           "Content-type: application/json; charset=%s" charset;
                         NotesDisplay.print_json conf base
                     | _ -> NotesDisplay.print conf base)
             | "OA" when conf.wizard || conf.friend ->
                 w_base @@ BirthDeathDisplay.print_oldest_alive
             | "OE" when conf.wizard || conf.friend ->
                 w_base @@ BirthDeathDisplay.print_oldest_engagements
             | "P" -> (
                 w_base @@ fun conf base ->
                 match p_getenv conf.env "v" with
                 | Some v -> Some.search_first_name_print conf base v
                 | None -> AllnDisplay.print_first_names conf base)
             | "PERSO" ->
                 w_base @@ w_person @@ Geneweb.Perso.interp_templ "perso"
             | "POP_PYR" when conf.wizard || conf.friend ->
                 w_base @@ BirthDeathDisplay.print_population_pyramid
             | "PORTRAIT_TO_BLASON" -> w_base @@ ImageCarrousel.print_main_c
             | "PS" -> w_base @@ PlaceDisplay.print_all_places_surnames
             | "PPS" -> w_base @@ Place.print_all_places_surnames
             | "R" -> w_base @@ w_person @@ relation_print
             | "REQUEST" ->
                 w_wizard @@ fun _ _ ->
                 Output.status conf Def.OK;
                 Output.header conf "Content-type: text";
                 List.iter
                   (fun s ->
                     Output.print_sstring conf s;
                     Output.print_sstring conf "\n")
                   conf.Config.request
             | "RESET_IMAGE_C_OK" -> w_base @@ ImageCarrousel.print_main_c
             | "RL" -> w_base @@ RelationLink.print
             | "RLM" -> w_base @@ RelationDisplay.print_multi
             | "S" ->
                 w_base @@ fun conf base ->
                 SearchName.print conf base specify unknown
             | "SND_IMAGE" ->
                 w_wizard @@ w_lock @@ w_base @@ ImageCarrousel.print
             | "SND_IMAGE_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ ImageCarrousel.print_send_ok
             | "SND_IMAGE_C" ->
                 w_base @@ w_person @@ Perso.interp_templ "carrousel"
             | "SND_IMAGE_C_OK" ->
                 w_wizard @@ w_lock @@ w_base @@ ImageCarrousel.print_main_c
             | "SRC" -> (
                 w_base @@ fun conf base ->
                 match p_getenv conf.env "v" with
                 | Some f -> SrcfileDisplay.print_source conf base f
                 | _ ->
                     incorrect_request conf base ~comment:"Missing v= for m=SRC"
                 )
             | "STAT" ->
                 w_base @@ fun conf _ -> BirthDeathDisplay.print_statistics conf
             | "TP" -> (
                 w_base @@ fun conf base ->
                 match Util.p_getenv conf.env "v" with
                 | Some f -> (
                     match Util.find_person_in_env conf base "" with
                     | Some p -> Perso.interp_templ ("tp_" ^ f) conf base p
                     | _ ->
                         Perso.interp_templ ("tp0_" ^ f) conf base
                           (Driver.empty_person base Driver.Iper.dummy))
                 | None ->
                     incorrect_request conf base ~comment:"Missing v= for m=TP")
             | "TT" -> w_base @@ TitleDisplay.print
             | "U" -> w_wizard @@ w_base @@ w_person @@ updmenu_print
             | "VIEW_WIZNOTES" when conf.authorized_wizards_notes ->
                 w_wizard @@ w_base @@ WiznotesDisplay.print_view
             | "WIZNOTES" when conf.authorized_wizards_notes ->
                 w_base @@ WiznotesDisplay.print
             | "WIZNOTES_SEARCH" when conf.authorized_wizards_notes ->
                 w_base @@ WiznotesDisplay.print_search
             | _ ->
                 w_base @@ fun conf base ->
                 let str = Format.sprintf "m=%s is not available here" m in
                 incorrect_request conf base ~comment:str)
              conf bfile)
      else
        let title _ =
          Printf.sprintf "%s %s %s"
            (transl conf "base" |> Utf8.capitalize_fst)
            conf.bname
            (transl conf "reserved to friends or wizards")
          |> Output.print_sstring conf
        in
        Hutil.rheader conf title;
        let base_name =
          if conf.cgi then Printf.sprintf "b=%s&" conf.bname else ""
        in
        let user = transl_nth conf "user/password/cancel" 0 in
        let passwd = transl_nth conf "user/password/cancel" 1 in
        let referer =
          match Util.extract_value '?' (get_referer conf :> string) with
          | exception Not_found -> ""
          | referer when referer <> "" -> "&" ^ referer
          | _ -> ""
        in
        let body =
          if conf.cgi then
            Printf.sprintf
              {|
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
            Printf.sprintf
              {|
            <div>
              <ul>
              <li>%s%s <a href="%s?%sw=f%s"> %s</a></li>
              <li>%s%s <a href="%s?%sw=w%s"> %s</a></li>
              </ul>
            </div> |}
              (transl conf "access" |> Utf8.capitalize_fst)
              (transl conf ":")
              (conf.command :> string)
              base_name referer
              (transl_nth conf "wizard/wizards/friend/friends/exterior" 2)
              (transl conf "access" |> Utf8.capitalize_fst)
              (transl conf ":")
              (conf.command :> string)
              base_name referer
              (transl_nth conf "wizard/wizards/friend/friends/exterior" 0)
        in
        Output.print_sstring conf
          (Printf.sprintf
             {|
        <form class="form-inline" method="post" action="%s">
          <div class="input-group mt-1">
            <input type="hidden" name="b" value="%s">
            %s
          </div>
        </form>
      |}
             (conf.command :> string)
             conf.bname body);
        Hutil.trailer conf
    in
    if conf.debug then
      Mutil.bench (__FILE__ ^ " " ^ string_of_int __LINE__) process
    else process ()

let treat_request conf =
  GWPARAM.init_etc conf.bname;
  (* TODO verify if we need init_etc here *)
  GWPARAM.nb_errors := 0;
  GWPARAM.errors_undef := [];
  GWPARAM.errors_other := [];
  let conf =
    {
      conf with
      base_env = Util.read_base_env conf.bname conf.gw_prefix conf.debug;
    }
  in
  try treat_request conf with Update.ModErr _ -> Output.flush conf
