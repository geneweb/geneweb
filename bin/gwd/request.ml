(* Copyright (c) 1998-2007 INRIA *)

let person_is_std_key conf base p k =
  let k = Name.strip_lower k in
  k = Name.strip_lower (Gwdb.p_first_name base p ^ " " ^ Gwdb.p_surname base p)
  || List.exists
       (fun n -> Name.strip n = k)
       (Gwdb.person_misc_names base p (Geneweb.Util.nobtit conf base))

let select_std_eq conf base pl k =
  List.filter (fun p -> person_is_std_key conf base p k) pl

let find_all conf base an =
  let sosa_ref = Geneweb.Util.find_sosa_ref conf base in
  let sosa_nb = Sosa.of_string an in
  match (sosa_ref, sosa_nb) with
  | Some p, Some n ->
      if n <> Sosa.zero then
        match Geneweb.Util.p_of_sosa conf base n p with
        | Some p -> ([ p ], true)
        | _ -> ([], false)
      else ([], false)
  | _ -> (
      let acc = Geneweb.SearchName.search_by_key conf base an in
      match acc with
      | Some acc -> ([ acc ], false)
      | None ->
          ( Geneweb.SearchName.search_key_aux
              (fun conf base acc an ->
                let spl = select_std_eq conf base acc an in
                if spl = [] then
                  if acc = [] then
                    Geneweb.SearchName.search_by_name conf base an
                  else acc
                else spl)
              conf base an,
            false ))

let relation_print conf base p =
  let p1 =
    match Geneweb.Util.p_getenv conf.Geneweb.Config.senv "ei" with
    | Some i ->
        conf.senv <- [];
        let i = Gwdb.iper_of_string i in
        if Gwdb.iper_exists base i then Some (Geneweb.Util.pget conf base i)
        else None
    | None -> (
        match Geneweb.Util.find_person_in_env conf base "1" with
        | Some p1 ->
            conf.senv <- [];
            Some p1
        | None -> None)
  in
  Geneweb.RelationDisplay.print conf base p p1

let specify conf base n pl =
  let title _ =
    Geneweb.Output.printf conf "%s : %s" n (Geneweb.Util.transl conf "specify")
  in
  let n = Name.crush_lower n in
  let ptll =
    List.map
      (fun p ->
        let tl = ref [] in
        let add_tl t =
          tl :=
            let rec add_rec = function
              | t1 :: tl1 ->
                  if
                    Gwdb.eq_istr t1.Def.t_ident t.Def.t_ident
                    && Gwdb.eq_istr t1.t_place t.t_place
                  then t1 :: tl1
                  else t1 :: add_rec tl1
              | [] -> [ t ]
            in
            add_rec !tl
        in
        let compare_and_add t pn =
          let pn = Gwdb.sou base pn in
          if Name.crush_lower pn = n then add_tl t
          else
            match Gwdb.get_qualifiers p with
            | nn :: _ ->
                let nn = Gwdb.sou base nn in
                if Name.crush_lower (pn ^ " " ^ nn) = n then add_tl t
            | _ -> ()
        in
        List.iter
          (fun t ->
            match (t.Def.t_name, Gwdb.get_public_name p) with
            | Tname s, _ -> compare_and_add t s
            | _, pn when Gwdb.sou base pn <> "" -> compare_and_add t pn
            | _ -> ())
          (Geneweb.Util.nobtit conf base p);
        (p, !tl))
      pl
  in
  Geneweb.Hutil.header conf title;
  Geneweb.Hutil.print_link_to_welcome conf true;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Geneweb.Util.print_tips_relationship conf;
  Geneweb.Output.print_sstring conf "<ul>\n";
  List.iter
    (fun (p, tl) ->
      Geneweb.Output.print_sstring conf "<li>";
      Geneweb.Sosa_cache.print_sosa ~conf ~base ~person:p ~link:true;
      (match tl with
      | [] ->
          Geneweb.Output.print_sstring conf " ";
          Geneweb.Output.print_string conf
            (Geneweb.NameDisplay.referenced_person_title_text conf base p)
      | t :: _ ->
          Geneweb.Output.print_sstring conf {|<a href="|};
          Geneweb.Output.print_string conf (Geneweb.Util.commd conf);
          Geneweb.Output.print_string conf (Geneweb.Util.acces conf base p);
          Geneweb.Output.print_sstring conf {|"> |};
          Geneweb.Output.print_string conf
            (Geneweb.NameDisplay.title_html_of_person conf base p t);
          Geneweb.Output.print_sstring conf "</a> ";
          List.iter
            (fun t ->
              Geneweb.Output.print_string conf
                (Geneweb.Util.one_title_text base t))
            tl);
      Geneweb.Output.print_string conf
        (Geneweb.DateDisplay.short_dates_text conf base p);
      (if Geneweb.Person.is_visible conf base p then
       match Gwdb.get_first_names_aliases p with
       | [] -> ()
       | fnal ->
           Geneweb.Output.print_sstring conf "\n<em>(";
           Ext_list.iter_first
             (fun first fna ->
               if not first then Geneweb.Output.print_sstring conf ", ";
               Gwdb.sou base fna |> Geneweb.Util.escape_html
               |> Geneweb.Output.print_string conf)
             fnal;
           Geneweb.Output.print_sstring conf ")</em>");
      let spouses =
        Array.fold_right
          (fun ifam spouses ->
            let cpl = Gwdb.foi base ifam in
            let spouse =
              Geneweb.Util.pget conf base (Gutil.spouse (Gwdb.get_iper p) cpl)
            in
            if Gwdb.p_surname base spouse <> "?" then spouse :: spouses
            else spouses)
          (Gwdb.get_family p) []
      in
      (match spouses with
      | [] -> ()
      | h :: hl ->
          let open Def in
          Geneweb.Output.print_sstring conf ", <em>&amp; ";
          List.fold_left
            (fun s h ->
              s ^^^ ",\n" ^<^ Geneweb.NameDisplay.person_title_text conf base h)
            (Geneweb.NameDisplay.person_title_text conf base h)
            hl
          |> Geneweb.Output.print_string conf;
          Geneweb.Output.print_sstring conf "</em>");
      Geneweb.Output.print_sstring conf "</li>")
    ptll;
  Geneweb.Output.print_sstring conf "</ul>";
  Geneweb.Hutil.trailer conf

let incorrect_request conf = Geneweb.Hutil.incorrect_request conf

let person_selected conf base p =
  match Geneweb.Util.p_getenv conf.Geneweb.Config.senv "em" with
  | Some "R" -> relation_print conf base p
  | Some _ -> incorrect_request conf
  | None ->
      Geneweb.Util.record_visited conf (Gwdb.get_iper p);
      Geneweb.Perso.print conf base p

let person_selected_with_redirect ~conf ~base ?(parameters = []) ~person () =
  match Geneweb.Util.p_getenv conf.Geneweb.Config.senv "em" with
  | Some "R" -> relation_print conf base person
  | Some _ -> incorrect_request conf
  | None ->
      let open Def in
      let url =
        (Geneweb.Util.commd conf ^^^ Geneweb.Util.acces conf base person
          :> string)
      in
      let url =
        List.fold_left
          (fun url (param_name, param_value) ->
            Printf.sprintf "%s&%s=%s" url param_name param_value)
          url parameters
      in
      Wserver.http_redirect_temporarily url

let updmenu_print = Geneweb.Perso.interp_templ "updmenu"

let very_unknown conf _ =
  match
    ( Geneweb.Util.p_getenv conf.Geneweb.Config.env "n",
      Geneweb.Util.p_getenv conf.env "p" )
  with
  | Some sname, Some fname ->
      let title _ =
        Geneweb.Util.transl conf "not found"
        |> Utf8.capitalize_fst
        |> Geneweb.Output.print_sstring conf;
        Geneweb.Output.print_sstring conf (Geneweb.Util.transl conf ":");
        Geneweb.Output.print_sstring conf {| "|};
        Geneweb.Output.print_string conf (Geneweb.Util.escape_html fname);
        Geneweb.Output.print_sstring conf {| |};
        Geneweb.Output.print_string conf (Geneweb.Util.escape_html sname);
        Geneweb.Output.print_sstring conf {|"|}
      in
      Geneweb.Output.status conf Def.Not_Found;
      Geneweb.Hutil.rheader conf title;
      Geneweb.Hutil.print_link_to_welcome conf false;
      Geneweb.Hutil.trailer conf
  | _ -> (
      match Geneweb.Util.p_getenv conf.env "i" with
      | Some i ->
          let title _ =
            Geneweb.Output.print_sstring conf "<kbd>";
            Geneweb.Output.print_string conf (Geneweb.Util.escape_html i);
            Geneweb.Output.print_sstring conf "</kbd>";
            Geneweb.Output.print_sstring conf (Geneweb.Util.transl conf ":");
            Geneweb.Output.print_sstring conf " ";
            Geneweb.Util.transl conf "not found"
            |> Utf8.capitalize_fst
            |> Geneweb.Output.print_sstring conf
          in
          Geneweb.Output.status conf Def.Not_Found;
          Geneweb.Hutil.rheader conf title;
          Geneweb.Hutil.print_link_to_welcome conf false;
          Geneweb.Hutil.trailer conf
      | None -> incorrect_request conf)

(* Print Not found page *)
let unknown conf n =
  let title _ =
    Geneweb.Util.transl conf "not found"
    |> Utf8.capitalize_fst
    |> Geneweb.Output.print_sstring conf;
    Geneweb.Output.print_sstring conf (Geneweb.Util.transl conf ":");
    Geneweb.Output.print_sstring conf {| "|};
    Geneweb.Output.print_string conf (Geneweb.Util.escape_html n);
    Geneweb.Output.print_sstring conf {|"|}
  in
  Geneweb.Output.status conf Def.Not_Found;
  Geneweb.Hutil.rheader conf title;
  Geneweb.Hutil.print_link_to_welcome conf false;
  Geneweb.Hutil.trailer conf

let make_henv conf base =
  let conf =
    match Geneweb.Util.find_sosa_ref conf base with
    | Some p ->
        let x =
          let first_name = Gwdb.p_first_name base p in
          let surname = Gwdb.p_surname base p in
          if Geneweb.Util.accessible_by_key conf base p first_name surname then
            [
              ("pz", Name.lower first_name |> Mutil.encode);
              ("nz", Name.lower surname |> Mutil.encode);
              ("ocz", Gwdb.get_occ p |> string_of_int |> Mutil.encode);
            ]
          else
            [ ("iz", Gwdb.get_iper p |> Gwdb.string_of_iper |> Mutil.encode) ]
        in
        { conf with henv = conf.henv @ x }
    | None -> conf
  in
  let conf =
    match Geneweb.Util.p_getenv conf.env "dsrc" with
    | Some "" | None -> conf
    | Some s -> { conf with henv = conf.henv @ [ ("dsrc", Mutil.encode s) ] }
  in
  let conf =
    match Geneweb.Util.p_getenv conf.env "templ" with
    | None -> conf
    | Some s -> { conf with henv = conf.henv @ [ ("templ", Mutil.encode s) ] }
  in
  let conf =
    match Geneweb.Util.p_getenv conf.env "escache" with
    | Some _ ->
        {
          conf with
          henv = conf.henv @ [ ("escache", Geneweb.Util.escache_value base) ];
        }
    | None -> conf
  in
  let conf =
    if Geneweb.Util.p_getenv conf.env "manitou" = Some "off" then
      { conf with henv = conf.henv @ [ ("manitou", Adef.encoded "off") ] }
    else conf
  in
  let aux param conf =
    match Geneweb.Util.p_getenv conf.Geneweb.Config.env param with
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
    "p_mod";
    "pure_xhtml";
    "pz";
    "size";
    "spouse";
    "templ";
    "wide";
  ]

let only_special_env env =
  List.for_all (fun (x, _) -> List.mem x special_vars) env

let make_senv conf base =
  let set_senv conf vm vi =
    let aux k v conf =
      if Geneweb.Util.p_getenv conf.Geneweb.Config.env k = Some v then
        { conf with senv = conf.senv @ [ (k, Mutil.encode v) ] }
      else conf
    in
    let conf =
      { conf with Geneweb.Config.senv = [ ("em", vm); ("ei", vi) ] }
      |> aux "image" "off" |> aux "long" "on" |> aux "spouse" "on"
    in
    let conf =
      match Geneweb.Util.p_getenv conf.env "et" with
      | Some x -> { conf with senv = conf.senv @ [ ("et", Mutil.encode x) ] }
      | _ -> conf
    in
    let conf = aux "cgl" "on" conf in
    let conf =
      match Geneweb.Util.p_getenv conf.env "bd" with
      | None | Some ("0" | "") -> conf
      | Some x -> { conf with senv = conf.senv @ [ ("bd", Mutil.encode x) ] }
    in
    match Geneweb.Util.p_getenv conf.env "color" with
    | Some x -> { conf with senv = conf.senv @ [ ("color", Mutil.encode x) ] }
    | _ -> conf
  in
  let get x = Geneweb.Util.p_getenv conf.Geneweb.Config.env x in
  match (get "em", get "ei", get "ep", get "en", get "eoc") with
  | Some vm, Some vi, _, _, _ ->
      set_senv conf (Mutil.encode vm) (Mutil.encode vi)
  | Some vm, None, Some vp, Some vn, voco ->
      let voc =
        match voco with
        | Some voc -> Option.value (int_of_string_opt voc) ~default:0
        | None -> 0
      in
      let ip =
        match Gwdb.person_of_key base vp vn voc with
        | Some ip -> ip
        | None ->
            incorrect_request conf;
            raise Exit
      in
      let vi = Gwdb.string_of_iper ip in
      set_senv conf (Mutil.encode vm) (Mutil.encode vi)
  | _ -> conf

let propose_base conf =
  let title _ = Geneweb.Output.print_sstring conf "Base" in
  Geneweb.Hutil.header conf title;
  Geneweb.Output.print_sstring conf {|<ul><li><form method="GET" action="|};
  Geneweb.Output.print_sstring conf conf.indep_command;
  Geneweb.Output.print_sstring conf {|">|};
  Geneweb.Output.print_sstring conf {|<input name="b" size="40"> =&gt; |};
  Geneweb.Output.print_sstring conf
    {|<button type="submit" class="btn btn-secondary btn-lg">|};
  Geneweb.Util.transl_nth conf "validate/delete" 0
  |> Utf8.capitalize_fst
  |> Geneweb.Output.print_sstring conf;
  Geneweb.Output.print_sstring conf "</button></li></ul>";
  Geneweb.Hutil.trailer conf

let try_plugin list conf base_name m =
  let fn =
    if List.mem "*" list then fun (_, fn) -> fn conf base_name
    else fun (ns, fn) ->
      (List.mem ns conf.Geneweb.Config.forced_plugins || List.mem ns list)
      && fn conf base_name
  in
  List.exists fn (Hashtbl.find_all GwdPlugin.ht m)

let w_lock ~onerror fn conf (base_name : string option) =
  let bfile = Geneweb.GWPARAM.bpath (conf.Geneweb.Config.bname ^ ".gwb") in
  Lock.control (Files.lock_file bfile) true
    ~onerror:(fun () -> onerror conf base_name)
    (fun () -> fn conf base_name)

let w_base ~none fn conf (bfile : string option) =
  match bfile with
  | None -> none conf
  | Some bfile -> (
      let base = try Some (Gwdb.open_base bfile) with _ -> None in
      match base with
      | None -> none conf
      | Some base ->
          let conf = make_henv conf base in
          let conf = make_senv conf base in
          let conf =
            match Geneweb.Util.default_sosa_ref conf base with
            | Some p ->
                { conf with default_sosa_ref = (Gwdb.get_iper p, Some p) }
            | None -> conf
          in
          fn conf base)

let w_person ~none fn conf base =
  match Geneweb.Util.find_person_in_env conf base "" with
  | Some p -> fn conf base p
  | _ -> none conf base

let output_error ?headers ?content conf code =
  Geneweb.GWPARAM.output_error ?headers ?content conf code

let w_wizard fn conf base =
  if conf.Geneweb.Config.wizard then fn conf base
  else if conf.just_friend_wizard then output_error conf Def.Forbidden
  else
    (* FIXME: send authentification headers *)
    output_error conf Def.Unauthorized

let treat_request =
  let w_lock =
    w_lock ~onerror:(fun conf _ -> Geneweb.Update.error_locked conf)
  in
  let w_base =
    let none conf =
      if conf.Geneweb.Config.bname = "" then output_error conf Def.Bad_Request
      else output_error conf Def.Not_Found
    in
    w_base ~none
  in
  let w_person = w_person ~none:very_unknown in
  fun conf ->
    let bfile =
      if conf.Geneweb.Config.bname = "" then None
      else
        let bfile = Geneweb.GWPARAM.bpath (conf.bname ^ ".gwb") in
        if Sys.file_exists bfile then Some bfile else None
    in
    let process () =
      if
        conf.wizard || conf.friend
        || List.assoc_opt "visitor_access" conf.base_env <> Some "no"
      then (
        let plugins =
          match List.assoc_opt "plugins" conf.Geneweb.Config.base_env with
          | None -> []
          | Some list -> String.split_on_char ',' list
        in
        if List.mem "*" plugins then
          List.iter (fun (_, fn) -> fn conf bfile) !GwdPlugin.se
        else
          List.iter
            (fun (ns, fn) -> if List.mem ns plugins then fn conf bfile)
            !GwdPlugin.se;
        let m = Option.value ~default:"" (Geneweb.Util.p_getenv conf.env "m") in
        if not @@ try_plugin plugins conf bfile m then
          ((if List.assoc_opt "counter" conf.base_env <> Some "no" then
            match
              if only_special_env conf.env then
                Geneweb.SrcfileDisplay.incr_welcome_counter conf
              else Geneweb.SrcfileDisplay.incr_request_counter conf
            with
            | Some (welcome_cnt, request_cnt, start_date) ->
                Log.log (fun oc ->
                    let thousand oc x =
                      output_string oc @@ Mutil.string_of_int_sep "," x
                    in
                    Printf.fprintf oc "  #accesses %a (#welcome %a) since %s\n"
                      thousand
                      (welcome_cnt + request_cnt)
                      thousand welcome_cnt start_date)
            | None -> ());
           let incorrect_request conf _ = incorrect_request conf in
           match m with
           | "" ->
               let base =
                 match bfile with
                 | None -> None
                 | Some bfile -> (
                     try Some (Gwdb.open_base bfile) with _ -> None)
               in
               if base <> None then
                 w_base
                 @@
                 if only_special_env conf.env then
                   Geneweb.SrcfileDisplay.print_start
                 else
                   w_person @@ fun conf base p ->
                   match Geneweb.Util.p_getenv conf.env "ptempl" with
                   | Some t
                     when List.assoc_opt "ptempl" conf.base_env = Some "yes" ->
                       Geneweb.Perso.interp_templ t conf base p
                   | _ -> person_selected conf base p
               else if conf.bname = "" then fun conf _ ->
                 Geneweb.Util.include_template conf [] "index" (fun () ->
                     propose_base conf)
               else
                 w_base
                   (if only_special_env conf.env then
                    Geneweb.SrcfileDisplay.print_start
                   else
                     w_person @@ fun conf base p ->
                     match Geneweb.Util.p_getenv conf.env "ptempl" with
                     | Some t
                       when List.assoc_opt "ptempl" conf.base_env = Some "yes"
                       ->
                         Geneweb.Perso.interp_templ t conf base p
                     | _ -> person_selected conf base p)
           | "A" -> Geneweb.Perso.print_ascend |> w_person |> w_base
           | "ADD_FAM" -> w_wizard @@ w_base @@ Geneweb.UpdateFam.print_add
           | "ADD_FAM_OK" ->
               w_wizard @@ w_lock @@ w_base @@ Geneweb.UpdateFamOk.print_add
           | "ADD_IND" -> w_wizard @@ w_base @@ Geneweb.UpdateInd.print_add
           | "ADD_IND_OK" ->
               w_wizard @@ w_lock @@ w_base @@ Geneweb.UpdateIndOk.print_add
           | "ADD_PAR" ->
               w_wizard @@ w_base @@ Geneweb.UpdateFam.print_add_parents
           | "ADD_PAR_OK" ->
               w_wizard @@ w_lock @@ w_base
               @@ Geneweb.UpdateFamOk.print_add_parents
           | "AN" -> (
               w_base @@ fun conf base ->
               match Geneweb.Util.p_getenv conf.env "v" with
               | Some x ->
                   Geneweb.BirthdayDisplay.print_birth conf base
                     (int_of_string x)
               | _ -> Geneweb.BirthdayDisplay.print_menu_birth conf base)
           | "AD" -> (
               w_base @@ fun conf base ->
               match Geneweb.Util.p_getenv conf.env "v" with
               | Some x ->
                   Geneweb.BirthdayDisplay.print_dead conf base
                     (int_of_string x)
               | _ -> Geneweb.BirthdayDisplay.print_menu_dead conf base)
           | "AM" -> (
               w_base @@ fun conf base ->
               match Geneweb.Util.p_getenv conf.env "v" with
               | Some x ->
                   Geneweb.BirthdayDisplay.print_marriage conf base
                     (int_of_string x)
               | _ -> Geneweb.BirthdayDisplay.print_menu_marriage conf base)
           | "C" -> w_base @@ w_person @@ Geneweb.CousinsDisplay.print
           | "CHG_CHN" when conf.wizard ->
               w_wizard @@ w_base @@ Geneweb.ChangeChildrenDisplay.print
           | "CHG_CHN_OK" ->
               w_wizard @@ w_lock @@ w_base
               @@ Geneweb.ChangeChildrenDisplay.print_ok
           | "CHG_EVT_IND_ORD" ->
               w_wizard @@ w_base @@ Geneweb.UpdateInd.print_change_event_order
           | "CHG_EVT_IND_ORD_OK" ->
               w_wizard @@ w_lock @@ w_base
               @@ Geneweb.UpdateIndOk.print_change_event_order
           | "CHG_EVT_FAM_ORD" ->
               w_wizard @@ w_base @@ Geneweb.UpdateFam.print_change_event_order
           | "CHG_EVT_FAM_ORD_OK" ->
               w_wizard @@ w_lock @@ w_base
               @@ Geneweb.UpdateFamOk.print_change_event_order
           | "CHG_FAM_ORD" ->
               w_wizard @@ w_base @@ Geneweb.UpdateFam.print_change_order
           | "CHG_FAM_ORD_OK" ->
               w_wizard @@ w_lock @@ w_base
               @@ Geneweb.UpdateFamOk.print_change_order_ok
           | "CONN_WIZ" ->
               w_wizard @@ w_base @@ Geneweb.WiznotesDisplay.connected_wizards
           | "D" -> w_base @@ w_person @@ Geneweb.DescendDisplay.print
           | "DAG" -> w_base @@ Geneweb.DagDisplay.print
           | "DEL_FAM" -> w_wizard @@ w_base @@ Geneweb.UpdateFam.print_del
           | "DEL_FAM_OK" ->
               w_wizard @@ w_lock @@ w_base @@ Geneweb.UpdateFamOk.print_del
           | "DEL_IND" -> w_wizard @@ w_base @@ Geneweb.UpdateInd.print_del
           | "DEL_IND_OK" ->
               w_wizard @@ w_lock @@ w_base @@ Geneweb.UpdateIndOk.print_del
           | "F" -> w_base @@ w_person @@ Geneweb.Perso.interp_templ "family"
           | "H" -> (
               w_base @@ fun conf base ->
               match Geneweb.Util.p_getenv conf.env "v" with
               | Some f -> Geneweb.SrcfileDisplay.print conf base f
               | None -> incorrect_request conf base)
           | "HIST" -> w_base @@ Geneweb.History.print
           | "HIST_CLEAN" ->
               w_wizard @@ w_base
               @@ fun conf _ -> Geneweb.HistoryDiffDisplay.print_clean conf
           | "HIST_CLEAN_OK" ->
               w_wizard @@ w_base
               @@ fun conf _ -> Geneweb.HistoryDiffDisplay.print_clean_ok conf
           | "HIST_DIFF" -> w_base @@ Geneweb.HistoryDiffDisplay.print
           | "HIST_SEARCH" -> w_base @@ Geneweb.History.print_search
           | "IM" -> w_base @@ Geneweb.ImageDisplay.print
           | "IMH" ->
               w_base @@ fun conf _ -> Geneweb.ImageDisplay.print_html conf
           | "INV_FAM" -> w_wizard @@ w_base @@ Geneweb.UpdateFam.print_inv
           | "INV_FAM_OK" ->
               w_wizard @@ w_lock @@ w_base @@ Geneweb.UpdateFamOk.print_inv
           | "KILL_ANC" ->
               w_wizard @@ w_lock @@ w_base
               @@ Geneweb.MergeIndDisplay.print_kill_ancestors
           | "LD" when conf.wizard || conf.friend ->
               w_base @@ Geneweb.BirthDeathDisplay.print_death
           | "LINKED" -> w_base @@ w_person @@ Geneweb.Perso.print_what_links
           | "MISC_NOTES" -> w_base @@ Geneweb.NotesDisplay.print_misc_notes
           | "MISC_NOTES_SEARCH" ->
               w_base @@ Geneweb.NotesDisplay.print_misc_notes_search
           | "MOD_DATA" ->
               w_wizard @@ w_base @@ Geneweb.UpdateDataDisplay.print_mod
           | "MOD_DATA_OK" ->
               w_wizard @@ w_lock @@ w_base
               @@ Geneweb.UpdateDataDisplay.print_mod_ok
           | "MOD_FAM" -> w_wizard @@ w_base @@ Geneweb.UpdateFam.print_mod
           | "MOD_FAM_OK" when conf.wizard ->
               w_wizard @@ w_lock @@ w_base @@ Geneweb.UpdateFamOk.print_mod
           | "MOD_IND" -> w_wizard @@ w_base @@ Geneweb.UpdateInd.print_mod
           | "MOD_IND_OK" ->
               w_wizard @@ w_lock @@ w_base @@ Geneweb.UpdateIndOk.print_mod
           | "MOD_NOTES" -> w_wizard @@ w_base @@ Geneweb.NotesDisplay.print_mod
           | "MOD_NOTES_OK" ->
               w_wizard @@ w_lock @@ w_base @@ Geneweb.NotesDisplay.print_mod_ok
           | "MOD_WIZNOTES" when conf.authorized_wizards_notes ->
               w_base @@ Geneweb.WiznotesDisplay.print_mod
           | "MOD_WIZNOTES_OK" when conf.authorized_wizards_notes ->
               w_lock @@ w_base @@ Geneweb.WiznotesDisplay.print_mod_ok
           | "MRG" ->
               w_wizard @@ w_base @@ w_person @@ Geneweb.MergeDisplay.print
           | "MRG_DUP" ->
               w_wizard @@ w_base @@ Geneweb.MergeDupDisplay.main_page
           | "MRG_DUP_IND_Y_N" ->
               w_wizard @@ w_lock @@ w_base
               @@ Geneweb.MergeDupDisplay.answ_ind_y_n
           | "MRG_DUP_FAM_Y_N" ->
               w_wizard @@ w_lock @@ w_base
               @@ Geneweb.MergeDupDisplay.answ_fam_y_n
           | "MRG_FAM" -> w_wizard @@ w_base @@ Geneweb.MergeFamDisplay.print
           | "MRG_FAM_OK" ->
               w_wizard @@ w_lock @@ w_base @@ Geneweb.MergeFamOk.print_merge
           | "MRG_MOD_FAM_OK" ->
               w_wizard @@ w_lock @@ w_base
               @@ Geneweb.MergeFamOk.print_mod_merge
           | "MRG_IND" ->
               w_wizard @@ w_lock @@ w_base @@ Geneweb.MergeIndDisplay.print
           | "MRG_IND_OK" ->
               (* despite the _OK suffix, this one does not actually update databse *)
               w_wizard @@ w_base @@ Geneweb.MergeIndOkDisplay.print_merge
           | "MRG_MOD_IND_OK" ->
               w_wizard @@ w_lock @@ w_base
               @@ Geneweb.MergeIndOkDisplay.print_mod_merge
           | "NG" -> (
               w_base @@ fun conf base ->
               (* Rétro-compatibilité <= 6.06 *)
               let env =
                 match Geneweb.Util.p_getenv conf.env "n" with
                 | Some n -> (
                     match Geneweb.Util.p_getenv conf.env "t" with
                     | Some "P" -> ("fn", Mutil.encode n) :: conf.env
                     | Some "N" -> ("sn", Mutil.encode n) :: conf.env
                     | _ -> ("v", Mutil.encode n) :: conf.env)
                 | None -> conf.env
               in
               let conf = { conf with env } in
               (* Nouveau mode de recherche. *)
               match Geneweb.Util.p_getenv conf.env "select" with
               | Some "input" | None -> (
                   (* Récupère le contenu non vide de la recherche. *)
                   let real_input label =
                     match Geneweb.Util.p_getenv conf.env label with
                     | Some s -> if s = "" then None else Some s
                     | None -> None
                   in
                   (* Recherche par clé, sosa, alias ... *)
                   let search n =
                     let pl, sosa_acc = find_all conf base n in
                     match pl with
                     | [] ->
                         let sres =
                           Geneweb.Search_name_display.search_surname conf base
                             n
                         in
                         Geneweb.Search_name_display.surname_print conf base
                           unknown sres n
                     | [ p ] ->
                         if
                           sosa_acc
                           || Gutil.person_of_string_key base n <> None
                           || person_is_std_key conf base p n
                         then
                           person_selected_with_redirect ~conf ~base ~person:p
                             ()
                         else specify conf base n pl
                     | pl -> specify conf base n pl
                   in
                   match real_input "v" with
                   | Some n -> search n
                   | None -> (
                       match (real_input "fn", real_input "sn") with
                       | Some fn, Some sn -> search (fn ^ " " ^ sn)
                       | Some fn, None ->
                           Geneweb.Search_name_display.first_name_print conf
                             base fn
                       | None, Some sn ->
                           let sres =
                             Geneweb.Search_name_display.search_surname conf
                               base sn
                           in
                           Geneweb.Search_name_display.surname_print conf base
                             unknown sres sn
                       | None, None -> incorrect_request conf base))
               | Some i ->
                   relation_print conf base
                     (Geneweb.Util.pget conf base (Gwdb.iper_of_string i)))
           | "NOTES" -> w_base @@ Geneweb.NotesDisplay.print
           | "PS" -> w_base @@ Geneweb.PlaceDisplay.print_all_places_surnames
           | "R" -> w_base @@ w_person @@ relation_print
           | "REQUEST" ->
               w_wizard @@ fun _ _ ->
               Geneweb.Output.status conf Def.OK;
               Geneweb.Output.header conf "Content-type: text";
               List.iter
                 (fun s ->
                   Geneweb.Output.print_sstring conf s;
                   Geneweb.Output.print_sstring conf "\n")
                 conf.Geneweb.Config.request
           | "RL" -> w_base @@ Geneweb.RelationLink.print
           | "RLM" -> w_base @@ Geneweb.RelationDisplay.print_multi
           | "S" ->
               w_base @@ fun conf base ->
               Geneweb.SearchName.print conf base specify unknown
           | "SRC" -> (
               w_base @@ fun conf base ->
               match Geneweb.Util.p_getenv conf.env "v" with
               | Some f -> Geneweb.SrcfileDisplay.print_source conf base f
               | _ -> incorrect_request conf base)
           | "STAT" ->
               w_base @@ fun conf _ ->
               Geneweb.BirthDeathDisplay.print_statistics conf
           | "CHANGE_WIZ_VIS" ->
               w_wizard @@ w_lock @@ w_base
               @@ Geneweb.WiznotesDisplay.change_wizard_visibility
           | "TT" -> w_base @@ Geneweb.TitleDisplay.print
           | "U" -> w_wizard @@ w_base @@ w_person @@ updmenu_print
           | "VIEW_WIZNOTES" when conf.authorized_wizards_notes ->
               w_wizard @@ w_base @@ Geneweb.WiznotesDisplay.print_view
           | "WIZNOTES" when conf.authorized_wizards_notes ->
               w_base @@ Geneweb.WiznotesDisplay.print
           | "WIZNOTES_SEARCH" when conf.authorized_wizards_notes ->
               w_base @@ Geneweb.WiznotesDisplay.print_search
           | _ -> incorrect_request)
            conf bfile;
        Geneweb.Output.flush conf)
      else
        let title _ =
          Geneweb.Util.transl conf "error"
          |> Utf8.capitalize_fst
          |> Geneweb.Output.print_sstring conf
        in
        Geneweb.Hutil.rheader conf title;
        Geneweb.Output.print_sstring conf "<ul><li>";
        Geneweb.Util.transl conf "base"
        |> Utf8.capitalize_fst
        |> Geneweb.Output.print_sstring conf;
        Geneweb.Output.print_sstring conf {| "|};
        Geneweb.Output.print_sstring conf conf.bname;
        Geneweb.Output.print_sstring conf {|" |};
        Geneweb.Util.transl conf "reserved to friends or wizards"
        |> Utf8.capitalize_fst
        |> Geneweb.Output.print_sstring conf;
        Geneweb.Output.print_sstring conf ".</li></ul>";
        Geneweb.Hutil.trailer conf
    in
    try
      if conf.debug then
        Mutil.bench (__FILE__ ^ " " ^ string_of_int __LINE__) process
      else process ()
    with Gwdb.Not_plain_text s ->
      Geneweb.Update.error conf (Geneweb.Update.not_plain_text_error s)

let treat_request conf =
  try treat_request conf
  with Geneweb.Update.ModErr _ -> Geneweb.Output.flush conf
