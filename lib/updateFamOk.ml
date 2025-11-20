(* Copyright (c) 1998-2007 INRIA *)

(* Liste des string dont on a supprimé un caractère.       *)
(* Utilisé pour le message d'erreur lors de la validation. *)
let removed_string = ref []
let get_purged_fn_sn = Update_util.get_purged_fn_sn removed_string
let reconstitute_somebody = Update_util.reconstitute_somebody removed_string

let reconstitute_parent_or_child conf var default_surname =
  let first_name = Ext_string.only_printable (Update_util.getn conf var "fn") in
  let surname =
    let surname = Ext_string.only_printable (Update_util.getn conf var "sn") in
    if surname = "" && first_name <> "" then default_surname else surname
  in
  (* S'il y a des caractères interdits, on les supprime *)
  let first_name, surname = get_purged_fn_sn first_name surname in
  let occ =
    try
      Option.value ~default:0
        (int_of_string_opt (Update_util.getn conf var "occ"))
    with Failure _ -> 0
  in
  let create_info =
    let b = Update.reconstitute_date conf (var ^ "b") in
    let bpl = Update_util.getn conf (var ^ "b") "pl" in
    let death =
      match Util.p_getenv conf.Config.env (var ^ "d_yyyy") with
      | Some "+" -> Def.DeadDontKnowWhen
      | Some ("-" | "=") -> NotDead
      | Some _ | None -> DontKnowIfDead
    in
    let d = Update.reconstitute_date conf (var ^ "d") in
    let dpl = Update_util.getn conf (var ^ "d") "pl" in
    let occupation =
      Ext_string.only_printable (Update_util.getn conf var "occupation")
    in
    let public = Update_util.getn conf (var ^ "b") "yyyy" = "p" in
    {
      Update.ci_birth_date = b;
      ci_birth_place = bpl;
      ci_death = death;
      ci_death_date = d;
      ci_death_place = dpl;
      ci_occupation = occupation;
      ci_public = public;
    }
  in
  let sex = Update_util.getenv_sex conf var in
  let create = Update_util.getn_p conf var ~create_info sex in
  (first_name, surname, occ, create, var)

let invert_children conf (c, children, ext) i =
  let var = "inv_ch" ^ string_of_int (i + 1) in
  match (Util.p_getenv conf.Config.env var, children) with
  | Some "on", c1 :: children -> (c1, c :: children, true)
  | _ -> (c, children, ext)

let insert_child conf (children, ext) i =
  let var = "ins_ch" ^ string_of_int i in
  match
    ( Util.p_getenv conf.Config.env var,
      Util.p_getint conf.Config.env (var ^ "_n") )
  with
  | _, Some n when n > 1 ->
      let children =
        let rec loop children n =
          if n > 0 then
            let new_child = ("", "", 0, Update.Create (Neuter, None), "") in
            loop (new_child :: children) (n - 1)
          else children
        in
        loop children n
      in
      (children, true)
  | Some "on", _ ->
      let new_child = ("", "", 0, Update.Create (Neuter, None), "") in
      (new_child :: children, true)
  | _ -> (children, ext)

let insert_parent conf (parents, ext) i =
  let var = "ins_pa" ^ string_of_int i in
  match
    ( Util.p_getenv conf.Config.env var,
      Util.p_getint conf.Config.env (var ^ "_n") )
  with
  | _, Some n when n > 1 ->
      let parents =
        let rec loop parents n =
          if n > 0 then
            let new_parent = ("", "", 0, Update.Create (Neuter, None), "") in
            loop (new_parent :: parents) (n - 1)
          else parents
        in
        loop parents n
      in
      (parents, true)
  | Some "on", _ ->
      let new_parent = ("", "", 0, Update.Create (Neuter, None), "") in
      (new_parent :: parents, true)
  | _ -> (parents, ext)

let reconstitute_insert_event conf ext cnt el =
  let var = "ins_event" ^ string_of_int cnt in
  let n =
    match
      ( Util.p_getenv conf.Config.env var,
        Util.p_getint conf.Config.env (var ^ "_n") )
    with
    | _, Some n when n > 1 -> n
    | Some "on", _ -> 1
    | _ -> 0
  in
  if n > 0 then
    let el =
      let rec loop el n =
        if n > 0 then
          let e1 =
            {
              Def.efam_name = Efam_Name "";
              efam_date = Date.cdate_None;
              efam_place = "";
              efam_reason = "";
              efam_note = "";
              efam_src = "";
              efam_witnesses = [||];
            }
          in
          loop (e1 :: el) (n - 1)
        else el
      in
      loop el n
    in
    (el, true)
  else (el, ext)

let rec reconstitute_events ~base conf ext cnt =
  match Update_util.get_nth conf "e_name" cnt with
  | None -> ([], ext)
  | Some efam_name ->
      let efam_name =
        match efam_name with
        | "#marr" -> Def.Efam_Marriage
        | "#nmar" -> Efam_NoMarriage
        | "#nmen" -> Efam_NoMention
        | "#enga" -> Efam_Engage
        | "#div" -> Efam_Divorce
        | "#sep" -> Efam_Separated
        | "#anul" -> Efam_Annulation
        | "#marb" -> Efam_MarriageBann
        | "#marc" -> Efam_MarriageContract
        | "#marl" -> Efam_MarriageLicense
        | "#pacs" -> Efam_PACS
        | "#resi" -> Efam_Residence
        | n -> Efam_Name (Ext_string.only_printable n)
      in
      let efam_date =
        Update.reconstitute_date conf ("e_date" ^ string_of_int cnt)
      in
      let efam_place =
        match Update_util.get_nth conf "e_place" cnt with
        | Some place -> Ext_string.only_printable place
        | None -> ""
      in
      let efam_note =
        match Update_util.get_nth conf "e_note" cnt with
        | Some note ->
            Ext_string.only_printable_or_nl
              (Ext_string.strip_all_trailing_spaces note)
        | None -> ""
      in
      let efam_src =
        match Update_util.get_nth conf "e_src" cnt with
        | Some src -> Ext_string.only_printable src
        | None -> ""
      in
      let witnesses, ext =
        let rec loop i ext =
          match
            try
              Some
                (reconstitute_somebody conf
                   ("e" ^ string_of_int cnt ^ "_witn" ^ string_of_int i))
            with Failure _ -> None
          with
          | None -> ([], ext)
          | Some c -> (
              let witnesses, ext = loop (i + 1) ext in
              let var_c =
                "e" ^ string_of_int cnt ^ "_witn" ^ string_of_int i ^ "_kind"
              in
              let wkind =
                match Util.p_getenv conf.Config.env var_c with
                | Some "godp" -> Def.Witness_GodParent
                | Some "offi" -> Witness_CivilOfficer
                | Some "reli" -> Witness_ReligiousOfficer
                | Some "info" -> Witness_Informant
                | Some "atte" -> Witness_Attending
                | Some "ment" -> Witness_Mentioned
                | Some "othe" -> Witness_Other
                | Some _ | None -> Witness
              in
              let wnote =
                let var_note =
                  "e" ^ string_of_int cnt ^ "_witn" ^ string_of_int i ^ "_note"
                in
                match Util.p_getenv conf.Config.env var_note with
                | Some wnote -> wnote
                | _ -> ""
              in
              let c = (c, wkind, wnote) in
              match
                Util.p_getenv conf.Config.env
                  ("e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i)
              with
              | Some "on" -> (
                  let ins_witn_n =
                    "e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i
                    ^ "_n"
                  in
                  match Util.p_getint conf.Config.env ins_witn_n with
                  | Some n when n > 1 ->
                      let rec loop_witn n witnesses =
                        if n = 0 then (c :: witnesses, true)
                        else
                          let new_witn =
                            ( ("", "", 0, Update.Create (Neuter, None), ""),
                              Def.Witness,
                              "" )
                          in
                          let witnesses = new_witn :: witnesses in
                          loop_witn (n - 1) witnesses
                      in
                      loop_witn n witnesses
                  | Some _ | None ->
                      let new_witn =
                        ( ("", "", 0, Update.Create (Neuter, None), ""),
                          Def.Witness,
                          "" )
                      in
                      (c :: new_witn :: witnesses, true))
              | Some _ | None -> (c :: witnesses, ext))
        in
        loop 1 ext
      in
      let witnesses =
        Update_util.witnesses_with_inferred_death_from_event ~conf ~base
          ~date:efam_date witnesses
      in
      let witnesses, ext =
        let evt_ins = "e" ^ string_of_int cnt ^ "_ins_witn0" in
        match Util.p_getenv conf.Config.env evt_ins with
        | Some "on" -> (
            let ins_witn_n = "e" ^ string_of_int cnt ^ "_ins_witn0_n" in
            match Util.p_getint conf.Config.env ins_witn_n with
            | Some n when n > 1 ->
                let rec loop_witn n witnesses =
                  if n = 0 then (witnesses, true)
                  else
                    let new_witn =
                      ( ("", "", 0, Update.Create (Neuter, None), ""),
                        Def.Witness,
                        "" )
                    in
                    let witnesses = new_witn :: witnesses in
                    loop_witn (n - 1) witnesses
                in
                loop_witn n witnesses
            | Some _ | None ->
                let new_witn =
                  ( ("", "", 0, Update.Create (Neuter, None), ""),
                    Def.Witness,
                    "" )
                in
                (new_witn :: witnesses, true))
        | Some _ | None -> (witnesses, ext)
      in
      let e =
        {
          Def.efam_name;
          efam_date = Date.cdate_of_od efam_date;
          efam_place;
          efam_reason = "";
          efam_note;
          efam_src;
          efam_witnesses = Array.of_list witnesses;
        }
      in
      let el, ext = reconstitute_events ~base conf ext (cnt + 1) in
      let el, ext = reconstitute_insert_event conf ext cnt el in
      (e :: el, ext)

(* S:
   * why is marriage record transformed into a tuple?
*)
let reconstitute_from_fevents (nsck : bool) (empty_string : 'string)
    (fevents : ('person, 'string) Def.gen_fam_event list) =
  let { Event.main_union; main_separation } =
    Event.get_main_family_events fevents
  in
  (* Il faut gérer le cas où l'on supprime délibérément l'évènement. *)
  let marr, wit =
    match main_union with
    | None ->
        ( ( Def.NoMention,
            Date.cdate_None,
            empty_string,
            empty_string,
            empty_string ),
          [||] )
    | Some { Event.kind; date; place; note; source = src; witnesses = wit } ->
        ((kind, date, place, note, src), wit)
  in
  (* Parents de même sexe. *)
  let marr =
    if nsck then
      let relation, date, place, note, src = marr in
      let relation =
        match relation with
        | Married -> Def.NoSexesCheckMarried
        | ( NotMarried | Engaged | NoSexesCheckNotMarried | NoMention
          | NoSexesCheckMarried | MarriageBann | MarriageContract
          | MarriageLicense | Pacs | Residence ) as x ->
            x
      in
      (relation, date, place, note, src)
    else marr
  in
  let div = Option.value ~default:Def.NotDivorced main_separation in
  ( marr,
    div,
    Array.map (fun { Event.person; kind; note } -> (person, kind, note)) wit )

let reconstitute_family conf base nsck =
  let events, ext = reconstitute_events ~base conf false 1 in
  let events, ext = reconstitute_insert_event conf ext 0 events in
  let surname = Update_util.getn conf "pa1" "sn" in
  let children, ext =
    let rec loop i ext =
      match
        try
          Some
            (reconstitute_parent_or_child conf ("ch" ^ string_of_int i) surname)
        with Failure _ -> None
      with
      | Some c ->
          let children, ext = loop (i + 1) ext in
          let c, children, ext = invert_children conf (c, children, ext) i in
          let children, ext = insert_child conf (children, ext) i in
          (c :: children, ext)
      | None -> ([], ext)
    in
    loop 1 ext
  in
  let children, ext = insert_child conf (children, ext) 0 in
  let parents, ext =
    let rec loop i ext =
      match
        try Some (reconstitute_parent_or_child conf ("pa" ^ string_of_int i) "")
        with Failure _ -> None
      with
      | Some c ->
          let parents, ext = loop (i + 1) ext in
          let parents, ext = insert_parent conf (parents, ext) i in
          (c :: parents, ext)
      | None -> ([], ext)
    in
    loop 1 ext
  in
  let comment =
    Ext_string.only_printable_or_nl
      (Ext_string.strip_all_trailing_spaces (Update_util.get conf "comment"))
  in
  let fsources = Ext_string.only_printable (Update_util.get conf "src") in
  let origin_file =
    Option.value ~default:"" (Util.p_getenv conf.Config.env "origin_file")
  in
  let fam_index =
    match Util.p_getenv conf.Config.env "i" with
    | Some i -> Gwdb.ifam_of_string i
    | None -> Gwdb.dummy_ifam
  in
  (* Mise à jour des évènements principaux. *)
  (* Attention, dans le cas où fevent est vide, i.e. on a valider   *)
  (* avec un texte vide, par exemple lors de l'ajout d'une famille, *)
  (* il faut ajouter un evenement no_mention.                       *)
  let events =
    if events = [] then
      let evt =
        {
          Def.efam_name = Efam_NoMention;
          efam_date = Date.cdate_None;
          efam_place = "";
          efam_reason = "";
          efam_note = "";
          efam_src = "";
          efam_witnesses = [||];
        }
      in
      [ evt ]
    else events
  in
  (* Attention, surtout pas les witnesses, parce que si on en créé un, *)
  (* on le créé aussi dans witness et on ne pourra jamais valider.     *)
  let marr, div, _ =
    (* FIXME: Use witnesses (and Array.map fst witnesses)
       when witnesses will be added inplace *)
    reconstitute_from_fevents nsck "" events
  in
  let relation, marriage, marriage_place, marriage_note, marriage_src = marr in
  (* Si parents de même sex ... Pas de mode multi parent. *)
  let relation =
    match parents with
    | [ father; mother ] -> (
        let father_sex =
          match father with
          | _, _, _, Update.Create (sex, _), _ -> sex
          | f, s, o, Update.Link, _ -> (
              match Gwdb.person_of_key base f s o with
              | Some ip -> Gwdb.get_sex (Gwdb.poi base ip)
              | _ -> Neuter)
        in
        let mother_sex =
          match mother with
          | _, _, _, Update.Create (sex, _), _ -> sex
          | f, s, o, Update.Link, _ -> (
              match Gwdb.person_of_key base f s o with
              | Some ip -> Gwdb.get_sex (Gwdb.poi base ip)
              | _ -> Neuter)
        in
        match (father_sex, mother_sex) with
        | Male, Male | Female, Female -> (
            match relation with
            | Married -> Def.NoSexesCheckMarried
            | _ -> NoSexesCheckNotMarried)
        | _ -> relation)
    | _ -> relation
  in
  let divorce = div in
  let fam =
    {
      Def.marriage;
      marriage_place;
      marriage_note;
      marriage_src;
      witnesses = [||];
      relation;
      divorce;
      fevents = events;
      comment;
      origin_file;
      fsources;
      fam_index;
    }
  and cpl = Adef.parent (Array.of_list parents)
  and des = { Def.children = Array.of_list children } in
  (fam, cpl, des, ext)

let strip_events fevents =
  let strip_array_witness pl =
    Array.of_list
    @@ Array.fold_right
         (fun (((f, _, _, _, _), _, _) as p) pl ->
           if f = "" then pl else p :: pl)
         pl []
  in
  List.fold_right
    (fun e accu ->
      let has_name =
        match e.Def.efam_name with Efam_Name s -> s <> "" | _ -> true
      in
      if has_name then
        let witnesses = strip_array_witness e.efam_witnesses in
        { e with efam_witnesses = witnesses } :: accu
      else accu)
    fevents []

let strip_array_persons pl =
  Array.of_list
  @@ Array.fold_right
       (fun ((f, _, _, _, _) as p) pl -> if f = "" then pl else p :: pl)
       pl []

let error_family conf err =
  Update.prerr conf err @@ fun () ->
  (err |> Update.string_of_error conf : Adef.safe_string :> string)
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "\n";
  Update.print_return conf

let check_parents conf cpl =
  let check get i =
    let fn, sn, _, _, _ = get cpl in
    if fn = "" then
      if sn <> "" then
        Some
          (Update.UERR_missing_first_name
             (Util.transl_nth conf "father/mother" i |> Adef.safe))
      else None
    else if sn = "" then
      Some
        (Update.UERR_missing_surname
           (Util.transl_nth conf "father/mother" i |> Adef.safe))
    else None
  in
  match check Adef.father 0 with
  | Some _ as err -> err
  | None -> check Adef.mother 1

let check_child conf p =
  let fn, sn, _, _, _ = p in
  if fn = "" && sn <> "" then
    Some
      (Update.UERR_missing_first_name
         (Util.transl_nth conf "child/children" 0 |> Adef.safe))
  else None

let check_children conf children =
  let len = Array.length children in
  let rec aux i =
    if i >= len then None
    else
      match check_child conf children.(i) with
      | Some _ as err_o -> err_o
      | None -> aux (succ i)
  in
  aux 0

let check_family conf fam cpl des :
    Update.update_error option
    * Update.update_error option
    * Update.update_error option =
  let err_parents = check_parents conf cpl in
  let err_fevent_witness =
    Update.check_missing_witnesses_names conf
      (fun e -> e.Def.efam_witnesses)
      fam.Def.fevents
  in
  let err_children = check_children conf des.Def.children in
  (err_fevent_witness, err_parents, err_children)

let strip_family fam des =
  let fam =
    {
      fam with
      Def.witnesses = strip_array_persons fam.Def.witnesses;
      fevents = strip_events fam.fevents;
    }
  in
  let des = { Def.children = strip_array_persons des.Def.children } in
  (fam, des)

let print_err_parents conf base p =
  let err = Update.UERR_already_has_parents (base, p) in
  Update.prerr conf err @@ fun () ->
  Output.print_sstring conf "\n";
  Output.print_string conf (Update.string_of_error conf err);
  Output.print_sstring conf "<p><ul><li>";
  Output.print_sstring conf
    (Utf8.capitalize_fst (Util.transl conf "first free number"));
  Output.print_sstring conf (Util.transl conf ":");
  Output.print_sstring conf @@ string_of_int
  @@ Gutil.find_free_occ base (Gwdb.p_first_name base p) (Gwdb.p_surname base p);
  Output.print_sstring conf "</li></ul>";
  Update.print_return conf

let print_err_sex conf base p =
  let err = Update.UERR_sex_incoherent (base, p) in
  Update.prerr conf err @@ fun () ->
  Output.print_string conf (Update.string_of_error conf err);
  Update.print_return conf

let print_err conf =
  let err =
    Update.UERR (Util.transl conf "error" |> Utf8.capitalize_fst |> Adef.safe)
  in
  Update.prerr conf err @@ fun () -> Update.print_return conf

let print_error_disconnected conf =
  let err =
    Update.UERR
      (Util.transl conf "msg error disconnected"
      |> Utf8.capitalize_fst |> Adef.safe)
  in
  Update.prerr conf err @@ fun () ->
  Hutil.print_link_to_welcome conf true;
  Output.print_string conf (Update.string_of_error conf err)

let family_exclude pfams efam =
  let pfaml =
    Array.fold_right
      (fun fam faml -> if fam = efam then faml else fam :: faml)
      pfams []
  in
  Array.of_list pfaml

let infer_origin_file_from_other_marriages base ifam ip =
  let u = Gwdb.poi base ip in
  let ufams = Gwdb.get_family u in
  let rec loop i =
    if i = Array.length ufams then None
    else if ufams.(i) = ifam then loop (i + 1)
    else
      let r = Gwdb.get_origin_file (Gwdb.foi base ufams.(i)) in
      if Gwdb.sou base r <> "" then Some r else loop (i + 1)
  in
  loop 0

let infer_origin_file conf base ifam ncpl ndes =
  let r = infer_origin_file_from_other_marriages base ifam (Adef.father ncpl) in
  let r =
    if r = None then
      infer_origin_file_from_other_marriages base ifam (Adef.mother ncpl)
    else r
  in
  let r =
    match r with
    | Some r -> r
    | None -> (
        let afath = Gwdb.poi base (Adef.father ncpl) in
        let amoth = Gwdb.poi base (Adef.mother ncpl) in
        match (Gwdb.get_parents afath, Gwdb.get_parents amoth) with
        | Some if1, _
          when Gwdb.sou base (Gwdb.get_origin_file (Gwdb.foi base if1)) <> "" ->
            Gwdb.get_origin_file (Gwdb.foi base if1)
        | _, Some if2
          when Gwdb.sou base (Gwdb.get_origin_file (Gwdb.foi base if2)) <> "" ->
            Gwdb.get_origin_file (Gwdb.foi base if2)
        | _ ->
            let rec loop i =
              if i = Array.length ndes.Def.children then
                Gwdb.insert_string base ""
              else
                let cifams =
                  Gwdb.get_family (Gwdb.poi base ndes.children.(i))
                in
                if Array.length cifams = 0 then loop (i + 1)
                else if
                  Gwdb.sou base
                    (Gwdb.get_origin_file (Gwdb.foi base cifams.(0)))
                  <> ""
                then Gwdb.get_origin_file (Gwdb.foi base cifams.(0))
                else loop (i + 1)
            in
            loop 0)
  in
  let no_dec =
    List.assoc_opt "propose_add_family" conf.Config.base_env = Some "no"
  in
  if no_dec && Gwdb.sou base r = "" then print_error_disconnected conf else r

(* TODO EVENT put this in Event *)
let fwitnesses_of fevents =
  List.fold_left
    (fun ipl e ->
      Array.fold_left (fun ipl (ip, _, _) -> ip :: ipl) ipl e.Def.efam_witnesses)
    [] fevents

let fwitnesses_of_fam_event fam_events =
  List.fold_left
    (fun l fevent ->
      Array.fold_left
        (fun l (ip, _) -> ip :: l)
        l
        (Gwdb.get_fevent_witnesses fevent))
    [] fam_events

(* Lorsqu'on ajout naissance décès par exemple en créant une personne. *)
let patch_person_with_pevents base ip =
  let p = Gwdb.poi base ip |> Gwdb.gen_person_of_person in
  let evt ~name ?(date = Date.cdate_None) ~place ~src ~note () =
    {
      Def.epers_name = name;
      epers_date = date;
      epers_place = place;
      epers_reason = Gwdb.empty_string;
      epers_note = note;
      epers_src = src;
      epers_witnesses = [||];
    }
    (* TODO why empty witnesses *)
  in
  let evt_birth =
    let evt ?date () =
      let name = Def.Epers_Birth in
      let place = p.birth_place in
      let note = p.birth_note in
      let src = p.birth_src in
      Some (evt ~name ?date ~place ~note ~src ())
    in
    if Option.is_some (Date.od_of_cdate p.birth) then evt ~date:p.birth ()
    else if Gwdb.sou base p.birth_place = "" then None
    else evt ()
  in
  let evt_baptism =
    let evt ?date () =
      let name = Def.Epers_Baptism in
      let place = p.baptism_place in
      let note = p.baptism_note in
      let src = p.baptism_src in
      Some (evt ~name ?date ~place ~note ~src ())
    in
    if Option.is_some (Date.od_of_cdate p.baptism) then evt ~date:p.baptism ()
    else if Gwdb.sou base p.baptism_place = "" then None
    else evt ()
  in
  let evt_death =
    let evt ?date () =
      let name = Def.Epers_Death in
      let place = p.death_place in
      let note = p.death_note in
      let src = p.death_src in
      Some (evt ~name ?date ~place ~note ~src ())
    in
    match Date.date_of_death p.death with
    | Some cd ->
        let date = Date.cdate_of_od (Some cd) in
        evt ~date ()
    | None -> if Gwdb.sou base p.death_place = "" then None else evt ()
  in
  (* Attention, on prend aussi les autres évènements sinon,  *)
  (* on va tout effacer et ne garder que naissance et décès. *)
  let pevents =
    let found_birth = ref false in
    let found_baptism = ref false in
    let found_death = ref false in
    let replace_witnesses event found new_event =
      (* Si il y avait des témoins, on les remets en place. *)
      if !found then event
      else
        match new_event with
        | None -> event
        | Some new_event ->
            found := true;
            { new_event with Def.epers_witnesses = event.Def.epers_witnesses }
    in
    let l =
      List.map
        (fun evt ->
          match evt.Def.epers_name with
          | Epers_Birth -> replace_witnesses evt found_birth evt_birth
          | Epers_Baptism -> replace_witnesses evt found_baptism evt_baptism
          | Epers_Death -> replace_witnesses evt found_death evt_death
          | _other -> evt)
        p.pevents
    in
    (* add default birth|baptism|death event if it was not found *)
    let complete found event_opt l =
      if found then l
      else match event_opt with None -> l | Some evt -> evt :: l
    in
    complete !found_birth evt_birth l
    |> complete !found_baptism evt_baptism
    |> complete !found_death evt_death
  in
  let p = { p with pevents } in
  Gwdb.patch_person base p.key_index p

let patch_parent_with_pevents base cpl =
  Array.iter (patch_person_with_pevents base) (Adef.parent_array cpl)

let patch_children_with_pevents base des =
  Array.iter (patch_person_with_pevents base) des.Def.children

(* On met à jour les témoins maintenant. *)
let update_family_with_fevents conf base fam =
  let marr, div, witnesses =
    reconstitute_from_fevents
      (Util.p_getenv conf.Config.env "nsck" = Some "on")
      (Gwdb.insert_string base "")
      fam.Def.fevents
  in
  let relation, marriage, marriage_place, marriage_note, marriage_src = marr in
  let divorce = div in
  let witnesses = Array.map (fun (ip, _, _) -> ip) witnesses in
  {
    fam with
    marriage;
    marriage_place;
    marriage_note;
    marriage_src;
    relation;
    divorce;
    witnesses;
  }

let aux_effective_mod conf base nsck sfam scpl sdes fi origin_file =
  let created_p = ref [] in
  let psrc =
    match Util.p_getenv conf.Config.env "psrc" with
    | Some s -> String.trim s
    | None -> ""
  in
  let ncpl =
    Futil.map_couple_p (Update.insert_person conf base psrc created_p) scpl
  in
  let nfam =
    Futil.map_family_ps
      (Update.insert_person conf base psrc created_p)
      Fun.id (Gwdb.insert_string base) sfam
  in
  let ndes =
    Futil.map_descend_p (Update.insert_person conf base psrc created_p) sdes
  in
  let nfath_p = Gwdb.poi base (Adef.father ncpl) in
  let nmoth_p = Gwdb.poi base (Adef.mother ncpl) in
  let nfam = update_family_with_fevents conf base nfam in
  let nfam =
    (* En mode api, on gère directement la relation de même sexe. *)
    if conf.Config.api_mode then { nfam with relation = sfam.relation }
    else nfam
  in
  if not nsck then (
    let exp sex p =
      let s = Gwdb.get_sex p in
      if s = Neuter then
        let p = { (Gwdb.gen_person_of_person p) with sex } in
        Gwdb.patch_person base p.key_index p
      else if s <> sex then print_err_sex conf base p
    in
    exp Male nfath_p;
    exp Female nmoth_p);
  if Adef.father ncpl = Adef.mother ncpl then print_err conf;
  let origin_file = origin_file nfam ncpl ndes in
  let nfam = { nfam with origin_file; fam_index = fi } in
  Gwdb.patch_family base fi nfam;
  Gwdb.patch_couple base fi ncpl;
  Gwdb.patch_descend base fi ndes;
  (nfath_p, nmoth_p, nfam, ncpl, ndes)

let effective_mod conf base nsck sfam scpl sdes =
  let fi = sfam.Def.fam_index in
  let oorigin, owitnesses, ofevents =
    let ofam = Gwdb.foi base fi in
    (Gwdb.get_origin_file ofam, Gwdb.get_witnesses ofam, Gwdb.get_fevents ofam)
  in
  let oarr, ofather, omother =
    let ocpl = Gwdb.foi base fi in
    (Gwdb.get_parent_array ocpl, Gwdb.get_father ocpl, Gwdb.get_mother ocpl)
  in
  let ochildren = Gwdb.get_children (Gwdb.foi base fi) in
  let origin_file nfam ncpl ndes =
    if sfam.origin_file = "" then
      if Gwdb.sou base oorigin <> "" then oorigin
      else infer_origin_file conf base fi ncpl ndes
    else nfam.Def.origin_file
  in
  let _, _, nfam, ncpl, ndes =
    aux_effective_mod conf base nsck sfam scpl sdes fi origin_file
  in
  let narr = Adef.parent_array ncpl in
  for i = 0 to Array.length oarr - 1 do
    if not (Array.mem oarr.(i) narr) then
      let ou = Gwdb.poi base oarr.(i) in
      let ou = { Def.family = family_exclude (Gwdb.get_family ou) fi } in
      Gwdb.patch_union base oarr.(i) ou
  done;
  for i = 0 to Array.length narr - 1 do
    if not (Array.mem narr.(i) oarr) then
      let nu = Gwdb.poi base narr.(i) in
      let nu = { Def.family = Array.append (Gwdb.get_family nu) [| fi |] } in
      Gwdb.patch_union base narr.(i) nu
  done;
  let cache = Hashtbl.create 101 in
  let find_asc ip =
    match Hashtbl.find_opt cache ip with
    | Some a -> a
    | None ->
        let a = Gwdb.poi base ip in
        let a =
          { Def.parents = Gwdb.get_parents a; consang = Gwdb.get_consang a }
        in
        Hashtbl.add cache ip a;
        a
  in
  let same_parents = Adef.father ncpl = ofather && Adef.mother ncpl = omother in
  Array.iter
    (fun ip ->
      let a = find_asc ip in
      let a =
        {
          Def.parents = None;
          consang =
            (if not (Array.mem ip ndes.children) then Adef.fix (-1)
            else a.consang);
        }
      in
      Hashtbl.replace cache ip a)
    ochildren;
  Array.iter
    (fun ip ->
      let a = find_asc ip in
      match a.parents with
      | Some _ -> print_err_parents conf base (Gwdb.poi base ip)
      | None ->
          let a =
            {
              Def.parents = Some fi;
              consang =
                (if (not (Array.mem ip ochildren)) || not same_parents then
                 Adef.fix (-1)
                else a.consang);
            }
          in
          Hashtbl.replace cache ip a)
    ndes.children;
  Array.iter
    (fun ip ->
      if not (Array.mem ip ndes.children) then
        Gwdb.patch_ascend base ip (find_asc ip))
    ochildren;
  Array.iter
    (fun ip ->
      if (not (Array.mem ip ochildren)) || not same_parents then
        Gwdb.patch_ascend base ip (find_asc ip))
    ndes.children;
  let ol =
    Array.fold_right
      (fun x acc -> x :: acc)
      owitnesses
      (fwitnesses_of_fam_event ofevents)
  in
  let nl =
    Array.fold_right
      (fun x acc -> x :: acc)
      nfam.witnesses
      (fwitnesses_of nfam.fevents)
  in
  let pi = Adef.father ncpl in
  Update.update_related_pointers base pi ol nl;
  (fi, nfam, ncpl, ndes)

let effective_add conf base nsck sfam scpl sdes =
  let fi =
    Gwdb.insert_family base
      (Gwdb.no_family Gwdb.dummy_ifam)
      Gwdb.no_couple Gwdb.no_descend
  in
  let origin_file _nfam ncpl ndes = infer_origin_file conf base fi ncpl ndes in
  let nfath_p, nmoth_p, nfam, ncpl, ndes =
    aux_effective_mod conf base nsck sfam scpl sdes fi origin_file
  in
  let nfath_u =
    { Def.family = Array.append (Gwdb.get_family nfath_p) [| fi |] }
  in
  let nmoth_u =
    { Def.family = Array.append (Gwdb.get_family nmoth_p) [| fi |] }
  in
  Gwdb.patch_union base (Adef.father ncpl) nfath_u;
  Gwdb.patch_union base (Adef.mother ncpl) nmoth_u;
  Array.iter
    (fun ip ->
      let p = Gwdb.poi base ip in
      match Gwdb.get_parents p with
      | Some _ -> print_err_parents conf base p
      | None ->
          let a = { Def.parents = Some fi; consang = Adef.fix (-1) } in
          Gwdb.patch_ascend base (Gwdb.get_iper p) a)
    ndes.children;
  let nl_witnesses = Array.to_list nfam.witnesses in
  let nl_fevents = fwitnesses_of nfam.fevents in
  let nl = List.append nl_witnesses nl_fevents in
  Update.update_related_pointers base (Adef.father ncpl) [] nl;
  (fi, nfam, ncpl, ndes)

let effective_inv conf base ip u ifam =
  let rec loop = function
    | ifam1 :: ifam2 :: ifaml ->
        if ifam2 = ifam then ifam2 :: ifam1 :: ifaml
        else ifam1 :: loop (ifam2 :: ifaml)
    | _ ->
        Hutil.incorrect_request conf;
        raise
        @@ Update.ModErr
             (Update.UERR (__FILE__ ^ " " ^ string_of_int __LINE__ |> Adef.safe))
  in
  let u =
    { Def.family = Array.of_list (loop (Array.to_list (Gwdb.get_family u))) }
  in
  Gwdb.patch_union base ip u

(* ************************************************************************ *)
(*  [Fonc] effective_chg_order : base -> iper -> person -> ifam -> int -> unit        *)

(* ************************************************************************ *)

(** [Description] : Modifie l'ordre de la famille en positionnant la famille
      ifam à la position n. Exemple : [f1 f2 f3 f4] f1 3 => [f2 f3 f1 f4].
    [Args] :
      - base : base de donnée
      - ip   : iper
      - u    : person
      - ifam : famille à changer de place
      - n    : nouvelle position de la famille
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                         *)
let effective_chg_order base ip u ifam n =
  let fam = UpdateFam.change_order u ifam n in
  let u = { Def.family = Array.of_list fam } in
  Gwdb.patch_union base ip u

let effective_del conf base ip fam =
  let ifam = Gwdb.get_ifam fam in
  Gwdb.delete_family base ifam;
  let changed =
    let gen_p =
      let p =
        if ip = Gwdb.get_mother fam then Gwdb.poi base (Gwdb.get_mother fam)
        else Gwdb.poi base (Gwdb.get_father fam)
      in
      Gwdb.gen_person_of_person p
    in
    let gen_fam = Gwdb.gen_family_of_family fam in
    Def.U_Delete_family (gen_p, gen_fam)
  in
  History.record conf base changed "df"

let is_a_link = function _, _, _, Update.Link, _ -> true | _ -> false

let is_created_or_already_there ochil_arr nchil schil =
  (not (is_a_link schil)) || Array.mem nchil ochil_arr

(* need_check_noloop: optimization
     The no-loop check being a big work on large databases, this
   optimization tests if this is really necessary or not. It is not
   necessary if:
   1/ either all parents are created,
   2/ or all children are created,
   3/ or the new family have the same parents than the old one *and*
      all linked (not created) new children were already children.
*)
(* Replaced && by || to do more checks. *)
(* Improvement : check the name on the parents/children if they linked *)

let need_check_noloop (scpl, sdes, onfs) =
  if
    Array.exists is_a_link (Adef.parent_array scpl)
    || Array.exists is_a_link sdes.Def.children
  then
    match onfs with
    | Some ((opar, ochil), (npar, nchil)) ->
        (not
           (Ext_array.forall2
              (is_created_or_already_there opar)
              npar (Adef.parent_array scpl)))
        || not
             (Ext_array.forall2
                (is_created_or_already_there ochil)
                nchil sdes.children)
    | None -> true
  else false

let all_checks_family conf base ifam gen_fam cpl des scdo =
  let wl = ref [] in
  let ml = ref [] in
  let error = Update.def_error conf base in
  let warning w = wl := w :: !wl in
  let misc m = ml := m :: !ml in
  if need_check_noloop scdo then
    Consang.check_noloop_for_person_list base error
      (Array.to_list (Adef.parent_array cpl));
  let fam = Gwdb.family_of_gen_family base (gen_fam, cpl, des) in
  CheckItem.family base warning ifam fam;
  CheckItem.check_other_fields base misc ifam fam;
  let wl, ml = (List.sort_uniq compare !wl, List.sort_uniq compare !ml) in
  List.iter
    (function
      | Warning.ChangedOrderOfMarriages (p, _, after) ->
          Gwdb.patch_union base (Gwdb.get_iper p) { family = after }
      | ChangedOrderOfFamilyEvents (ifam, _, after) ->
          Gwdb.patch_family base ifam { gen_fam with fevents = after }
      | _ -> ())
    wl;
  (wl, ml)

let print_family conf base (wl, ml) cpl des =
  let rdsrc =
    match Util.p_getenv conf.Config.env "rdsrc" with
    | Some "on" -> Util.p_getenv conf.Config.env "src"
    | Some _ | None -> Util.p_getenv conf.Config.env "dsrc"
  in
  (match rdsrc with
  | Some x ->
      conf.Config.henv <- List.remove_assoc "dsrc" conf.Config.henv;
      if x <> "" then
        conf.Config.henv <- ("dsrc", Mutil.encode x) :: conf.Config.henv
  | None -> ());
  Output.print_sstring conf "<ul>\n";
  Output.print_sstring conf "<li>";
  Output.print_string conf
    (NameDisplay.referenced_person_text conf base
       (Gwdb.poi base (Adef.father cpl)));
  Output.print_sstring conf "</li>";
  Output.print_sstring conf "\n";
  Output.print_sstring conf "<li>";
  Output.print_string conf
    (NameDisplay.referenced_person_text conf base
       (Gwdb.poi base (Adef.mother cpl)));
  Output.print_sstring conf "</li>";
  Output.print_sstring conf "</ul>\n";
  if des.Def.children <> [||] then (
    Output.print_sstring conf "<ul>\n";
    Array.iter
      (fun ip ->
        Output.print_sstring conf "<li>";
        Output.print_string conf
          (NameDisplay.referenced_person_text conf base (Gwdb.poi base ip));
        Output.print_sstring conf "</li>")
      des.children;
    Output.print_sstring conf "</ul>\n");
  Update.print_warnings_and_miscs conf base wl ml

let print_title conf fmt _ =
  Output.print_sstring conf (Utf8.capitalize_fst (Util.transl conf fmt))

let print_mod_ok conf base (wl, ml) cpl des =
  Hutil.header conf @@ print_title conf "family modified";
  Hutil.print_link_to_welcome conf true;
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then (
    Output.print_sstring conf "<h3 class=\"error\">";
    Output.printf conf
      (Util.fcapitale (Util.ftransl conf "%s forbidden char"))
      (List.fold_left
         (fun acc c -> acc ^ "'" ^ Char.escaped c ^ "' ")
         " " Name.forbidden_char);
    Output.print_sstring conf "</h3>\n";
    List.iter (Output.printf conf "<p>%s</p>") !removed_string);
  print_family conf base (wl, ml) cpl des;
  Hutil.trailer conf

let print_change_event_order_ok conf base (wl, ml) cpl des =
  Hutil.header conf @@ print_title conf "family modified";
  Hutil.print_link_to_welcome conf true;
  print_family conf base (wl, ml) cpl des;
  Hutil.trailer conf

let print_add_ok conf base (wl, ml) cpl des =
  Hutil.header conf @@ print_title conf "family added";
  Hutil.print_link_to_welcome conf true;
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then (
    Output.printf conf "<h2 class=\"error\">%s</h2>\n"
      (Utf8.capitalize_fst (Util.transl conf "forbidden char"));
    List.iter (Output.printf conf "<p>%s</p>") !removed_string);
  print_family conf base (wl, ml) cpl des;
  Hutil.trailer conf

let print_del_ok conf base wl =
  Hutil.header conf @@ print_title conf "family deleted";
  Hutil.print_link_to_welcome conf true;
  (match Util.p_getenv conf.Config.env "ip" with
  | Some i ->
      let p = Gwdb.poi base (Gwdb.iper_of_string i) in
      Output.print_sstring conf "<ul><li>";
      Output.print_string conf
        (NameDisplay.reference conf base p
           (NameDisplay.fullname_html_of_person conf base p));
      Output.print_sstring conf "\n</ul>"
  | None -> ());
  Update.print_warnings conf base wl;
  Hutil.trailer conf

let print_del conf base =
  match Util.p_getenv conf.Config.env "i" with
  | Some i ->
      let ifam = Gwdb.ifam_of_string i in
      let fam = Gwdb.foi base ifam in
      let ip =
        match Util.p_getenv conf.Config.env "ip" with
        | Some i when Gwdb.get_mother fam = Gwdb.iper_of_string i ->
            Gwdb.get_mother fam
        | Some _ | None -> Gwdb.get_father fam
      in
      effective_del conf base ip fam;
      Util.commit_patches conf base;
      print_del_ok conf base []
  | None -> Hutil.incorrect_request conf

let print_inv_ok conf base p =
  Hutil.header conf @@ print_title conf "inversion done";
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "\n";
  Output.print_string conf (NameDisplay.referenced_person_text conf base p);
  Output.print_sstring conf "\n";
  Hutil.trailer conf

let get_create (_, _, _, create, _) = create

let forbidden_disconnected conf scpl sdes =
  let no_dec =
    List.assoc_opt "propose_add_family" conf.Config.base_env = Some "no"
  in
  if no_dec then
    if
      get_create (Adef.father scpl) = Update.Link
      || get_create (Adef.mother scpl) = Update.Link
    then false
    else Array.for_all (fun p -> get_create p <> Update.Link) sdes.Def.children
  else false

let print_add o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let conf = Update.update_conf o_conf in
  let nsck = Util.p_getenv conf.Config.env "nsck" = Some "on" in
  let sfam, scpl, sdes, ext = reconstitute_family conf base nsck in
  let redisp = Option.is_some (Util.p_getenv conf.Config.env "return") in
  let digest =
    match Util.p_getenv conf.Config.env "ip" with
    | Some ip ->
        string_of_int
          (Array.length
             (Gwdb.get_family (Gwdb.poi base (Gwdb.iper_of_string ip))))
    | None -> ""
  in
  let sdigest = Update_util.get conf "digest" in
  if digest <> "" && sdigest <> "" && digest <> sdigest then
    Update.error_digest conf
  else if ext || redisp then
    UpdateFam.print_update_fam conf base (sfam, scpl, sdes) ""
  else if forbidden_disconnected conf scpl sdes then
    print_error_disconnected conf
  else
    match check_family conf sfam scpl sdes with
    | Some err, _, _ | _, Some err, _ | _, _, Some err -> error_family conf err
    | None, None, None ->
        let sfam, sdes = strip_family sfam sdes in
        let nsck = Util.p_getenv conf.Config.env "nsck" = Some "on" in
        let ifam, fam, cpl, des = effective_add conf base nsck sfam scpl sdes in
        let () = patch_parent_with_pevents base cpl in
        let () = patch_children_with_pevents base des in
        let wl, ml =
          all_checks_family conf base ifam fam cpl des (scpl, sdes, None)
        in
        let changed, act =
          let ip, act =
            match Util.p_getenv conf.Config.env "ip" with
            | Some i -> (
                let i = Gwdb.iper_of_string i in
                if Adef.mother cpl = i then (Adef.mother cpl, "af")
                else
                  let a = Gwdb.poi base i in
                  match Gwdb.get_parents a with
                  | Some x when x = ifam -> (i, "aa")
                  | _ -> (Adef.father cpl, "af"))
            | None -> (Adef.father cpl, "af")
          in
          match act with
          | "af" ->
              let gen_p = Gwdb.gen_person_of_person (Gwdb.poi base ip) in
              (Def.U_Add_family (gen_p, fam), "af")
          | _ ->
              let gen_p = Gwdb.gen_person_of_person (Gwdb.poi base ip) in
              (U_Add_parent (gen_p, fam), "aa")
        in
        Util.commit_patches conf base;
        History.record conf base changed act;
        Update.delete_topological_sort conf base;
        print_add_ok conf base (wl, ml) cpl des

(* If we only have two linked parents,
   with one linked child and not other informations,
   and if a union already exists between the parents,
   edit the existing union in order to add a child.
   Else, create a new union. *)
let print_add_parents o_conf base =
  let conf = Update.update_conf o_conf in
  let nsck = Util.p_getenv conf.Config.env "nsck" = Some "on" in
  let sfam, scpl, sdes, _ = reconstitute_family conf base nsck in
  if
    sfam.marriage = Date.cdate_None
    && sfam.marriage_place = "" && sfam.marriage_note = ""
    && sfam.marriage_src = "" && sfam.witnesses = [||]
    && sfam.relation = Married && sfam.divorce = NotDivorced
    && sfam.fevents
       = [
           {
             efam_name = Efam_Marriage;
             efam_date = Date.cdate_None;
             efam_place = "";
             efam_reason = "";
             efam_note = "";
             efam_src = "";
             efam_witnesses = [||];
           };
         ]
    && sfam.comment = "" && sfam.origin_file = ""
    && sfam.fsources
       = Option.value ~default:"" (Util.p_getenv conf.Config.env "dsrc")
    && sfam.fam_index = Gwdb.dummy_ifam
  then
    match (Adef.father scpl, Adef.mother scpl, sdes.children) with
    | ( (ff, fs, fo, Update.Link, _),
        (mf, ms, mo, Update.Link, _),
        [| (cf, cs, co, Update.Link, _) |] ) -> (
        match
          ( Gwdb.person_of_key base ff fs fo,
            Gwdb.person_of_key base mf ms mo,
            Gwdb.person_of_key base cf cs co )
        with
        | Some fath, Some moth, Some child ->
            let ffam = Gwdb.get_family @@ Gwdb.poi base fath in
            let mfam = Gwdb.get_family @@ Gwdb.poi base moth in
            let rec loop i =
              if i = -1 then print_add o_conf base
              else
                let ifam = Array.unsafe_get ffam i in
                if Array.exists (( = ) ifam) mfam then (
                  let f = Gwdb.foi base ifam in
                  let sfam = Gwdb.gen_family_of_family f in
                  let scpl = Gwdb.gen_couple_of_family f in
                  let sdes =
                    {
                      Def.children =
                        Array.append (Gwdb.gen_descend_of_family f).children
                          [| child |];
                    }
                  in
                  Gwdb.patch_descend base ifam sdes;
                  Gwdb.patch_ascend base child
                    { parents = Some ifam; consang = Adef.fix (-1) };
                  Util.commit_patches conf base;
                  let f' = Gwdb.family_of_gen_family base (sfam, scpl, sdes) in
                  let wl = ref [] in
                  let warning w = wl := w :: !wl in
                  CheckItem.family ~onchange:true base warning ifam f';
                  let hr =
                    Def.U_Modify_family
                      ( Gwdb.poi base child |> Gwdb.gen_person_of_person,
                        sfam,
                        sfam )
                  in
                  History.record conf base hr "mf";
                  Update.delete_topological_sort conf base;
                  print_mod_ok conf base (!wl, []) scpl sdes)
                else loop (i - 1)
            in
            loop (Array.length ffam)
        | _ -> print_add o_conf base)
    | _ -> print_add o_conf base
  else print_add o_conf base

let print_mod_aux conf base callback =
  let nsck = Util.p_getenv conf.Config.env "nsck" = Some "on" in
  let sfam, scpl, sdes, ext = reconstitute_family conf base nsck in
  let redisp = Option.is_some (Util.p_getenv conf.Config.env "return") in
  let digest =
    let ini_sfam = UpdateFam.string_family_of base sfam.fam_index in
    Update.digest_family ini_sfam
  in
  if digest = Update_util.get conf "digest" then
    if ext || redisp then
      UpdateFam.print_update_fam conf base (sfam, scpl, sdes) digest
    else
      match check_family conf sfam scpl sdes with
      | Some err, _, _ | _, Some err, _ | _, _, Some err ->
          error_family conf err
      | None, None, None ->
          let sfam, sdes = strip_family sfam sdes in
          callback sfam scpl sdes
  else Update.error_digest conf

let family_structure base ifam =
  let fam = Gwdb.foi base ifam in
  (Gwdb.get_parent_array fam, Gwdb.get_children fam)

let print_mod o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let o_f =
    let ifam =
      match Util.p_getenv o_conf.Config.env "i" with
      | Some i -> Gwdb.ifam_of_string i
      | None -> Gwdb.dummy_ifam
    in
    Gwdb.gen_family_of_family (Gwdb.foi base ifam)
  in
  let conf = Update.update_conf o_conf in
  let callback sfam scpl sdes =
    let ofs = family_structure base sfam.Def.fam_index in
    let nsck = Util.p_getenv conf.Config.env "nsck" = Some "on" in
    let ifam, fam, cpl, des = effective_mod conf base nsck sfam scpl sdes in
    let () = patch_parent_with_pevents base cpl in
    let () = patch_children_with_pevents base des in
    let s =
      let sl =
        [ fam.comment; fam.fsources; fam.marriage_note; fam.marriage_src ]
      in
      let sl =
        let rec loop l accu =
          match l with
          | [] -> accu
          | evt :: l -> loop l (evt.Def.efam_note :: evt.efam_src :: accu)
        in
        loop fam.fevents sl
      in
      String.concat " " (List.map (Gwdb.sou base) sl)
    in
    Notes.update_notes_links_db base (Def.NLDB.PgFam ifam) s;
    let nfs = (Adef.parent_array cpl, des.children) in
    let onfs = Some (ofs, nfs) in
    let wl, ml =
      all_checks_family conf base ifam fam cpl des (scpl, sdes, onfs)
    in
    Util.commit_patches conf base;
    let changed =
      let ip =
        match Util.p_getenv o_conf.Config.env "ip" with
        | Some i -> Gwdb.iper_of_string i
        | None -> Gwdb.dummy_iper
      in
      let p = Gwdb.gen_person_of_person (Gwdb.poi base ip) in
      Def.U_Modify_family (p, o_f, fam)
    in
    History.record conf base changed "mf";
    Update.delete_topological_sort conf base;
    print_mod_ok conf base (wl, ml) cpl des
  in
  print_mod_aux conf base callback

let print_inv conf base =
  match
    (Util.p_getenv conf.Config.env "i", Util.p_getenv conf.Config.env "f")
  with
  | Some ip, Some ifam ->
      let ip = Gwdb.iper_of_string ip in
      let ifam = Gwdb.ifam_of_string ifam in
      let p = Gwdb.poi base ip in
      effective_inv conf base (Gwdb.get_iper p) p ifam;
      Util.commit_patches conf base;
      let changed =
        let gen_p = Gwdb.gen_person_of_person p in
        Def.U_Invert_family (gen_p, ifam)
      in
      History.record conf base changed "if";
      print_inv_ok conf base p
  | _ -> Hutil.incorrect_request conf

let print_change_order_ok conf base =
  match
    ( Util.p_getenv conf.Config.env "i",
      Util.p_getenv conf.Config.env "f",
      Util.p_getint conf.Config.env "n" )
  with
  | Some ip, Some ifam, Some n ->
      let ip = Gwdb.iper_of_string ip in
      let ifam = Gwdb.ifam_of_string ifam in
      let p = Gwdb.poi base ip in
      effective_chg_order base (Gwdb.get_iper p) p ifam n;
      Util.commit_patches conf base;
      let changed =
        let gen_p = Gwdb.gen_person_of_person p in
        Def.U_Invert_family (gen_p, ifam)
      in
      History.record conf base changed "if";
      print_inv_ok conf base p
  | _ -> Hutil.incorrect_request conf

let print_change_event_order conf base =
  match Util.p_getenv conf.Config.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some s ->
      let ifam = Gwdb.ifam_of_string s in
      let fam = Gwdb.foi base ifam in
      let o_f = Gwdb.gen_family_of_family fam in
      (* TODO_EVENT use Event.sorted_event *)
      let ht = Hashtbl.create 50 in
      let () =
        ignore
        @@ List.fold_left
             (fun id evt ->
               Hashtbl.add ht id evt;
               succ id)
             1 (Gwdb.get_fevents fam)
      in
      let sorted_fevents =
        List.sort
          (fun (_, pos1) (_, pos2) -> compare pos1 pos2)
          (Update_util.reconstitute_sorted_events conf 1)
      in
      let fevents =
        List.fold_right
          (fun (id, _) accu ->
            match Hashtbl.find_opt ht id with
            | Some event -> Gwdb.gen_fevent_of_fam_event event :: accu
            | None -> failwith "Sorting event")
          sorted_fevents []
      in
      let fam = Gwdb.gen_family_of_family fam in
      let fam = { fam with fevents } in
      let fam = update_family_with_fevents conf base fam in
      Gwdb.patch_family base fam.fam_index fam;
      let a = Gwdb.foi base fam.fam_index in
      let cpl = Adef.parent (Gwdb.get_parent_array a) in
      let des = { Def.children = Gwdb.get_children a } in
      let wl =
        let wl = ref [] in
        let warning w = wl := w :: !wl in
        let nfam = Gwdb.family_of_gen_family base (fam, cpl, des) in
        CheckItem.family base warning fam.fam_index nfam;
        List.iter
          (function
            | Warning.ChangedOrderOfFamilyEvents (ifam, _, after) ->
                Gwdb.patch_family base ifam { fam with fevents = after }
            | _ -> ())
          !wl;
        List.rev !wl
      in
      Util.commit_patches conf base;
      let changed =
        let ip =
          match Util.p_getenv conf.Config.env "ip" with
          | Some i -> Gwdb.iper_of_string i
          | None -> Gwdb.dummy_iper
        in
        let p = Gwdb.gen_person_of_person (Gwdb.poi base ip) in
        Def.U_Modify_family (p, o_f, fam)
      in
      History.record conf base changed "mf";
      print_change_event_order_ok conf base (wl, []) cpl des
