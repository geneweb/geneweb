(* Copyright (c) 1998-2007 INRIA *)

(* Liste des string dont on a supprimé un caractère.       *)
(* Utilisé pour le message d'erreur lors de la validation. *)
let removed_string = ref []
let get_purged_fn_sn = Update_util.get_purged_fn_sn removed_string
let reconstitute_somebody = Update_util.reconstitute_somebody removed_string

let rec reconstitute_string_list conf var form_is_modified cnt =
  match Update_util.get_nth conf var cnt with
  | None -> ([], form_is_modified)
  | Some s -> (
      let s = Ext_string.only_printable s in
      let sl, form_is_modified =
        reconstitute_string_list conf var form_is_modified (cnt + 1)
      in
      match Update_util.get_nth conf ("add_" ^ var) cnt with
      | Some "on" -> (s :: "" :: sl, true)
      | Some _ | None -> (s :: sl, form_is_modified))

let reconstitute_insert_title conf form_is_modified cnt tl =
  let var = "ins_title" ^ string_of_int cnt in
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
    let tl =
      let rec loop tl n =
        if n > 0 then
          let t1 =
            {
              Def.t_name = Tnone;
              t_ident = "";
              t_place = "";
              t_date_start = Date.cdate_None;
              t_date_end = Date.cdate_None;
              t_nth = 0;
            }
          in
          loop (t1 :: tl) (n - 1)
        else tl
      in
      loop tl n
    in
    (tl, true)
  else (tl, form_is_modified)

let rec reconstitute_titles conf form_is_modified cnt =
  match
    ( Update_util.get_nth conf "t_ident" cnt,
      Update_util.get_nth conf "t_place" cnt,
      Update_util.get_nth conf "t_name" cnt )
  with
  | Some t_ident, Some t_place, Some t_name ->
      let t_name =
        match (Update_util.get_nth conf "t_main_title" cnt, t_name) with
        | Some "on", _ -> Def.Tmain
        | _, "" -> Tnone
        | _, _ -> Tname (Ext_string.only_printable t_name)
      in
      let t_date_start =
        Update.reconstitute_date conf ("t_date_start" ^ string_of_int cnt)
      in
      let t_date_end =
        Update.reconstitute_date conf ("t_date_end" ^ string_of_int cnt)
      in
      let t_nth =
        Option.value
          (Option.bind (Update_util.get_nth conf "t_nth" cnt) int_of_string_opt)
          ~default:0
      in
      let t =
        {
          Def.t_name;
          t_ident = Ext_string.only_printable t_ident;
          t_place = Ext_string.only_printable t_place;
          t_date_start = Date.cdate_of_od t_date_start;
          t_date_end = Date.cdate_of_od t_date_end;
          t_nth;
        }
      in
      let tl, form_is_modified =
        reconstitute_titles conf form_is_modified (cnt + 1)
      in
      let tl, form_is_modified =
        reconstitute_insert_title conf form_is_modified cnt tl
      in
      (t :: tl, form_is_modified)
  | _ -> ([], form_is_modified)

type 'a form_pevent =
  | FormBirth of ('a, string) Def.gen_pers_event
  | FormBapt of ('a, string) Def.gen_pers_event
  | FormPevent of ('a, string) Def.gen_pers_event
  | FormDeath of
      [ `Auto
      | `DeadYoung
      | `DontKnowIfDead
      | `NotDead
      | `OfCourseDead
      | `Death ]
      * ('a, string) Def.gen_pers_event
  | FormBurial of
      [ `UnknownBurial | `Burial | `Cremation ]
      * ('a, string) Def.gen_pers_event

let reconstitute_insert_pevent conf form_is_modified el =
  let var = "ins_event" in
  let n =
    match
      ( Util.p_getenv conf.Config.env var,
        Util.p_getint conf.Config.env (var ^ "_n") )
    with
    | Some "on", Some n when n > 0 -> n
    | _, _ -> 0
  in
  if n > 0 then
    let el =
      let rec loop el n =
        if n > 0 then
          let e1 =
            FormPevent
              {
                Def.epers_name = Epers_Name "";
                epers_date = Date.cdate_None;
                epers_place = "";
                epers_reason = "";
                epers_note = "";
                epers_src = "";
                epers_witnesses = [||];
              }
          in
          loop (e1 :: el) (n - 1)
        else el
      in
      loop el n
    in
    (el, true)
  else (el, form_is_modified)

let is_empty_burial conf cnt =
  let buri = Update_util.get_nth conf "buri_select" cnt in
  match buri with Some "#buri" | Some "#crem" -> false | _ -> true

let is_empty_event pe =
  pe.Def.epers_date = Date.cdate_None
  && pe.epers_place = "" && pe.epers_reason = "" && pe.epers_note = ""
  && pe.epers_src = ""
  && Array.length pe.epers_witnesses = 0

let pevent_of_form_pevent = function
  | FormBirth pe -> pe
  | FormBapt pe -> pe
  | FormPevent pe -> pe
  | FormDeath (_death_status, pe) -> pe
  | FormBurial (_burial_status, pe) -> pe

let is_empty_death conf cnt =
  let death = Update_util.get_nth conf "death_select" cnt in
  match death with Some "Auto" | Some "DontKnowIfDead" -> true | _ -> false

let burial_event conf cnt pe =
  let tag =
    match Update_util.get_nth conf "buri_select" cnt with
    | Some "#buri" -> `Burial
    | Some "#crem" -> `Cremation
    | Some "buri_or_crem" | Some _ | None -> `UnknownBurial
  in
  FormBurial (tag, pe)

let death_event conf cnt pe =
  let tag =
    match Update_util.get_nth conf "death_select" cnt with
    | Some "Auto" -> `Auto
    | Some "DeadYoung" -> `DeadYoung
    | Some "DontKnowIfDead" -> `DontKnowIfDead
    | Some "NotDead" -> `NotDead
    | Some "OfCourseDead" -> `OfCourseDead
    | Some "Death" | Some _ | None -> `Death
  in
  FormDeath (tag, pe)

let rec reconstitute_pevents ~base conf form_is_modified cnt =
  match Update_util.get_nth conf "e_name" cnt with
  | None -> ([], form_is_modified)
  | Some epers_name ->
      let epers_name =
        (* TODO EVENT to/of_string *)
        match epers_name with
        | "#birt" -> Def.Epers_Birth
        | "#bapt" -> Epers_Baptism
        | "#deat" -> Epers_Death
        | "#buri" -> Epers_Burial
        | "#crem" -> Epers_Cremation
        | "#acco" -> Epers_Accomplishment
        | "#acqu" -> Epers_Acquisition
        | "#adhe" -> Epers_Adhesion
        | "#awar" -> Epers_Decoration
        | "#bapl" -> Epers_BaptismLDS
        | "#barm" -> Epers_BarMitzvah
        | "#basm" -> Epers_BatMitzvah
        | "#bles" -> Epers_Benediction
        | "#cens" -> Epers_Recensement
        | "#chgn" -> Epers_ChangeName
        | "#circ" -> Epers_Circumcision
        | "#conf" -> Epers_Confirmation
        | "#conl" -> Epers_ConfirmationLDS
        | "#degr" -> Epers_Diploma
        | "#demm" -> Epers_DemobilisationMilitaire
        | "#dist" -> Epers_Distinction
        | "#dotl" -> Epers_DotationLDS
        | "#educ" -> Epers_Education
        | "#elec" -> Epers_Election
        | "#emig" -> Epers_Emigration
        | "#endl" -> Epers_Dotation
        | "#exco" -> Epers_Excommunication
        | "#fcom" -> Epers_FirstCommunion
        | "#flkl" -> Epers_FamilyLinkLDS
        | "#fune" -> Epers_Funeral
        | "#grad" -> Epers_Graduate
        | "#hosp" -> Epers_Hospitalisation
        | "#illn" -> Epers_Illness
        | "#immi" -> Epers_Immigration
        | "#lpas" -> Epers_ListePassenger
        | "#mdis" -> Epers_MilitaryDistinction
        | "#mobm" -> Epers_MobilisationMilitaire
        | "#mpro" -> Epers_MilitaryPromotion
        | "#mser" -> Epers_MilitaryService
        | "#natu" -> Epers_Naturalisation
        | "#occu" -> Epers_Occupation
        | "#ordn" -> Epers_Ordination
        | "#prop" -> Epers_Property
        | "#resi" -> Epers_Residence
        | "#reti" -> Epers_Retired
        | "#slgc" -> Epers_ScellentChildLDS
        | "#slgp" -> Epers_ScellentParentLDS
        | "#slgs" -> Epers_ScellentSpouseLDS
        | "#vteb" -> Epers_VenteBien
        | "#will" -> Epers_Will
        | "buri_or_crem" -> Epers_Burial
        | n -> Epers_Name (Ext_string.only_printable n)
      in
      let epers_date =
        Update.reconstitute_date conf ("e_date" ^ string_of_int cnt)
      in
      let epers_place =
        match Update_util.get_nth conf "e_place" cnt with
        | Some place -> Ext_string.only_printable place
        | None -> ""
      in
      let epers_note =
        match Update_util.get_nth conf "e_note" cnt with
        | Some note ->
            Ext_string.only_printable_or_nl
              (Ext_string.strip_all_trailing_spaces note)
        | None -> ""
      in
      let epers_src =
        match Update_util.get_nth conf "e_src" cnt with
        | Some src -> Ext_string.only_printable src
        | None -> ""
      in
      (* Type du témoin par défaut lors de l'insertion de nouveaux témoins. *)
      let wk =
        if epers_name = Epers_Baptism then Def.Witness_GodParent else Witness
      in
      let witnesses, form_is_modified =
        let rec loop i form_is_modified =
          match
            try
              let var = "e" ^ string_of_int cnt ^ "_witn" ^ string_of_int i in
              Some (reconstitute_somebody conf var)
            with Failure _ -> None
          with
          | Some c -> (
              let witnesses, form_is_modified = loop (i + 1) form_is_modified in
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
                | _ -> Witness
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

              let var_w =
                "e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i
              in
              match Util.p_getenv conf.Config.env var_w with
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
                              wk,
                              "" )
                          in
                          let witnesses = new_witn :: witnesses in
                          loop_witn (n - 1) witnesses
                      in
                      loop_witn n witnesses
                  | _ ->
                      let new_witn =
                        (("", "", 0, Update.Create (Neuter, None), ""), wk, "")
                      in
                      (c :: new_witn :: witnesses, true))
              | _ -> (c :: witnesses, form_is_modified))
          | None -> ([], form_is_modified)
        in
        loop 1 form_is_modified
      in
      let witnesses =
        Update_util.witnesses_with_inferred_death_from_event ~conf ~base
          ~date:epers_date witnesses
      in
      let witnesses, form_is_modified =
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
                      (("", "", 0, Update.Create (Neuter, None), ""), wk, "")
                    in
                    let witnesses = new_witn :: witnesses in
                    loop_witn (n - 1) witnesses
                in
                loop_witn n witnesses
            | Some _ | None ->
                let new_witn =
                  (("", "", 0, Update.Create (Neuter, None), ""), wk, "")
                in
                (new_witn :: witnesses, true))
        | Some _ | None -> (witnesses, form_is_modified)
      in
      let e =
        {
          Def.epers_name;
          epers_date = Date.cdate_of_od epers_date;
          epers_place;
          epers_reason = "";
          epers_note;
          epers_src;
          epers_witnesses = Array.of_list witnesses;
        }
      in
      let e =
        match epers_name with
        | Epers_Birth -> FormBirth e
        | Epers_Baptism -> FormBapt e
        | Epers_Burial -> burial_event conf cnt e
        | Epers_Cremation -> burial_event conf cnt e
        | Epers_Death -> death_event conf cnt e
        | _ -> FormPevent e
      in
      let el, form_is_modified =
        reconstitute_pevents ~base conf form_is_modified (cnt + 1)
      in
      let skip =
        match epers_name with
        | Epers_Burial when is_empty_burial conf cnt -> true
        | Epers_Death when is_empty_death conf cnt -> true
        | _ -> false
      in
      let el = if not skip then e :: el else el in
      (el, form_is_modified)

let reconstitute_add_relation conf form_is_modified cnt rl =
  match Update_util.get_nth conf "add_relation" cnt with
  | Some "on" ->
      let r =
        { Def.r_type = GodParent; r_fath = None; r_moth = None; r_sources = "" }
      in
      (r :: rl, true)
  | Some _ | None -> (rl, form_is_modified)

let deleted_relation = ref []

let reconstitute_relation_parent conf var key sex =
  match
    ( Update_util.getn conf var (key ^ "_fn"),
      Update_util.getn conf var (key ^ "_sn") )
  with
  | ("", _ | _, "" | "?", _ | _, "?") as n ->
      let p =
        Ext_string.only_printable (fst n) ^ Ext_string.only_printable (snd n)
      in
      if p = "" || p = "??" then ()
      else deleted_relation := p :: !deleted_relation;
      None
  | fn, sn ->
      let fn = Ext_string.only_printable fn in
      let sn = Ext_string.only_printable sn in
      (* S'il y a des caractères interdits, on les supprime *)
      let fn, sn = get_purged_fn_sn fn sn in
      let occ =
        try
          Option.value ~default:0
            (int_of_string_opt (Update_util.getn conf var (key ^ "_occ")))
        with Failure _ -> 0
      in
      let create =
        (* why is it key ^ "_p" here *)
        match Update_util.getn conf var (key ^ "_p") with
        | "create" -> Update.Create (sex, None)
        | _ -> Update.Link
      in
      Some (fn, sn, occ, create, var ^ "_" ^ key)

let reconstitute_relation conf var =
  try
    let r_fath = reconstitute_relation_parent conf var "fath" Male in
    let r_moth = reconstitute_relation_parent conf var "moth" Female in
    let r_type =
      match Update_util.getn conf var "type" with
      | "Adoption" -> Def.Adoption
      | "Recognition" -> Recognition
      | "CandidateParent" -> CandidateParent
      | "GodParent" -> GodParent
      | "FosterParent" -> FosterParent
      | _s -> GodParent
    in
    Some { Def.r_type; r_fath; r_moth; r_sources = "" }
  with Failure _ -> None

let rec reconstitute_relations conf form_is_modified cnt =
  match reconstitute_relation conf ("r" ^ string_of_int cnt) with
  | Some r ->
      let rl, form_is_modified =
        reconstitute_relations conf form_is_modified (cnt + 1)
      in
      let rl, form_is_modified =
        reconstitute_add_relation conf form_is_modified cnt rl
      in
      (r :: rl, form_is_modified)
  | None -> ([], form_is_modified)

let infer_death death_status burial_status burial_place death_pers_event =
  if death_pers_event.Def.epers_date <> Date.cdate_None then
    Def.Death (Unspecified, death_pers_event.Def.epers_date)
  else
    match death_status with
    | `DeadYoung -> Def.DeadYoung
    | (`DontKnowIfDead | `Auto | `NotDead)
      when death_pers_event.Def.epers_place <> ""
           || burial_status <> `UnknownBurial
           || burial_place <> "" ->
        DeadDontKnowWhen
    | `DontKnowIfDead | `Auto -> DontKnowIfDead
    | `NotDead -> NotDead
    | `OfCourseDead -> OfCourseDead
    | `Death when death_pers_event.epers_date = Date.cdate_None ->
        DeadDontKnowWhen
    | `Death -> Death (Unspecified, death_pers_event.Def.epers_date)

(* TODO EVENT put this in Event *)
let sort_form_pevents form_pevents =
  Event.sort_events
    (fun evt -> Event.Pevent (pevent_of_form_pevent evt).Def.epers_name)
    (fun evt -> (pevent_of_form_pevent evt).epers_date)
    form_pevents

let pevent_of_form_pevent = function
  | FormBirth pe -> pe
  | FormBapt pe -> pe
  | FormPevent pe -> pe
  | FormDeath (_death_status, pe) -> pe
  | FormBurial (_burial_status, pe) -> pe

let remove_unnamed_events form_pevents =
  List.filter
    (fun pe -> (pevent_of_form_pevent pe).Def.epers_name <> Epers_Name "")
    form_pevents

let get_main_form_pevents form_pevents =
  let form_pevents : 'a form_pevent list = sort_form_pevents form_pevents in
  let rec find found_birth found_baptism found_death found_burial = function
    | [] -> (found_birth, found_baptism, found_death, found_burial)
    | (FormBirth _evt as evt) :: pevents when found_birth = None ->
        find (Some evt) found_baptism found_death found_burial pevents
    | (FormBapt _evt as evt) :: pevents when found_baptism = None ->
        find found_birth (Some evt) found_death found_burial pevents
    | (FormDeath (_, _evt) as evt) :: pevents when found_death = None ->
        find found_birth found_baptism (Some evt) found_burial pevents
    | (FormBurial (_, _evt) as evt) :: pevents when found_burial = None ->
        find found_birth found_baptism found_death (Some evt) pevents
    | _evt :: pevents ->
        find found_birth found_baptism found_death found_burial pevents
  in
  let birth, bapt, death, burial = find None None None None form_pevents in
  let empty_evt =
    FormPevent
      {
        Def.epers_name = Epers_Name "";
        epers_date = Date.cdate_None;
        epers_place = "";
        epers_note = "";
        epers_src = "";
        epers_reason = "";
        epers_witnesses = [||];
      }
  in
  let birth = Option.value ~default:empty_evt birth in
  let bapt = Option.value ~default:empty_evt bapt in
  let burial = Option.value ~default:empty_evt burial in
  let death = Option.value ~default:empty_evt death in
  (birth, bapt, death, burial)

let form_pevent_of_pevent death_status pe =
  match pe.Def.epers_name with
  | Epers_Birth -> FormBirth pe
  | Epers_Baptism -> FormBapt pe
  | Epers_Death -> FormDeath (death_status, pe)
  | Epers_Burial -> FormBurial (`Burial, pe)
  | Epers_Cremation -> FormBurial (`Cremation, pe)
  | _ -> FormPevent pe

let get_burial_status_opt = function
  | FormBurial (burial_status, _) -> Some burial_status
  | _ -> None

let get_death_status_opt = function
  | FormDeath (death_status, _) -> Some death_status
  | _ -> None

let get_main_pevents ?(death_status = `DontKnowIfDead) pevents =
  let form_pevents = List.map (form_pevent_of_pevent death_status) pevents in
  let bi, ba, de, bu = get_main_form_pevents form_pevents in
  let burial_status =
    Option.value ~default:`UnknownBurial (get_burial_status_opt bu)
  in
  let bi = pevent_of_form_pevent bi in
  let ba = pevent_of_form_pevent ba in
  let de = pevent_of_form_pevent de in
  let bu = pevent_of_form_pevent bu in
  let death = infer_death death_status burial_status bu.epers_place de in
  let burial_status =
    match burial_status with
    | `Burial -> Def.Buried bu.Def.epers_date
    | `Cremation -> Cremated bu.epers_date
    | `UnknownBurial -> UnknownBurial
  in
  let pevents =
    List.map pevent_of_form_pevent (remove_unnamed_events form_pevents)
  in
  ( (bi.Def.epers_date, bi.epers_place, bi.epers_note, bi.epers_src),
    (ba.Def.epers_date, ba.epers_place, ba.epers_note, ba.epers_src),
    (death, de.epers_place, de.epers_note, de.epers_src),
    (burial_status, bu.epers_place, bu.epers_note, bu.epers_src),
    pevents )

let reconstitute_person ~base conf =
  let form_is_modified = false in
  let key_index =
    match Util.p_getenv conf.Config.env "i" with
    | Some s -> (
        try Gwdb.iper_of_string (String.trim s)
        with Failure _ -> Gwdb.dummy_iper)
    | None -> Gwdb.dummy_iper
  in
  let first_name =
    Ext_string.only_printable (Update_util.get conf "first_name")
  in
  let surname = Ext_string.only_printable (Update_util.get conf "surname") in
  (* S'il y a des caractères interdits, on les supprime *)
  let first_name, surname = get_purged_fn_sn first_name surname in
  let occ =
    try
      Option.value ~default:0
        (int_of_string_opt (String.trim (Update_util.get conf "occ")))
    with Failure _ -> 0
  in
  let image = Ext_string.only_printable (Update_util.get conf "image") in
  let first_names_aliases, form_is_modified =
    reconstitute_string_list conf "first_name_alias" form_is_modified 0
  in
  let surnames_aliases, form_is_modified =
    reconstitute_string_list conf "surname_alias" form_is_modified 0
  in
  let public_name =
    Ext_string.only_printable (Update_util.get conf "public_name")
  in
  let qualifiers, form_is_modified =
    reconstitute_string_list conf "qualifier" form_is_modified 0
  in
  let aliases, form_is_modified =
    reconstitute_string_list conf "alias" form_is_modified 0
  in
  let titles, form_is_modified = reconstitute_titles conf form_is_modified 1 in
  let titles, form_is_modified =
    reconstitute_insert_title conf form_is_modified 0 titles
  in
  let rparents, form_is_modified =
    reconstitute_relations conf form_is_modified 1
  in
  let rparents, form_is_modified =
    reconstitute_add_relation conf form_is_modified 0 rparents
  in
  let access =
    match Util.p_getenv conf.Config.env "access" with
    | Some "Public" -> Def.Public
    | Some "Private" -> Private
    | Some _ | None -> IfTitles
  in
  let occupation = Ext_string.only_printable (Update_util.get conf "occu") in
  let sex =
    match Util.p_getenv conf.Config.env "sex" with
    | Some "M" -> Def.Male
    | Some "F" -> Female
    | Some _ | None -> Neuter
  in
  let pevents, form_is_modified =
    reconstitute_pevents ~base conf form_is_modified 1
  in
  let pevents, form_is_modified =
    reconstitute_insert_pevent conf form_is_modified pevents
  in
  let pevents =
    if form_is_modified then pevents else remove_unnamed_events pevents
  in
  let notes =
    if first_name = "?" || surname = "?" then ""
    else
      Ext_string.only_printable_or_nl
        (Ext_string.strip_all_trailing_spaces (Update_util.get conf "notes"))
  in
  let psources = Ext_string.only_printable (Update_util.get conf "src") in
  let birth_form, bapt_form, death_form, burial_form =
    get_main_form_pevents pevents
  in
  let burial_status =
    Option.value ~default:`UnknownBurial (get_burial_status_opt burial_form)
  in
  let death_status =
    Option.value ~default:`Auto (get_death_status_opt death_form)
  in
  let death = pevent_of_form_pevent death_form in
  let burial = pevent_of_form_pevent burial_form in
  let birth = pevent_of_form_pevent birth_form in
  let bapt = pevent_of_form_pevent bapt_form in
  let death_status =
    infer_death death_status burial_status burial.epers_place death
  in
  let burial_status =
    match burial_status with
    | `Burial -> Def.Buried burial.Def.epers_date
    | `Cremation -> Cremated burial.epers_date
    | `UnknownBurial -> UnknownBurial
  in
  (* Mise à jour des évènements principaux. *)
  let pevents =
    List.filter_map
      (function
        | FormDeath ((`Auto | `NotDead | `DontKnowIfDead), pe)
          when is_empty_event pe ->
            None
        | fe -> Some (pevent_of_form_pevent fe))
      pevents
  in
  let p =
    {
      Def.first_name;
      surname;
      occ;
      image;
      first_names_aliases;
      surnames_aliases;
      public_name;
      qualifiers;
      aliases;
      titles;
      rparents;
      occupation;
      related = [];
      sex;
      access;
      birth = birth.Def.epers_date;
      birth_place = birth.epers_place;
      birth_note = birth.epers_note;
      birth_src = birth.epers_src;
      baptism = bapt.epers_date;
      baptism_place = bapt.epers_place;
      baptism_note = bapt.epers_note;
      baptism_src = bapt.epers_src;
      death = death_status;
      death_place = death.epers_place;
      death_note = death.epers_note;
      death_src = death.epers_src;
      burial = burial_status;
      burial_place = burial.epers_place;
      burial_note = burial.epers_note;
      burial_src = burial.epers_src;
      pevents;
      notes;
      psources;
      key_index;
    }
  in
  (p, form_is_modified)

let reconstitute_from_pevents pevents (death_status, _, _, _)
    (_burial_status, _, _, _) =
  let death_status =
    match death_status with
    | Def.DontKnowIfDead -> `DontKnowIfDead
    | NotDead -> `NotDead
    | Death _ -> `Death
    | DeadYoung -> `DeadYoung
    | DeadDontKnowWhen -> `Death
    | OfCourseDead -> `OfCourseDead
  in
  let bi, ba, de, bu, pevents = get_main_pevents ~death_status pevents in
  (bi, ba, de, bu, pevents)

let check_person conf base p =
  let bind_none x f = match x with Some _ -> x | None -> f () in
  let ( >>= ) = bind_none in
  Update.check_missing_name base p >>= fun () ->
  Update.check_person_occurrence_number p >>= fun () ->
  Update.check_missing_witnesses_names conf
    (fun e -> e.Def.epers_witnesses)
    p.pevents
  >>= fun () -> Update.check_illegal_access_update base p

let error_person conf err =
  if not conf.Config.api_mode then (
    let title _ =
      Output.print_sstring conf (Utf8.capitalize_fst (Util.transl conf "error"))
    in
    Hutil.rheader conf title;
    Output.print_sstring conf
      (Utf8.capitalize_fst
         (Update.string_of_error conf err : Adef.safe_string :> string));
    Output.print_sstring conf "\n";
    Update.print_return conf;
    Hutil.trailer conf);
  raise @@ Update.ModErr err

let strip_pevents p =
  let strip_array_witness pl =
    let pl =
      Array.fold_right
        (fun (((f, _, _, _, _), _, _) as p) pl ->
          if f = "" then pl else p :: pl)
        pl []
    in
    Array.of_list pl
  in
  List.fold_right
    (fun e accu ->
      let has_infos, witnesses =
        match e.Def.epers_name with
        | Epers_Name s -> (s <> "", strip_array_witness e.epers_witnesses)
        | Epers_Birth | Epers_Baptism ->
            ( Date.od_of_cdate e.epers_date <> None
              || e.epers_place <> "" || e.epers_reason <> ""
              || e.epers_note <> "" || e.epers_src <> "",
              strip_array_witness e.epers_witnesses )
        | _ -> (true, strip_array_witness e.epers_witnesses)
      in
      if has_infos || Array.length witnesses > 0 then
        { e with epers_witnesses = witnesses } :: accu
      else accu)
    p.Def.pevents []

let strip_list = List.filter (fun s -> s <> "")

let strip_person p =
  {
    p with
    Def.first_names_aliases = strip_list p.first_names_aliases;
    surnames_aliases = strip_list p.surnames_aliases;
    qualifiers = strip_list p.Def.qualifiers;
    aliases = strip_list p.aliases;
    titles = List.filter (fun t -> t.Def.t_ident <> "") p.titles;
    pevents = strip_pevents p;
    rparents =
      List.filter (fun r -> r.Def.r_fath <> None || r.r_moth <> None) p.rparents;
  }

let default_prerr conf base = function
  | Update.UERR_sex_married p as err ->
      Update.prerr conf err @@ fun () ->
      Update.print_error conf err;
      Output.print_sstring conf "<ul><li>";
      Output.print_string conf (NameDisplay.referenced_person_text conf base p);
      Output.print_sstring conf "</li></ul>";
      Update.print_return conf;
      Update.print_continue conf "nsck" (Adef.encoded "on")
  | _ -> assert false

let check_sex_married ?(prerr = default_prerr) conf base sp op =
  if
    sp.Def.sex <> Gwdb.get_sex op
    && Array.exists
         (fun ifam ->
           let fam = Gwdb.foi base ifam in
           (sp.sex = Male && sp.key_index <> Gwdb.get_father fam)
           || (sp.sex = Female && sp.key_index <> Gwdb.get_mother fam))
         (Gwdb.get_family op)
  then prerr conf base (Update.UERR_sex_married op)

let rparents_of rparents =
  List.fold_left
    (fun ipl r ->
      match (r.Def.r_fath, r.r_moth) with
      | Some ip1, Some ip2 -> ip1 :: ip2 :: ipl
      | Some ip, _ -> ip :: ipl
      | _, Some ip -> ip :: ipl
      | _ -> ipl)
    [] rparents

(* TODO EVENT put this in Event *)
let pwitnesses_of pevents =
  List.fold_left
    (fun ipl e ->
      Array.fold_left
        (fun ipl (ip, _, _) -> ip :: ipl)
        ipl e.Def.epers_witnesses)
    [] pevents

let pwitnesses_of_pers_events pevents =
  List.fold_left
    (fun l e ->
      Array.fold_left (fun l (ip, _) -> ip :: l) l (Gwdb.get_pevent_witnesses e))
    [] pevents

(* sp.death *)
let effective_mod ?prerr ?skip_conflict conf base sp =
  let pi = sp.Def.key_index in
  let op = Gwdb.poi base pi in
  let ofn = Gwdb.p_first_name base op in
  let osn = Gwdb.p_surname base op in
  let oocc = Gwdb.get_occ op in
  (if ofn <> sp.first_name || osn <> sp.surname || oocc <> sp.occ then
   match Gwdb.person_of_key base sp.first_name sp.surname sp.occ with
   | Some p' when p' <> pi && Some p' <> skip_conflict ->
       Update.print_create_conflict conf base (Gwdb.poi base p') ""
   | _ -> Image.rename_portrait conf base op (sp.first_name, sp.surname, sp.occ));
  if (List.assoc_opt "nsck" conf.Config.env :> string option) <> Some "on" then
    check_sex_married ?prerr conf base sp op;
  let created_p = ref [] in
  let np =
    Futil.map_person_ps
      (Update.insert_person conf base "" created_p)
      (Gwdb.insert_string base) sp
  in
  let np = { np with related = Gwdb.get_related op } in
  let ol_rparents = rparents_of (Gwdb.get_rparents op) in
  let nl_rparents = rparents_of np.rparents in
  let ol_pevents = pwitnesses_of_pers_events (Gwdb.get_pevents op) in
  let nl_pevents = pwitnesses_of np.pevents in
  let ol = List.append ol_rparents ol_pevents in
  let nl = List.append nl_rparents nl_pevents in
  let pi = np.key_index in
  Update.update_related_pointers base pi ol nl;
  np

let effective_add conf base sp =
  (match Gwdb.person_of_key base sp.Def.first_name sp.surname sp.occ with
  | Some p' -> Update.print_create_conflict conf base (Gwdb.poi base p') ""
  | None -> ());
  let created_p = ref [] in
  (* TODO this insert an empty person with surname = empty_string ;
     looks like the only place we insert a person with empty_string *)
  (* is this just to get a iper? *)
  let pi =
    Gwdb.insert_person base
      (Gwdb.no_person Gwdb.dummy_iper)
      Gwdb.no_ascend Gwdb.no_union
  in
  let np =
    Futil.map_person_ps
      (Update.insert_person conf base "" created_p)
      (Gwdb.insert_string base) { sp with key_index = pi }
  in
  Gwdb.patch_person base pi np;
  Gwdb.patch_ascend base pi Gwdb.no_ascend;
  Gwdb.patch_union base pi Gwdb.no_union;
  (np, Gwdb.no_ascend)

let update_relations_of_related base ip old_related =
  List.iter
    (fun ip1 ->
      let p1 = Gwdb.poi base ip1 in
      let rparents, rparents_are_different =
        List.fold_right
          (fun rel (list, rad) ->
            let rfath, rad =
              match rel.Def.r_fath with
              | Some ip2 -> if ip2 = ip then (None, true) else (Some ip2, rad)
              | None -> (None, rad)
            in
            let rmoth, rad =
              match rel.r_moth with
              | Some ip2 -> if ip2 = ip then (None, true) else (Some ip2, rad)
              | None -> (None, rad)
            in
            if rfath = None && rmoth = None then (list, true)
            else
              let rel = { rel with r_fath = rfath; r_moth = rmoth } in
              (rel :: list, rad))
          (Gwdb.get_rparents p1) ([], false)
      in
      let pevents, pevents_are_different =
        let p1_pevents =
          Gwdb.get_pevents p1 |> List.map Gwdb.gen_pevent_of_pers_event
        in
        List.fold_right
          (fun e (list, rad) ->
            let witnesses, rad =
              Array.fold_right
                (fun (ip2, k, wnotes) (accu, rad) ->
                  if ip2 = ip then (accu, true)
                  else ((ip2, k, wnotes) :: accu, rad))
                e.Def.epers_witnesses ([], rad)
            in
            let e = { e with epers_witnesses = Array.of_list witnesses } in
            (e :: list, rad))
          p1_pevents ([], false)
      in
      (if rparents_are_different || pevents_are_different then
       let p = Gwdb.gen_person_of_person p1 in
       let rparents = if rparents_are_different then rparents else p.rparents in
       let pevents = if pevents_are_different then pevents else p.pevents in
       Gwdb.patch_person base ip1 { p with rparents; pevents });
      let families = Gwdb.get_family p1 in
      for i = 0 to Array.length families - 1 do
        let ifam = families.(i) in
        let fam = Gwdb.foi base ifam in
        let old_witnesses = Array.to_list (Gwdb.get_witnesses fam) in
        let new_witnesses = List.filter (( <> ) ip) old_witnesses in
        let fevents, fevents_are_different =
          let fam_events =
            Gwdb.get_fevents fam |> List.map Gwdb.gen_fevent_of_fam_event
          in
          List.fold_right
            (fun e (list, rad) ->
              let witnesses, rad =
                Array.fold_right
                  (fun (ip2, wkind, wnote) (accu, rad) ->
                    if ip2 = ip then (accu, true)
                    else ((ip2, wkind, wnote) :: accu, rad))
                  e.Def.efam_witnesses ([], rad)
              in
              let e = { e with efam_witnesses = Array.of_list witnesses } in
              (e :: list, rad))
            fam_events ([], false)
        in
        if new_witnesses <> old_witnesses || fevents_are_different then
          let fam = Gwdb.gen_family_of_family fam in
          let witnesses =
            if new_witnesses <> old_witnesses then Array.of_list new_witnesses
            else fam.witnesses
          in
          let fevents =
            if fevents_are_different then fevents else fam.fevents
          in
          Gwdb.patch_family base ifam { fam with witnesses; fevents }
      done)
    old_related

let effective_del_no_commit base op =
  update_relations_of_related base op.Def.key_index op.related;
  Update.update_related_pointers base op.key_index
    (rparents_of op.rparents @ pwitnesses_of op.pevents)
    [];
  Gwdb.delete_person base op.key_index

let effective_del_commit conf base op =
  Notes.update_notes_links_db base (Def.NLDB.PgInd op.Def.key_index) "";
  Util.commit_patches conf base;
  let changed = Def.U_Delete_person op in
  History.record conf base changed "dp"

let effective_del conf base p =
  let op = Gwdb.gen_person_of_person p in
  effective_del_no_commit base op;
  effective_del_commit conf base op

let print_title conf fmt _ =
  Output.print_sstring conf (Utf8.capitalize_fst (Util.transl conf fmt))

let print_mod_ok conf base wl pgl p ofn osn oocc =
  Hutil.header conf @@ print_title conf "person modified";
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
  (* Si on a supprimé des relations, on les mentionne *)
  (match !deleted_relation with
  | [] -> ()
  | _l ->
      Output.print_sstring conf "<p>\n";
      Output.printf conf "%s, %s %s %s :"
        (Utf8.capitalize_fst (Util.transl_nth conf "relation/relations" 0))
        (Util.transl conf "first name missing")
        (Util.transl conf "or")
        (Util.transl conf "surname missing");
      Output.print_sstring conf "<ul>\n";
      List.iter
        (fun s ->
          Output.print_sstring conf "<li>";
          Output.print_string conf (Util.escape_html s);
          Output.print_sstring conf "</li>")
        !deleted_relation;
      Output.print_sstring conf "</ul>\n";
      Output.print_sstring conf "</p>\n");
  Output.print_sstring conf "<p>";
  Output.print_string conf
    (NameDisplay.referenced_person_text conf base
       (Gwdb.poi base p.Def.key_index));
  Output.print_sstring conf "</p>";
  Update.print_warnings conf base wl;
  let pi = p.key_index in
  let np = Gwdb.poi base pi in
  let nfn = Gwdb.p_first_name base np in
  let nsn = Gwdb.p_surname base np in
  let nocc = Gwdb.get_occ np in
  if pgl <> [] && (ofn <> nfn || osn <> nsn || oocc <> nocc) then (
    Output.print_sstring conf
      {|<div class="alert alert-danger mx-auto mt-1" role="alert">|};
    Output.print_sstring conf
      (Util.transl conf "name changed. update linked pages");
    Output.print_sstring conf "</div>\n";
    let soocc = if oocc <> 0 then Printf.sprintf "/%d" oocc else "" in
    let snocc = if nocc <> 0 then Printf.sprintf "/%d" nocc else "" in
    Output.printf conf
      "<span class=\"unselectable float-left\">%s%s</span>\n\
       <span class=\"float-left ml-1\">%s/%s%s</span>\n\
       <br>"
      (Utf8.capitalize_fst (Util.transl conf "old name"))
      (Util.transl conf ":") ofn osn soocc;
    Output.printf conf
      "<span class=\"unselectable float-left\">%s%s</span>\n\
       <span class=\"float-left ml-1\">%s/%s%s</span>\n\
       <br>"
      (Utf8.capitalize_fst (Util.transl conf "new name"))
      (Util.transl conf ":") nfn nsn snocc;
    Output.printf conf "<span>%s%s</span>"
      (Utf8.capitalize_fst (Util.transl conf "linked pages"))
      (Util.transl conf ":");
    NotesDisplay.print_linked_list conf base pgl);
  Hutil.trailer conf

let relation_sex_is_coherent base warning p =
  List.iter
    (fun r ->
      (match r.Def.r_fath with
      | Some ip ->
          let p = Gwdb.poi base ip in
          if Gwdb.get_sex p <> Male then
            warning (Warning.IncoherentSex (p, 0, 0))
      | None -> ());
      match r.r_moth with
      | Some ip ->
          let p = Gwdb.poi base ip in
          if Gwdb.get_sex p <> Female then warning (IncoherentSex (p, 0, 0))
      | None -> ())
    p.Def.rparents

let all_checks_person base p a u =
  let wl = ref [] in
  let warning w =
    if not (List.exists (CheckItem.eq_warning w) !wl) then wl := w :: !wl
  in
  let pp = Gwdb.person_of_gen_person base (p, a, u) in
  ignore @@ CheckItem.person base warning pp;
  relation_sex_is_coherent base warning p;
  CheckItem.on_person_update base warning pp;
  let wl = List.sort_uniq compare !wl in
  List.iter
    (function
      | Warning.ChangedOrderOfChildren (ifam, _, _, after) ->
          Gwdb.patch_descend base ifam { children = after }
      | ChangedOrderOfPersonEvents (_, _, after) ->
          Gwdb.patch_person base p.key_index { p with pevents = after }
      | _ -> ())
    wl;
  wl

let print_add_ok conf base wl p =
  Hutil.header conf @@ print_title conf "person added";
  Hutil.print_link_to_welcome conf true;
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then (
    Output.printf conf "<h2 class=\"error\">%s</h2>\n"
      (Utf8.capitalize_fst (Util.transl conf "forbidden char"));
    List.iter (Output.printf conf "<p>%s</p>") !removed_string);
  (* Si on a supprimé des relations, on les mentionne *)
  List.iter
    (fun s ->
      Output.print_string conf (Util.escape_html s);
      Output.print_sstring conf " -> ";
      Output.print_sstring conf (Util.transl conf "forbidden char");
      Output.print_sstring conf "\n")
    !deleted_relation;
  Output.print_sstring conf "\n";
  Output.print_string conf
    (NameDisplay.referenced_person_text conf base
       (Gwdb.poi base p.Def.key_index));
  Output.print_sstring conf "\n";
  Update.print_warnings conf base wl;
  Hutil.trailer conf

let print_del_ok conf =
  Hutil.header conf @@ print_title conf "person deleted";
  Hutil.print_link_to_welcome conf false;
  Hutil.trailer conf

let print_change_event_order_ok conf base wl p =
  Hutil.header conf @@ print_title conf "person modified";
  Hutil.print_link_to_welcome conf true;
  Update.print_warnings conf base wl;
  Output.print_sstring conf "\n";
  Output.print_string conf
    (NameDisplay.referenced_person_text conf base
       (Gwdb.poi base p.Def.key_index));
  Output.print_sstring conf "\n";
  Hutil.trailer conf

let print_add o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let conf = Update.update_conf o_conf in
  let sp, form_is_modified = reconstitute_person ~base conf in
  let redisp = Option.is_some (Util.p_getenv conf.Config.env "return") in
  if form_is_modified || redisp then UpdateInd.print_update_ind conf base sp ""
  else
    let sp = strip_person sp in
    match check_person conf base sp with
    | Some err -> error_person conf err
    | None ->
        let p, a = effective_add conf base sp in
        let u = { Def.family = Gwdb.get_family (Gwdb.poi base p.key_index) } in
        let wl = all_checks_person base p a u in
        Util.commit_patches conf base;
        let changed = Def.U_Add_person p in
        History.record conf base changed "ap";
        print_add_ok conf base wl p

let print_del conf base =
  match Util.p_getenv conf.Config.env "i" with
  | Some i ->
      let ip = Gwdb.iper_of_string i in
      let p = Gwdb.poi base ip in
      effective_del conf base p;
      print_del_ok conf
  | None -> Hutil.incorrect_request conf

let print_mod_aux ?(check_person_f = check_person) conf base callback =
  let p, form_is_modified = reconstitute_person ~base conf in
  let redisp = Option.is_some (Util.p_getenv conf.Config.env "return") in
  let ini_ps = UpdateInd.string_person_of base (Gwdb.poi base p.key_index) in
  let digest = Update.digest_person ini_ps in
  if digest = Update_util.get conf "digest" then
    if form_is_modified || redisp then
      UpdateInd.print_update_ind conf base p digest
    else
      let p = strip_person p in
      match check_person_f conf base p with
      | Some err -> error_person conf err
      | None -> callback p
  else Update.error_digest conf

let print_mod ?prerr o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let o_p =
    match Util.p_getenv o_conf.Config.env "i" with
    | Some ip ->
        Gwdb.gen_person_of_person (Gwdb.poi base (Gwdb.iper_of_string ip))
    | None -> Gwdb.gen_person_of_person (Gwdb.poi base Gwdb.dummy_iper)
  in
  let ofn = Gwdb.sou base o_p.first_name in
  let osn = Gwdb.sou base o_p.surname in
  let oocc = o_p.occ in
  let key = (Name.lower ofn, Name.lower osn, oocc) in
  let conf = Update.update_conf o_conf in
  let pgl =
    let db = Gwdb.read_nldb base in
    let db = Notes.merge_possible_aliases conf db in
    Perso.links_to_ind conf base db key
  in
  let callback sp =
    let p = effective_mod ?prerr conf base sp in
    let op = Gwdb.poi base p.key_index in
    let u = { Def.family = Gwdb.get_family op } in
    Gwdb.patch_person base p.key_index p;
    let s =
      let sl =
        [
          p.notes;
          p.occupation;
          p.birth_note;
          p.birth_src;
          p.baptism_note;
          p.baptism_src;
          p.death_note;
          p.death_src;
          p.burial_note;
          p.burial_src;
          p.psources;
        ]
      in
      let sl =
        let rec loop l accu =
          match l with
          | [] -> accu
          | evt :: l -> loop l (evt.Def.epers_note :: evt.epers_src :: accu)
        in
        loop p.pevents sl
      in
      String.concat " " (List.map (Gwdb.sou base) sl)
    in
    Notes.update_notes_links_db base (Def.NLDB.PgInd p.key_index) s;
    let wl =
      let a = Gwdb.poi base p.key_index in
      let a =
        { Def.parents = Gwdb.get_parents a; consang = Gwdb.get_consang a }
      in
      all_checks_person base p a u
    in
    Util.commit_patches conf base;
    let changed = Def.U_Modify_person (o_p, p) in
    History.record conf base changed "mp";
    Update.delete_topological_sort_v conf base;
    print_mod_ok conf base wl pgl p ofn osn oocc
  in
  print_mod_aux conf base callback

let print_change_event_order conf base =
  match Util.p_getenv conf.Config.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some s ->
      let p = Gwdb.poi base (Gwdb.iper_of_string s) in
      let o_p = Gwdb.gen_person_of_person p in
      (* TODO_EVENT use Event.sorted_event *)
      let ht = Hashtbl.create 50 in
      let _ =
        List.fold_left
          (fun id evt ->
            Hashtbl.add ht id evt;
            succ id)
          1 (Gwdb.get_pevents p)
      in
      let sorted_pevents =
        List.sort
          (fun (_, pos1) (_, pos2) -> compare pos1 pos2)
          (Update_util.reconstitute_sorted_events conf 1)
      in
      let pevents =
        List.fold_right
          (fun (id, _) accu ->
            match Hashtbl.find_opt ht id with
            | Some event -> Gwdb.gen_pevent_of_pers_event event :: accu
            | None -> failwith "Sorting event")
          sorted_pevents []
      in
      let p = Gwdb.gen_person_of_person p in
      let p = { p with pevents } in
      Gwdb.patch_person base p.key_index p;
      let wl =
        let a = Gwdb.poi base p.key_index in
        let a =
          { Def.parents = Gwdb.get_parents a; consang = Gwdb.get_consang a }
        in
        let u = Gwdb.poi base p.key_index in
        let u = { Def.family = Gwdb.get_family u } in
        all_checks_person base p a u
      in
      Util.commit_patches conf base;
      let changed = Def.U_Modify_person (o_p, p) in
      History.record conf base changed "mp";
      print_change_event_order_ok conf base wl p
