(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
open Update_util
module Driver = Geneweb_db.Driver

(* Liste des string dont on a supprimé un caractère.       *)
(* Utilisé pour le message d'erreur lors de la validation. *)
let removed_string = ref []
let get_purged_fn_sn = Update_util.get_purged_fn_sn removed_string
let reconstitute_somebody = Update_util.reconstitute_somebody removed_string

let rec reconstitute_string_list conf var ext cnt =
  match get_nth conf var cnt with
  | None -> ([], ext)
  | Some s -> (
      let s = only_printable s in
      let sl, ext = reconstitute_string_list conf var ext (cnt + 1) in
      match get_nth conf ("add_" ^ var) cnt with
      | Some "on" -> (s :: "" :: sl, true)
      | Some _ | None -> (s :: sl, ext))

let reconstitute_insert_title conf ext cnt tl =
  let var = "ins_title" ^ string_of_int cnt in
  let n =
    match (p_getenv conf.env var, p_getint conf.env (var ^ "_n")) with
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
              t_name = Tnone;
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
  else (tl, ext)

let rec reconstitute_titles conf ext cnt =
  match
    ( get_nth conf "t_ident" cnt,
      get_nth conf "t_place" cnt,
      get_nth conf "t_name" cnt )
  with
  | Some t_ident, Some t_place, Some t_name ->
      let t_name =
        match (get_nth conf "t_main_title" cnt, t_name) with
        | Some "on", _ -> Tmain
        | _, "" -> Tnone
        | _, _ -> Tname (only_printable t_name)
      in
      let t_date_start =
        Update.reconstitute_date conf ("t_date_start" ^ string_of_int cnt)
      in
      let t_date_end =
        Update.reconstitute_date conf ("t_date_end" ^ string_of_int cnt)
      in
      let t_nth =
        match get_nth conf "t_nth" cnt with
        | Some s -> ( try int_of_string s with Failure _ -> 0)
        | None -> 0
      in
      let t =
        {
          t_name;
          t_ident = only_printable t_ident;
          t_place = only_printable t_place;
          t_date_start = Date.cdate_of_od t_date_start;
          t_date_end = Date.cdate_of_od t_date_end;
          t_nth;
        }
      in
      let tl, ext = reconstitute_titles conf ext (cnt + 1) in
      let tl, ext = reconstitute_insert_title conf ext cnt tl in
      (t :: tl, ext)
  | _ -> ([], ext)

let reconstitute_insert_pevent conf ext cnt el =
  let var = "ins_event" ^ string_of_int cnt in
  let n =
    match (p_getenv conf.env var, p_getint conf.env (var ^ "_n")) with
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
              epers_name = Epers_Name "";
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
  else (el, ext)

let rec reconstitute_pevents conf ext cnt =
  match get_nth conf "e_name" cnt with
  | None -> ([], ext)
  | Some epers_name ->
      let epers_name =
        (* TODO EVENT to/of_string *)
        match epers_name with
        | "#birt" -> Epers_Birth
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
        | n -> Epers_Name (only_printable n)
      in
      let epers_date =
        Update.reconstitute_date conf ("e_date" ^ string_of_int cnt)
      in
      let epers_place =
        match get_nth conf "e_place" cnt with
        | Some place -> only_printable place
        | None -> ""
      in
      let epers_note =
        match get_nth conf "e_note" cnt with
        | Some note ->
            only_printable_or_nl (Mutil.strip_all_trailing_spaces note)
        | None -> ""
      in
      let epers_src =
        match get_nth conf "e_src" cnt with
        | Some src -> only_printable src
        | None -> ""
      in
      (* Type du témoin par défaut lors de l'insertion de nouveaux témoins. *)
      let wk =
        if epers_name = Epers_Baptism then Witness_GodParent else Witness
      in
      let witnesses, ext =
        let rec loop i ext =
          let key = "e" ^ string_of_int cnt ^ "_witn" ^ string_of_int i in
          match
            try Some (reconstitute_somebody conf key) with Failure _ -> None
          with
          | Some (fn, sn, occ, create, var) -> (
              let witnesses, ext = loop (i + 1) ext in
              let create = update_ci conf create key in
              let c = (fn, sn, occ, create, var) in
              let key_c = key ^ "_kind" in
              let c =
                match p_getenv conf.env key_c with
                | Some "godp" -> (c, Witness_GodParent)
                | Some "offi" -> (c, Witness_CivilOfficer)
                | Some "reli" -> (c, Witness_ReligiousOfficer)
                | Some "info" -> (c, Witness_Informant)
                | Some "atte" -> (c, Witness_Attending)
                | Some "ment" -> (c, Witness_Mentioned)
                | Some "othe" -> (c, Witness_Other)
                | _ -> (c, Witness)
              in
              let var_w =
                "e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i
              in
              match p_getenv conf.env var_w with
              | Some "on" -> (
                  let ins_witn_n =
                    "e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i
                    ^ "_n"
                  in
                  match p_getint conf.env ins_witn_n with
                  | Some n when n > 1 ->
                      let rec loop_witn n witnesses =
                        if n = 0 then (c :: witnesses, true)
                        else
                          let new_witn =
                            (("", "", 0, Update.Create (Neuter, None), ""), wk)
                          in
                          let witnesses = new_witn :: witnesses in
                          loop_witn (n - 1) witnesses
                      in
                      loop_witn n witnesses
                  | _ ->
                      let new_witn =
                        (("", "", 0, Update.Create (Neuter, None), ""), wk)
                      in
                      (c :: new_witn :: witnesses, true))
              | _ -> (c :: witnesses, ext))
          | None -> ([], ext)
        in
        loop 1 ext
      in
      let witnesses, ext =
        let evt_ins = "e" ^ string_of_int cnt ^ "_ins_witn0" in
        match p_getenv conf.env evt_ins with
        | Some "on" -> (
            let ins_witn_n = "e" ^ string_of_int cnt ^ "_ins_witn0_n" in
            match p_getint conf.env ins_witn_n with
            | Some n when n > 1 ->
                let rec loop_witn n witnesses =
                  if n = 0 then (witnesses, true)
                  else
                    let new_witn =
                      (("", "", 0, Update.Create (Neuter, None), ""), wk)
                    in
                    let witnesses = new_witn :: witnesses in
                    loop_witn (n - 1) witnesses
                in
                loop_witn n witnesses
            | Some _ | None ->
                let new_witn =
                  (("", "", 0, Update.Create (Neuter, None), ""), wk)
                in
                (new_witn :: witnesses, true))
        | Some _ | None -> (witnesses, ext)
      in
      let e =
        {
          epers_name;
          epers_date = Date.cdate_of_od epers_date;
          epers_place;
          epers_reason = "";
          epers_note;
          epers_src;
          epers_witnesses = Array.of_list witnesses;
        }
      in
      let el, ext = reconstitute_pevents conf ext (cnt + 1) in
      let el, ext = reconstitute_insert_pevent conf ext (cnt + 1) el in
      (e :: el, ext)

let reconstitute_add_relation conf ext cnt rl =
  match get_nth conf "add_relation" cnt with
  | Some "on" ->
      let r =
        { r_type = GodParent; r_fath = None; r_moth = None; r_sources = "" }
      in
      (r :: rl, true)
  | Some _ | None -> (rl, ext)

let deleted_relation = ref []

let reconstitute_relation_parent conf var key sex =
  match (getn conf var (key ^ "_fn"), getn conf var (key ^ "_sn")) with
  | ("", _ | _, "" | "?", _ | _, "?") as n ->
      let p = only_printable (fst n) ^ only_printable (snd n) in
      if p = "" || p = "??" then ()
      else deleted_relation := p :: !deleted_relation;
      None
  | fn, sn ->
      let fn = only_printable fn in
      let sn = only_printable sn in
      (* S'il y a des caractères interdits, on les supprime *)
      let fn, sn = get_purged_fn_sn fn sn in
      let occ =
        try int_of_string (getn conf var (key ^ "_occ")) with Failure _ -> 0
      in
      let create =
        (* why is it key ^ "_p" here *)
        match getn conf var (key ^ "_p") with
        | "create" -> Update.Create (sex, None)
        | _ -> Update.Link
      in
      let create = Update_util.update_ci conf create (var ^ "_" ^ key) in
      Some (fn, sn, occ, create, var ^ "_" ^ key)

let reconstitute_relation conf var =
  try
    let r_fath = reconstitute_relation_parent conf var "fath" Male in
    let r_moth = reconstitute_relation_parent conf var "moth" Female in
    let r_type =
      match getn conf var "type" with
      | "Adoption" -> Adoption
      | "Recognition" -> Recognition
      | "CandidateParent" -> CandidateParent
      | "GodParent" -> GodParent
      | "FosterParent" -> FosterParent
      | _s -> GodParent
    in
    Some { r_type; r_fath; r_moth; r_sources = "" }
  with Failure _ -> None

let rec reconstitute_relations conf ext cnt =
  match reconstitute_relation conf ("r" ^ string_of_int cnt) with
  | Some r ->
      let rl, ext = reconstitute_relations conf ext (cnt + 1) in
      let rl, ext = reconstitute_add_relation conf ext cnt rl in
      (r :: rl, ext)
  | None -> ([], ext)

let reconstitute_death conf birth baptism death_place burial burial_place =
  let d = Update.reconstitute_date conf "death" in
  let dr =
    match p_getenv conf.env "death_reason" with
    | Some "Killed" -> Killed
    | Some "Murdered" -> Murdered
    | Some "Executed" -> Executed
    | Some "Disappeared" -> Disappeared
    | Some "Unspecified" | None -> Unspecified
    | Some x -> failwith ("bad death reason type " ^ x)
  in
  match get conf "death" with
  | "Auto" when d = None ->
      if
        death_place <> "" || burial <> UnknownBurial || burial_place <> ""
        || dr <> Unspecified
      then DeadDontKnowWhen
      else Update.infer_death_bb conf birth baptism
  | "DeadYoung" when d = None -> DeadYoung
  | "DontKnowIfDead" when d = None -> DontKnowIfDead
  | "NotDead" -> NotDead
  | "OfCourseDead" when d = None -> OfCourseDead
  | _s -> (
      match d with
      | Some d -> Death (dr, Date.cdate_of_date d)
      | None -> DeadDontKnowWhen)

let reconstitute_burial conf burial_place =
  let d = Update.reconstitute_date conf "burial" in
  match p_getenv conf.env "burial" with
  | Some "UnknownBurial" | None -> (
      match (d, burial_place) with
      | None, "" -> UnknownBurial
      | _ -> Buried (Date.cdate_of_od d))
  | Some "Buried" -> Buried (Date.cdate_of_od d)
  | Some "Cremated" -> Cremated (Date.cdate_of_od d)
  | Some x -> failwith ("bad burial type " ^ x)

(* TODO EVENT put this in Event *)
let sort_pevents pevents =
  Event.sort_events
    (fun evt -> Event.Pevent evt.epers_name)
    (fun evt -> evt.epers_date)
    pevents

let reconstitute_from_pevents pevents ext bi bp de bu =
  (* On tri les évènements pour être sûr. *)
  let pevents = sort_pevents pevents in
  let found_birth = ref false in
  let found_baptism = ref false in
  let found_death = ref false in
  let found_burial = ref false in
  let death_reason_std_fields =
    let death_std_fields, _, _, _ = de in
    match death_std_fields with
    | Death (dr, _) -> dr
    | NotDead | DeadYoung | DeadDontKnowWhen | DontKnowIfDead | OfCourseDead ->
        Unspecified
  in
  let rec loop pevents bi bp de bu =
    match pevents with
    | [] -> (bi, bp, de, bu)
    | evt :: l -> (
        match evt.epers_name with
        | Epers_Birth ->
            if !found_birth then loop l bi bp de bu
            else
              let bi =
                (evt.epers_date, evt.epers_place, evt.epers_note, evt.epers_src)
              in
              let () = found_birth := true in
              loop l bi bp de bu
        | Epers_Baptism ->
            if !found_baptism then loop l bi bp de bu
            else
              let bp =
                (evt.epers_date, evt.epers_place, evt.epers_note, evt.epers_src)
              in
              let () = found_baptism := true in
              loop l bi bp de bu
        | Epers_Death ->
            if !found_death then loop l bi bp de bu
            else
              let death =
                match Date.od_of_cdate evt.epers_date with
                | Some _d -> Death (death_reason_std_fields, evt.epers_date)
                | None -> (
                    let death, _, _, _ = de in
                    (* On ajoute DontKnowIfDead dans le cas où tous les *)
                    (* champs sont vides.                               *)
                    match death with
                    | ( DeadYoung | DeadDontKnowWhen | OfCourseDead
                      | DontKnowIfDead ) as death ->
                        death
                    | Death _ | NotDead -> DeadDontKnowWhen)
              in
              let de =
                (death, evt.epers_place, evt.epers_note, evt.epers_src)
              in
              let () = found_death := true in
              loop l bi bp de bu
        | Epers_Burial ->
            if !found_burial then loop l bi bp de bu
            else
              let bu =
                ( Buried evt.epers_date,
                  evt.epers_place,
                  evt.epers_note,
                  evt.epers_src )
              in
              let () = found_burial := true in
              loop l bi bp de bu
        | Epers_Cremation ->
            if !found_burial then loop l bi bp de bu
            else
              let bu =
                ( Cremated evt.epers_date,
                  evt.epers_place,
                  evt.epers_note,
                  evt.epers_src )
              in
              let () = found_burial := true in
              loop l bi bp de bu
        | _ -> loop l bi bp de bu)
  in
  let bi, bp, de, bu = loop pevents bi bp de bu in
  (* Hack *)
  let pevents =
    if not !found_death then
      let remove_evt = ref false in
      List.fold_left
        (fun accu evt ->
          if not !remove_evt then
            if evt.epers_name = Epers_Name "" then (
              remove_evt := true;
              accu)
            else evt :: accu
          else evt :: accu)
        [] (List.rev pevents)
    else pevents
  in
  let pevents =
    if not !found_burial then
      let remove_evt = ref false in
      List.fold_left
        (fun accu evt ->
          if not !remove_evt then
            if evt.epers_name = Epers_Name "" then (
              remove_evt := true;
              accu)
            else evt :: accu
          else evt :: accu)
        [] (List.rev pevents)
    else pevents
  in
  let pevents =
    if not ext then
      let remove_evt = ref false in
      List.fold_left
        (fun accu evt ->
          if not !remove_evt then
            if evt.epers_name = Epers_Name "" then (
              remove_evt := true;
              accu)
            else evt :: accu
          else evt :: accu)
        [] (List.rev pevents)
    else pevents
  in
  (* Il faut gérer le cas où l'on supprime délibérément l'évènement. *)
  let bi = if not !found_birth then (Date.cdate_None, "", "", "") else bi in
  let bp = if not !found_baptism then (Date.cdate_None, "", "", "") else bp in
  let de =
    if not !found_death then
      if !found_burial then (DeadDontKnowWhen, "", "", "")
      else
        let death, _, _, _ = de in
        match death with
        | NotDead -> (NotDead, "", "", "")
        | DeadYoung | DeadDontKnowWhen | OfCourseDead | DontKnowIfDead | Death _
          ->
            (DontKnowIfDead, "", "", "")
    else de
  in
  let bu = if not !found_burial then (UnknownBurial, "", "", "") else bu in
  (bi, bp, de, bu, pevents)

let reconstitute_person conf =
  let ext = false in
  let key_index =
    match p_getenv conf.env "i" with
    | Some s -> (
        try Driver.Iper.of_string (String.trim s)
        with Failure _ -> Driver.Iper.dummy)
    | None -> Driver.Iper.dummy
  in
  let first_name = only_printable (get conf "first_name") in
  let surname = only_printable (get conf "surname") in
  (* S'il y a des caractères interdits, on les supprime *)
  let first_name, surname = get_purged_fn_sn first_name surname in
  let occ =
    try int_of_string (String.trim (get conf "occ")) with Failure _ -> 0
  in
  let image = only_printable (get conf "image") in
  let first_names_aliases, ext =
    reconstitute_string_list conf "first_name_alias" ext 0
  in
  let surnames_aliases, ext =
    reconstitute_string_list conf "surname_alias" ext 0
  in
  let public_name = only_printable (get conf "public_name") in
  let qualifiers, ext = reconstitute_string_list conf "qualifier" ext 0 in
  let aliases, ext = reconstitute_string_list conf "alias" ext 0 in
  let titles, ext = reconstitute_titles conf ext 1 in
  let titles, ext = reconstitute_insert_title conf ext 0 titles in
  let rparents, ext = reconstitute_relations conf ext 1 in
  let rparents, ext = reconstitute_add_relation conf ext 0 rparents in
  let access =
    match p_getenv conf.env "access" with
    | Some "Public" -> Public
    | Some "SemiPublic" -> SemiPublic
    | Some "Private" -> Private
    | Some _ | None -> IfTitles
  in
  let occupation = only_printable (get conf "occu") in
  let sex =
    match p_getenv conf.env "sex" with
    | Some "M" -> Male
    | Some "F" -> Female
    | Some _ | None -> Neuter
  in
  let birth = Update.reconstitute_date conf "birth" in
  let birth_place = only_printable (get conf "birth_place") in
  let birth_note =
    only_printable_or_nl
      (Mutil.strip_all_trailing_spaces (get conf "birth_note"))
  in
  let birth_src = only_printable (get conf "birth_src") in
  let bapt = Update.reconstitute_date conf "bapt" in
  let bapt_place = only_printable (get conf "bapt_place") in
  let bapt_note =
    only_printable_or_nl
      (Mutil.strip_all_trailing_spaces (get conf "bapt_note"))
  in
  let bapt_src = only_printable (get conf "bapt_src") in
  let burial_place = only_printable (get conf "burial_place") in
  let burial_note =
    only_printable_or_nl
      (Mutil.strip_all_trailing_spaces (get conf "burial_note"))
  in
  let burial_src = only_printable (get conf "burial_src") in
  let burial = reconstitute_burial conf burial_place in
  let death_place = only_printable (get conf "death_place") in
  let death_note =
    only_printable_or_nl
      (Mutil.strip_all_trailing_spaces (get conf "death_note"))
  in
  let death_src = only_printable (get conf "death_src") in
  let death =
    reconstitute_death conf birth bapt death_place burial burial_place
  in
  let death_place =
    match death with
    | Death _ | DeadYoung | DeadDontKnowWhen -> death_place
    | NotDead | DontKnowIfDead | OfCourseDead -> ""
  in
  let death =
    match death with
    | NotDead | DontKnowIfDead -> (
        match burial with
        | Buried _ | Cremated _ -> DeadDontKnowWhen
        | UnknownBurial -> death)
    | Death _ | DeadYoung | DeadDontKnowWhen | OfCourseDead -> death
  in
  let pevents, ext = reconstitute_pevents conf ext 1 in
  let pevents, ext = reconstitute_insert_pevent conf ext 0 pevents in
  let notes =
    if first_name = "?" || surname = "?" then ""
    else
      only_printable_or_nl (Mutil.strip_all_trailing_spaces (get conf "notes"))
  in
  let psources = only_printable (get conf "src") in
  (* Mise à jour des évènements principaux. *)
  let bi, bp, de, bu, pevents =
    reconstitute_from_pevents pevents ext
      (Date.cdate_of_od birth, birth_place, birth_note, birth_src)
      (Date.cdate_of_od bapt, bapt_place, bapt_note, bapt_src)
      (death, death_place, death_note, death_src)
      (burial, burial_place, burial_note, burial_src)
  in
  let birth, birth_place, birth_note, birth_src = bi in
  let bapt, bapt_place, bapt_note, bapt_src = bp in
  let death, death_place, death_note, death_src = de in
  let burial, burial_place, burial_note, burial_src = bu in
  (* Maintenant qu'on a propagé les evèenements, on a *)
  (* peut-être besoin de refaire un infer_death.      *)
  let death =
    match death with
    | DontKnowIfDead ->
        (* FIXME: do not use _bb version *)
        Update.infer_death_bb conf (Date.od_of_cdate birth)
          (Date.od_of_cdate bapt)
    | NotDead | Death _ | DeadYoung | DeadDontKnowWhen | OfCourseDead -> death
  in
  let p =
    {
      first_name;
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
      birth;
      birth_place;
      birth_note;
      birth_src;
      baptism = bapt;
      baptism_place = bapt_place;
      baptism_note = bapt_note;
      baptism_src = bapt_src;
      death;
      death_place;
      death_note;
      death_src;
      burial;
      burial_place;
      burial_note;
      burial_src;
      pevents;
      notes;
      psources;
      key_index;
    }
  in
  (p, ext)

let check_person conf base p =
  match Update.check_missing_name base p with
  | Some _ as err -> err
  | None ->
      Update.check_missing_witnesses_names conf
        (fun e -> e.epers_witnesses)
        p.pevents

let error_person conf err =
  if not conf.api_mode then (
    let title _ =
      Output.print_sstring conf (Utf8.capitalize_fst (transl conf "error"))
    in
    Hutil.rheader conf title;
    Output.print_sstring conf
      (Utf8.capitalize_fst
         (Update.string_of_error conf err : Adef.safe_string :> string));
    Output.print_sstring conf "\n";
    Update.print_return conf;
    Hutil.trailer conf);
  raise @@ Update.ModErr err

(* TODO EVENT put this in Event *)
let strip_pevents p =
  let strip_array_witness pl =
    let pl =
      Array.fold_right
        (fun (((f, _, _, _, _), _) as p) pl -> if f = "" then pl else p :: pl)
        pl []
    in
    Array.of_list pl
  in
  List.fold_right
    (fun e accu ->
      let has_infos, witnesses =
        match e.epers_name with
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
    p.pevents []

let strip_list = List.filter (fun s -> s <> "")

let strip_person p =
  {
    p with
    first_names_aliases = strip_list p.first_names_aliases;
    surnames_aliases = strip_list p.surnames_aliases;
    qualifiers = strip_list p.qualifiers;
    aliases = strip_list p.aliases;
    titles = List.filter (fun t -> t.t_ident <> "") p.titles;
    pevents = strip_pevents p;
    rparents =
      List.filter (fun r -> r.r_fath <> None || r.r_moth <> None) p.rparents;
  }

let default_prerr conf base = function
  | Update.UERR_sex_married p as err ->
      Update.prerr conf err @@ fun () ->
      Update.print_error conf err;
      Output.print_sstring conf "<ul><li>";
      Output.print_string conf (Util.referenced_person_text conf base p);
      Output.print_sstring conf "</li></ul>";
      Update.print_return conf;
      Update.print_continue conf "nsck" (Adef.encoded "on")
  | _ -> assert false

let check_sex_married ?(prerr = default_prerr) conf base sp op =
  if
    sp.sex <> Driver.get_sex op
    && Array.exists
         (fun ifam ->
           let fam = Driver.foi base ifam in
           (sp.sex = Male && sp.key_index <> Driver.get_father fam)
           || (sp.sex = Female && sp.key_index <> Driver.get_mother fam))
         (Driver.get_family op)
  then prerr conf base (Update.UERR_sex_married op)

let rparents_of rparents =
  List.fold_left
    (fun ipl r ->
      match (r.r_fath, r.r_moth) with
      | Some ip1, Some ip2 -> ip1 :: ip2 :: ipl
      | Some ip, _ -> ip :: ipl
      | _, Some ip -> ip :: ipl
      | _ -> ipl)
    [] rparents

(* TODO EVENT put this in Event *)
let pwitnesses_of pevents =
  List.fold_left
    (fun ipl e ->
      Array.fold_left (fun ipl (ip, _) -> ip :: ipl) ipl e.epers_witnesses)
    [] pevents

(* sp.death *)
let effective_mod ?prerr ?skip_conflict conf base sp =
  let pi = sp.key_index in
  let op = Driver.poi base pi in
  let ofn = Driver.p_first_name base op in
  let osn = Driver.p_surname base op in
  let oocc = Driver.get_occ op in
  (if
     (not (String.equal ofn sp.first_name && String.equal osn sp.surname))
     || oocc <> sp.occ
   then
     match
       Geneweb_db.Driver.person_of_key base sp.first_name sp.surname sp.occ
     with
     | Some p' when p' <> pi && Some p' <> skip_conflict ->
         Update.print_create_conflict conf base (Driver.poi base p') ""
     | _ ->
         Image.rename_portrait_and_blason conf base op
           (sp.first_name, sp.surname, sp.occ));
  if (List.assoc_opt "nsck" conf.env :> string option) <> Some "on" then
    check_sex_married ?prerr conf base sp op;
  let created_p = ref [] in
  let np =
    Futil.map_person_ps
      (Update.insert_person conf base "" created_p)
      (Driver.insert_string base)
      sp
  in
  let np = { np with related = Driver.get_related op } in
  let ol_rparents = rparents_of (Driver.get_rparents op) in
  let nl_rparents = rparents_of np.rparents in
  let ol_pevents = pwitnesses_of (Driver.get_pevents op) in
  let nl_pevents = pwitnesses_of np.pevents in
  let ol = List.append ol_rparents ol_pevents in
  let nl = List.append nl_rparents nl_pevents in
  let pi = np.key_index in
  Update.update_related_pointers base pi ol nl;
  np

let effective_add conf base sp =
  (match
     Geneweb_db.Driver.person_of_key base sp.first_name sp.surname sp.occ
   with
  | Some p' -> Update.print_create_conflict conf base (Driver.poi base p') ""
  | None -> ());
  let created_p = ref [] in
  let pi =
    Driver.insert_person_with_union_and_ascendants base
      (Driver.no_person Driver.Iper.dummy)
      Driver.no_ascend Driver.no_union
  in
  let np =
    Futil.map_person_ps
      (Update.insert_person conf base "" created_p)
      (Driver.insert_string base)
      { sp with key_index = pi }
  in
  Driver.patch_person base pi np;
  Driver.patch_ascend base pi Driver.no_ascend;
  Driver.patch_union base pi Driver.no_union;
  (np, Driver.no_ascend)

let update_relations_of_related base ip old_related =
  List.iter
    (fun ip1 ->
      let p1 = Driver.poi base ip1 in
      let rparents, rparents_are_different =
        List.fold_right
          (fun rel (list, rad) ->
            let rfath, rad =
              match rel.r_fath with
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
          (Driver.get_rparents p1) ([], false)
      in
      let pevents, pevents_are_different =
        List.fold_right
          (fun e (list, rad) ->
            let witnesses, rad =
              Array.fold_right
                (fun (ip2, k) (accu, rad) ->
                  if ip2 = ip then (accu, true) else ((ip2, k) :: accu, rad))
                e.epers_witnesses ([], rad)
            in
            let e = { e with epers_witnesses = Array.of_list witnesses } in
            (e :: list, rad))
          (Driver.get_pevents p1) ([], false)
      in
      (if rparents_are_different || pevents_are_different then
         let p = Driver.gen_person_of_person p1 in
         let rparents =
           if rparents_are_different then rparents else p.rparents
         in
         let pevents = if pevents_are_different then pevents else p.pevents in
         Driver.patch_person base ip1 { p with rparents; pevents });
      let families = Driver.get_family p1 in
      for i = 0 to Array.length families - 1 do
        let ifam = families.(i) in
        let fam = Driver.foi base ifam in
        let old_witnesses = Array.to_list (Driver.get_witnesses fam) in
        let new_witnesses = List.filter (( <> ) ip) old_witnesses in
        let fevents, fevents_are_different =
          List.fold_right
            (fun e (list, rad) ->
              let witnesses, rad =
                Array.fold_right
                  (fun (ip2, k) (accu, rad) ->
                    if ip2 = ip then (accu, true) else ((ip2, k) :: accu, rad))
                  e.efam_witnesses ([], rad)
              in
              let e = { e with efam_witnesses = Array.of_list witnesses } in
              (e :: list, rad))
            (Driver.get_fevents fam) ([], false)
        in
        if new_witnesses <> old_witnesses || fevents_are_different then
          let fam = Driver.gen_family_of_family fam in
          let witnesses =
            if new_witnesses <> old_witnesses then Array.of_list new_witnesses
            else fam.witnesses
          in
          let fevents =
            if fevents_are_different then fevents else fam.fevents
          in
          Driver.patch_family base ifam { fam with witnesses; fevents }
      done)
    old_related

let effective_del_no_commit base op =
  update_relations_of_related base op.key_index op.related;
  Update.update_related_pointers base op.key_index
    (rparents_of op.rparents @ pwitnesses_of op.pevents)
    [];
  Driver.delete_person base op.key_index

let effective_del_commit conf base op =
  Notes.update_notes_links_db base (Def.NLDB.PgInd op.key_index) "";
  let key =
    Util.make_key base
      (Driver.gen_person_of_person (Driver.poi base op.key_index))
  in
  Notes.update_cache_linked_pages conf Notes.Delete key key 0;
  Util.commit_patches conf base;
  let changed = U_Delete_person op in
  History.record conf base changed "dp"

let effective_del conf base p =
  let op = Util.string_gen_person base (Driver.gen_person_of_person p) in
  effective_del_no_commit base op;
  effective_del_commit conf base op

let print_title conf fmt _ =
  Output.print_sstring conf (Utf8.capitalize_fst (transl conf fmt))

let print_mod_ok conf base wl pgl p ofn osn oocc =
  Hutil.header conf @@ print_title conf "person modified";
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then (
    Output.print_sstring conf "<h3 class=\"error\">";
    Output.printf conf
      (fcapitale (ftransl conf "%s forbidden char"))
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
        (Utf8.capitalize_fst (transl_nth conf "relation/relations" 0))
        (transl conf "first name missing")
        (transl conf "or")
        (transl conf "surname missing");
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
    (referenced_person_text conf base (Driver.poi base p.key_index));
  Output.print_sstring conf "</p>";
  Update.print_warnings conf base wl;
  let pi = p.key_index in
  let np = Driver.poi base pi in
  let nfn = Driver.p_first_name base np in
  let nsn = Driver.p_surname base np in
  let nocc = Driver.get_occ np in
  if
    pgl <> []
    && ((not (String.equal ofn nfn && String.equal osn nsn)) || oocc <> nocc)
  then (
    Output.print_sstring conf
      {|<div class="alert alert-danger mx-auto mt-1" role="alert">|};
    Output.print_sstring conf (transl conf "name changed. updated linked pages");
    Output.print_sstring conf "</div>\n";
    let soocc = if oocc <> 0 then Printf.sprintf "/%d" oocc else "" in
    let snocc = if nocc <> 0 then Printf.sprintf "/%d" nocc else "" in
    Output.printf conf
      "<span class=\"unselectable float-left\">%s%s</span>\n\
       <span class=\"float-left ml-1\">%s/%s%s</span>\n\
       <br>"
      (Utf8.capitalize_fst (transl conf "old name"))
      (transl conf ":") ofn osn soocc;
    Output.printf conf
      "<span class=\"unselectable float-left\">%s%s</span>\n\
       <span class=\"float-left ml-1\">%s/%s%s</span>\n\
       <br>"
      (Utf8.capitalize_fst (transl conf "new name"))
      (transl conf ":") nfn nsn snocc;
    Output.printf conf "<span>%s%s</span>"
      (Utf8.capitalize_fst (transl conf "linked pages"))
      (transl conf ":");
    NotesDisplay.print_linked_list conf base pgl);
  Hutil.trailer conf

let relation_sex_is_coherent base warning p =
  List.iter
    (fun r ->
      (match r.r_fath with
      | Some ip ->
          let p = Driver.poi base ip in
          if Driver.get_sex p <> Male then warning (IncoherentSex (p, 0, 0))
      | None -> ());
      match r.r_moth with
      | Some ip ->
          let p = Driver.poi base ip in
          if Driver.get_sex p <> Female then warning (IncoherentSex (p, 0, 0))
      | None -> ())
    p.rparents

let all_checks_person base p a u =
  let wl = ref [] in
  let warning w =
    if not (List.exists (CheckItem.eq_warning base w) !wl) then wl := w :: !wl
  in
  let pp = Driver.person_of_gen_person base (p, a, u) in
  ignore @@ CheckItem.person base warning pp;
  relation_sex_is_coherent base warning p;
  CheckItem.on_person_update base warning pp;
  let wl = List.sort_uniq compare !wl in
  List.iter
    (function
      | ChangedOrderOfChildren (ifam, _, _, after) ->
          Driver.patch_descend base ifam { children = after }
      | ChangedOrderOfPersonEvents (_, _, after) ->
          Driver.patch_person base p.key_index { p with pevents = after }
      | _ -> ())
    wl;
  wl

let print_add_ok conf base wl p =
  Hutil.header conf @@ print_title conf "person added";
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then (
    Output.printf conf "<h2 class=\"error\">%s</h2>\n"
      (Utf8.capitalize_fst (transl conf "forbidden char"));
    List.iter (Output.printf conf "<p>%s</p>") !removed_string);
  (* Si on a supprimé des relations, on les mentionne *)
  List.iter
    (fun s ->
      Output.print_string conf (Util.escape_html s);
      Output.print_sstring conf " -> ";
      Output.print_sstring conf (transl conf "forbidden char");
      Output.print_sstring conf "\n")
    !deleted_relation;
  Output.print_sstring conf "\n";
  Output.print_string conf
    (referenced_person_text conf base (Driver.poi base p.key_index));
  Output.print_sstring conf "\n";
  Update.print_warnings conf base wl;
  Hutil.trailer conf

let print_del_ok conf =
  Hutil.header conf @@ print_title conf "person deleted";
  Hutil.trailer conf

let print_change_event_order_ok conf base wl p =
  Hutil.header conf @@ print_title conf "person modified";
  Update.print_warnings conf base wl;
  Output.print_sstring conf "\n";
  Output.print_string conf
    (referenced_person_text conf base (Driver.poi base p.key_index));
  Output.print_sstring conf "\n";
  Hutil.trailer conf

let print_add o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let conf = Update.update_conf o_conf in
  let sp, ext = reconstitute_person conf in
  let redisp = Option.is_some (p_getenv conf.env "return") in
  if ext || redisp then UpdateInd.print_update_ind conf base sp ""
  else
    let sp = strip_person sp in
    match check_person conf base sp with
    | Some err -> error_person conf err
    | None ->
        let p, a = effective_add conf base sp in
        let u = { family = Driver.get_family (Driver.poi base p.key_index) } in
        let wl = all_checks_person base p a u in
        Util.commit_patches conf base;
        let changed = U_Add_person (Util.string_gen_person base p) in
        History.record conf base changed "ap";
        print_add_ok conf base wl p

let print_del conf base =
  match p_getenv conf.env "i" with
  | Some i ->
      let ip = Driver.Iper.of_string i in
      let p = Driver.poi base ip in
      effective_del conf base p;
      print_del_ok conf
  | None -> Hutil.incorrect_request conf

let print_mod_aux conf base callback =
  let p, ext = reconstitute_person conf in
  let redisp = Option.is_some (p_getenv conf.env "return") in
  let ini_ps = UpdateInd.string_person_of base (Driver.poi base p.key_index) in
  let salt = Option.get conf.secret_salt in
  let digest = Update.digest_person ~salt ini_ps in
  if digest = get conf "digest" then
    if ext || redisp then UpdateInd.print_update_ind conf base p digest
    else
      let p = strip_person p in
      match check_person conf base p with
      | Some err -> error_person conf err
      | None -> callback p
  else Update.error_digest conf

let print_mod ?prerr o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let o_p =
    match p_getenv o_conf.env "i" with
    | Some ip ->
        Util.string_gen_person base
          (Driver.gen_person_of_person
             (Driver.poi base (Driver.Iper.of_string ip)))
    | None ->
        Util.string_gen_person base
          (Driver.gen_person_of_person (Driver.poi base Driver.Iper.dummy))
  in
  let ofn = o_p.first_name in
  let osn = o_p.surname in
  let oocc = o_p.occ in
  let old_key =
    Util.make_key base
      (Driver.gen_person_of_person (Driver.poi base o_p.key_index))
  in
  let conf = Update.update_conf o_conf in
  let pgl =
    let db = Driver.read_nldb base in
    let db = Notes.merge_possible_aliases conf db in
    Notes.links_to_ind conf base db old_key None
  in
  let callback sp =
    let p = effective_mod ?prerr conf base sp in
    let op = Driver.poi base p.key_index in
    let u = { family = Driver.get_family op } in
    Driver.patch_person base p.key_index p;
    let new_key = Util.make_key base p in
    if old_key <> new_key then (
      (* Needs the updates in this order in case of self-reference *)
      Notes.update_notes_links_person base p;
      Notes.update_ind_key conf base pgl old_key new_key;
      Notes.update_cache_linked_pages conf Notes.Rename old_key new_key 0);
    let wl =
      let a = Driver.poi base p.key_index in
      let a =
        { parents = Driver.get_parents a; consang = Driver.get_consang a }
      in
      all_checks_person base p a u
    in
    Util.commit_patches conf base;
    let changed = U_Modify_person (o_p, Util.string_gen_person base p) in
    History.record conf base changed "mp";
    Update.delete_topological_sort_v conf base;
    print_mod_ok conf base wl pgl p ofn osn oocc
  in
  print_mod_aux conf base callback

let print_change_event_order conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some s ->
      let p = Driver.poi base (Driver.Iper.of_string s) in
      let o_p = Util.string_gen_person base (Driver.gen_person_of_person p) in
      (* TODO_EVENT use Event.sorted_event *)
      let ht = Hashtbl.create 50 in
      let _ =
        List.fold_left
          (fun id evt ->
            Hashtbl.add ht id evt;
            succ id)
          1 (Driver.get_pevents p)
      in
      let sorted_pevents =
        List.sort
          (fun (_, pos1) (_, pos2) -> compare pos1 pos2)
          (reconstitute_sorted_events conf 1)
      in
      let pevents =
        List.fold_right
          (fun (id, _) accu ->
            try Hashtbl.find ht id :: accu
            with Not_found -> failwith "Sorting event")
          sorted_pevents []
      in
      let p = Driver.gen_person_of_person p in
      let p = { p with pevents } in
      Driver.patch_person base p.key_index p;
      let wl =
        let a = Driver.poi base p.key_index in
        let a =
          { parents = Driver.get_parents a; consang = Driver.get_consang a }
        in
        let u = Driver.poi base p.key_index in
        let u = { family = Driver.get_family u } in
        all_checks_person base p a u
      in
      Util.commit_patches conf base;
      let changed = U_Modify_person (o_p, Util.string_gen_person base p) in
      History.record conf base changed "mp";
      print_change_event_order_ok conf base wl p
