(* /!\ This is mostly copy/paste of the Perso module /!\ *)
(* Sync with perso from ed7525bac *)

type fam = Gwdb.ifam * Gwdb.family * (Gwdb.iper * Gwdb.iper * Gwdb.iper) * bool
type rel = Gwdb.relation * Gwdb.person option

type env = {
  all_gp : Geneweb.Perso.generation_person list option;
  baseprefix : string option;
  desc_level_table : (int array * int array) Lazy.t option;
  desc_mark : bool array ref option;
  f_link : bool option;
  fam : fam option;
  fam_link : fam option;
  p_link : bool option;
  prev_fam : fam option;
  sosa : (Gwdb.iper * (Sosa.t * Gwdb.person) option) list ref option;
  sosa_ref : Gwdb.person option Lazy.t option;
  src : string option;
}

let conf_w_baseprefix conf env =
  match env.baseprefix with
  | Some baseprefix -> { conf with Geneweb.Config.command = baseprefix }
  | None -> conf

let empty =
  {
    all_gp = None;
    baseprefix = None;
    desc_level_table = None;
    desc_mark = None;
    fam = None;
    f_link = None;
    fam_link = None;
    p_link = None;
    prev_fam = None;
    sosa = None;
    sosa_ref = None;
    src = None;
  }

let env = empty
let get_env x = match x with Some x -> x | None -> raise Not_found

let sex_of_index = function
  | 0 -> Def.Male
  | 1 -> Def.Female
  | 2 -> Def.Neuter
  | _ -> raise (Invalid_argument "sex_of_index")

module Person = struct
  let children base p = Gwdb.children_of_p base p

  let consanguinity p =
    let c = Gwdb.get_consang p in
    if c != Adef.fix (-1) && c >= Adef.fix_of_float 0.0001 then
      Adef.float_of_fix c
    else 0.

  let dates conf base p = Geneweb.DateDisplay.short_dates_text conf base p
  let death p = Gwdb.get_death p

  (* TODOWHY: should it be Geneweb.Event.sorted_events or can it be just Geneweb.Event.events? *)
  let events = Geneweb.Event.sorted_events
  let first_name base p = Gwdb.p_first_name base p

  let history_file base p =
    let fn = Gwdb.sou base (Gwdb.get_first_name p) in
    let sn = Gwdb.sou base (Gwdb.get_surname p) in
    let occ = Gwdb.get_occ p in
    Geneweb.HistoryDiff.history_file fn sn occ

  let is_accessible_by_key conf base p =
    Geneweb.Util.accessible_by_key conf base p (Gwdb.p_first_name base p)
      (Gwdb.p_surname base p)

  let linked_page conf base p s =
    let db = Gwdb.read_nldb base in
    let db = Geneweb.Notes.merge_possible_aliases conf db in
    let key =
      let fn = Name.lower (Gwdb.sou base (Gwdb.get_first_name p)) in
      let sn = Name.lower (Gwdb.sou base (Gwdb.get_surname p)) in
      (fn, sn, Gwdb.get_occ p)
    in
    List.fold_left
      (Geneweb.Perso.linked_page_text conf base p s key)
      (Adef.safe "") db

  let note conf base p =
    if not conf.Geneweb.Config.no_note then Gwdb.sou base (Gwdb.get_notes p)
    else ""

  let related conf base p =
    List.sort (fun (c1, _) (c2, _) ->
        let mk_date c =
          match Date.od_of_cdate (Gwdb.get_baptism c) with
          | None -> Date.od_of_cdate (Gwdb.get_birth c)
          | x -> x
        in
        match (mk_date c1, mk_date c2) with
        | Some d1, Some d2 -> Date.compare_date d1 d2
        | _ -> -1)
    @@ List.fold_left
         (fun list ic ->
           let c = Geneweb.Util.pget conf base ic in
           List.fold_left
             (fun acc r ->
               match (r.Def.r_fath, r.Def.r_moth) with
               | Some ip, _ when ip = Gwdb.get_iper p -> (c, r) :: acc
               | _, Some ip when ip = Gwdb.get_iper p -> (c, r) :: acc
               | _ -> acc)
             list (Gwdb.get_rparents c))
         []
         (List.sort_uniq compare (Gwdb.get_related p))

  (* Why isnt this already unique? *)
  let relations p = List.sort_uniq compare (Gwdb.get_related p)

  let siblings base p =
    match Gwdb.get_parents p with
    | Some ifam ->
        let ip = Gwdb.get_iper p in
        Array.fold_right
          (fun i acc -> if i <> ip then i :: acc else acc)
          (Gwdb.get_children (Gwdb.foi base ifam))
          []
    | None -> []

  let half_siblings base p =
    match Gwdb.get_parents p with
    | Some ifam ->
        let ip = Gwdb.get_iper p in
        let f = Gwdb.foi base ifam in
        let filter (acc : Gwdb.iper list) i =
          if i = ifam then acc
          else
            Array.fold_right
              (fun i acc -> if i <> ip then i :: acc else acc)
              (Gwdb.get_children (Gwdb.foi base i))
              acc
        in
        let hs =
          let ifath = Gwdb.get_father f in
          if ifath = Gwdb.dummy_iper then []
          else Array.fold_left filter [] (Gwdb.get_family @@ Gwdb.poi base ifath)
        in
        let imoth = Gwdb.get_mother f in
        if imoth = Gwdb.dummy_iper then hs
        else Array.fold_left filter hs (Gwdb.get_family @@ Gwdb.poi base imoth)
    | None -> []

  let sex p = Geneweb.Util.index_of_sex (Gwdb.get_sex p)
  let surname base p = Gwdb.p_surname base p
end

module Family = struct
  let children (_, fam, _, _) = Gwdb.get_children fam

  let divorce_date (_, fam, _, auth) =
    match Gwdb.get_divorce fam with
    | Def.Divorced d when auth -> Date.od_of_cdate d
    | _ -> None

  let events (_, fam, (_, _, isp), auth) =
    if auth then
      List.fold_right
        (fun evt fam_fevents ->
          let ei = Geneweb.Event.event_item_of_fevent ~sp:(Some isp) evt in
          ei :: fam_fevents)
        (Gwdb.get_fevents fam) []
    else []

  let father (_, _, (ifath, _, _), _) = ifath
  let ifam (ifam, _, _, _) = Gwdb.string_of_ifam ifam

  let marriage_date (_, fam, (_, _, _), auth) =
    if auth then Date.od_of_cdate (Gwdb.get_marriage fam) else None

  let marriage_place (_, fam, _, _) = Gwdb.get_marriage_place fam

  let marriage_note (_, fam, _, auth) =
    if auth then Gwdb.get_marriage_note fam else Gwdb.empty_string

  let marriage_source (_, fam, _, auth) =
    if auth then Gwdb.get_marriage_src fam else Gwdb.empty_string

  let mother (_, _, (_, imoth, _), _) = imoth

  let note conf base (_, fam, _, auth) =
    if auth && not conf.Geneweb.Config.no_note then
      Gwdb.sou base (Gwdb.get_comment fam)
    else ""

  let origin_file conf base (_, fam, _, _) =
    if conf.Geneweb.Config.wizard then Gwdb.sou base (Gwdb.get_origin_file fam)
    else ""

  let spouse_iper (_, _, (_, _, ip), _) = ip
  let witnesses (_, fam, _, auth) = if auth then Gwdb.get_witnesses fam else [||]

  let sources base (_, fam, _, auth) =
    if auth then Gwdb.sou base (Gwdb.get_fsources fam) else ""
end

module Event = struct
  let name conf base ei =
    match Geneweb.Event.get_name ei with
    | Geneweb.Event.Pevent name ->
        Geneweb.Util.string_of_pevent_name conf base name
    | Geneweb.Event.Fevent name ->
        Geneweb.Util.string_of_fevent_name conf base name

  let kind ei =
    match Geneweb.Event.get_name ei with
    | Geneweb.Event.Pevent Def.Epers_Birth -> "EPERS_BIRTH"
    | Pevent Def.Epers_Baptism -> "EPERS_BAPTISM"
    | Pevent Def.Epers_Death -> "EPERS_DEATH"
    | Pevent Def.Epers_Burial -> "EPERS_BURIAL"
    | Pevent Def.Epers_Cremation -> "EPERS_CREMATION"
    | Pevent Def.Epers_Accomplishment -> "EPERS_ACCOMPLISHMENT"
    | Pevent Def.Epers_Acquisition -> "EPERS_ACQUISITION"
    | Pevent Def.Epers_Adhesion -> "EPERS_ADHESION"
    | Pevent Def.Epers_BaptismLDS -> "EPERS_BAPTISMLDS"
    | Pevent Def.Epers_BarMitzvah -> "EPERS_BARMITZVAH"
    | Pevent Def.Epers_BatMitzvah -> "EPERS_BATMITZVAH"
    | Pevent Def.Epers_Benediction -> "EPERS_BENEDICTION"
    | Pevent Def.Epers_ChangeName -> "EPERS_CHANGENAME"
    | Pevent Def.Epers_Circumcision -> "EPERS_CIRCUMCISION"
    | Pevent Def.Epers_Confirmation -> "EPERS_CONFIRMATION"
    | Pevent Def.Epers_ConfirmationLDS -> "EPERS_CONFIRMATIONLDS"
    | Pevent Def.Epers_Decoration -> "EPERS_DECORATION"
    | Pevent Def.Epers_DemobilisationMilitaire ->
        "EPERS_DEMOBILISATIONMILITAIRE"
    | Pevent Def.Epers_Diploma -> "EPERS_DIPLOMA"
    | Pevent Def.Epers_Distinction -> "EPERS_DISTINCTION"
    | Pevent Def.Epers_Dotation -> "EPERS_DOTATION"
    | Pevent Def.Epers_DotationLDS -> "EPERS_DOTATIONLDS"
    | Pevent Def.Epers_Education -> "EPERS_EDUCATION"
    | Pevent Def.Epers_Election -> "EPERS_ELECTION"
    | Pevent Def.Epers_Emigration -> "EPERS_EMIGRATION"
    | Pevent Def.Epers_Excommunication -> "EPERS_EXCOMMUNICATION"
    | Pevent Def.Epers_FamilyLinkLDS -> "EPERS_FAMILYLINKLDS"
    | Pevent Def.Epers_FirstCommunion -> "EPERS_FIRSTCOMMUNION"
    | Pevent Def.Epers_Funeral -> "EPERS_FUNERAL"
    | Pevent Def.Epers_Graduate -> "EPERS_GRADUATE"
    | Pevent Def.Epers_Hospitalisation -> "EPERS_HOSPITALISATION"
    | Pevent Def.Epers_Illness -> "EPERS_ILLNESS"
    | Pevent Def.Epers_Immigration -> "EPERS_IMMIGRATION"
    | Pevent Def.Epers_ListePassenger -> "EPERS_LISTEPASSENGER"
    | Pevent Def.Epers_MilitaryDistinction -> "EPERS_MILITARYDISTINCTION"
    | Pevent Def.Epers_MilitaryPromotion -> "EPERS_MILITARYPROMOTION"
    | Pevent Def.Epers_MilitaryService -> "EPERS_MILITARYSERVICE"
    | Pevent Def.Epers_MobilisationMilitaire -> "EPERS_MOBILISATIONMILITAIRE"
    | Pevent Def.Epers_Naturalisation -> "EPERS_NATURALISATION"
    | Pevent Def.Epers_Occupation -> "EPERS_OCCUPATION"
    | Pevent Def.Epers_Ordination -> "EPERS_ORDINATION"
    | Pevent Def.Epers_Property -> "EPERS_PROPERTY"
    | Pevent Def.Epers_Recensement -> "EPERS_RECENSEMENT"
    | Pevent Def.Epers_Residence -> "EPERS_RESIDENCE"
    | Pevent Def.Epers_Retired -> "EPERS_RETIRED"
    | Pevent Def.Epers_ScellentChildLDS -> "EPERS_SCELLENTCHILDLDS"
    | Pevent Def.Epers_ScellentParentLDS -> "EPERS_SCELLENTPARENTLDS"
    | Pevent Def.Epers_ScellentSpouseLDS -> "EPERS_SCELLENTSPOUSELDS"
    | Pevent Def.Epers_VenteBien -> "EPERS_VENTEBIEN"
    | Pevent Def.Epers_Will -> "EPERS_WILL"
    | Pevent Def.Epers_Adoption -> "EPERS_ADOPTION"
    | Fevent Def.Efam_Marriage -> "EFAM_MARRIAGE"
    | Fevent Def.Efam_NoMarriage -> "EFAM_NO_MARRIAGE"
    | Fevent Def.Efam_NoMention -> "EFAM_NO_MENTION"
    | Fevent Def.Efam_Engage -> "EFAM_ENGAGE"
    | Fevent Def.Efam_Divorce -> "EFAM_DIVORCE"
    | Fevent Def.Efam_Separated -> "EFAM_SEPARATED"
    | Fevent Def.Efam_Annulation -> "EFAM_ANNULATION"
    | Fevent Def.Efam_MarriageBann -> "EFAM_MARRIAGE_BANN"
    | Fevent Def.Efam_MarriageContract -> "EFAM_MARRIAGE_CONTRACT"
    | Fevent Def.Efam_MarriageLicense -> "EFAM_MARRIAGE_LICENSE"
    | Fevent Def.Efam_PACS -> "EFAM_PACS"
    | Fevent Def.Efam_Residence -> "EFAM_RESIDENCE"
    | Pevent (Def.Epers_Name _) -> "EPERS"
    | Fevent (Def.Efam_Name _) -> "EFAM"

  let date ei = Date.od_of_cdate (Geneweb.Event.get_date ei)
  let place = Geneweb.Event.get_place

  let note conf base e =
    let n = Geneweb.Event.get_note e in
    if conf.Geneweb.Config.no_note then "" else Gwdb.sou base n

  let src base e = Gwdb.sou base (Geneweb.Event.get_src e)

  let witnesses e : (Gwdb.iper * Def.witness_kind * Gwdb.istr) array =
    Geneweb.Event.get_witnesses_and_notes e

  let spouse_opt = Geneweb.Event.get_spouse_iper
end
