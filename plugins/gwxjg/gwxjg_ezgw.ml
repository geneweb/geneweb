(* /!\ This is mostly copy/paste of the Perso module /!\ *)
(* Sync with perso from ed7525bac *)
open Geneweb
open Config
open Def
open Util
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver

type fam =
  Driver.ifam * Driver.family * (Driver.iper * Driver.iper * Driver.iper) * bool

type rel = Driver.relation * Driver.person option

type env = {
  all_gp : Perso.generation_person list option;
  baseprefix : string option;
  desc_level_table : (int array * int array) Lazy.t option;
  desc_mark : bool array ref option;
  fam : fam option;
  fam_link : fam option;
  prev_fam : fam option;
  sosa : (Driver.iper * (Sosa.t * Driver.person) option) list ref option;
  sosa_ref : Driver.person option Lazy.t option;
  src : string option;
}

let conf_w_baseprefix conf env =
  match env.baseprefix with
  | Some baseprefix -> { conf with command = baseprefix }
  | None -> conf

let empty =
  {
    all_gp = None;
    baseprefix = None;
    desc_level_table = None;
    desc_mark = None;
    fam = None;
    fam_link = None;
    prev_fam = None;
    sosa = None;
    sosa_ref = None;
    src = None;
  }

let env = empty
let get_env x = match x with Some x -> x | None -> raise Not_found

let sex_of_index = function
  | 0 -> Male
  | 1 -> Female
  | 2 -> Neuter
  | _ -> raise (Invalid_argument "sex_of_index")

module Person = struct
  let children base p = Driver.children_of_p base p

  let consanguinity p =
    let c = Driver.get_consang p in
    if c != Adef.fix (-1) && c >= Adef.fix_of_float 0.0001 then
      Adef.float_of_fix c
    else 0.

  let dates conf base p = DateDisplay.short_dates_text conf base p
  let death p = Driver.get_death p

  (* TODOWHY: should it be Event.sorted_events or can it be just Event.events? *)
  let events = Event.sorted_events
  let first_name base p = Driver.p_first_name base p

  let history_file base p =
    let fn = Driver.sou base (Driver.get_first_name p) in
    let sn = Driver.sou base (Driver.get_surname p) in
    let occ = Driver.get_occ p in
    HistoryDiff.history_file fn sn occ

  let is_accessible_by_key conf base p =
    Util.accessible_by_key conf base p
      (Driver.p_first_name base p)
      (Driver.p_surname base p)

  let linked_page conf base p s =
    let db = Driver.read_nldb base in
    let db = Notes.merge_possible_aliases conf db in
    let key =
      let fn = Name.lower (Driver.sou base (Driver.get_first_name p)) in
      let sn = Name.lower (Driver.sou base (Driver.get_surname p)) in
      (fn, sn, Driver.get_occ p)
    in
    List.fold_left (Perso.linked_page_text conf base p s key) (Adef.safe "") db

  let note conf base p =
    if not conf.no_note then Driver.sou base (Driver.get_notes p) else ""

  let related conf base p =
    List.sort (fun (c1, _) (c2, _) ->
        let mk_date c =
          match Date.od_of_cdate (Driver.get_baptism c) with
          | None -> Date.od_of_cdate (Driver.get_birth c)
          | x -> x
        in
        match (mk_date c1, mk_date c2) with
        | Some d1, Some d2 -> Date.compare_date d1 d2
        | _ -> -1)
    @@ List.fold_left
         (fun list ic ->
           let c = pget conf base ic in
           List.fold_left
             (fun acc r ->
               match (r.r_fath, r.r_moth) with
               | Some ip, _ when ip = Driver.get_iper p -> (c, r) :: acc
               | _, Some ip when ip = Driver.get_iper p -> (c, r) :: acc
               | _ -> acc)
             list (Driver.get_rparents c))
         []
         (List.sort_uniq compare (Driver.get_related p))

  (* Why isnt this already unique? *)
  let relations p = List.sort_uniq compare (Driver.get_related p)

  let siblings base p =
    match Driver.get_parents p with
    | Some ifam ->
        let ip = Driver.get_iper p in
        Array.fold_right
          (fun i acc -> if i <> ip then i :: acc else acc)
          (Driver.get_children (Driver.foi base ifam))
          []
    | None -> []

  let half_siblings base p =
    match Driver.get_parents p with
    | Some ifam ->
        let ip = Driver.get_iper p in
        let f = Driver.foi base ifam in
        let filter (acc : Driver.iper list) i =
          if i = ifam then acc
          else
            Array.fold_right
              (fun i acc -> if i <> ip then i :: acc else acc)
              (Driver.get_children (Driver.foi base i))
              acc
        in
        let hs =
          let ifath = Driver.get_father f in
          if ifath = Driver.Iper.dummy then []
          else
            Array.fold_left filter []
              (Driver.get_family @@ Driver.poi base ifath)
        in
        let imoth = Driver.get_mother f in
        if imoth = Driver.Iper.dummy then hs
        else
          Array.fold_left filter hs (Driver.get_family @@ Driver.poi base imoth)
    | None -> []

  let sex p = index_of_sex (Driver.get_sex p)
  let surname base p = Driver.p_surname base p
end

module Family = struct
  let children (_, fam, _, _) = Driver.get_children fam

  let divorce_date (_, fam, _, auth) =
    match Driver.get_divorce fam with
    | Divorced d when auth -> Date.od_of_cdate d
    | _ -> None

  let events (_, fam, (_, _, isp), auth) =
    if auth then
      List.fold_right
        (fun evt fam_fevents ->
          let name = Event.Fevent evt.efam_name in
          let date = evt.efam_date in
          let place = evt.efam_place in
          let note = evt.efam_note in
          let src = evt.efam_src in
          let wl = evt.efam_witnesses in
          let x = (name, date, place, note, src, wl, Some isp) in
          x :: fam_fevents)
        (Driver.get_fevents fam) []
    else []

  let father (_, _, (ifath, _, _), _) = ifath
  let ifam (ifam, _, _, _) = Driver.Ifam.to_string ifam

  let marriage_date (_, fam, (_, _, _), auth) =
    if auth then Date.od_of_cdate (Driver.get_marriage fam) else None

  let marriage_place (_, fam, _, _) = Driver.get_marriage_place fam

  let marriage_note (_, fam, _, auth) =
    if auth then Driver.get_marriage_note fam else Driver.Istr.empty

  let marriage_source (_, fam, _, auth) =
    if auth then Driver.get_marriage_src fam else Driver.Istr.empty

  let mother (_, _, (_, imoth, _), _) = imoth

  let note conf base (_, fam, _, auth) =
    if auth && not conf.no_note then Driver.sou base (Driver.get_comment fam)
    else ""

  let origin_file conf base (_, fam, _, _) =
    if conf.wizard then Driver.sou base (Driver.get_origin_file fam) else ""

  let spouse_iper (_, _, (_, _, ip), _) = ip

  let witnesses (_, fam, _, auth) =
    if auth then Driver.get_witnesses fam else [||]

  let sources base (_, fam, _, auth) =
    if auth then Driver.sou base (Driver.get_fsources fam) else ""
end

module Event = struct
  let name conf base (n, _, _, _, _, _, _) =
    match n with
    | Event.Pevent name -> Util.string_of_pevent_name conf base name
    | Event.Fevent name -> Util.string_of_fevent_name conf base name

  let kind (n, _, _, _, _, _, _) =
    match n with
    | Event.Pevent Epers_Birth -> "EPERS_BIRTH"
    | Pevent Epers_Baptism -> "EPERS_BAPTISM"
    | Pevent Epers_Death -> "EPERS_DEATH"
    | Pevent Epers_Burial -> "EPERS_BURIAL"
    | Pevent Epers_Cremation -> "EPERS_CREMATION"
    | Pevent Epers_Accomplishment -> "EPERS_ACCOMPLISHMENT"
    | Pevent Epers_Acquisition -> "EPERS_ACQUISITION"
    | Pevent Epers_Adhesion -> "EPERS_ADHESION"
    | Pevent Epers_BaptismLDS -> "EPERS_BAPTISMLDS"
    | Pevent Epers_BarMitzvah -> "EPERS_BARMITZVAH"
    | Pevent Epers_BatMitzvah -> "EPERS_BATMITZVAH"
    | Pevent Epers_Benediction -> "EPERS_BENEDICTION"
    | Pevent Epers_ChangeName -> "EPERS_CHANGENAME"
    | Pevent Epers_Circumcision -> "EPERS_CIRCUMCISION"
    | Pevent Epers_Confirmation -> "EPERS_CONFIRMATION"
    | Pevent Epers_ConfirmationLDS -> "EPERS_CONFIRMATIONLDS"
    | Pevent Epers_Decoration -> "EPERS_DECORATION"
    | Pevent Epers_DemobilisationMilitaire -> "EPERS_DEMOBILISATIONMILITAIRE"
    | Pevent Epers_Diploma -> "EPERS_DIPLOMA"
    | Pevent Epers_Distinction -> "EPERS_DISTINCTION"
    | Pevent Epers_Dotation -> "EPERS_DOTATION"
    | Pevent Epers_DotationLDS -> "EPERS_DOTATIONLDS"
    | Pevent Epers_Education -> "EPERS_EDUCATION"
    | Pevent Epers_Election -> "EPERS_ELECTION"
    | Pevent Epers_Emigration -> "EPERS_EMIGRATION"
    | Pevent Epers_Excommunication -> "EPERS_EXCOMMUNICATION"
    | Pevent Epers_FamilyLinkLDS -> "EPERS_FAMILYLINKLDS"
    | Pevent Epers_FirstCommunion -> "EPERS_FIRSTCOMMUNION"
    | Pevent Epers_Funeral -> "EPERS_FUNERAL"
    | Pevent Epers_Graduate -> "EPERS_GRADUATE"
    | Pevent Epers_Hospitalisation -> "EPERS_HOSPITALISATION"
    | Pevent Epers_Illness -> "EPERS_ILLNESS"
    | Pevent Epers_Immigration -> "EPERS_IMMIGRATION"
    | Pevent Epers_ListePassenger -> "EPERS_LISTEPASSENGER"
    | Pevent Epers_MilitaryDistinction -> "EPERS_MILITARYDISTINCTION"
    | Pevent Epers_MilitaryPromotion -> "EPERS_MILITARYPROMOTION"
    | Pevent Epers_MilitaryService -> "EPERS_MILITARYSERVICE"
    | Pevent Epers_MobilisationMilitaire -> "EPERS_MOBILISATIONMILITAIRE"
    | Pevent Epers_Naturalisation -> "EPERS_NATURALISATION"
    | Pevent Epers_Occupation -> "EPERS_OCCUPATION"
    | Pevent Epers_Ordination -> "EPERS_ORDINATION"
    | Pevent Epers_Property -> "EPERS_PROPERTY"
    | Pevent Epers_Recensement -> "EPERS_RECENSEMENT"
    | Pevent Epers_Residence -> "EPERS_RESIDENCE"
    | Pevent Epers_Retired -> "EPERS_RETIRED"
    | Pevent Epers_ScellentChildLDS -> "EPERS_SCELLENTCHILDLDS"
    | Pevent Epers_ScellentParentLDS -> "EPERS_SCELLENTPARENTLDS"
    | Pevent Epers_ScellentSpouseLDS -> "EPERS_SCELLENTSPOUSELDS"
    | Pevent Epers_VenteBien -> "EPERS_VENTEBIEN"
    | Pevent Epers_Will -> "EPERS_WILL"
    | Fevent Efam_Marriage -> "EFAM_MARRIAGE"
    | Fevent Efam_NoMarriage -> "EFAM_NO_MARRIAGE"
    | Fevent Efam_NoMention -> "EFAM_NO_MENTION"
    | Fevent Efam_Engage -> "EFAM_ENGAGE"
    | Fevent Efam_Divorce -> "EFAM_DIVORCE"
    | Fevent Efam_Separated -> "EFAM_SEPARATED"
    | Fevent Efam_Annulation -> "EFAM_ANNULATION"
    | Fevent Efam_MarriageBann -> "EFAM_MARRIAGE_BANN"
    | Fevent Efam_MarriageContract -> "EFAM_MARRIAGE_CONTRACT"
    | Fevent Efam_MarriageLicense -> "EFAM_MARRIAGE_LICENSE"
    | Fevent Efam_PACS -> "EFAM_PACS"
    | Fevent Efam_Residence -> "EFAM_RESIDENCE"
    | Pevent (Epers_Name _) -> "EPERS"
    | Fevent (Efam_Name _) -> "EFAM"

  let date (_, d, _, _, _, _, _) = Date.od_of_cdate d
  let place base (_, _, p, _, _, _, _) = Driver.sou base p

  let note conf base (_, _, _, n, _, _, _) =
    if conf.no_note then "" else Driver.sou base n

  let src base (_, _, _, _, s, _, _) = Driver.sou base s
  let witnesses (_, _, _, _, _, w, _) = w
  let spouse_opt (_, _, _, _, _, _, isp) = isp
end
