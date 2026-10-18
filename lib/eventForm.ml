open Def

type form = At | Of

let pevent_key (name : _ gen_pers_event_name) =
  match name with
  | Epers_Birth -> Some "birth"
  | Epers_Baptism -> Some "baptism"
  | Epers_Death -> Some "death"
  | Epers_Burial -> Some "burial"
  | Epers_Cremation -> Some "cremation"
  | Epers_Accomplishment -> Some "accomplishment"
  | Epers_Acquisition -> Some "acquisition"
  | Epers_Adhesion -> Some "adhesion"
  | Epers_BaptismLDS -> Some "baptismLDS"
  | Epers_BarMitzvah -> Some "bar mitzvah"
  | Epers_BatMitzvah -> Some "bat mitzvah"
  | Epers_Benediction -> Some "benediction"
  | Epers_ChangeName -> Some "change name"
  | Epers_Circumcision -> Some "circumcision"
  | Epers_Confirmation -> Some "confirmation"
  | Epers_ConfirmationLDS -> Some "confirmation LDS"
  | Epers_Decoration -> Some "decoration"
  | Epers_DemobilisationMilitaire -> Some "demobilisationMilitaire"
  | Epers_Diploma -> Some "diploma"
  | Epers_Distinction -> Some "distinction"
  | Epers_Dotation -> Some "dotation"
  | Epers_DotationLDS -> Some "dotationLDS"
  | Epers_Education -> Some "education"
  | Epers_Election -> Some "election"
  | Epers_Emigration -> Some "emigration"
  | Epers_Excommunication -> Some "excommunication"
  | Epers_FamilyLinkLDS -> Some "familyLinkLDS"
  | Epers_FirstCommunion -> Some "firstCommunion"
  | Epers_Funeral -> Some "funeral"
  | Epers_Graduate -> Some "graduate"
  | Epers_Hospitalisation -> Some "hospitalisation"
  | Epers_Illness -> Some "illness"
  | Epers_Immigration -> Some "immigration"
  | Epers_ListePassenger -> Some "listePassenger"
  | Epers_MilitaryDistinction -> Some "militaryDistinction"
  | Epers_MilitaryPromotion -> Some "militaryPromotion"
  | Epers_MilitaryService -> Some "militaryService"
  | Epers_MobilisationMilitaire -> Some "mobilisationMilitaire"
  | Epers_Naturalisation -> Some "naturalisation"
  | Epers_Occupation -> Some "occupation/occupations"
  | Epers_Ordination -> Some "ordination"
  | Epers_Property -> Some "property"
  | Epers_Recensement -> Some "recensement"
  | Epers_Residence -> Some "residence"
  | Epers_Retired -> Some "retired"
  | Epers_ScellentChildLDS -> Some "scellentChildLDS"
  | Epers_ScellentParentLDS -> Some "scellentParentLDS"
  | Epers_ScellentSpouseLDS -> Some "scellentSpouseLDS"
  | Epers_VenteBien -> Some "venteBien"
  | Epers_Will -> Some "will"
  | Epers_Name _ -> None

let fevent_key (name : _ gen_fam_event_name) =
  match name with
  | Efam_Marriage -> Some "marriage event"
  | Efam_NoMarriage -> Some "no marriage event"
  | Efam_NoMention -> Some "no mention"
  | Efam_Engage -> Some "engage event"
  | Efam_Divorce -> Some "divorce event"
  | Efam_Separated -> Some "separate event"
  | Efam_Annulation -> Some "annulation"
  | Efam_MarriageBann -> Some "marriage bann"
  | Efam_MarriageContract -> Some "marriage contract"
  | Efam_MarriageLicense -> Some "marriage licence"
  | Efam_PACS -> Some "PACS"
  | Efam_Residence -> Some "residence"
  | Efam_Name _ -> None

let form_key form key =
  let key =
    match String.index_opt key '/' with
    | Some i -> String.sub key 0 i
    | None -> key
  in
  match form with At -> key ^ " (at)" | Of -> key ^ " (of)"

let find conf form key =
  Option.bind key (fun k ->
      Hashtbl.find_opt conf.Config.lexicon (form_key form k))

let pevent conf form name = find conf form (pevent_key name)
let fevent conf form name = find conf form (fevent_key name)
