open Def

type form = At | Of
type 'a key = Key of string | Custom of 'a

let pevent_key (name : _ gen_pers_event_name) =
  match name with
  | Epers_Birth -> Key "birth"
  | Epers_Baptism -> Key "baptism"
  | Epers_Death -> Key "death"
  | Epers_Burial -> Key "burial"
  | Epers_Cremation -> Key "cremation"
  | Epers_Accomplishment -> Key "accomplishment"
  | Epers_Acquisition -> Key "acquisition"
  | Epers_Adhesion -> Key "adhesion"
  | Epers_BaptismLDS -> Key "baptismLDS"
  | Epers_BarMitzvah -> Key "bar mitzvah"
  | Epers_BatMitzvah -> Key "bat mitzvah"
  | Epers_Benediction -> Key "benediction"
  | Epers_ChangeName -> Key "change name"
  | Epers_Circumcision -> Key "circumcision"
  | Epers_Confirmation -> Key "confirmation"
  | Epers_ConfirmationLDS -> Key "confirmation LDS"
  | Epers_Decoration -> Key "decoration"
  | Epers_DemobilisationMilitaire -> Key "demobilisationMilitaire"
  | Epers_Diploma -> Key "diploma"
  | Epers_Distinction -> Key "distinction"
  | Epers_Dotation -> Key "dotation"
  | Epers_DotationLDS -> Key "dotationLDS"
  | Epers_Education -> Key "education"
  | Epers_Election -> Key "election"
  | Epers_Emigration -> Key "emigration"
  | Epers_Excommunication -> Key "excommunication"
  | Epers_FamilyLinkLDS -> Key "familyLinkLDS"
  | Epers_FirstCommunion -> Key "firstCommunion"
  | Epers_Funeral -> Key "funeral"
  | Epers_Graduate -> Key "graduate"
  | Epers_Hospitalisation -> Key "hospitalisation"
  | Epers_Illness -> Key "illness"
  | Epers_Immigration -> Key "immigration"
  | Epers_ListePassenger -> Key "listePassenger"
  | Epers_MilitaryDistinction -> Key "militaryDistinction"
  | Epers_MilitaryPromotion -> Key "militaryPromotion"
  | Epers_MilitaryService -> Key "militaryService"
  | Epers_MobilisationMilitaire -> Key "mobilisationMilitaire"
  | Epers_Naturalisation -> Key "naturalisation"
  | Epers_Occupation -> Key "occupation/occupations"
  | Epers_Ordination -> Key "ordination"
  | Epers_Property -> Key "property"
  | Epers_Recensement -> Key "recensement"
  | Epers_Residence -> Key "residence"
  | Epers_Retired -> Key "retired"
  | Epers_ScellentChildLDS -> Key "scellentChildLDS"
  | Epers_ScellentParentLDS -> Key "scellentParentLDS"
  | Epers_ScellentSpouseLDS -> Key "scellentSpouseLDS"
  | Epers_VenteBien -> Key "venteBien"
  | Epers_Will -> Key "will"
  | Epers_Name n -> Custom n

let fevent_key (name : _ gen_fam_event_name) =
  match name with
  | Efam_Marriage -> Key "marriage event"
  | Efam_NoMarriage -> Key "no marriage event"
  | Efam_NoMention -> Key "no mention"
  | Efam_Engage -> Key "engage event"
  | Efam_Divorce -> Key "divorce event"
  | Efam_Separated -> Key "separate event"
  | Efam_Annulation -> Key "annulation"
  | Efam_MarriageBann -> Key "marriage bann"
  | Efam_MarriageContract -> Key "marriage contract"
  | Efam_MarriageLicense -> Key "marriage licence"
  | Efam_PACS -> Key "PACS"
  | Efam_Residence -> Key "residence"
  | Efam_Name n -> Custom n

let form_key form key =
  let key =
    match String.index_opt key '/' with
    | Some i -> String.sub key 0 i
    | None -> key
  in
  match form with At -> key ^ " (at)" | Of -> key ^ " (of)"

let find conf form = function
  | Key k -> Hashtbl.find_opt conf.Config.lexicon (form_key form k)
  | Custom _ -> None

let pevent conf form name = find conf form (pevent_key name)
let fevent conf form name = find conf form (fevent_key name)
