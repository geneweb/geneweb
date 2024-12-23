type cdate = Adef.cdate

let pp_cdate fmt x =
  match Geneweb_util.Date.od_of_cdate x with
  | Some d -> Geneweb_date_show.Date_show.pp_date fmt d
  | None -> Format.fprintf fmt "None"

let show_cdate x =
  match Geneweb_util.Date.od_of_cdate x with
  | Some d -> Geneweb_date_show.Date_show.show_date d
  | None -> "None"

type relation_kind = Def.relation_kind =
  | Married
  | NotMarried
  | Engaged
  | NoSexesCheckNotMarried
  | NoMention
  | NoSexesCheckMarried
  | MarriageBann
  | MarriageContract
  | MarriageLicense
  | Pacs
  | Residence
[@@deriving show]

type divorce = Def.divorce = NotDivorced | Divorced of cdate | Separated
[@@deriving show]

type death_reason = Def.death_reason =
  | Killed
  | Murdered
  | Executed
  | Disappeared
  | Unspecified
[@@deriving show]

type death = Def.death =
  | NotDead
  | Death of death_reason * cdate
  | DeadYoung
  | DeadDontKnowWhen
  | DontKnowIfDead
  | OfCourseDead
[@@deriving show]

type burial = Def.burial = UnknownBurial | Buried of cdate | Cremated of cdate
[@@deriving show]

type access = Def.access = IfTitles | Public | Private [@@deriving show]

type 'string gen_title_name = 'string Def.gen_title_name =
  | Tmain
  | Tname of 'string
  | Tnone
[@@deriving show]

type 'string gen_title = 'string Def.gen_title = {
  t_name : 'string gen_title_name;
  t_ident : 'string;
  t_place : 'string;
  t_date_start : cdate;
  t_date_end : cdate;
  t_nth : int;
}
[@@deriving show]

type witness_kind = Def.witness_kind =
  | Witness
  | Witness_GodParent
  | Witness_CivilOfficer
  | Witness_ReligiousOfficer
  | Witness_Informant
  | Witness_Attending
  | Witness_Mentioned
  | Witness_Other
[@@deriving show]

type 'string gen_pers_event_name = 'string Def.gen_pers_event_name =
  | Epers_Birth
  | Epers_Baptism
  | Epers_Death
  | Epers_Burial
  | Epers_Cremation
  | Epers_Accomplishment
  | Epers_Acquisition
  | Epers_Adhesion
  | Epers_BaptismLDS
  | Epers_BarMitzvah
  | Epers_BatMitzvah
  | Epers_Benediction
  | Epers_ChangeName
  | Epers_Circumcision
  | Epers_Confirmation
  | Epers_ConfirmationLDS
  | Epers_Decoration
  | Epers_DemobilisationMilitaire
  | Epers_Diploma
  | Epers_Distinction
  | Epers_Dotation
  | Epers_DotationLDS
  | Epers_Education
  | Epers_Election
  | Epers_Emigration
  | Epers_Excommunication
  | Epers_FamilyLinkLDS
  | Epers_FirstCommunion
  | Epers_Funeral
  | Epers_Graduate
  | Epers_Hospitalisation
  | Epers_Illness
  | Epers_Immigration
  | Epers_ListePassenger
  | Epers_MilitaryDistinction
  | Epers_MilitaryPromotion
  | Epers_MilitaryService
  | Epers_MobilisationMilitaire
  | Epers_Naturalisation
  | Epers_Occupation
  | Epers_Ordination
  | Epers_Property
  | Epers_Recensement
  | Epers_Residence
  | Epers_Retired
  | Epers_ScellentChildLDS
  | Epers_ScellentParentLDS
  | Epers_ScellentSpouseLDS
  | Epers_VenteBien
  | Epers_Will
  | Epers_Name of 'string
[@@deriving show]

type ('person, 'string) gen_pers_event =
      ('person, 'string) Def.gen_pers_event = {
  epers_name : 'string gen_pers_event_name;
  epers_date : cdate;
  epers_place : 'string;
  epers_reason : 'string;
  epers_note : 'string;
  epers_src : 'string;
  epers_witnesses : ('person * witness_kind * 'string) array;
}
[@@deriving show]

type 'string gen_fam_event_name = 'string Def.gen_fam_event_name =
  | Efam_Marriage
  | Efam_NoMarriage
  | Efam_NoMention
  | Efam_Engage
  | Efam_Divorce
  | Efam_Separated
  | Efam_Annulation
  | Efam_MarriageBann
  | Efam_MarriageContract
  | Efam_MarriageLicense
  | Efam_PACS
  | Efam_Residence
  | Efam_Name of 'string
[@@deriving show]

type ('person, 'string) gen_fam_event = ('person, 'string) Def.gen_fam_event = {
  efam_name : 'string gen_fam_event_name;
  efam_date : cdate;
  efam_place : 'string;
  efam_reason : 'string;
  efam_note : 'string;
  efam_src : 'string;
  efam_witnesses : ('person * witness_kind * 'string) array;
}
[@@deriving show]

type relation_type = Def.relation_type =
  | Adoption
  | Recognition
  | CandidateParent
  | GodParent
  | FosterParent
[@@deriving show]

type ('person, 'string) gen_relation = ('person, 'string) Def.gen_relation = {
  r_type : relation_type;
  r_fath : 'person option;
  r_moth : 'person option;
  r_sources : 'string;
}
[@@deriving show]

type sex = Def.sex = Male | Female | Neuter [@@deriving show]

type ('iper, 'person, 'string) gen_person =
      ('iper, 'person, 'string) Def.gen_person = {
  first_name : 'string;
  surname : 'string;
  occ : int;
  image : 'string;
  public_name : 'string;
  qualifiers : 'string list;
  aliases : 'string list;
  first_names_aliases : 'string list;
  surnames_aliases : 'string list;
  titles : 'string gen_title list;
  (* relations with not native parents *)
  rparents : ('person, 'string) gen_relation list;
  (* related persons like (father of witnessed family,
     concerned person of witnessed event, adopted child, etc.) *)
  related : 'person list;
  occupation : 'string;
  sex : sex;
  access : access;
  birth : cdate;
  birth_place : 'string;
  birth_note : 'string;
  birth_src : 'string;
  baptism : cdate;
  baptism_place : 'string;
  baptism_note : 'string;
  baptism_src : 'string;
  death : death;
  death_place : 'string;
  death_note : 'string;
  death_src : 'string;
  burial : burial;
  burial_place : 'string;
  burial_note : 'string;
  burial_src : 'string;
  pevents : ('person, 'string) gen_pers_event list;
  notes : 'string;
  psources : 'string;
  key_index : 'iper;
}
[@@deriving show]

type fix = Adef.fix

let pp_fix fmt x = Format.fprintf fmt "%d" @@ Adef.fix_repr x
let show_fix x = string_of_int @@ Adef.fix_repr x

type 'family gen_ascend = 'family Def.gen_ascend = {
  parents : 'family option;
  consang : fix;
}
[@@deriving show { with_path = false }]

type 'family gen_union = 'family Def.gen_union = { family : 'family array }
[@@deriving show]

type ('person, 'ifam, 'string) gen_family =
      ('person, 'ifam, 'string) Def.gen_family = {
  marriage : cdate;
  marriage_place : 'string;
  marriage_note : 'string;
  marriage_src : 'string;
  witnesses : 'person array;
  relation : relation_kind;
  divorce : divorce;
  fevents : ('person, 'string) gen_fam_event list;
  comment : 'string;
  origin_file : 'string; (* .gw filename where family is defined *)
  fsources : 'string;
  fam_index : 'ifam;
}
[@@deriving show]

type 'person gen_couple =
  ('person Adef.gen_couple
  [@polyprinter
    fun pp fmt x ->
      fprintf fmt "[ %a ; %a ]" pp (Adef.father x) pp (Adef.mother x)])
[@@deriving show { with_path = false }]

type 'person gen_descend = 'person Def.gen_descend = {
  children : 'person array;
}
[@@deriving show]
