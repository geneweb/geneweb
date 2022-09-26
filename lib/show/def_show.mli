type date = Adef.date = Dgreg of dmy * calendar | Dtext of string
and calendar = Adef.calendar = Dgregorian | Djulian | Dfrench | Dhebrew
and dmy =
  Adef.dmy = {
  day : int;
  month : int;
  year : int;
  prec : precision;
  delta : int;
}
and dmy2 =
  Adef.dmy2 = {
  day2 : int;
  month2 : int;
  year2 : int;
  delta2 : int;
}
and precision =
  Adef.precision =
    Sure
  | About
  | Maybe
  | Before
  | After
  | OrYear of dmy2
  | YearInt of dmy2

(** Printer for [date] *)
val pp_date : Format.formatter -> date -> unit

(** Convert [date] to string. *)
val show_date : date -> string

(** Printer for [calendar] *)
val pp_calendar :
  Format.formatter ->
  calendar -> unit

(** Convert [calendar] to string *)
val show_calendar : calendar -> string

(** Printer for [dmy] *)
val pp_dmy :
  Format.formatter -> dmy -> unit

(** Convert [dmy] to string *)
val show_dmy : dmy -> string

(** Printer for [dmy2] *)
val pp_dmy2 :
  Format.formatter -> dmy2 -> unit

(** Convert [dmy2] to string *)
val show_dmy2 : dmy2 -> string

(** Printer for [precision] *)
val pp_precision :
  Format.formatter ->
  precision -> unit

(** Convert [precision] to string *)
val show_precision : precision -> string

type cdate = Adef.cdate

(** Printer for [cdate] *)
val pp_cdate :
  Format.formatter ->
  Adef.cdate -> unit

(** Convert [cdate] to string *)
val show_cdate : Adef.cdate -> string

type relation_kind =
  Def.relation_kind =
    Married
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

(** Printer for [relation_kind] *)
val pp_relation_kind :
  Format.formatter ->
  relation_kind -> unit

(** Convert [relation_kind] to string *)
val show_relation_kind : relation_kind -> string

type divorce = Def.divorce = NotDivorced | Divorced of cdate | Separated

(** Printer for [divorce] *)
val pp_divorce :
  Format.formatter ->
  divorce -> unit

(** Convert [divorce] to string *)
val show_divorce : divorce -> string

type death_reason =
  Def.death_reason =
    Killed
  | Murdered
  | Executed
  | Disappeared
  | Unspecified

(** Printer for [death_reason] *)
val pp_death_reason :
  Format.formatter ->
  death_reason -> unit

(** Convert [death_reason] to string *)
val show_death_reason : death_reason -> string

type death =
  Def.death =
    NotDead
  | Death of death_reason * cdate
  | DeadYoung
  | DeadDontKnowWhen
  | DontKnowIfDead
  | OfCourseDead

(** Printer for [death] *)
val pp_death :
  Format.formatter -> death -> unit

(** Convert [death] to string *)
val show_death : death -> string

type burial =
  Def.burial =
    UnknownBurial
  | Buried of cdate
  | Cremated of cdate

(** Printer for [burial] *)
val pp_burial :
  Format.formatter ->
  burial -> unit

(** Convert [burial] to string *)
val show_burial : burial -> string

type access = Def.access = IfTitles | Public | Private

(** Printer for [access] *)
val pp_access :
  Format.formatter ->
  access -> unit

(** Convert [access] to string *)
val show_access : access -> string

type 'string gen_title_name =
  'string Def.gen_title_name =
    Tmain
  | Tname of 'string
  | Tnone

(** Printer for [gen_title_name] *)
val pp_gen_title_name :
  (Format.formatter ->
   'string -> unit) ->
  Format.formatter ->
  'string gen_title_name -> unit

(** Convert [gen_title_name] to string *)
val show_gen_title_name :
  (Format.formatter ->
   'string -> unit) ->
  'string gen_title_name -> string

type 'string gen_title =
  'string Def.gen_title = {
  t_name : 'string gen_title_name;
  t_ident : 'string;
  t_place : 'string;
  t_date_start : cdate;
  t_date_end : cdate;
  t_nth : int;
}

(** Printer for [gen_title] *)
val pp_gen_title :
  (Format.formatter ->
   'string -> unit) ->
  Format.formatter ->
  'string gen_title -> unit

(** Convert [gen_title] to string *)
val show_gen_title :
  (Format.formatter ->
   'string -> unit) ->
  'string gen_title -> string

type witness_kind =
  Def.witness_kind =
    Witness
  | Witness_GodParent
  | Witness_CivilOfficer
  | Witness_ReligiousOfficer
  | Witness_Informant
  | Witness_Attending
  | Witness_Mentioned
  | Witness_Other

(** Printer for [witness_kind] *)
val pp_witness_kind :
  Format.formatter ->
  witness_kind -> unit

(** Convert [witness_kind] to string *)
val show_witness_kind : witness_kind -> string

type 'string gen_pers_event_name =
  'string Def.gen_pers_event_name =
    Epers_Birth
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

(** Printer for [gen_pers_event_name] *)
val pp_gen_pers_event_name :
  (Format.formatter ->
   'string -> unit) ->
  Format.formatter ->
  'string gen_pers_event_name -> unit

(** Convert [gen_pers_event_name] to string *)
val show_gen_pers_event_name :
  (Format.formatter ->
   'string -> unit) ->
  'string gen_pers_event_name -> string

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

(** Printer for [gen_pers_event] *)
val pp_gen_pers_event :
  (Format.formatter ->
   'person -> unit) ->
  (Format.formatter ->
   'string -> unit) ->
  Format.formatter ->
  ('person, 'string) gen_pers_event -> unit

(** Convert [gen_pers_event] to string *)
val show_gen_pers_event :
  (Format.formatter ->
   'person -> unit) ->
  (Format.formatter ->
   'string -> unit) ->
  ('person, 'string) gen_pers_event -> string

type 'string gen_fam_event_name =
  'string Def.gen_fam_event_name =
    Efam_Marriage
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

(** Printer for [gen_fam_event_name] *)
val pp_gen_fam_event_name :
  (Format.formatter ->
   'string -> unit) ->
  Format.formatter ->
  'string gen_fam_event_name -> unit

(** Convert [gen_fam_event_name] to string *)
val show_gen_fam_event_name :
  (Format.formatter ->
   'string -> unit) ->
  'string gen_fam_event_name -> string

type ('person, 'string) gen_fam_event =
  ('person, 'string) Def.gen_fam_event = {
  efam_name : 'string gen_fam_event_name;
  efam_date : cdate;
  efam_place : 'string;
  efam_reason : 'string;
  efam_note : 'string;
  efam_src : 'string;
  efam_witnesses : ('person * witness_kind) array;
}

(** Printer for [gen_fam_event] *)
val pp_gen_fam_event :
  (Format.formatter ->
   'person -> unit) ->
  (Format.formatter ->
   'string -> unit) ->
  Format.formatter ->
  ('person, 'string) gen_fam_event -> unit

(** Convert [gen_fam_event] to string *)
val show_gen_fam_event :
  (Format.formatter ->
   'person -> unit) ->
  (Format.formatter ->
   'string -> unit) ->
  ('person, 'string) gen_fam_event -> string

type relation_type =
  Def.relation_type =
    Adoption
  | Recognition
  | CandidateParent
  | GodParent
  | FosterParent

(** Printer for [relation_type] *)
val pp_relation_type :
  Format.formatter ->
  relation_type -> unit

(** Convert [relation_type] to string *)
val show_relation_type : relation_type -> string

type ('person, 'string) gen_relation =
  ('person, 'string) Def.gen_relation = {
  r_type : relation_type;
  r_fath : 'person option;
  r_moth : 'person option;
  r_sources : 'string;
}

(** Printer for [gen_relation] *)
val pp_gen_relation :
  (Format.formatter ->
   'person -> unit) ->
  (Format.formatter ->
   'string -> unit) ->
  Format.formatter ->
  ('person, 'string) gen_relation -> unit

(** Convert [gen_relation] to string *)
val show_gen_relation :
  (Format.formatter ->
   'person -> unit) ->
  (Format.formatter ->
   'string -> unit) ->
  ('person, 'string) gen_relation -> string

type sex = Def.sex = Male | Female | Neuter

(** Printer for [sex] *)
val pp_sex :
  Format.formatter -> sex -> unit

(** Convert [sex] to string *)
val show_sex : sex -> string

type place =
  Def.place = {
  other : string;
  town : string;
  township : string;
  canton : string;
  district : string;
  county : string;
  region : string;
  country : string;
}

(** Printer for [place] *)
val pp_place :
  Format.formatter -> place -> unit

(** Convert [place] to string *)
val show_place : place -> string

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
  rparents : ('person, 'string) gen_relation list;
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

(** Printer for [gen_person] *)
val pp_gen_person :
  (Format.formatter ->
   'iper -> unit) ->
  (Format.formatter ->
   'person -> unit) ->
  (Format.formatter ->
   'string -> unit) ->
  Format.formatter ->
  ('iper, 'person, 'string) gen_person -> unit

(** Convert [gen_person] to string *)
val show_gen_person :
  (Format.formatter ->
   'iper -> unit) ->
  (Format.formatter ->
   'person -> unit) ->
  (Format.formatter ->
   'string -> unit) ->
  ('iper, 'person, 'string) gen_person -> string

type fix = Adef.fix

(** Printer for [fix] *)
val pp_fix : Format.formatter -> Adef.fix -> unit

(** Convert [fix] to string *)
val show_fix : Adef.fix -> string

type 'family gen_ascend =
  'family Def.gen_ascend = {
  parents : 'family option;
  consang : fix;
}

(** Printer for [gen_ascend] *)
val pp_gen_ascend :
  (Format.formatter ->
   'family -> unit) ->
  Format.formatter ->
  'family gen_ascend -> unit

(** Convert [gen_ascend] to string *)
val show_gen_ascend :
  (Format.formatter ->
   'family -> unit) ->
  'family gen_ascend -> string

type 'family gen_union = 'family Def.gen_union = { family : 'family array; }

(** Printer for [gen_union] *)
val pp_gen_union :
  (Format.formatter ->
   'family -> unit) ->
  Format.formatter ->
  'family gen_union -> unit

(** Convert [gen_union] to string *)
val show_gen_union :
  (Format.formatter ->
   'family -> unit) ->
  'family gen_union -> string

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
  origin_file : 'string;
  fsources : 'string;
  fam_index : 'ifam;
}

(** Printer for [gen_family] *)
val pp_gen_family :
  (Format.formatter ->
   'person -> unit) ->
  (Format.formatter ->
   'ifam -> unit) ->
  (Format.formatter ->
   'string -> unit) ->
  Format.formatter ->
  ('person, 'ifam, 'string) gen_family -> unit

(** Convert [gen_family] to string *)
val show_gen_family :
  (Format.formatter ->
   'person -> unit) ->
  (Format.formatter ->
   'ifam -> unit) ->
  (Format.formatter ->
   'string -> unit) ->
  ('person, 'ifam, 'string) gen_family -> string

type 'person gen_couple = 'person Adef.gen_couple

(** Printer for [gen_couple] *)
val pp_gen_couple :
  (Format.formatter ->
   'person -> unit) ->
  Format.formatter ->
  'person gen_couple -> unit

(** Convert [gen_couple] to string *)
val show_gen_couple :
  (Format.formatter ->
   'person -> unit) ->
  'person gen_couple -> string

type 'person gen_descend =
  'person Def.gen_descend = {
  children : 'person array;
}

(** Printer for [gen_descend] *)
val pp_gen_descend :
  (Format.formatter ->
   'person -> unit) ->
  Format.formatter ->
  'person gen_descend -> unit

(** Convert [gen_descend] to string *)
val show_gen_descend :
  (Format.formatter ->
   'person -> unit) ->
  'person gen_descend -> string

