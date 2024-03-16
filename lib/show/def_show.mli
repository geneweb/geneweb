type date = Adef.date = Dgreg of dmy * calendar | Dtext of string
and calendar = Adef.calendar = Dgregorian | Djulian | Dfrench | Dhebrew

and dmy = Adef.dmy = {
  day : int;
  month : int;
  year : int;
  prec : precision;
  delta : int;
}

and dmy2 = Adef.dmy2 = { day2 : int; month2 : int; year2 : int; delta2 : int }

and precision = Adef.precision =
  | Sure
  | About
  | Maybe
  | Before
  | After
  | OrYear of dmy2
  | YearInt of dmy2

val pp_date : Format.formatter -> date -> unit
(** Printer for [date] *)

val show_date : date -> string
(** Convert [date] to string. *)

val pp_calendar : Format.formatter -> calendar -> unit
(** Printer for [calendar] *)

val show_calendar : calendar -> string
(** Convert [calendar] to string *)

val pp_dmy : Format.formatter -> dmy -> unit
(** Printer for [dmy] *)

val show_dmy : dmy -> string
(** Convert [dmy] to string *)

val pp_dmy2 : Format.formatter -> dmy2 -> unit
(** Printer for [dmy2] *)

val show_dmy2 : dmy2 -> string
(** Convert [dmy2] to string *)

val pp_precision : Format.formatter -> precision -> unit
(** Printer for [precision] *)

val show_precision : precision -> string
(** Convert [precision] to string *)

type cdate = Adef.cdate

val pp_cdate : Format.formatter -> Adef.cdate -> unit
(** Printer for [cdate] *)

val show_cdate : Adef.cdate -> string
(** Convert [cdate] to string *)

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

val pp_relation_kind : Format.formatter -> relation_kind -> unit
(** Printer for [relation_kind] *)

val show_relation_kind : relation_kind -> string
(** Convert [relation_kind] to string *)

type divorce = Def.divorce =
  | NotDivorced
  | Divorced of cdate
  | Separated_old
  | NotSeparated
  | Separated of cdate

val pp_divorce : Format.formatter -> divorce -> unit
(** Printer for [divorce] *)

val show_divorce : divorce -> string
(** Convert [divorce] to string *)

type death_reason = Def.death_reason =
  | Killed
  | Murdered
  | Executed
  | Disappeared
  | Unspecified

val pp_death_reason : Format.formatter -> death_reason -> unit
(** Printer for [death_reason] *)

val show_death_reason : death_reason -> string
(** Convert [death_reason] to string *)

type death = Def.death =
  | NotDead
  | Death of death_reason * cdate
  | DeadYoung
  | DeadDontKnowWhen
  | DontKnowIfDead
  | OfCourseDead

val pp_death : Format.formatter -> death -> unit
(** Printer for [death] *)

val show_death : death -> string
(** Convert [death] to string *)

type burial = Def.burial = UnknownBurial | Buried of cdate | Cremated of cdate

val pp_burial : Format.formatter -> burial -> unit
(** Printer for [burial] *)

val show_burial : burial -> string
(** Convert [burial] to string *)

type access = Def.access = IfTitles | Public | SemiPublic | Private

val pp_access : Format.formatter -> access -> unit
(** Printer for [access] *)

val show_access : access -> string
(** Convert [access] to string *)

type 'string gen_title_name = 'string Def.gen_title_name =
  | Tmain
  | Tname of 'string
  | Tnone

val pp_gen_title_name :
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  'string gen_title_name ->
  unit
(** Printer for [gen_title_name] *)

val show_gen_title_name :
  (Format.formatter -> 'string -> unit) -> 'string gen_title_name -> string
(** Convert [gen_title_name] to string *)

type 'string gen_title = 'string Def.gen_title = {
  t_name : 'string gen_title_name;
  t_ident : 'string;
  t_place : 'string;
  t_date_start : cdate;
  t_date_end : cdate;
  t_nth : int;
}

val pp_gen_title :
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  'string gen_title ->
  unit
(** Printer for [gen_title] *)

val show_gen_title :
  (Format.formatter -> 'string -> unit) -> 'string gen_title -> string
(** Convert [gen_title] to string *)

type witness_kind = Def.witness_kind =
  | Witness
  | Witness_GodParent
  | Witness_CivilOfficer
  | Witness_ReligiousOfficer
  | Witness_Informant
  | Witness_Attending
  | Witness_Mentioned
  | Witness_Other

val pp_witness_kind : Format.formatter -> witness_kind -> unit
(** Printer for [witness_kind] *)

val show_witness_kind : witness_kind -> string
(** Convert [witness_kind] to string *)

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

val pp_gen_pers_event_name :
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  'string gen_pers_event_name ->
  unit
(** Printer for [gen_pers_event_name] *)

val show_gen_pers_event_name :
  (Format.formatter -> 'string -> unit) -> 'string gen_pers_event_name -> string
(** Convert [gen_pers_event_name] to string *)

type ('person, 'string) gen_pers_event =
      ('person, 'string) Def.gen_pers_event = {
  epers_name : 'string gen_pers_event_name;
  epers_date : cdate;
  epers_place : 'string;
  epers_reason : 'string;
  epers_note : 'string;
  epers_src : 'string;
  epers_witnesses : ('person * witness_kind) array;
}

val pp_gen_pers_event :
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  ('person, 'string) gen_pers_event ->
  unit
(** Printer for [gen_pers_event] *)

val show_gen_pers_event :
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  ('person, 'string) gen_pers_event ->
  string
(** Convert [gen_pers_event] to string *)

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

val pp_gen_fam_event_name :
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  'string gen_fam_event_name ->
  unit
(** Printer for [gen_fam_event_name] *)

val show_gen_fam_event_name :
  (Format.formatter -> 'string -> unit) -> 'string gen_fam_event_name -> string
(** Convert [gen_fam_event_name] to string *)

type ('person, 'string) gen_fam_event = ('person, 'string) Def.gen_fam_event = {
  efam_name : 'string gen_fam_event_name;
  efam_date : cdate;
  efam_place : 'string;
  efam_reason : 'string;
  efam_note : 'string;
  efam_src : 'string;
  efam_witnesses : ('person * witness_kind) array;
}

val pp_gen_fam_event :
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  ('person, 'string) gen_fam_event ->
  unit
(** Printer for [gen_fam_event] *)

val show_gen_fam_event :
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  ('person, 'string) gen_fam_event ->
  string
(** Convert [gen_fam_event] to string *)

type relation_type = Def.relation_type =
  | Adoption
  | Recognition
  | CandidateParent
  | GodParent
  | FosterParent

val pp_relation_type : Format.formatter -> relation_type -> unit
(** Printer for [relation_type] *)

val show_relation_type : relation_type -> string
(** Convert [relation_type] to string *)

type ('person, 'string) gen_relation = ('person, 'string) Def.gen_relation = {
  r_type : relation_type;
  r_fath : 'person option;
  r_moth : 'person option;
  r_sources : 'string;
}

val pp_gen_relation :
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  ('person, 'string) gen_relation ->
  unit
(** Printer for [gen_relation] *)

val show_gen_relation :
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  ('person, 'string) gen_relation ->
  string
(** Convert [gen_relation] to string *)

type sex = Def.sex = Male | Female | Neuter

val pp_sex : Format.formatter -> sex -> unit
(** Printer for [sex] *)

val show_sex : sex -> string
(** Convert [sex] to string *)

type place = Def.place = {
  other : string;
  town : string;
  township : string;
  canton : string;
  district : string;
  county : string;
  region : string;
  country : string;
}

val pp_place : Format.formatter -> place -> unit
(** Printer for [place] *)

val show_place : place -> string
(** Convert [place] to string *)

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

val pp_gen_person :
  (Format.formatter -> 'iper -> unit) ->
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  ('iper, 'person, 'string) gen_person ->
  unit
(** Printer for [gen_person] *)

val show_gen_person :
  (Format.formatter -> 'iper -> unit) ->
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  ('iper, 'person, 'string) gen_person ->
  string
(** Convert [gen_person] to string *)

type fix = Adef.fix

val pp_fix : Format.formatter -> Adef.fix -> unit
(** Printer for [fix] *)

val show_fix : Adef.fix -> string
(** Convert [fix] to string *)

type 'family gen_ascend = 'family Def.gen_ascend = {
  parents : 'family option;
  consang : fix;
}

val pp_gen_ascend :
  (Format.formatter -> 'family -> unit) ->
  Format.formatter ->
  'family gen_ascend ->
  unit
(** Printer for [gen_ascend] *)

val show_gen_ascend :
  (Format.formatter -> 'family -> unit) -> 'family gen_ascend -> string
(** Convert [gen_ascend] to string *)

type 'family gen_union = 'family Def.gen_union = { family : 'family array }

val pp_gen_union :
  (Format.formatter -> 'family -> unit) ->
  Format.formatter ->
  'family gen_union ->
  unit
(** Printer for [gen_union] *)

val show_gen_union :
  (Format.formatter -> 'family -> unit) -> 'family gen_union -> string
(** Convert [gen_union] to string *)

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

val pp_gen_family :
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'ifam -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  ('person, 'ifam, 'string) gen_family ->
  unit
(** Printer for [gen_family] *)

val show_gen_family :
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'ifam -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  ('person, 'ifam, 'string) gen_family ->
  string
(** Convert [gen_family] to string *)

type 'person gen_couple = 'person Adef.gen_couple

val pp_gen_couple :
  (Format.formatter -> 'person -> unit) ->
  Format.formatter ->
  'person gen_couple ->
  unit
(** Printer for [gen_couple] *)

val show_gen_couple :
  (Format.formatter -> 'person -> unit) -> 'person gen_couple -> string
(** Convert [gen_couple] to string *)

type 'person gen_descend = 'person Def.gen_descend = {
  children : 'person array;
}

val pp_gen_descend :
  (Format.formatter -> 'person -> unit) ->
  Format.formatter ->
  'person gen_descend ->
  unit
(** Printer for [gen_descend] *)

val show_gen_descend :
  (Format.formatter -> 'person -> unit) -> 'person gen_descend -> string
(** Convert [gen_descend] to string *)
