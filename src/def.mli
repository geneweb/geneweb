(* $Id: def.mli,v 5.22 2008-01-08 11:58:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type choice 'a 'b = [ Left of 'a | Right of 'b ];

type iper = Adef.iper;
type ifam = Adef.ifam;

type codate = Adef.codate;

type date = Adef.date ==
  [ Dgreg of dmy and calendar
  | Dtext of string ]
and calendar = Adef.calendar ==
  [ Dgregorian
  | Djulian
  | Dfrench
  | Dhebrew ]
and dmy = Adef.dmy ==
  { day : int;
    month : int;
    year : int;
    prec : precision;
    delta : int }
and dmy2 = Adef.dmy2 ==
  { day2 : int;
    month2 : int;
    year2 : int;
    delta2 : int }
and precision = Adef.precision ==
  [ Sure | About | Maybe | Before | After | OrYear of dmy2 | YearInt of dmy2 ]
;

type relation_kind =
  [ Married | NotMarried | Engaged | NoSexesCheckNotMarried | NoMention
  | NoSexesCheckMarried ]
;

type divorce = [ NotDivorced | Divorced of codate | Separated ];

type death_reason =
  [ Killed | Murdered | Executed | Disappeared | Unspecified ]
;
type death =
  [ NotDead
  | Death of death_reason and codate
  | DeadYoung
  | DeadDontKnowWhen
  | DontKnowIfDead
  | OfCourseDead ]
;

type burial = [ UnknownBurial | Buried of codate | Cremated of codate ];

type access = [ IfTitles | Public | Private ];

type gen_title_name 'string = [ Tmain | Tname of 'string | Tnone ];
type gen_title 'string =
  { t_name : gen_title_name 'string;
    t_ident : 'string;
    t_place : 'string;
    t_date_start : codate;
    t_date_end : codate;
    t_nth : int }
;

type witness_kind =
  [ Witness | Witness_GodParent ]
;

type gen_pers_event_name 'string =
  [ Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial | Epers_Cremation
  | Epers_Accomplishment | Epers_Acquisition | Epers_Adhesion
  | Epers_BaptismLDS | Epers_BarMitzvah | Epers_BatMitzvah | Epers_Benediction
  | Epers_ChangeName | Epers_Circumcision | Epers_Confirmation
  | Epers_ConfirmationLDS | Epers_Decoration | Epers_DemobilisationMilitaire
  | Epers_Diploma | Epers_Distinction | Epers_Dotation | Epers_DotationLDS
  | Epers_Education | Epers_Election | Epers_Emigration | Epers_Excommunication
  | Epers_FamilyLinkLDS | Epers_FirstCommunion | Epers_Funeral | Epers_Graduate
  | Epers_Hospitalisation | Epers_Illness | Epers_Immigration
  | Epers_ListePassenger | Epers_MilitaryDistinction | Epers_MilitaryPromotion
  | Epers_MilitaryService | Epers_MobilisationMilitaire | Epers_Naturalisation
  | Epers_Occupation | Epers_Ordination | Epers_Property | Epers_Recensement
  | Epers_Residence | Epers_Retired | Epers_ScellentChildLDS
  | Epers_ScellentParentLDS | Epers_ScellentSpouseLDS | Epers_VenteBien
  | Epers_Will
  | Epers_Name of 'string ]
;
type gen_pers_event 'person 'string =
  { epers_name : gen_pers_event_name 'string;
    epers_date : codate;
    epers_place : 'string;
    epers_reason : 'string;
    epers_note : 'string;
    epers_src : 'string;
    epers_witnesses : array ('person * witness_kind) }
;

type gen_fam_event_name 'string =
  [ Efam_Marriage | Efam_NoMarriage | Efam_NoMention | Efam_Engage
  | Efam_Divorce  | Efam_Separated
  | Efam_Annulation | Efam_MarriageBann | Efam_MarriageContract
  | Efam_MarriageLicense | Efam_PACS | Efam_Residence
  | Efam_Name of 'string ]
;
type gen_fam_event 'person 'string =
  { efam_name : gen_fam_event_name 'string;
    efam_date : codate;
    efam_place : 'string;
    efam_reason : 'string;
    efam_note : 'string;
    efam_src : 'string;
    efam_witnesses : array ('person * witness_kind) }
;


type relation_type =
  [ Adoption | Recognition | CandidateParent | GodParent | FosterParent ]
;

type gen_relation 'person 'string =
  { r_type : relation_type;
    r_fath : option 'person;
    r_moth : option 'person;
    r_sources : 'string }
;

type sex = [ Male | Female | Neuter ];

type place =
  { other : string;
    town : string;
    township : string;
    canton : string;
    district : string;
    county : string;
    region : string;
    country : string }
;

(* person *)

type gen_person 'person 'string =
  { first_name : 'string;
    surname : 'string;
    occ : int;
    image : 'string;
    public_name : 'string;
    qualifiers : list 'string;
    aliases : list 'string;
    first_names_aliases : list 'string;
    surnames_aliases : list 'string;
    titles : list (gen_title 'string);
    rparents : list (gen_relation 'person 'string);
    related : list iper;
    occupation : 'string;
    sex : sex;
    access : access;
    birth : codate;
    birth_place : 'string;
    birth_note : 'string;
    birth_src : 'string;
    baptism : codate;
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
    pevents : list (gen_pers_event 'person 'string);
    notes : 'string;
    psources : 'string;
    key_index : iper }
;


type gen_ascend 'family =
  { parents : option 'family;
    consang : Adef.fix }
;

type gen_union 'family =
  { family : array 'family }
;

(* family *)

type gen_family 'person 'string =
  { marriage : codate;
    marriage_place : 'string;
    marriage_note : 'string;
    marriage_src : 'string;
    witnesses : array 'person;
    relation : relation_kind;
    divorce : divorce;
    fevents : list (gen_fam_event 'person 'string);
    comment : 'string;
    origin_file : 'string;
    fsources : 'string;
    fam_index : ifam }
;

type gen_couple 'person = Adef.gen_couple 'person;

type gen_descend 'person =
  { children : array 'person }
;

type error 'person =
  [ AlreadyDefined of 'person
  | OwnAncestor of 'person
  | BadSexOfMarriedPerson of 'person ]
;

type warning 'person 'descend 'title 'pevent 'fevent =
  [ BigAgeBetweenSpouses of 'person and 'person and dmy
  | BirthAfterDeath of 'person
  | IncoherentSex of 'person and int and int
  | ChangedOrderOfChildren of ifam and 'descend and array iper and array iper
  | ChangedOrderOfMarriages of 'person and array ifam and array ifam
  | ChangedOrderOfFamilyEvents of ifam and list 'fevent and list 'fevent
  (* | ChangedOrderOfPersonEvents of 'person and list 'pevent and list 'pevent *)
  | ChildrenNotInOrder of ifam and 'descend and 'person and 'person
  | CloseChildren of ifam and 'descend and 'person and 'person
  | DeadOld of 'person and dmy
  | DeadTooEarlyToBeFather of 'person and 'person
  | FEventOrder of 'person and 'fevent and 'fevent
  | FWitnessEventAfterDeath of 'person and 'fevent
  | FWitnessEventBeforeBirth of 'person and 'fevent
  | IncoherentAncestorDate of 'person and 'person
  | MarriageDateAfterDeath of 'person
  | MarriageDateBeforeBirth of 'person
  | MotherDeadAfterChildBirth of 'person and 'person
  | ParentBornAfterChild of 'person and 'person
  | ParentTooOld of 'person and dmy
  | ParentTooYoung of 'person and dmy
  | PEventOrder of 'person and 'pevent and 'pevent
  | PWitnessEventAfterDeath of 'person and 'pevent
  | PWitnessEventBeforeBirth of 'person and 'pevent
  | TitleDatesError of 'person and 'title
  | UndefinedSex of 'person
  | WitnessDateAfterDeath of 'person
  | WitnessDateBeforeBirth of 'person
  | YoungForMarriage of 'person and dmy ]
;

type misc 'person 'descend 'title = [ MissingSources ];

type rn_mode = [ RnAll | Rn1Ln | RnDeg ];


(* Historique des modifications *)

type base_changed 'person 'string =
  [ U_Add_person of gen_person 'person 'string
  | U_Modify_person of gen_person 'person 'string and gen_person 'person 'string
  | U_Delete_person of gen_person 'person 'string
  | U_Merge_person of gen_person 'person 'string and gen_person 'person 'string
      and gen_person 'person 'string
  | U_Send_image of gen_person 'person 'string
  | U_Delete_image of gen_person 'person 'string
  | U_Add_family of gen_person 'person 'string and gen_family 'person 'string
  | U_Modify_family of gen_person 'person 'string and
      gen_family 'person 'string and gen_family 'person 'string
  | U_Delete_family of gen_person 'person 'string and gen_family 'person 'string
  | U_Invert_family of gen_person 'person 'string and ifam
  | U_Merge_family of gen_person 'person 'string and
      gen_family 'person 'string and gen_family 'person 'string and
      gen_family 'person 'string
  | U_Change_children_name of gen_person 'person 'string and
      list ((string * string * int * iper) * (string * string * int *iper))
  | U_Add_parent of gen_person 'person 'string and gen_family 'person 'string
  | U_Kill_ancestors of gen_person 'person 'string
  | U_Multi of gen_person 'person 'string
  | U_Notes of option int and string ]
;
