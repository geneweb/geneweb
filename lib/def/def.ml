(* Copyright (c) 1998-2007 INRIA *)

(** Http response status *)
type httpStatus =
  | OK (* 200 *)
  | Moved_Temporarily (* 302 *)
  | Bad_Request (* 400 *)
  | Unauthorized (* 401 *)
  | Forbidden (* 403 *)
  | Not_Found (* 404 *)
  | Conflict (* 409 *)
  | Internal_Server_Error (* 500 *)
  | Service_Unavailable (* 503 *)

exception HttpExn of httpStatus * string

(* TODO OCaml 4.12 : use Either *)

(** Type that represents 2 possible choices *)
type ('a, 'b) choice = Left of 'a | Right of 'b

type cdate = Adef.cdate
(** Alias to [Adef.cdate] *)

(** Alias to [Adef.date] *)
type date = Adef.date =
  | Dgreg of dmy * calendar
  (* textual form of the date *)
  | Dtext of string

(** Alias to [Adef.calendar] *)
and calendar = Adef.calendar = Dgregorian | Djulian | Dfrench | Dhebrew

and dmy = Adef.dmy = {
  day : int;
  month : int;
  year : int;
  prec : precision;
  delta : int;
}
(** Alias to [Adef.dmy] *)

and dmy2 = Adef.dmy2 = { day2 : int; month2 : int; year2 : int; delta2 : int }
(** Alias to [Adef.dmy2] *)

(** Alias to [Adef.precision] *)
and precision = Adef.precision =
  | Sure
  | About
  | Maybe
  | Before
  | After
  | OrYear of dmy2
  (* inteval *)
  | YearInt of dmy2

(** Relation kind between couple in the family *)
type relation_kind =
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

(** Divorce status *)
type divorce =
  | NotDivorced
  | Divorced of cdate
  | Separated_old
  | NotSeparated
  | Separated of cdate

(** Death reason *)
type death_reason = Killed | Murdered | Executed | Disappeared | Unspecified

(** Death status *)
type death =
  | NotDead
  | Death of death_reason * cdate
  | DeadYoung
  | DeadDontKnowWhen
  | DontKnowIfDead
  | OfCourseDead

(** Burial information *)
type burial = UnknownBurial | Buried of cdate | Cremated of cdate

(** Rights for access to the personal data *)
type access = IfTitles | Public | SemiPublic | Private

(** Title name *)
type 'string gen_title_name = Tmain | Tname of 'string | Tnone

type 'string gen_title = {
  t_name : 'string gen_title_name;
  t_ident : 'string;
  t_place : 'string;
  t_date_start : cdate;
  t_date_end : cdate;
  t_nth : int;
}
(** Type that represents information about nobility title of a person *)

(** Witness kind for an event *)
type witness_kind =
  | Witness
  | Witness_GodParent
  | Witness_CivilOfficer
  | Witness_ReligiousOfficer
  | Witness_Informant
  | Witness_Attending
  | Witness_Mentioned
  | Witness_Other

(** Personal event name. *)
type 'string gen_pers_event_name =
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

type ('person, 'string) gen_pers_event = {
  epers_name : 'string gen_pers_event_name;
  epers_date : cdate;
  epers_place : 'string;
  epers_reason : 'string;
  epers_note : 'string;
  epers_src : 'string;
  epers_witnesses : ('person * witness_kind) array;
}
(** Personal event information *)

(** Event name pertaining a family. *)
type 'string gen_fam_event_name =
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

type ('person, 'string) gen_fam_event = {
  efam_name : 'string gen_fam_event_name;
  efam_date : cdate;
  efam_place : 'string;
  efam_reason : 'string;
  efam_note : 'string;
  efam_src : 'string;
  efam_witnesses : ('person * witness_kind) array;
}
(** Event information pertaining a family. *)

(** Relation type with parent (if not native) *)
type relation_type =
  | Adoption
  | Recognition
  | CandidateParent
  | GodParent
  | FosterParent

type ('person, 'string) gen_relation = {
  r_type : relation_type;
  r_fath : 'person option;
  r_moth : 'person option;
  r_sources : 'string;
}
(** Relation information with parents (if not native) *)

(** Sex of person *)
type sex = Male | Female | Neuter

type place = {
  other : string;
  town : string;
  township : string;
  canton : string;
  district : string;
  county : string;
  region : string;
  country : string;
}
(** Place information *)

type ('iper, 'person, 'string) gen_person = {
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
(** Polymorphic type describing information about person. *)

type 'family gen_ascend = { parents : 'family option; consang : Adef.fix }
(** Person's ascendants (family where he is a children) with its consangunity
    rate (equal to relationship betwen his parents). *)

(* Person's families to which he belongs as parent (union of families) *)
type 'family gen_union = { family : 'family array }

type 'person gen_descend = { children : 'person array }
(** Children of the family *)

type ('person, 'ifam, 'string) gen_family = {
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
(** Polymorphic type describing information about family. *)

type 'person gen_couple = 'person Adef.gen_couple
(** Alias to [Adef.gen_couple] *)

(** Database errors describing bad specification of the person *)
type 'person error =
  | AlreadyDefined of 'person
  | OwnAncestor of 'person  (** Person is his own ancestor *)
  | BadSexOfMarriedPerson of 'person

(** Database warnings attached to the specification of the person, family,
    relation, etc. *)
type ('iper, 'person, 'family, 'descend, 'title, 'pevent, 'fevent) warning =
  | BigAgeBetweenSpouses of
      'person
      * 'person
      * dmy (* Age differece between couples is greater then 50 years *)
  | BirthAfterDeath of 'person  (** Person is born after his death *)
  | IncoherentSex of 'person * int * int  (** Incoherent sex of person *)
  | ChangedOrderOfChildren of 'family * 'descend * 'iper array * 'iper array
      (** Children order has been modified *)
  | ChangedOrderOfMarriages of 'person * 'family array * 'family array
      (** Person's marriages order has been modified *)
  | ChangedOrderOfFamilyEvents of 'family * 'fevent list * 'fevent list
      (** Family's events order has been modified *)
  | ChangedOrderOfPersonEvents of 'person * 'pevent list * 'pevent list
      (** Person's events order has been modified *)
  | ChildrenNotInOrder of 'family * 'descend * 'person * 'person
      (** Children aren't ordered *)
  | CloseChildren of 'family * 'person * 'person
      (** Age difference between two child is less then 7 month (except for
          twins) *)
  | DeadOld of 'person * dmy
      (** Dead old (at the age older then 109 after 1900 year and older then 100
          before) *)
  | DeadTooEarlyToBeFather of 'person * 'person
      (** Children is born in more then 1 year after his father's death *)
  | DistantChildren of 'family * 'person * 'person
      (** Age gap between two of siblings greater then 50 years *)
  | FEventOrder of 'person * 'fevent * 'fevent
      (** Familial events haven't been ordered correctly *)
  | FWitnessEventAfterDeath of 'person * 'fevent * 'family
      (** Witness is dead before familial event date *)
  | FWitnessEventBeforeBirth of 'person * 'fevent * 'family
      (** Witness is born after familial event date *)
  | IncoherentAncestorDate of 'person * 'person
      (** Ancestor is born after person's birth *)
  | MarriageDateAfterDeath of 'person  (** Person is married after his death *)
  | MarriageDateBeforeBirth of 'person
      (** Person is married before his birth *)
  | MotherDeadBeforeChildBirth of 'person * 'person
      (** Children is born after his mother's death *)
  | ParentBornAfterChild of 'person * 'person
      (** Parent is born after one of his children *)
  | ParentTooOld of 'person * dmy * 'person
      (** Person became a parent at age older then 55 years for mother and 70
          for father *)
  | ParentTooYoung of 'person * dmy * 'person
      (** Person became a parent at age younger then 11 years old *)
  | PEventOrder of 'person * 'pevent * 'pevent
      (** Personal events haven't been ordered correctly *)
  | PossibleDuplicateFam of 'family * 'family
      (** There is a possibility that two families are a duplicate of each other
      *)
  | PossibleDuplicateFamHomonymous of 'family * 'family * 'person
      (** There is a possibility that two families are a duplicate of each other
          (Homonymous spouse) *)
  | PWitnessEventAfterDeath of 'person * 'pevent * 'person
      (** Witness is dead before personal event date *)
  | PWitnessEventBeforeBirth of 'person * 'pevent * 'person
      (** Witness is born after personal event date *)
  | TitleDatesError of 'person * 'title
      (** Title's start date is after end date or person is born after title
          dates *)
  | UndefinedSex of 'person  (** Person has undefined sex (Neuter) *)
  | YoungForMarriage of 'person * dmy * 'family
      (** Person is married before he was 12 years old *)
  | OldForMarriage of 'person * dmy * 'family
      (** Person is married after he was 100 years old *)

(** Missing sources warning *)
type ('person, 'descend, 'title) misc = MissingSources

(** Database note/page reading mode *)
type rn_mode =
  | RnAll  (** Read all content *)
  | Rn1Ln  (** Read first line *)
  | RnDeg  (** If file isn't empty returns a space *)

type base_notes = {
  (* read content of the page with giving mode.
     Page "" represent database note *)
  nread : string -> rn_mode -> string; (* origin .gw filename *)
  norigin_file : string; (* returns list of extended pages *)
  efiles : unit -> string list;
}
(** Database note/page explorer structure *)

(** Update modification used for history tracking *)
type ('iper, 'person, 'family, 'string) base_changed =
  | U_Add_person of ('iper, 'person, 'string) gen_person
  | U_Modify_person of
      ('iper, 'person, 'string) gen_person
      * ('iper, 'person, 'string) gen_person
  | U_Delete_person of ('iper, 'person, 'string) gen_person
  | U_Merge_person of
      ('iper, 'person, 'string) gen_person
      * ('iper, 'person, 'string) gen_person
      * ('iper, 'person, 'string) gen_person
  | U_Send_image of ('iper, 'person, 'string) gen_person
  | U_Delete_image of ('iper, 'person, 'string) gen_person
  | U_Add_family of
      ('iper, 'person, 'string) gen_person
      * ('person, 'family, 'string) gen_family
  | U_Modify_family of
      ('iper, 'person, 'string) gen_person
      * ('person, 'family, 'string) gen_family
      * ('person, 'family, 'string) gen_family
  | U_Delete_family of
      ('iper, 'person, 'string) gen_person
      * ('person, 'family, 'string) gen_family
  | U_Invert_family of ('iper, 'person, 'string) gen_person * 'family
  | U_Merge_family of
      ('iper, 'person, 'string) gen_person
      * ('person, 'family, 'string) gen_family
      * ('person, 'family, 'string) gen_family
      * ('person, 'family, 'string) gen_family
  | U_Change_children_name of
      ('iper, 'person, 'string) gen_person
      * ((string * string * int * 'person) * (string * string * int * 'person))
        list
  | U_Add_parent of
      ('iper, 'person, 'string) gen_person
      * ('person, 'family, 'string) gen_family
  | U_Kill_ancestors of ('iper, 'person, 'string) gen_person
  (* Modification U_Multi used when multiple persons are modified successively. Separation with U_Modify_person is necessary to inform foreign notify_change script
     about database change without creating process for every person. *)
  | U_Multi of
      ('iper, 'person, 'string) gen_person
      * ('iper, 'person, 'string) gen_person
      * bool
  | U_Notes of int option * string

(** TODOOCP : doc *)
module NLDB = struct
  type ('a, 'b) page =
    | PgInd of 'a
    | PgFam of 'b
    | PgNotes
    | PgMisc of string
    | PgWizard of string

  type key = string * string * int
  type ind = { lnTxt : string option; lnPos : int }
  type ('a, 'b) t = (('a, 'b) page * (string list * (key * ind) list)) list

  let equal_key (fn1, sn1, oc1) (fn2, sn2, oc2) =
    oc1 = oc2 && String.equal fn1 fn2 && String.equal sn1 sn2
end

let ( ^^^ ) = Adef.( ^^^ )
let ( ^>^ ) = Adef.( ^>^ )
let ( ^<^ ) = Adef.( ^<^ )
