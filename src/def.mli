(* $Id: def.mli,v 5.22 2008-01-08 11:58:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type choice 'a 'b = [ Left of 'a | Right of 'b ];

type iper = Adef.iper;
type ifam = Adef.ifam;

type cdate = Adef.cdate;
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
and precision = Adef.precision ==
  [ Sure | About | Maybe | Before | After | OrYear of int | YearInt of int ]
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
  | Death of death_reason and cdate
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
    birth_src : 'string;
    baptism : codate;
    baptism_place : 'string;
    baptism_src : 'string;
    death : death;
    death_place : 'string;
    death_src : 'string;
    burial : burial;
    burial_place : 'string;
    burial_src : 'string;
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
    marriage_src : 'string;
    witnesses : array 'person;
    relation : relation_kind;
    divorce : divorce;
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

type warning 'person 'descend 'title =
  [ BirthAfterDeath of 'person
  | IncoherentSex of 'person and int and int
  | ChangedOrderOfChildren of ifam and 'descend and array iper and array iper
  | ChildrenNotInOrder of ifam and 'descend and 'person and 'person
  | DeadTooEarlyToBeFather of 'person and 'person
  | IncoherentAncestorDate of 'person and 'person
  | MarriageDateAfterDeath of 'person
  | MarriageDateBeforeBirth of 'person
  | MotherDeadAfterChildBirth of 'person and 'person
  | ParentBornAfterChild of 'person and 'person
  | ParentTooYoung of 'person and dmy
  | TitleDatesError of 'person and 'title
  | UndefinedSex of 'person
  | YoungForMarriage of 'person and dmy ]
;

type misc 'person 'descend 'title = [ MissingSources ];

type rn_mode = [ RnAll | Rn1Ln | RnDeg ];
