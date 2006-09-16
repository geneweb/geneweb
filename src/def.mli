(* $Id: def.mli,v 5.5 2006-09-16 03:16:05 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

type choice 'a 'b = [ Left of 'a | Right of 'b ];

type iper = Adef.iper;
type ifam = Adef.ifam;
type istr = Adef.istr;
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
  | DontKnowIfDead ]
;

type burial = [ UnknownBurial | Buried of codate | Cremated of codate ];

type access = [ IfTitles | Public | Private ];

type gen_title_name 'string = [ Tmain | Tname of 'string | Tnone ];
type gen_title 'string =
  { t_name : mutable gen_title_name 'string;
    t_ident : mutable 'string;
    t_place : mutable 'string;
    t_date_start : mutable codate;
    t_date_end : mutable codate;
    t_nth : mutable int }
;

type relation_type =
  [ Adoption | Recognition | CandidateParent | GodParent | FosterParent ]
;

type gen_relation 'person 'string =
  { r_type : mutable relation_type;
    r_fath : mutable option 'person;
    r_moth : mutable option 'person;
    r_sources : mutable 'string }
;

type sex = [ Male | Female | Neuter ];

(* The types "gen_person" and "gen_family" can be extended preserving
   backward compatibility if:
      1/ the extensions take place at the end of the record type
      2/ the types of the new fields are implemented by "int" (e.g. "int",
         "bool", "istr"), or by a sum type having at least a constructor
         without parameter (e.g. the type "option" which has "None", the
         type "list" has "[]"); does not work with arrays and strings!
   If these conditions are respected, old databases can be read with
   new version of this "def.mli"; the values of the missing fields have
   the value implemented as "0" (0, False, None, [], and so on). *)

(* person *)

type gen_person 'person 'string =
  { first_name : mutable 'string;
    surname : mutable 'string;
    occ : mutable int;
    image : 'string;
    public_name : 'string;
    qualifiers : list 'string;
    aliases : list 'string;
    first_names_aliases : list 'string;
    surnames_aliases : list 'string;
    titles : list (gen_title 'string);
    rparents : mutable list (gen_relation 'person 'string);
    related : mutable list iper;
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
    cle_index : iper }
;

type gen_ascend 'family =
  { parents : mutable option 'family;
    consang : mutable Adef.fix }
;

type gen_union 'family =
  { family : mutable array 'family }
;

(* family *)

type gen_family 'person 'string =
  { marriage : mutable codate;
    marriage_place : mutable 'string;
    marriage_src : mutable 'string;
    witnesses : mutable array 'person;
    relation : mutable relation_kind;
    divorce : mutable divorce;
    comment : mutable 'string;
    origin_file : mutable 'string;
    fsources : mutable 'string;
    fam_index : mutable ifam }
;

type gen_couple 'person = Adef.gen_couple 'person;

type gen_descend 'person =
  { children : mutable array 'person }
;
