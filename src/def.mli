(* $Id: def.mli,v 3.6 2000-09-10 13:25:56 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

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

type relation_kind = [ Married | NotMarried | Engaged ];

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

(* person *)

type gen_person 'person 'string =
  { first_name : mutable 'string;
    surname : mutable 'string;
    occ : mutable int;
    image : mutable 'string;
    public_name : mutable 'string;
    nick_names : mutable list 'string;
    aliases : mutable list 'string;
    first_names_aliases : mutable list 'string;
    surnames_aliases : mutable list 'string;
    titles : mutable list (gen_title 'string);
    rparents : mutable list (gen_relation 'person 'string);
    related : mutable list iper;
    occupation : mutable 'string;
    sex : mutable sex;
    access : mutable access;
    birth : mutable codate;
    birth_place : mutable 'string;
    birth_src : mutable 'string;
    baptism : mutable codate;
    baptism_place : mutable 'string;
    baptism_src : mutable 'string;
    death : mutable death;
    death_place : mutable 'string;
    death_src : mutable 'string;
    burial : mutable burial;
    burial_place : mutable 'string;
    burial_src : mutable 'string;
    notes : mutable 'string;
    psources : mutable 'string;
    cle_index : mutable iper }
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

type gen_couple 'person =
  { father : mutable 'person;
    mother : mutable 'person }
;

type gen_descend 'person =
  { children : mutable array 'person }
;

(* data base *)

type person = gen_person iper istr;
type ascend = gen_ascend ifam;
type union = gen_union ifam;

type family = gen_family iper istr;
type couple = gen_couple iper;
type descend = gen_descend iper;

type relation = gen_relation iper istr;
type title = gen_title istr;

type notes =
  { nread : mutable int -> string;
    norigin_file : mutable string }
;

type cache 'a =
  { array : mutable unit -> array 'a;
    get : mutable int -> 'a;
    len : mutable int;
    clear_array : unit -> unit }
;

type istr_iper_index =
  { find : istr -> list iper;
    cursor : string -> istr;
    next : istr -> istr }
;

type base_data =
  { persons : cache person;
    ascends : cache ascend;
    unions : cache union;
    families : cache family;
    couples : cache couple;
    descends : cache descend;
    strings : cache string;
    bnotes : notes }
;

type base_func =
  { persons_of_name : string -> list iper;
    strings_of_fsname : string -> list istr;
    index_of_string : string -> istr;
    persons_of_surname : istr_iper_index;
    persons_of_first_name : istr_iper_index;
    patch_person : iper -> person -> unit;
    patch_ascend : iper -> ascend -> unit;
    patch_union : iper -> union -> unit;
    patch_family : ifam -> family -> unit;
    patch_couple : ifam -> couple -> unit;
    patch_descend : ifam -> descend -> unit;
    patch_string : istr -> string -> unit;
    patch_name : string -> iper -> unit;
    commit_patches : unit -> unit;
    commit_notes : string -> unit;
    patched_ascends : unit -> list iper;
    cleanup : unit -> unit }
;

type base =
  { data : base_data;
    func : base_func }
;
