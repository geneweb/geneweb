(* $Id: def.mli,v 2.2 1999-03-25 20:25:33 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

type iper = Adef.iper;
type ifam = Adef.ifam;
type istr = Adef.istr;
type cdate = Adef.cdate;
type codate = Adef.codate;

type precision = Adef.precision ==
  [ Sure | About | Maybe | Before | After | OrYear of int | YearInt of int ]
;
type date = Adef.date ==
  { day : int;
    month : int;
    year : int;
    prec : precision }
;

type divorce = [ NotDivorced | Divorced of codate ];

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

type title_name 'string = [ Tmain | Tname of 'string | Tnone ];
type title 'string =
  { t_name : mutable title_name 'string;
    t_title : mutable 'string;
    t_place : mutable 'string;
    t_date_start : mutable codate;
    t_date_end : mutable codate;
    t_nth : mutable int }
;

type sex = [ Masculine | Feminine | Neuter ];

type person 'string =
  { first_name : mutable 'string;
    surname : mutable 'string;
    occ : mutable int;
    photo : mutable 'string;
    public_name : mutable 'string;
    nick_names : mutable list 'string;
    aliases : mutable list 'string;
    first_names_aliases : mutable list 'string;
    surnames_aliases : mutable list 'string;
    titles : mutable list (title 'string);
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
    family : mutable array ifam;
    notes : mutable 'string;
    psources : mutable 'string;
    cle_index : mutable iper }
;

type ascend =
  { parents : mutable option ifam;
    consang : mutable Adef.fix }
;

type family 'person 'string =
  { marriage : mutable codate;
    marriage_place : mutable 'string;
    marriage_src : mutable 'string;
    not_married : mutable bool;
    divorce : mutable divorce;
    children : mutable array 'person;
    comment : mutable 'string;
    origin_file : mutable 'string;
    fsources : mutable 'string;
    fam_index : mutable ifam }
;

type couple 'person =
  { father : mutable 'person;
    mother : mutable 'person }
;

type base_person = person istr;
type base_ascend = ascend;
type base_family = family iper istr;
type base_couple = couple iper;

type cache 'a =
  { array : mutable unit -> array 'a;
    get : mutable int -> 'a;
    len : mutable int }
;

type istr_iper_index =
  { find : istr -> list iper;
    cursor : string -> istr;
    next : istr -> istr }
;

type base_data =
  { persons : cache base_person;
    ascends : cache base_ascend;
    families : cache base_family;
    couples : cache base_couple;
    strings : cache string;
    has_family_patches : bool }
;

type base_func =
  { persons_of_name : string -> list iper;
    strings_of_fsname : string -> list istr;
    index_of_string : string -> istr;
    persons_of_surname : istr_iper_index;
    persons_of_first_name : istr_iper_index;
    patch_person : iper -> base_person -> unit;
    patch_ascend : iper -> base_ascend -> unit;
    patch_family : ifam -> base_family -> unit;
    patch_couple : ifam -> base_couple -> unit;
    patch_string : istr -> string -> unit;
    patch_name : string -> iper -> unit;
    commit_patches : unit -> unit;
    cleanup : unit -> unit }
;

type base =
  { data : base_data;
    func : base_func }
;
