(* $Id: gwdb.mli,v 5.22 2006-09-21 02:04:47 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Adef;

type db_person 'person 'string = 'abstract;
type db_ascend 'family = 'abstract;
type db_union 'family = 'abstract;

type db_family 'person 'string = 'abstract;
type db_couple 'person = 'abstract;
type db_descend 'person = 'abstract;

type person = db_person iper istr;
type ascend = db_ascend ifam;
type union = db_union ifam;

type family = db_family iper istr;
type couple = db_couple iper;
type descend = db_descend iper;

type relation = Def.gen_relation iper istr;
type title = Def.gen_title istr;

type rn_mode = [ RnAll | Rn1Ch | Rn1Ln ];

type notes =
  { nread : string -> rn_mode -> string;
    norigin_file : string;
    efiles : unit -> list string }
;

type cache 'a =
  { array : unit -> array 'a;
    get : int -> 'a;
    len : mutable int;
    clear_array : unit -> unit }
;

type istr_iper_index =
  { find : istr -> list iper;
    cursor : string -> istr;
    next : istr -> istr }
;

type visible_cache =
  { v_write : unit -> unit;
    v_get : (person -> bool) -> int -> bool }
;

type base_data =
  { persons : cache person;
    ascends : cache ascend;
    unions : cache union;
    visible : visible_cache;
    families : cache family;
    couples : cache couple;
    descends : cache descend;
    strings : cache string;
    particles : list string;
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
    commit_notes : string -> string -> unit;
    patched_ascends : unit -> list iper;
    is_patched_person : iper -> bool;
    cleanup : unit -> unit }
;

type base =
  { data : base_data;
    func : base_func }
;

value get_access : db_person 'p 's -> Def.access;
value get_aliases : db_person 'p 's -> list 's;
value get_baptism : db_person 'p 's -> codate;
value get_baptism_place : db_person 'p 's -> 's;
value get_baptism_src : db_person 'p 's -> 's;
value get_birth : db_person 'p 's -> codate;
value get_birth_place : db_person 'p 's -> 's;
value get_birth_src : db_person 'p 's -> 's;
value get_burial : db_person 'p 's -> Def.burial;
value get_burial_place : db_person 'p 's -> 's;
value get_burial_src : db_person 'p 's -> 's;
value get_cle_index : db_person 'p 's -> iper;
value get_death : db_person 'p 's -> Def.death;
value get_death_place : db_person 'p 's -> 's;
value get_death_src : db_person 'p 's -> 's;
value get_first_name : db_person 'p 's -> 's;
value get_first_names_aliases : db_person 'p 's -> list 's;
value get_image : db_person 'p 's -> 's;
value get_notes : db_person 'p 's -> 's;
value get_occ : db_person 'p 's -> int;
value get_occupation : db_person 'p 's -> 's;
value get_psources : db_person 'p 's -> 's;
value get_public_name : db_person 'p 's -> 's;
value get_qualifiers : db_person 'p 's -> list 's;
value get_related : db_person 'p 's -> list iper;
value get_rparents : db_person 'p 's -> list (Def.gen_relation 'p 's);
value get_sex : db_person 'p 's -> Def.sex;
value get_surname : db_person 'p 's -> 's;
value get_surnames_aliases : db_person 'p 's -> list 's;
value get_titles : db_person 'p 's -> list (Def.gen_title 's);

value person_of_gen_person : Def.gen_person 'p 's -> db_person 'p 's;
value gen_person_of_person : db_person 'p 's -> Def.gen_person 'p 's;

value get_parents : db_ascend 'f -> option 'f;
value get_consang : db_ascend 'f -> Adef.fix;

value ascend_of_gen_ascend : Def.gen_ascend 'f -> db_ascend 'f;
value gen_ascend_of_ascend : db_ascend 'f -> Def.gen_ascend 'f;

value get_family : db_union 'f -> array 'f;

value union_of_gen_union : Def.gen_union 'f -> db_union 'f;

value get_comment : db_family 'p 's -> 's;
value get_divorce : db_family 'p 's -> Def.divorce;
value get_fam_index : db_family 'p 's -> ifam;
value get_fsources : db_family 'p 's -> 's;
value get_marriage : db_family 'p 's -> codate;
value get_marriage_place : db_family 'p 's -> 's;
value get_marriage_src : db_family 'p 's -> 's;
value get_origin_file : db_family 'p 's -> 's;
value get_relation : db_family 'p 's -> Def.relation_kind;
value get_witnesses : db_family 'p 's -> array 'p;

value family_of_gen_family : Def.gen_family 'p 's -> db_family 'p 's;
value gen_family_of_family : db_family 'p 's -> Def.gen_family 'p 's;

value get_father : db_couple 'p -> 'p;
value get_mother : db_couple 'p -> 'p;
value get_parent_array : db_couple 'p -> array 'p;

value gen_couple_of_couple : db_couple 'p -> Def.gen_couple 'p;
value couple_of_gen_couple : Def.gen_couple 'p -> db_couple 'p;

value get_children : db_descend 'p -> array 'p;

value descend_of_gen_descend : Def.gen_descend 'p -> db_descend 'p;
value gen_descend_of_descend : db_descend 'p -> Def.gen_descend 'p;
