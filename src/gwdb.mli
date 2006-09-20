(* $Id: gwdb.mli,v 5.16 2006-09-20 12:35:43 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Adef;

type db_person 'person 'string = 'abstract;
type db_ascend 'family = 'abstract;
type db_union 'family = 'abstract;

type person = db_person iper istr;
type ascend = db_ascend ifam;
type union = db_union ifam;

type family = Def.gen_family iper istr;
type couple = Def.gen_couple iper;
type descend = Def.gen_descend iper;

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
