(* $Id: gwdb.ml,v 5.11 2006-09-20 16:28:37 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Adef;

type db_person 'person 'string = Def.gen_person 'person 'string;
type db_ascend 'family = Def.gen_ascend 'family;
type db_union 'family = Def.gen_union 'family;

type db_family 'person 'string = Def.gen_family 'person 'string;

type person = db_person iper istr;
type ascend = db_ascend ifam;
type union = db_union ifam;

type family = db_family iper istr;
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

value get_access p = p.Def.access;
value get_aliases p = p.Def.aliases;
value get_baptism p = p.Def.baptism;
value get_baptism_place p = p.Def.baptism_place;
value get_baptism_src p = p.Def.baptism_src;
value get_birth p = p.Def.birth;
value get_birth_place p = p.Def.birth_place;
value get_birth_src p = p.Def.birth_src;
value get_burial p = p.Def.burial;
value get_burial_place p = p.Def.burial_place;
value get_burial_src p = p.Def.burial_src;
value get_cle_index p = p.Def.cle_index;
value get_death p = p.Def.death;
value get_death_place p = p.Def.death_place;
value get_death_src p = p.Def.death_src;
value get_first_name p = p.Def.first_name;
value get_first_names_aliases p = p.Def.first_names_aliases;
value get_image p = p.Def.image;
value get_notes p = p.Def.notes;
value get_occ p = p.Def.occ;
value get_occupation p = p.Def.occupation;
value get_psources p = p.Def.psources;
value get_public_name p = p.Def.public_name;
value get_qualifiers p = p.Def.qualifiers;
value get_related p = p.Def.related;
value get_rparents p = p.Def.rparents;
value get_sex p = p.Def.sex;
value get_surname p = p.Def.surname;
value get_surnames_aliases p = p.Def.surnames_aliases;
value get_titles p = p.Def.titles;

value person_of_gen_person p = p;
value gen_person_of_person p = p;

value get_consang a = a.Def.consang;
value get_parents a = a.Def.parents;

value ascend_of_gen_ascend a = a;
value gen_ascend_of_ascend a = a;

value get_family u = u.Def.family;

value union_of_gen_union u = u;

value get_comment f = f.Def.comment;
value get_divorce f = f.Def.divorce;
value get_fam_index f = f.Def.fam_index;
value get_fsources f = f.Def.fsources;
value get_marriage f = f.Def.marriage;
value get_marriage_place f = f.Def.marriage_place;
value get_marriage_src f = f.Def.marriage_src;
value get_origin_file f = f.Def.origin_file;
value get_relation f = f.Def.relation;
value get_witnesses f = f.Def.witnesses;

value family_of_gen_family f = f;
value gen_family_of_family f = f;
