(* $Id: gwdb.mli,v 5.59 2006-10-24 19:27:42 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Adef;
open Config;

type istr = 'abstract;

type person = 'abstract;
type ascend = 'abstract;
type union = 'abstract;

type family = 'abstract;
type couple = 'abstract;
type descend = 'abstract;

type relation = Def.gen_relation iper istr;
type title = Def.gen_title istr;

type string_person_index = 'abstract;

type base = 'abstract;

value open_base : string -> base;
value close_base : base -> unit;

value eq_istr : istr -> istr -> bool;
value is_empty_string : istr -> bool;
value is_quest_string : istr -> bool;
value empty_person : iper -> person;

value get_access : person -> Def.access;
value get_aliases : person -> list istr;
value get_baptism : person -> codate;
value get_baptism_place : person -> istr;
value get_baptism_src : person -> istr;
value get_birth : person -> codate;
value get_birth_place : person -> istr;
value get_birth_src : person -> istr;
value get_burial : person -> Def.burial;
value get_burial_place : person -> istr;
value get_burial_src : person -> istr;
value get_death : person -> Def.death;
value get_death_place : person -> istr;
value get_death_src : person -> istr;
value get_first_name : person -> istr;
value get_first_names_aliases : person -> list istr;
value get_image : person -> istr;
value get_key_index : person -> iper;
value get_notes : person -> istr;
value get_occ : person -> int;
value get_occupation : person -> istr;
value get_psources : person -> istr;
value get_public_name : person -> istr;
value get_qualifiers : person -> list istr;
value get_related : person -> list iper;
value get_rparents : person -> list relation;
value get_sex : person -> Def.sex;
value get_surname : person -> istr;
value get_surnames_aliases : person -> list istr;
value get_titles : person -> list title;

value person_with_key : person -> istr -> istr -> int -> person;
value person_with_related : person -> list iper -> person;
value person_with_rparents : person -> list relation -> person;
value person_with_sex : person -> Def.sex -> person;
value person_of_gen_person : base -> Def.gen_person iper istr -> person;
value gen_person_of_person : person -> Def.gen_person iper istr;

value get_parents : ascend -> option ifam;
value get_consang : ascend -> Adef.fix;

value ascend_of_gen_ascend : base -> Def.gen_ascend ifam -> ascend;

value get_family : union -> array ifam;

value union_of_gen_union : base -> Def.gen_union ifam -> union;

value get_comment : family -> istr;
value get_divorce : family -> Def.divorce;
value get_fam_index : family -> ifam;
value get_fsources : family -> istr;
value get_marriage : family -> codate;
value get_marriage_place : family -> istr;
value get_marriage_src : family -> istr;
value get_origin_file : family -> istr;
value get_relation : family -> Def.relation_kind;
value get_witnesses : family -> array iper;

value family_with_origin_file : family -> istr -> ifam -> family;

value family_of_gen_family : base -> Def.gen_family iper istr -> family;
value gen_family_of_family : family -> Def.gen_family iper istr;

value get_father : couple -> iper;
value get_mother : couple -> iper;
value get_parent_array : couple -> array iper;

value couple_of_gen_couple : base -> Def.gen_couple iper -> couple;
value gen_couple_of_couple : couple -> Def.gen_couple iper;

value get_children : descend -> array iper;

value descend_of_gen_descend : base -> Def.gen_descend iper -> descend;
value gen_descend_of_descend : descend -> Def.gen_descend iper;

value poi : base -> iper -> person;
value aoi : base -> iper -> ascend;
value uoi : base -> iper -> union;

value foi : base -> ifam -> family;
value coi : base -> ifam -> couple;
value doi : base -> ifam -> descend;

value sou : base -> istr -> string;

value nb_of_persons : base -> int;
value nb_of_families : base -> int;

value patch_person : base -> iper -> person -> unit;
value patch_ascend : base -> iper -> ascend -> unit;
value patch_union : base -> iper -> union -> unit;
value patch_family : base -> ifam -> family -> unit;
value patch_descend : base -> ifam -> descend -> unit;
value patch_couple : base -> ifam -> couple -> unit;
value patch_name : base -> string -> iper -> unit;
value insert_string : base -> string -> istr;
value commit_patches : base -> unit;
value commit_notes : base -> string -> string -> unit;
value is_patched_person : base -> iper -> bool;
value patched_ascends : base -> list iper;
value output_consang_tab : base -> array Adef.fix -> unit;

value person_of_key : base -> string -> string -> int -> option iper;
value persons_of_name : base -> string -> list iper;
value persons_of_first_name : base -> string_person_index;
value persons_of_surname : base -> string_person_index;

value spi_cursor : string_person_index -> string -> istr;
value spi_find : string_person_index -> istr -> list iper;
value spi_next : string_person_index -> istr -> istr;

value base_visible_get : base -> (person -> bool) -> int -> bool;
value base_visible_write : base -> unit;
value base_particles : base -> list string;
value base_strings_of_first_name : base -> string -> list istr;
value base_strings_of_surname : base -> string -> list istr;

value load_ascends_array : base -> unit;
value load_unions_array : base -> unit;
value load_couples_array : base -> unit;
value load_descends_array : base -> unit;
value load_strings_array : base -> unit;

value persons_array : base -> (int -> person * int -> person -> unit);
value ascends_array :
  base ->
    (int -> option ifam * int -> Adef.fix * int -> Adef.fix -> unit *
     option (array Adef.fix));

value base_notes_read : base -> string -> string;
value base_notes_read_first_line : base -> string -> string;
value base_notes_are_empty : base -> string -> bool;
value base_notes_origin_file : base -> string;

value person_misc_names :
  base -> person -> (person -> list title) -> list string
;
value nobtit : config -> base -> person -> list title;

value p_first_name : base -> person -> string;
value p_surname : base -> person -> string;

(**/**)
(** For database builders *)

value base_of_dsk_base : Dbdisk.dsk_base -> base;
value apply_as_dsk_base : (Dbdisk.dsk_base -> 'a) -> base -> 'a;
value dsk_person_of_person : person -> Dbdisk.dsk_person;
