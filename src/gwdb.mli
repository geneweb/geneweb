(* $Id: gwdb.mli,v 5.102 2007-03-02 11:44:13 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Adef;

type istr = 'abstract;

type person = 'abstract;
type family = 'abstract;

type relation = Def.gen_relation iper istr;
type title = Def.gen_title istr;

type string_person_index = 'abstract;

type base = 'abstract;

value open_base : string -> base;
value close_base : base -> unit;

value eq_istr : istr -> istr -> bool;
value is_empty_string : istr -> bool;
value is_quest_string : istr -> bool;
value empty_person : base -> iper -> person;

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

value get_parents : person -> option ifam;
value get_consang : person -> Adef.fix;

value get_family : person -> array ifam;

value gen_person_of_person : person -> Def.gen_person iper istr;

value get_comment : family -> istr;
value get_divorce : family -> Def.divorce;
value get_fsources : family -> istr;
value get_marriage : family -> codate;
value get_marriage_place : family -> istr;
value get_marriage_src : family -> istr;
value get_origin_file : family -> istr;
value get_relation : family -> Def.relation_kind;
value get_witnesses : family -> array iper;

value get_father : family -> iper;
value get_mother : family -> iper;
value get_parent_array : family -> array iper;

value get_children : family -> array iper;

value gen_family_of_family : family -> Def.gen_family iper istr;
value gen_couple_of_couple : family -> Def.gen_couple iper;
value gen_descend_of_descend : family -> Def.gen_descend iper;

value person_of_gen_person :
  base ->
    (Def.gen_person iper istr * Def.gen_ascend ifam * Def.gen_union ifam) ->
    person;

value family_of_gen_family :
  base ->
    (Def.gen_family iper istr * Def.gen_couple iper * Def.gen_descend iper) ->
    family;

value poi : base -> iper -> person;
value foi : base -> ifam -> family;
value sou : base -> istr -> string;

value nb_of_persons : base -> int;
value nb_of_families : base -> int;

value patch_person : base -> iper -> Def.gen_person iper istr -> unit;
value patch_ascend : base -> iper -> Def.gen_ascend ifam -> unit;
value patch_union : base -> iper -> Def.gen_union ifam -> unit;
value patch_family : base -> ifam -> Def.gen_family iper istr -> unit;
value patch_descend : base -> ifam -> Def.gen_descend iper -> unit;
value patch_couple : base -> ifam -> Def.gen_couple iper -> unit;

value patch_name : base -> string -> iper -> unit;
value patch_key : base -> iper -> string -> string -> int -> unit;
value delete_key : base -> string -> string -> int -> unit;
value insert_string : base -> string -> istr;
value commit_patches : base -> unit;
value commit_notes : base -> string -> string -> unit;
value is_patched_person : base -> iper -> bool;
value patched_ascends : base -> list iper;

value delete_family : base -> ifam -> unit;
value is_deleted_family : family -> bool;

value person_of_key : base -> string -> string -> int -> option iper;
value persons_of_name : base -> string -> list iper;
value persons_of_first_name : base -> string_person_index;
value persons_of_surname : base -> string_person_index;

value spi_first : string_person_index -> string -> istr;
  (* first [first/sur]name starting with that string *)
value spi_next : string_person_index -> istr -> bool -> (istr * int);
  (* next [first/sur]name by alphabetical order *)
value spi_find : string_person_index -> istr -> list iper;
  (* all persons having that [first/sur]name *)

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

value persons_array :
  base ->
    (int -> Def.gen_person iper istr *
     int -> Def.gen_person iper istr -> unit);
value ascends_array :
  base ->
    (int -> option ifam *
     int -> Adef.fix *
     int -> Adef.fix -> unit *
     option (array Adef.fix));

value base_notes_read : base -> string -> string;
value base_notes_read_first_line : base -> string -> string;
value base_notes_are_empty : base -> string -> bool;
value base_notes_origin_file : base -> string;
value base_notes_dir : base -> string;
value base_wiznotes_dir : base -> string;

value gen_person_misc_names :
  base -> Def.gen_person iper istr ->
    (Def.gen_person iper istr -> list (Def.gen_title istr)) -> list string;

value person_misc_names :
  base -> person -> (person -> list title) -> list string;
value nobtit :
  base -> Lazy.t (list string) -> Lazy.t (list string) -> person ->
    list title;

value p_first_name : base -> person -> string;
value p_surname : base -> person -> string;

value date_of_last_change : base -> float;

(**/**)
(** For database builders *)

value base_of_base1 : Dbdisk.dsk_base -> base;
value dsk_person_of_person : person -> Dbdisk.dsk_person;

value apply_base1 : base -> (Dbdisk.dsk_base -> unit) -> unit;
value apply_base2 : base -> (Db2disk.db2 -> unit) -> unit;
