(* $Id: gwdb.ml,v 5.37 2006-10-10 18:48:26 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Adef;
open Dbdisk;
open Def;
open Mutil;

type person = dsk_person;
type ascend = dsk_ascend;
type union = dsk_union;

type family = dsk_family;
type couple = dsk_couple;
type descend = dsk_descend;

type relation = Def.gen_relation iper istr;
type title = Def.gen_title istr;

type string_person_index = Dbdisk.string_person_index ==
  { find : istr -> list iper;
    cursor : string -> istr;
    next : istr -> istr }
;

type base = Dbdisk.dsk_base;

value is_empty_string istr = Adef.int_of_istr istr = 0;
value is_quest_string istr = Adef.int_of_istr istr = 1;

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
value get_death p = p.Def.death;
value get_death_place p = p.Def.death_place;
value get_death_src p = p.Def.death_src;
value get_first_name p = p.Def.first_name;
value get_first_names_aliases p = p.Def.first_names_aliases;
value get_image p = p.Def.image;
value get_key_index p = p.Def.key_index;
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

value person_with_key p fn sn oc =
  {(p) with first_name = fn; surname = sn; occ = oc}
;
value person_with_related p r = {(p) with related = r};
value person_with_rparents p r = {(p) with rparents = r};
value person_with_sex p s = {(p) with sex = s};
value person_of_gen_person p = p;
value gen_person_of_person p = p;

value get_consang a = a.Def.consang;
value get_parents a = a.Def.parents;

value ascend_with_consang a c = {parents = a.parents; consang = c};
value ascend_with_parents a p = {parents = p; consang = a.consang};
value ascend_of_gen_ascend a = a;

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

value get_father c = Adef.father c;
value get_mother c = Adef.mother c;
value get_parent_array c = Adef.parent_array c;

value gen_couple_of_couple c = c;
value couple_of_gen_couple c = c;

value get_children d = d.Def.children;

value descend_of_gen_descend d = d;
value gen_descend_of_descend d = d;

value poi base i = base.data.persons.get (Adef.int_of_iper i);
value aoi base i = base.data.ascends.get (Adef.int_of_iper i);
value uoi base i = base.data.unions.get (Adef.int_of_iper i);

value foi base i = base.data.families.get (Adef.int_of_ifam i);
value coi base i = base.data.couples.get (Adef.int_of_ifam i);
value doi base i = base.data.descends.get (Adef.int_of_ifam i);

value sou base i = base.data.strings.get (Adef.int_of_istr i);

value nb_of_persons base = base.data.persons.len;
value nb_of_families base = base.data.families.len;
value nb_of_strings base = base.data.strings.len;

value patch_person base = base.func.patch_person;
value patch_ascend base = base.func.patch_ascend;
value patch_union base = base.func.patch_union;
value patch_family base = base.func.patch_family;
value patch_descend base = base.func.patch_descend;
value patch_couple base = base.func.patch_couple;
value patch_name base = base.func.patch_name;
value patch_string base = base.func.patch_string;
value commit_patches base = base.func.commit_patches ();
value commit_notes base = base.func.commit_notes;
value is_patched_person base = base.func.is_patched_person;
value patched_ascends base = base.func.patched_ascends ();

value persons_of_name base = base.func.persons_of_name;
value persons_of_first_name base = base.func.persons_of_first_name;
value persons_of_surname base = base.func.persons_of_surname;

value spi_cursor spi = spi.cursor;
value spi_find spi = spi.find;
value spi_next spi = spi.next;

value base_visible_get base = base.data.visible.v_get;
value base_visible_write base = base.data.visible.v_write ();
value base_index_of_string base = base.func.index_of_string;
value base_particles base = base.data.particles;
value base_strings_of_fsname base = base.func.strings_of_fsname;
value base_cleanup base = base.func.cleanup ();

value load_ascends_array base = base.data.ascends.load_array ();
value load_unions_array base = base.data.unions.load_array ();
value load_couples_array base = base.data.couples.load_array ();
value load_descends_array base = base.data.descends.load_array ();
value load_strings_array base = base.data.strings.load_array ();

value persons_array base = (base.data.persons.get, base.data.persons.set);
value ascends_array base = (base.data.ascends.get, base.data.ascends.set);

value base_notes_read base fn = base.data.bnotes.nread fn RnAll;
value base_notes_read_first_line base fn = base.data.bnotes.nread fn Rn1Ln;
value base_notes_read_first_char base fn = base.data.bnotes.nread fn Rn1Ch;
value base_notes_origin_file base = base.data.bnotes.norigin_file;

value p_first_name base p = nominative (sou base (get_first_name p));
value p_surname base p = nominative (sou base (get_surname p));

value nobtit = Dutil.dsk_nobtit;
value person_misc_names = Dutil.dsk_person_misc_names;

value base_of_dsk_base base = base;
value apply_as_dsk_base f base = f base;
