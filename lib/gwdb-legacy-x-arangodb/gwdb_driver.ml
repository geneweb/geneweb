module L = Gwdb_driver_legacy
module A = Gwdb_driver_arango

type legacy_base = L.base

type arango_base =
  ( L.iper
  , L.ifam
  , Dbdisk.dsk_person
  , Dbdisk.dsk_ascend
  , Dbdisk.dsk_union
  , Dbdisk.dsk_family
  , Dbdisk.dsk_couple
  , Dbdisk.dsk_descend) A.base

(* Gwdb_driver interface *)

type iper = L.iper
type ifam = L.ifam
type istr = L.istr

let string_of_iper = L.string_of_iper
let string_of_ifam = L.string_of_ifam
let string_of_istr = L.string_of_istr

let iper_of_string = L.iper_of_string
let ifam_of_string = L.ifam_of_string
let istr_of_string = L.istr_of_string

type person = L.person
type family = L.family
type relation = L.relation
type title = L.title
type pers_event = L.pers_event
type fam_event = L.fam_event

type string_person_index = L.string_person_index

type base =
  { arango : arango_base option
  ; legacy : legacy_base
  }

let use_arango legacy =
  Sys.file_exists @@
  Filename.concat legacy.Dbdisk.data.Dbdisk.bdir "use_arango"

let open_base bname : base =
  let legacy = L.open_base bname in
  let arango =
    if use_arango legacy
    then Some (A.open_base legacy.Dbdisk.data.Dbdisk.bdir)
    else None
  in
  { arango ; legacy }

let legacy fn b = fn b.legacy

let close_base = legacy L.close_base

let dummy_iper = L.dummy_iper
let dummy_ifam = L.dummy_ifam
let empty_string = L.empty_string
let quest_string = L.quest_string

let eq_istr = L.eq_istr
let is_empty_string = L.is_empty_string
let is_quest_string = L.is_quest_string

let sou = legacy L.sou

let bname = legacy L.bname
let nb_of_persons = legacy L.nb_of_persons
let nb_of_real_persons = legacy L.nb_of_real_persons
let nb_of_families = legacy L.nb_of_families

let new_iper = legacy L.new_iper
let new_ifam = legacy L.new_ifam

let opt base fn = match base.arango with None -> () | Some a -> fn a

let insert_person b i p =
  opt b (fun b -> A.insert_person b i p) ;
  L.insert_person b.legacy i p

let insert_ascend b i a =
  opt b (fun b -> A.insert_ascend b i a) ;
  L.insert_ascend b.legacy i a

let insert_union b i u =
  opt b (fun b -> A.insert_union b i u) ;
  L.insert_union b.legacy i u

let insert_family b i f =
  opt b (fun b -> A.insert_family b i f) ;
  L.insert_family b.legacy i f

let insert_descend b i d =
  opt b (fun b -> A.insert_descend b i d) ;
  L.insert_descend b.legacy i d

let insert_couple b i c =
  opt b (fun b -> A.insert_couple b i c) ;
  L.insert_couple b.legacy i c

let patch_person b i p =
  opt b (fun b -> A.patch_person b i p) ;
  L.patch_person b.legacy i p

let patch_ascend b i a =
  opt b (fun b -> A.patch_ascend b i a) ;
  L.patch_ascend b.legacy i a

let patch_union b i u =
  opt b (fun b -> A.patch_union b i u) ;
  L.patch_union b.legacy i u

let patch_family b i f =
  opt b (fun b -> A.patch_family b i f) ;
  L.patch_family b.legacy i f

let patch_descend b i d =
  opt b (fun b -> A.patch_descend b i d) ;
  L.patch_descend b.legacy i d

let patch_couple b i c =
  opt b (fun b -> A.patch_couple b i c) ;
  L.patch_couple b.legacy i c

let delete_person b i =
  opt b (fun b -> A.delete_person b i) ;
  L.delete_person b.legacy i

let delete_ascend b i =
  opt b (fun b -> A.delete_ascend b i) ;
  L.delete_ascend b.legacy i

let delete_union b i =
  opt b (fun b -> A.delete_union b i) ;
  L.delete_union b.legacy i

let delete_family b i =
  opt b (fun b -> A.delete_family b i) ;
  L.delete_family b.legacy i

let delete_descend b i =
  opt b (fun b -> A.delete_descend b i) ;
  L.delete_descend b.legacy i

let delete_couple b i =
  opt b (fun b -> A.delete_couple b i) ;
  L.delete_couple b.legacy i

let insert_string = legacy L.insert_string

let commit_patches b =
  opt b begin fun a ->
    let open Dbdisk in
    let b = b.legacy in
    A.commit_patches
      a
      (L.sou b)
      b.data.persons.get
      b.data.ascends.get
      b.data.unions.get
      b.data.families.get
      b.data.couples.get
      b.data.descends.get
  end ;
  L.commit_patches b.legacy

let commit_notes = legacy L.commit_notes

let person_of_key = legacy L.person_of_key
let persons_of_name = legacy L.persons_of_name
let persons_of_first_name = legacy L.persons_of_first_name
let persons_of_surname = legacy L.persons_of_surname

let spi_first = L.spi_first
let spi_next = L.spi_next
let spi_find = L.spi_find

let base_particles = legacy L.base_particles

let base_strings_of_first_name = legacy L.base_strings_of_first_name

let base_strings_of_surname = legacy L.base_strings_of_surname

let load_ascends_array = legacy L.load_ascends_array
let load_unions_array = legacy L.load_unions_array
let load_couples_array = legacy L.load_couples_array
let load_descends_array = legacy L.load_descends_array
let load_strings_array = legacy L.load_strings_array
let load_persons_array = legacy L.load_persons_array
let load_families_array = legacy L.load_families_array

let clear_ascends_array = legacy L.clear_ascends_array
let clear_unions_array = legacy L.clear_unions_array
let clear_couples_array = legacy L.clear_couples_array
let clear_descends_array = legacy L.clear_descends_array
let clear_strings_array = legacy L.clear_strings_array
let clear_persons_array = legacy L.clear_persons_array
let clear_families_array = legacy L.clear_families_array

let date_of_last_change = legacy L.date_of_last_change

let persons = legacy L.persons
let families = legacy L.families

let make particles bname arrays =
  let legacy = L.make particles bname arrays in
  let arango =
    if use_arango legacy
    then Some begin
        let bdir = legacy.Dbdisk.data.Dbdisk.bdir in
        (* FIXME: implement Gwdb_driver_arango.make *)
        Gwdb_arango_migrate.delete (fun _ _ _ -> ()) bdir ;
        Gwdb_arango_migrate.import (fun _ _ _ -> ()) bdir ;
        A.open_base bdir
      end
    else None
  in
  { legacy ; arango }

let read_nldb = legacy L.read_nldb
let write_nldb = legacy L.write_nldb
let sync ?scratch b = (L.sync ?scratch b.legacy)

let base_notes_origin_file = legacy L.base_notes_origin_file
let base_notes_dir = legacy L.base_notes_dir
let base_wiznotes_dir = legacy L.base_wiznotes_dir

let base_notes_read = legacy L.base_notes_read
let base_notes_read_first_line = legacy L.base_notes_read_first_line
let base_notes_are_empty = legacy L.base_notes_are_empty

let get_access = L.get_access
let get_aliases = L.get_aliases
let get_baptism = L.get_baptism
let get_baptism_note = L.get_baptism_note
let get_baptism_place = L.get_baptism_place
let get_baptism_src = L.get_baptism_src
let get_birth = L.get_birth
let get_birth_note = L.get_birth_note
let get_birth_place = L.get_birth_place
let get_birth_src = L.get_birth_src
let get_burial = L.get_burial
let get_burial_note = L.get_burial_note
let get_burial_place = L.get_burial_place
let get_burial_src = L.get_burial_src
let get_children = L.get_children
let get_comment = L.get_comment
let get_consang = L.get_consang
let get_death = L.get_death
let get_death_note = L.get_death_note
let get_death_place = L.get_death_place
let get_death_src = L.get_death_src
let get_divorce = L.get_divorce
let get_family = L.get_family
let get_father = L.get_father
let get_fevents = L.get_fevents
let get_first_name = L.get_first_name
let get_first_names_aliases = L.get_first_names_aliases
let get_fsources = L.get_fsources
let get_ifam = L.get_ifam
let get_image = L.get_image
let get_iper = L.get_iper
let get_marriage = L.get_marriage
let get_marriage_note = L.get_marriage_note
let get_marriage_place = L.get_marriage_place
let get_marriage_src = L.get_marriage_src
let get_mother = L.get_mother
let get_notes = L.get_notes
let get_occ = L.get_occ
let get_occupation = L.get_occupation
let get_origin_file = L.get_origin_file
let get_parent_array = L.get_parent_array
let get_parents = L.get_parents
let get_pevents = L.get_pevents
let get_psources = L.get_psources
let get_public_name = L.get_public_name
let get_qualifiers = L.get_qualifiers
let get_related = L.get_related
let get_relation = L.get_relation
let get_rparents = L.get_rparents
let get_sex = L.get_sex
let get_surname = L.get_surname
let get_surnames_aliases = L.get_surnames_aliases
let get_titles = L.get_titles
let get_witnesses = L.get_witnesses

let person_of_gen_person b = L.person_of_gen_person b.legacy
let family_of_gen_family b = L.family_of_gen_family b.legacy

let gen_family_of_family = L.gen_family_of_family
let gen_person_of_person = L.gen_person_of_person
let gen_ascend_of_person = L.gen_ascend_of_person
let gen_union_of_person = L.gen_union_of_person

let dummy_marker = L.dummy_marker
let ifam_marker = L.ifam_marker
let iper_marker = L.iper_marker
let dummy_collection = L.dummy_collection
let ifams = legacy L.ifams
let ipers = legacy L.ipers
module Marker = L.Marker
module Collection = L.Collection
let base_visible_write b = L.base_visible_write b.legacy
let base_visible_get b = L.base_visible_get b.legacy
let foi b = L.foi b.legacy
let poi b = L.poi b.legacy
let gen_descend_of_family = L.gen_descend_of_family
let gen_couple_of_family = L.gen_couple_of_family
let empty_family b = L.empty_family b.legacy
let empty_person b = L.empty_person b.legacy

let no_couple = L.no_couple
let no_descend = L.no_descend
let no_family = L.no_family
let no_union = L.no_union
let no_ascend = L.no_ascend
let no_person = L.no_person
