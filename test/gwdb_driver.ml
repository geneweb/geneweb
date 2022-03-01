type ifam = int
type iper = int
type istr = int

type base =
  { persons: (iper, iper, istr) Def.gen_person array
  ; ascends: ifam Def.gen_ascend array
  ; unions: ifam Def.gen_union array
  ; families: (iper, ifam, istr) Def.gen_family array
  ; couples: iper Def.gen_couple array
  ; descends: iper Def.gen_descend array
  ; strings: string array
  }

type fam_event = (iper, istr) Def.gen_fam_event
type family = (base * ifam)
type pers_event = (iper, istr) Def.gen_pers_event
type person = (base * iper)
type relation = (iper, istr) Def.gen_relation
type string_person_index
type title = istr Def.gen_title

module Collection = struct
  type 'a t
  let length _ = assert false
  let map _ = assert false
  let iter _ = assert false
  let iteri _ = assert false
  let fold ?from:_ ?until:_ _ = assert false
  let fold_until _ = assert false
  let iterator _ = assert false
end

module Marker = struct
  type ('k, 'v) t
  let get _ = assert false
  let set _ = assert false
end

let rec base_notes_are_empty _ = assert false
and base_notes_dir _ = assert false
and base_notes_origin_file _ = assert false
and base_notes_read _ = assert false
and base_notes_read_first_line _ = assert false
and base_particles _ = assert false
and base_strings_of_first_name _ = assert false
and base_strings_of_surname _ = assert false
and base_visible_get _ = assert false
and base_visible_write _ = assert false
and base_wiznotes_dir _ = assert false
and bname _ = assert false
and clear_ascends_array _ = ()
and clear_couples_array _ = ()
and clear_descends_array _ = ()
and clear_families_array _ = ()
and clear_persons_array _ = ()
and clear_strings_array _ = ()
and clear_unions_array _ = ()
and close_base _ = assert false
and commit_notes _ = assert false
and commit_patches _ = assert false
and date_of_last_change _ = assert false
and delete_ascend _ = assert false
and delete_couple _ = assert false
and delete_descend _ = assert false
and delete_family _ = assert false
and delete_person _ = assert false
and delete_union _ = assert false
and dummy_collection _ = assert false
and dummy_ifam = -1
and dummy_iper = -1
and dummy_marker _ = assert false
and empty_family _ = assert false
and empty_person _ = assert false
and empty_string = 0
and eq_istr = (=)
and families ?select:_ _ = assert false
and family_of_gen_family _ = assert false
and foi base i = (base, i)
and gen_ascend_of_person _ = assert false
and gen_couple_of_family _ = assert false
and gen_descend_of_family _ = assert false
and gen_family_of_family _ = assert false
and gen_person_of_person _ = assert false
and gen_union_of_person _ = assert false
and get_access _ = assert false
and get_aliases _ = assert false
and get_baptism _ = assert false
and get_baptism_note _ = assert false
and get_baptism_place _ = assert false
and get_baptism_src _ = assert false
and get_birth _ = assert false
and get_birth_note _ = assert false
and get_birth_place _ = assert false
and get_birth_src _ = assert false
and get_burial _ = assert false
and get_burial_note _ = assert false
and get_burial_place _ = assert false
and get_burial_src _ = assert false
and get_children _ = assert false
and get_comment _ = assert false
and get_consang _ = assert false
and get_death _ = assert false
and get_death_note _ = assert false
and get_death_place _ = assert false
and get_death_src _ = assert false
and get_divorce _ = assert false
and get_family (base, i) = (Array.get base.unions i).family
and get_father (base, i) = Adef.father (Array.get base.couples i)
and get_fevents _ = assert false
and get_first_name _ = assert false
and get_first_names_aliases _ = assert false
and get_fsources _ = assert false
and get_ifam (base, i) = i
and get_image _ = assert false
and get_iper (base, i) = i
and get_marriage _ = assert false
and get_marriage_note _ = assert false
and get_marriage_place _ = assert false
and get_marriage_src _ = assert false
and get_mother (base, i) = Adef.mother (Array.get base.couples i)
and get_notes _ = assert false
and get_occ _ = assert false
and get_occupation _ = assert false
and get_origin_file _ = assert false
and get_parent_array _ = assert false
and get_parents (base, i) = (Array.get base.ascends i).parents
and get_pevents _ = assert false
and get_psources _ = assert false
and get_public_name _ = assert false
and get_qualifiers _ = assert false
and get_related _ = assert false
and get_relation _ = assert false
and get_rparents _ = assert false
and get_sex _ = assert false
and get_surname _ = assert false
and get_surnames_aliases _ = assert false
and get_titles _ = assert false
and get_witnesses _ = assert false
and ifam_exists _ = assert false
and ifam_marker _ = assert false
and ifam_of_string = int_of_string
and ifams ?select:_ _ = assert false
and insert_ascend _ = assert false
and insert_couple _ = assert false
and insert_descend _ = assert false
and insert_family _ = assert false
and insert_person _ = assert false
and insert_string _ = assert false
and insert_union _ = assert false
and iper_exists _ = assert false
and iper_marker _ = assert false
and iper_of_string = int_of_string
and ipers _ = assert false
and is_empty_string i = eq_istr empty_string i
and is_quest_string i = eq_istr quest_string i
and istr_of_string = int_of_string
and load_ascends_array _ = ()
and load_couples_array _ = ()
and load_descends_array _ = ()
and load_families_array _ = ()
and load_persons_array _ = ()
and load_strings_array _ = ()
and load_unions_array _ = ()

and make _ _ ((persons, ascends, unions), (families, couples, descends), strings, _) =
  { persons
  ; ascends
  ; unions
  ; families
  ; couples
  ; descends
  ; strings
  }

and nb_of_families _ = assert false
and nb_of_persons _ = assert false
and nb_of_real_persons _ = assert false
and new_ifam _ = assert false
and new_iper _ = assert false
and no_ascend = { Def.parents = None ; consang = Adef.no_consang }
and no_descend = { Def.children = [||] }
and no_family ifam = { (Mutil.empty_family empty_string) with fam_index = ifam }
and no_person ip = { (Mutil.empty_person empty_string quest_string) with key_index = ip }
and no_union = { Def.family = [||] }
and open_base _ = assert false
and patch_ascend _ = assert false
and patch_couple _ = assert false
and patch_descend _ = assert false
and patch_family _ = assert false
and patch_person _ = assert false
and patch_union _ = assert false
and person_of_gen_person _ = assert false
and person_of_key _ = assert false
and persons _ = assert false
and persons_of_first_name _ = assert false
and persons_of_name _ = assert false
and persons_of_surname _ = assert false
and poi base i = (base, i)
and quest_string = 1
and read_nldb _ = assert false
and sou _ = assert false
and spi_find _ = assert false
and spi_first _ = assert false
and spi_next _ = assert false
and string_of_ifam = string_of_int
and string_of_iper = string_of_int
and string_of_istr = string_of_int
and sync ?scratch:_ = assert false
and write_nldb _ = assert false

let no_couple = Adef.couple dummy_iper dummy_iper
