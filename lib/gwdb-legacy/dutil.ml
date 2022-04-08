(* Copyright (c) 2006-2007 INRIA *)

open Def
open Dbdisk

external identity : 'a -> 'a = "%identity"
                             
type name_index_data = int array array
type strings_of_fsname = int array array

let magic_GnWb0020 = "GnWb0020"
let magic_GnWb0021 = "GnWb0021"
let magic_GnWb0022 = "GnWb0022"
let magic_GnWb0023 = "GnWb0023"
let magic_GnWb0024 = "GnWb0024"
let table_size = 0x3fff

let poi base i = base.data.persons.get i
let aoi base i = base.data.ascends.get i
let uoi base i = base.data.unions.get i
let coi base i = base.data.couples.get i
let sou base i = base.data.strings.get i

let p_first_name base p = Mutil.nominative (sou base p.first_name)
let p_surname base p = Mutil.nominative (sou base p.surname)

let husbands base p =
  Array.map begin fun ifam ->
    let cpl = coi base ifam in
    let husband = poi base (Adef.father cpl) in
    let husband_surname = husband.surname in
    let husband_surnames_aliases = husband.surnames_aliases in
    husband_surname, husband_surnames_aliases
  end (uoi base p.key_index).family

let father_titles_places base p (nobtit : dsk_person -> dsk_title list) =
  match (aoi base p.key_index).parents with
  | Some ifam ->
    let cpl = coi base ifam in
    let fath = poi base (Adef.father cpl) in
    (nobtit fath)
  | None -> []

let dsk_person_misc_names :   dsk_base -> dsk_person -> (dsk_person -> dsk_title list) -> string list = fun base p nobtit ->
  Futil.gen_person_misc_names
    (sou base) 0 1
    p.first_name p.surname p.public_name p.qualifiers p.aliases
    p.first_names_aliases p.surnames_aliases
    (nobtit p)
    (if p.sex = Female then husbands base p else [||])
    (father_titles_places base p nobtit)

let compare_snames base_data s1 s2 =
  Mutil.compare_after_particle (Lazy.force base_data.particles) s1 s2

let compare_snames_i base_data is1 is2 =
  if is1 = is2 then 0
  else
    compare_snames base_data
      (base_data.strings.get is1)
      (base_data.strings.get is2)

let compare_fnames = String.compare

let compare_fnames_i base_data is1 is2 =
  if is1 = is2 then 0
  else
    compare_fnames
      (base_data.strings.get is1)
      (base_data.strings.get is2)

let int_size = 4

let output_value_no_sharing oc v =
  Marshal.to_channel oc v [Marshal.No_sharing]

module IntHT = Hashtbl.Make (struct
    type t = int
    let equal = (=)
    let hash x = x
  end)

let name_index s = Hashtbl.hash (Name.crush_lower s) mod table_size

let empty_person empty what =
  { Dbdisk.first_name = what
  ; surname = what
  ; occ = 0
  ; public_name = empty
  ; image = empty
  ; qualifiers = []
  ; aliases = []
  ; first_names_aliases = []
  ; surnames_aliases = []
  ; titles = []
  ; rparents = []
  ; related = []
  ; occupation = empty
  ; sex = Neuter
  ; access = IfTitles
  ; birth = Adef.cdate_None
  ; birth_place = empty
  ; birth_note = empty
  ; birth_src = empty
  ; baptism = Adef.cdate_None
  ; baptism_place = empty
  ; baptism_note = empty
  ; baptism_src = empty
  ; death = DontKnowIfDead
  ; death_place = empty
  ; death_note = empty
  ; death_src = empty
  ; burial = UnknownBurial
  ; burial_place = empty
  ; burial_note = empty
  ; burial_src = empty
  ; pevents = []
  ; notes = empty
  ; psources = empty
  ; key_index = ()
  }

let map_person_ps ?(fd = identity) fp fs p =
  { first_name = fs p.first_name
  ; surname = fs p.surname
  ; occ = p.occ
  ; image = fs p.image
  ; first_names_aliases = List.map fs p.first_names_aliases
  ; surnames_aliases = List.map fs p.surnames_aliases
  ; public_name = fs p.public_name
  ; qualifiers = List.map fs p.qualifiers
  ; titles = List.map (Futil.map_title_strings ~fd fs) p.titles
  ; rparents = List.map (Futil.map_relation_ps fp fs) p.rparents
  ; related = List.map fp p.related
  ; aliases = List.map fs p.aliases
  ;  occupation = fs p.occupation
  ; sex = p.sex
  ; access = p.access
  ; birth = Futil.map_cdate fd p.birth
  ; birth_place = fs p.birth_place
  ; birth_note = fs p.birth_note
  ; birth_src = fs p.birth_src
  ; baptism = Futil.map_cdate fd p.baptism
  ; baptism_place = fs p.baptism_place
  ; baptism_note = fs p.baptism_note
  ; baptism_src = fs p.baptism_src
  ; death = Futil.map_death fd p.death
  ; death_place = fs p.death_place
  ; death_note = fs p.death_note
  ; death_src = fs p.death_src
  ; burial = Futil.map_burial fd p.burial
  ; burial_place = fs p.burial_place
  ; burial_note = fs p.burial_note
  ; burial_src = fs p.burial_src
  ; pevents = List.map (Futil.map_pers_event ~fd fp fs) p.pevents
  ; notes = fs p.notes
  ; psources = fs p.psources
  ; key_index = p.key_index
  }
