(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk
open Def

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
  Array.map
    (fun ifam ->
      let cpl = coi base ifam in
      let husband = poi base (Adef.father cpl) in
      let husband_surname = husband.surname in
      let husband_surnames_aliases = husband.surnames_aliases in
      (husband_surname, husband_surnames_aliases))
    (uoi base p.key_index).family

let father_titles_places base p nobtit =
  match (aoi base p.key_index).parents with
  | Some ifam ->
      let cpl = coi base ifam in
      let fath = poi base (Adef.father cpl) in
      nobtit fath
  | None -> []

let dsk_person_misc_names base p nobtit =
  Futil.gen_person_misc_names (sou base) 0 1 p.first_name p.surname
    p.public_name p.qualifiers p.aliases p.first_names_aliases
    p.surnames_aliases (nobtit p)
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
  else compare_fnames (base_data.strings.get is1) (base_data.strings.get is2)

let int_size = 4

let output_value_no_sharing oc v =
  Marshal.to_channel oc v [ Marshal.No_sharing ]

module IntHT = Hashtbl.Make (struct
  type t = int

  let equal = ( = )
  let hash x = x
end)

let name_index s = Hashtbl.hash (Name.crush_lower s) mod table_size
