(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk
open Def

type name_index_data = int array array
type strings_of_fsname = int array array

let magic_GnWb0020 = "GnWb0020"
let magic_GnWb0021 = "GnWb0021"
let table_size = 0x3fff

let poi base i = base.data.persons.get i
let aoi base i = base.data.ascends.get i
let uoi base i = base.data.unions.get i
let coi base i = base.data.couples.get i
let sou base i = base.data.strings.get i

let p_first_name base p = Mutil.nominative (sou base p.first_name)
let p_surname base p = Mutil.nominative (sou base p.surname)

let husbands base p =
  let u = uoi base p.key_index in
  List.map
    (fun ifam ->
       let cpl = coi base ifam in
       let husband = poi base (Adef.father cpl) in
       let husband_surname = p_surname base husband in
       let husband_surnames_aliases =
         List.map (sou base) husband.surnames_aliases
       in
       husband_surname, husband_surnames_aliases)
    (Array.to_list u.family)

let father_titles_places base p nobtit =
  match (aoi base p.key_index).parents with
    Some ifam ->
      let cpl = coi base ifam in
      let fath = poi base (Adef.father cpl) in
      List.map (fun t -> sou base t.t_place) (nobtit fath)
  | None -> []

let dsk_person_misc_names base p nobtit =
  let sou = sou base in
  Futil.gen_person_misc_names (sou p.first_name) (sou p.surname)
    (sou p.public_name) (List.map sou p.qualifiers) (List.map sou p.aliases)
    (List.map sou p.first_names_aliases) (List.map sou p.surnames_aliases)
    (List.map (Futil.map_title_strings sou) (nobtit p))
    (if p.sex = Female then husbands base p else [])
    (father_titles_places base p nobtit)

let compare_names base_data s1 s2 =
  Mutil.compare_after_particle base_data.particles s1 s2

let compare_istr_fun base_data is1 is2 =
  if is1 = is2 then 0
  else
    compare_names base_data
      (base_data.strings.get is1)
      (base_data.strings.get is2)

let int_size = 4

let output_value_no_sharing oc v =
  Marshal.to_channel oc v [Marshal.No_sharing]

let output_array_no_sharing oc arr_get arr_len =
  let header_pos = Iovalue.create_output_value_header oc in
  Iovalue.output_block_header oc 0 arr_len;
  for i = 0 to arr_len - 1 do Iovalue.output oc (arr_get i) done;
  let pos_end = Iovalue.patch_output_value_header oc header_pos in
  seek_out oc pos_end

module IntHT = Hashtbl.Make (struct
    type t = int
    let equal = (=)
    let hash x = x
  end)
