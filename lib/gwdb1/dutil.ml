(* $Id: dutil.ml,v 5.12 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk
open Def
open Type

type name_index_data = iper array array
type strings_of_fsname = istr array array

let magic_gwb = "GnWb0020"
let magic_gwb_iso_8859_1 = "GnWb001y"
let table_size = 0x3fff

let poi base i = base.data.persons.get (Type.int_of_iper i)
let aoi base i = base.data.ascends.get (Type.int_of_iper i)
let uoi base i = base.data.unions.get (Type.int_of_iper i)
let coi base i = base.data.couples.get (Type.int_of_ifam i)
let sou base i = base.data.strings.get (Type.int_of_istr i)

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

let check_magic ic =
  let b = really_input_string ic (String.length magic_gwb) in
  if b <> magic_gwb then
    if b = magic_gwb_iso_8859_1
    then failwith "this is a iso_8859_1 GeneWeb base, but you need utf-8"
    else if String.sub magic_gwb 0 4 = String.sub b 0 4
    then failwith "this is a GeneWeb base, but not compatible"
    else failwith "this is not a GeneWeb base, or it is a very old version"

let compare_names base_data s1 s2 =
  Mutil.compare_after_particle base_data.particles s1 s2

let compare_istr_fun base_data is1 is2 =
  if is1 = is2 then 0
  else
    compare_names base_data (base_data.strings.get (Type.int_of_istr is1))
      (base_data.strings.get (Type.int_of_istr is2))
