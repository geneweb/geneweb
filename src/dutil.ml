(* $Id: dutil.ml,v 5.12 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk;
open Def;
open Mutil;

type name_index_data = array (array iper);
type strings_of_fsname = array (array dsk_istr);

value magic_gwb = "GnWb0020";
value magic_gwb_iso_8859_1 = "GnWb001y";
value table_size = 0x3fff;

value poi base i = base.data.persons.get (Adef.int_of_iper i);
value aoi base i = base.data.ascends.get (Adef.int_of_iper i);
value uoi base i = base.data.unions.get (Adef.int_of_iper i);
value coi base i = base.data.couples.get (Adef.int_of_ifam i);
value sou base i = base.data.strings.get (Adef.int_of_istr i);

value p_first_name base p = nominative (sou base p.first_name);
value p_surname base p = nominative (sou base p.surname);

value husbands base p =
  let u = uoi base p.key_index in
  List.map
    (fun ifam ->
       let cpl = coi base ifam in
       let husband = poi base (Adef.father cpl) in
       let husband_surname = p_surname base husband in
       let husband_surnames_aliases =
         List.map (sou base) husband.surnames_aliases
       in
       (husband_surname, husband_surnames_aliases))
    (Array.to_list u.family)
;

value father_titles_places base p nobtit =
  match (aoi base p.key_index).parents with
  [ Some ifam ->
      let cpl = coi base ifam in
      let fath = poi base (Adef.father cpl) in
      List.map (fun t -> sou base t.t_place) (nobtit fath)
  | None -> [] ]
;

value dsk_person_misc_names base p nobtit =
  let sou = sou base in
  Futil.gen_person_misc_names (sou p.first_name) (sou p.surname)
    (sou p.public_name) (List.map sou p.qualifiers) (List.map sou p.aliases)
    (List.map sou p.first_names_aliases) (List.map sou p.surnames_aliases)
    (List.map (Futil.map_title_strings sou) (nobtit p))
    (if p.sex = Female then husbands base p else [])
    (father_titles_places base p nobtit)
;

value check_magic =
  let b = String.create (String.length magic_gwb) in
  fun ic ->
    do {
      really_input ic b 0 (String.length b);
      Mutil.utf_8_db.val := True;
      if b <> magic_gwb then
        if b = magic_gwb_iso_8859_1 then Mutil.utf_8_db.val := False
        else if String.sub magic_gwb 0 4 = String.sub b 0 4 then
          failwith "this is a GeneWeb base, but not compatible"
        else
          failwith "this is not a GeneWeb base, or it is a very old version"
      else ()
    }
;

value unaccent =
  fun
  [ 'à' | 'á' | 'â' | 'ã' | 'ä' | 'å' | 'æ' -> 'a'
  | 'ç' -> 'c'
  | 'è' | 'é' | 'ê' | 'ë' -> 'e'
  | 'ì' | 'í' | 'î' | 'ï' -> 'i'
  | 'ğ' -> 'd'
  | 'ñ' -> 'n'
  | 'ò' | 'ó' | 'ô' | 'õ' | 'ö' | 'ø' -> 'o'
  | 'ù' | 'ú' | 'û' | 'ü' -> 'u'
  | 'ı' | 'ÿ' -> 'y'
  | 'ş' -> 'p'
  | c -> c ]
;

value compare_names_1 s1 s2 =
  let compare_aux e1 e2 =
    loop where rec loop i1 i2 =
      if i1 = e1 && i2 = e2 then 0
      else if i1 = e1 then -1
      else if i2 = e2 then 1
      else
        let c1 = unaccent (Char.lowercase s1.[i1]) in
        let c2 = unaccent (Char.lowercase s2.[i2]) in
        match (c1, c2) with
        [ ('a'..'z', 'a'..'z') ->
            if c1 < c2 then -1
            else if c1 > c2 then 1
            else loop (i1 + 1) (i2 + 1)
        | ('a'..'z', _) -> 1
        | (_, 'a'..'z') -> -1
        | _ -> loop (i1 + 1) (i2 + 1) ]
  in
  if s1 = s2 then 0
  else
    let i1 = initial s1 in
    let i2 = initial s2 in
    match compare_aux (String.length s1) (String.length s2) i1 i2 with
    [ 0 -> compare_aux i1 i2 0 0
    | x -> x ]
;

value compare_names base_data s1 s2 =
  if utf_8_db.val then compare_after_particle base_data.particles s1 s2
  else compare_names_1 s1 s2
;

value compare_istr_fun base_data is1 is2 =
  if is1 = is2 then 0
  else
    compare_names base_data (base_data.strings.get (Adef.int_of_istr is1))
      (base_data.strings.get (Adef.int_of_istr is2))
;
