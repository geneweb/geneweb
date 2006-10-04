(* $Id: dutil.ml,v 5.2 2006-10-04 21:26:12 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Config;
open Dbdisk;
open Def;
open Mutil;

type name_index_data = array (array iper);
type strings_of_fsname = array (array istr);

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

value dsk_person_misc_names base p nobtit =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if first_name = "?" || surname = "?" then []
  else
    let public_names =
      let titles_names =
        let tnl = ref [] in
        do {
          List.iter
            (fun t ->
               match t.t_name with
               [ Tmain | Tnone -> ()
               | Tname x -> tnl.val := [x :: tnl.val] ])
            (nobtit p);
          tnl.val
        }
      in
      if sou base p.public_name = "" || nobtit p = [] then titles_names
      else [p.public_name :: titles_names]
    in
    let first_names =
      let pn =
        if sou base p.public_name <> "" && nobtit p = [] then
          [p.public_name :: public_names]
        else public_names
      in
      [first_name :: List.map (sou base) (p.first_names_aliases @ pn)]
    in
    let surnames =
      [surname ::
       surnames_pieces surname @
         List.map (sou base) (p.surnames_aliases @ p.qualifiers)]
    in
    let surnames =
      if p.sex == Female then
        let u = uoi base p.key_index in
        List.fold_left
          (fun list ifam ->
             let cpl = coi base ifam in
             let husband = poi base (Adef.father cpl) in
             let husband_surname = p_surname base husband in
             let husband_surnames_aliases =
               List.map (sou base) husband.surnames_aliases
             in
             if p_surname base husband = "?" then
               husband_surnames_aliases @ list
             else
               [husband_surname ::
                surnames_pieces husband_surname @ husband_surnames_aliases @
                  list])
          surnames (Array.to_list u.family)
      else surnames
    in
    let list = [] in
    let list =
      List.fold_left (fun list s -> [sou base s :: list]) list public_names
    in
    let list =
      List.fold_left
        (fun list f ->
           List.fold_left (fun list s -> [f ^ " " ^ s :: list]) list surnames)
        list first_names
    in
    let list =
      let first_names =
        [first_name :: List.map (sou base) (p.first_names_aliases)]
      in
      List.fold_left
        (fun list t ->
           let s = sou base t.t_place in
           if s = "" then list
           else
             let first_names =
               match t.t_name with
               [ Tname f -> [sou base f :: first_names]
               | Tmain | Tnone ->
                   let f = sou base (p.public_name) in
                   if f = "" then first_names else [f :: first_names] ]
             in
             List.fold_left (fun list f -> [f ^ " " ^ s :: list]) list
               first_names)
        list (nobtit p)
    in
    let list =
      match (aoi base p.key_index).parents with
      [ Some ifam ->
          let cpl = coi base ifam in
          let fath = poi base (Adef.father cpl) in
          let first_names =
            [first_name :: List.map (sou base) (p.first_names_aliases)]
          in
          List.fold_left
            (fun list t ->
               let s = sou base t.t_place in
               if s = "" then list
               else
                 List.fold_left (fun list f -> [f ^ " " ^ s :: list]) list
                   first_names)
            list (nobtit fath)
      | _ -> list ]
    in
    let list =
      List.fold_left (fun list s -> [sou base s :: list]) list p.aliases
    in
    let fn = Name.lower (first_name ^ " " ^ surname) in
    List.fold_left
      (fun list s ->
         let s = Name.lower s in
         if s = fn || List.mem s list then list else [s :: list])
      [] list
;

value dsk_nobtit conf base p =
  match Lazy.force conf.allowed_titles with
  [ [] -> p.titles
  | allowed_titles ->
      List.fold_right
        (fun t l ->
           let id = sou base t.t_ident in
           let pl = sou base t.t_place in
           if List.mem (id ^ "/" ^ pl) allowed_titles then [t :: l] else l)
        p.titles [] ]
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
  | 'ð' -> 'd'
  | 'ñ' -> 'n'
  | 'ò' | 'ó' | 'ô' | 'õ' | 'ö' | 'ø' -> 'o'
  | 'ù' | 'ú' | 'û' | 'ü' -> 'u'
  | 'ý' | 'ÿ' -> 'y'
  | 'þ' -> 'p'
  | c -> c ]
;

value compare_names_1 s1 s2 =
  let compare_aux e1 e2 =
    loop where rec loop i1 i2 =
      if i1 == e1 && i2 == e2 then 0
      else if i1 == e1 then -1
      else if i2 == e2 then 1
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

value start_with ini s =
  loop 0 0 where rec loop i j =
    if i = String.length ini then True
    else if j = String.length s then False
    else if String.unsafe_get ini i = String.unsafe_get s j then
      loop (i + 1) (j + 1)
    else False
;

value get_particle s =
  loop where rec loop =
    fun
    [ [part :: parts] -> if start_with part s then part else loop parts
    | [] -> "" ]
;

value compare_part particles s1 s2 =
  let p1 = get_particle s1 particles in
  let p2 = get_particle s2 particles in
  loop (String.length p1) (String.length p2) where rec loop i1 i2 =
    if i1 = String.length s1 && i2 = String.length s2 then compare p1 p2
    else if i1 = String.length s1 then -1
    else if i2 = String.length s2 then 1
    else
      let c1 = String.unsafe_get s1 i1 in
      let c2 = String.unsafe_get s2 i2 in
      if c1 < c2 then -1
      else if c1 > c2 then 1
      else loop (i1 + 1) (i2 + 1)
;

value compare_names base_data s1 s2 =
  if utf_8_db.val then compare_part base_data.particles s1 s2
  else compare_names_1 s1 s2
;

value compare_istr_fun base_data is1 is2 =
  if is1 == is2 then 0
  else
    compare_names base_data (base_data.strings.get (Adef.int_of_istr is1))
      (base_data.strings.get (Adef.int_of_istr is2))
;
