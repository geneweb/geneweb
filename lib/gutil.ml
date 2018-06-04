(* $Id: gutil.ml,v 5.52 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb
open Mutil

let string_sub s i len =
  let i = min (String.length s) (max 0 i) in
  let len = min (String.length s - i) (max 0 len) in String.sub s i len

let designation base p =
  let first_name = p_first_name base p in
  let nom = p_surname base p in
  iso_8859_1_of_utf_8
    (first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ nom)

let father = Adef.father
let mother = Adef.mother
let couple multi fath moth =
  if not multi then Adef.couple fath moth else Adef.multi_couple fath moth
let parent_array = Adef.parent_array

let spouse ip cpl =
  if ip = get_father cpl then get_mother cpl else get_father cpl

let person_ht_add base s ip = patch_name base s ip

let person_is_key base p k =
  let k = Name.crush_lower k in
  if k = Name.crush_lower (p_first_name base p ^ " " ^ p_surname base p) then
    true
  else if
    List.exists (fun x -> k = Name.crush_lower x)
      (person_misc_names base p get_titles)
  then
    true
  else false

let find_num s i =
  let rec loop start i =
    if i = String.length s then None
    else
      match s.[i] with
        '0'..'9' -> loop start (i + 1)
      | c ->
          if i = start then
            if c = ' ' then loop (start + 1) (start + 1) else None
          else Some (int_of_string (String.sub s start (i - start)), i)
  in
  loop i i

let split_key s i =
  let rec loop i =
    if i = String.length s then None
    else if s.[i] = '.' then
      match find_num s (i + 1) with
        Some (occ, j) ->
          let first_name = String.sub s 0 i in
          let surname = String.sub s j (String.length s - j) in
          Some (i, first_name, occ, surname)
      | None -> loop (i + 1)
    else loop (i + 1)
  in
  loop i

let person_of_string_key base s =
  let rec loop i =
    match split_key s i with
      Some (i, first_name, occ, surname) ->
        begin match person_of_key base first_name surname occ with
          Some ip -> Some ip
        | None -> loop (i + 1)
        end
    | None -> None
  in
  loop 0

let rsplit_key s =
  let rec loop i =
    if i = 0 then None
    else if s.[i] = '.' then
      match find_num s (i + 1) with
        Some (occ, j) ->
          let first_name = String.sub s 0 i in
          let surname = String.sub s j (String.length s - j) in
          Some (first_name, occ, surname)
      | None -> loop (i - 1)
    else loop (i - 1)
  in
  loop (String.length s - 1)

let person_of_string_dot_key base s =
  match rsplit_key s with
    Some (first_name, occ, surname) ->
      person_of_key base first_name surname occ
  | None -> None

let person_not_a_key_find_all base s =
  let ipl = persons_of_name base s in
  let rec select =
    function
      ip :: ipl ->
        if person_is_key base (poi base ip) s then
          let ipl = select ipl in if List.mem ip ipl then ipl else ip :: ipl
        else select ipl
    | [] -> []
  in
  select ipl

let person_ht_find_all base s =
  match person_of_string_key base s with
    Some p -> [p]
  | None -> person_not_a_key_find_all base s

let find_same_name base p =
  let f = p_first_name base p in
  let s = p_surname base p in
  let ipl = person_ht_find_all base (f ^ " " ^ s) in
  let f = Name.strip_lower f in
  let s = Name.strip_lower s in
  let pl =
    List.fold_left
      (fun pl ip ->
         let p = poi base ip in
         if Name.strip_lower (p_first_name base p) = f &&
            Name.strip_lower (p_surname base p) = s
         then
           p :: pl
         else pl)
      [] ipl
  in
  List.sort (fun p1 p2 -> compare (get_occ p1) (get_occ p2)) pl

let gen_strip_spaces strip_heading str =
  let start =
    if strip_heading then
      let rec loop i =
        if i = String.length str then i
        else
          match str.[i] with
            ' ' | '\r' | '\n' | '\t' -> loop (i + 1)
          | _ -> i
      in
      loop 0
    else 0
  in
  let stop =
    let rec loop i =
      if i = -1 then i + 1
      else
        match str.[i] with
          ' ' | '\r' | '\n' | '\t' -> loop (i - 1)
        | _ -> i + 1
    in
    loop (String.length str - 1)
  in
  if start = 0 && stop = String.length str then str
  else if start > stop then ""
  else String.sub str start (stop - start)

let strip_spaces = gen_strip_spaces true

let alphabetic_utf_8 n1 n2 =
  let rec loop i1 i2 =
    if i1 >= String.length n1 && i2 >= String.length n2 then i1 - i2
    else if i1 >= String.length n1 then -1
    else if i2 >= String.length n2 then 1
    else
      let (cv1, ii1) = Name.unaccent_utf_8 false n1 i1 in
      let (cv2, ii2) = Name.unaccent_utf_8 false n2 i2 in
      let c =
        if cv1 = cv2 then
          compare (String.sub n1 i1 (ii1 - i1)) (String.sub n2 i2 (ii2 - i2))
        else compare cv1 cv2
      in
      if c = 0 then loop ii1 ii2 else c
  in
  if n1 = n2 then 0 else loop 0 0

let alphabetic_value =
  let tab = Array.make 256 0 in
  for i = 0 to 255 do tab.(i) <- 10 * i done;
  tab.(Char.code 'à') <- tab.(Char.code 'a') + 1;
  tab.(Char.code 'á') <- tab.(Char.code 'a') + 2;
  tab.(Char.code 'â') <- tab.(Char.code 'a') + 3;
  tab.(Char.code 'è') <- tab.(Char.code 'e') + 1;
  tab.(Char.code 'é') <- tab.(Char.code 'e') + 2;
  tab.(Char.code 'ê') <- tab.(Char.code 'e') + 3;
  tab.(Char.code 'ë') <- tab.(Char.code 'e') + 4;
  tab.(Char.code 'ô') <- tab.(Char.code 'o') + 1;
  tab.(Char.code 'Á') <- tab.(Char.code 'A') + 2;
  tab.(Char.code 'Æ') <- tab.(Char.code 'A') + 5;
  tab.(Char.code 'È') <- tab.(Char.code 'E') + 1;
  tab.(Char.code 'É') <- tab.(Char.code 'E') + 2;
  tab.(Char.code 'Ö') <- tab.(Char.code 'O') + 4;
  tab.(Char.code '?') <- 3000;
  fun x -> tab.(Char.code x)

let alphabetic_iso_8859_1 n1 n2 =
  let rec loop i1 i2 =
    if i1 = String.length n1 && i2 = String.length n2 then i1 - i2
    else if i1 = String.length n1 then -1
    else if i2 = String.length n2 then 1
    else
      let c1 = n1.[i1] in
      let c2 = n2.[i2] in
      if alphabetic_value c1 < alphabetic_value c2 then -1
      else if alphabetic_value c1 > alphabetic_value c2 then 1
      else loop (succ i1) (succ i2)
  in
  if n1 = n2 then 0 else loop (initial n1) (initial n2)

let alphabetic n1 n2 =
  (*
    if utf_8_db.val then alphabetic_utf_8 n1 n2 else alphabetic_iso_8859_1 n1 n2
  *)
  alphabetic_iso_8859_1 n1 n2

let alphabetic_order n1 n2 =
  if !utf_8_db then alphabetic_utf_8 n1 n2 else alphabetic_iso_8859_1 n1 n2

let arg_list_of_string line =
  let rec loop list i len quote =
    if i = String.length line then
      if len = 0 then List.rev list else List.rev (Buff.get len :: list)
    else
      match quote, line.[i] with
        Some c1, c2 ->
          if c1 = c2 then loop list (i + 1) len None
          else loop list (i + 1) (Buff.store len c2) quote
      | None, ' ' ->
          let list = if len = 0 then list else Buff.get len :: list in
          loop list (i + 1) 0 quote
      | None, ('"' | '\'' as c) -> loop list (i + 1) 0 (Some c)
      | None, c -> loop list (i + 1) (Buff.store len c) None
  in
  loop [] 0 0 None

let sort_person_list base pl =
  List.sort
    (fun p1 p2 ->
       match
         Adef.od_of_codate (get_birth p1), get_death p1,
         Adef.od_of_codate (get_birth p2), get_death p2
       with
         Some d1, _, Some d2, _ ->
           if CheckItem.strictly_before d1 d2 then -1 else 1
       | Some d1, _, _, Death (_, d2) ->
           if CheckItem.strictly_before d1 (Adef.date_of_cdate d2) then -1
           else 1
       | _, Death (_, d1), Some d2, _ ->
           if CheckItem.strictly_before (Adef.date_of_cdate d1) d2 then -1
           else 1
       | _, Death (_, d1), _, Death (_, d2) ->
           if CheckItem.strictly_before (Adef.date_of_cdate d1)
                (Adef.date_of_cdate d2)
           then
             -1
           else 1
       | Some _, _, _, _ -> 1
       | _, Death (_, _), _, _ -> 1
       | _, _, Some _, _ -> -1
       | _, _, _, Death (_, _) -> -1
       | _ ->
           let c = alphabetic (p_surname base p1) (p_surname base p2) in
           if c = 0 then
             let c =
               alphabetic (p_first_name base p1) (p_first_name base p2)
             in
             if c = 0 then compare (get_occ p1) (get_occ p2) else c
           else c)
    pl

let find_free_occ base f s i =
  let ipl = persons_of_name base (f ^ " " ^ s) in
  let first_name = Name.lower f in
  let surname = Name.lower s in
  let list_occ =
    let rec loop list =
      function
        ip :: ipl ->
          let p = poi base ip in
          if not (List.mem (get_occ p) list) &&
             first_name = Name.lower (p_first_name base p) &&
             surname = Name.lower (p_surname base p)
          then
            loop (get_occ p :: list) ipl
          else loop list ipl
      | [] -> list
    in
    loop [] ipl
  in
  let list_occ = List.sort compare list_occ in
  let rec loop cnt1 =
    function
      cnt2 :: list -> if cnt1 = cnt2 then loop (cnt1 + 1) list else cnt1
    | [] -> cnt1
  in
  loop 0 list_occ
