(* Copyright (c) 1998-2007 INRIA *)

open Def

let designation base p =
  let first_name = Driver.p_first_name base p in
  let nom = Driver.p_surname base p in
  first_name ^ "." ^ string_of_int (Driver.get_occ p) ^ " " ^ nom

let father = Adef.father
let mother = Adef.mother

let couple multi fath moth =
  if not multi then Adef.couple fath moth else Adef.multi_couple fath moth

let parent_array = Adef.parent_array

let spouse ip cpl =
  if ip = Driver.get_father cpl then Driver.get_mother cpl
  else Driver.get_father cpl

let person_is_key base p k =
  let k = Name.crush_lower k in
  if
    k
    = Name.crush_lower
        (Driver.p_first_name base p ^ " " ^ Driver.p_surname base p)
  then true
  else if
    List.exists
      (fun x -> k = Name.crush_lower x)
      (Driver.person_misc_names base p Driver.get_titles)
  then true
  else false

let find_num s i =
  let rec loop start i =
    if i = String.length s then None
    else
      match s.[i] with
      | '0' .. '9' -> loop start (i + 1)
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
      | Some (occ, j) ->
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
    | Some (i, first_name, occ, surname) -> (
        match Driver.person_of_key base first_name surname occ with
        | Some ip -> Some ip
        | None -> loop (i + 1))
    | None -> None
  in
  loop 0

let rsplit_key s =
  let rec loop i =
    if i = 0 then None
    else if s.[i] = '.' then
      match find_num s (i + 1) with
      | Some (occ, j) ->
          let first_name = String.sub s 0 i in
          let surname = String.sub s j (String.length s - j) in
          Some (first_name, occ, surname)
      | None -> loop (i - 1)
    else loop (i - 1)
  in
  loop (String.length s - 1)

let person_of_string_dot_key base s =
  match rsplit_key s with
  | Some (first_name, occ, surname) ->
      Driver.person_of_key base first_name surname occ
  | None -> None

let person_not_a_key_find_all base s =
  let ipl = Driver.persons_of_name base s in
  let rec select = function
    | ip :: ipl ->
        if person_is_key base (Driver.poi base ip) s then
          let ipl = select ipl in
          if List.mem ip ipl then ipl else ip :: ipl
        else select ipl
    | [] -> []
  in
  select ipl

let person_ht_find_all base s =
  match person_of_string_key base s with
  | Some p -> [ p ]
  | None -> person_not_a_key_find_all base s

let find_same_name base p =
  let f = Driver.p_first_name base p in
  let s = Driver.p_surname base p in
  let ipl = person_ht_find_all base (f ^ " " ^ s) in
  let f = Name.strip_lower f in
  let s = Name.strip_lower s in
  let pl =
    List.fold_left
      (fun pl ip ->
        let p = Driver.poi base ip in
        if
          Name.strip_lower (Driver.p_first_name base p) = f
          && Name.strip_lower (Driver.p_surname base p) = s
        then p :: pl
        else pl)
      [] ipl
  in
  List.sort (fun p1 p2 -> compare (Driver.get_occ p1) (Driver.get_occ p2)) pl

let trim_trailing_spaces s =
  let len = String.length s in
  let len' =
    let rec loop i =
      if i = -1 then 0
      else
        match String.unsafe_get s i with
        | ' ' | '\r' | '\n' | '\t' -> loop (i - 1)
        | _ -> i + 1
    in
    loop (len - 1)
  in
  if len' = 0 then "" else if len' = len then s else String.sub s 0 len'

let alphabetic_utf_8 n1 n2 =
  let rec loop i1 i2 =
    if i1 >= String.length n1 && i2 >= String.length n2 then i1 - i2
    else if i1 >= String.length n1 then -1
    else if i2 >= String.length n2 then 1
    else
      let cv1, ii1 = Name.unaccent_utf_8 false n1 i1 in
      let cv2, ii2 = Name.unaccent_utf_8 false n2 i2 in
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
  for i = 0 to 255 do
    tab.(i) <- 10 * i
  done;
  tab.(Char.code '\xE0') <- (*'à'*) tab.(Char.code 'a') + 1;
  tab.(Char.code '\xE1') <- (*'á'*) tab.(Char.code 'a') + 2;
  tab.(Char.code '\xE2') <- (*'â'*) tab.(Char.code 'a') + 3;
  tab.(Char.code '\xE8') <- (*'è'*) tab.(Char.code 'e') + 1;
  tab.(Char.code '\xE9') <- (*'é'*) tab.(Char.code 'e') + 2;
  tab.(Char.code '\xEA') <- (*'ê'*) tab.(Char.code 'e') + 3;
  tab.(Char.code '\xEB') <- (*'ë'*) tab.(Char.code 'e') + 4;
  tab.(Char.code '\xF4') <- (*'ô'*) tab.(Char.code 'o') + 1;
  tab.(Char.code '\xC1') <- (*'Á'*) tab.(Char.code 'A') + 2;
  tab.(Char.code '\xC6') <- (*'Æ'*) tab.(Char.code 'A') + 5;
  tab.(Char.code '\xC8') <- (*'È'*) tab.(Char.code 'E') + 1;
  tab.(Char.code '\xC9') <- (*'É'*) tab.(Char.code 'E') + 2;
  tab.(Char.code '\xD6') <- (*'Ö'*) tab.(Char.code 'O') + 4;
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
  if n1 = n2 then 0 else loop (Mutil.initial n1) (Mutil.initial n2)

(* ??? *)
let alphabetic n1 n2 =
  (*
    if Mutil.utf_8_db.val then alphabetic_utf_8 n1 n2 else alphabetic_iso_8859_1 n1 n2
  *)
  alphabetic_iso_8859_1 n1 n2

let alphabetic_order n1 n2 = alphabetic_utf_8 n1 n2

let arg_list_of_string line =
  let rec loop list i len quote =
    if i = String.length line then
      if len = 0 then List.rev list else List.rev (Buff.get len :: list)
    else
      match (quote, line.[i]) with
      | Some c1, c2 ->
          if c1 = c2 then loop list (i + 1) len None
          else loop list (i + 1) (Buff.store len c2) quote
      | None, ' ' ->
          let list = if len = 0 then list else Buff.get len :: list in
          loop list (i + 1) 0 quote
      | None, (('"' | '\'') as c) -> loop list (i + 1) 0 (Some c)
      | None, c -> loop list (i + 1) (Buff.store len c) None
  in
  loop [] 0 0 None

let sort_person_list_aux sort base =
  let default p1 p2 =
    match alphabetic (Driver.p_surname base p1) (Driver.p_surname base p2) with
    | 0 -> (
        match
          alphabetic (Driver.p_first_name base p1) (Driver.p_first_name base p2)
        with
        | 0 -> (
            match compare (Driver.get_occ p1) (Driver.get_occ p2) with
            | 0 -> compare (Driver.get_iper p1) (Driver.get_iper p2)
            | c -> c)
        | c -> c)
    | c -> c
  in
  sort (fun p1 p2 ->
      if Driver.get_iper p1 = Driver.get_iper p2 then 0
      else
        match
          match
            ( Date.od_of_cdate (Driver.get_birth p1),
              Driver.get_death p1,
              Date.od_of_cdate (Driver.get_birth p2),
              Driver.get_death p2 )
          with
          | Some d1, _, Some d2, _ -> Date.compare_date d1 d2
          | Some d1, _, _, Death (_, d2) ->
              Date.compare_date d1 (Date.date_of_cdate d2)
          | _, Death (_, d1), Some d2, _ ->
              Date.compare_date (Date.date_of_cdate d1) d2
          | _, Death (_, d1), _, Death (_, d2) ->
              Date.compare_date (Date.date_of_cdate d1) (Date.date_of_cdate d2)
          | Some _, _, _, _ -> 1
          | _, Death (_, _), _, _ -> 1
          | _, _, Some _, _ -> -1
          | _, _, _, Death (_, _) -> -1
          | _ -> 0
        with
        | 0 -> default p1 p2
        | c -> c)

let sort_person_list = sort_person_list_aux List.sort
let sort_uniq_person_list = sort_person_list_aux List.sort_uniq

let find_free_occ base f s =
  let ipl = Driver.persons_of_name base (f ^ " " ^ s) in
  let first_name = Name.lower f in
  let surname = Name.lower s in
  let list_occ =
    let rec loop list = function
      | ip :: ipl ->
          let p = Driver.poi base ip in
          if
            (not (List.mem (Driver.get_occ p) list))
            && first_name = Name.lower (Driver.p_first_name base p)
            && surname = Name.lower (Driver.p_surname base p)
          then loop (Driver.get_occ p :: list) ipl
          else loop list ipl
      | [] -> list
    in
    loop [] ipl
  in
  let list_occ = List.sort compare list_occ in
  let rec loop cnt1 = function
    | cnt2 :: list -> if cnt1 = cnt2 then loop (cnt1 + 1) list else cnt1
    | [] -> cnt1
  in
  loop 0 list_occ

let get_birth_death_date p =
  let birth_date, approx =
    match Date.od_of_cdate (Driver.get_birth p) with
    | None -> (Date.od_of_cdate (Driver.get_baptism p), true)
    | x -> (x, false)
  in
  let death_date, approx =
    match Date.date_of_death (Driver.get_death p) with
    | Some d -> (Some d, approx)
    | None -> (
        match Driver.get_burial p with
        | Buried cd | Cremated cd -> (Date.od_of_cdate cd, true)
        | UnknownBurial -> (None, approx))
  in
  (birth_date, death_date, approx)
