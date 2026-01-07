(* Copyright (c) 1998-2007 INRIA *)

let designation base p =
  let first_name = Gwdb.p_first_name base p in
  let nom = Gwdb.p_surname base p in
  Utf8.iso_8859_1_of_utf_8
    (first_name ^ "." ^ string_of_int (Gwdb.get_occ p) ^ " " ^ nom)

let spouse ip cpl =
  if ip = Gwdb.get_father cpl then Gwdb.get_mother cpl else Gwdb.get_father cpl

let person_is_key base p k =
  let k = Name.crush_lower k in
  if
    k = Name.crush_lower (Gwdb.p_first_name base p ^ " " ^ Gwdb.p_surname base p)
  then true
  else if
    List.exists
      (fun x -> k = Name.crush_lower x)
      (Gwdb.person_misc_names base p Gwdb.get_titles)
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
        match Gwdb.person_of_key base first_name surname occ with
        | Some ip -> Some ip
        | None -> loop (i + 1))
    | None -> None
  in
  loop 0

let split_key s =
  Option.map (fun (_, fn, occ, sn) -> (fn, occ, sn)) (split_key s 0)

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
      Gwdb.person_of_key base first_name surname occ
  | None -> None

let person_not_a_key_find_all base s =
  let ipl = Gwdb.persons_of_name base s in
  let rec select = function
    | ip :: ipl ->
        if person_is_key base (Gwdb.poi base ip) s then
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
  let f = Gwdb.p_first_name base p in
  let s = Gwdb.p_surname base p in
  let ipl = person_ht_find_all base (f ^ " " ^ s) in
  let f = Name.strip_lower f in
  let s = Name.strip_lower s in
  let pl =
    List.fold_left
      (fun pl ip ->
        let p = Gwdb.poi base ip in
        if
          Name.strip_lower (Gwdb.p_first_name base p) = f
          && Name.strip_lower (Gwdb.p_surname base p) = s
        then p :: pl
        else pl)
      [] ipl
  in
  List.sort (fun p1 p2 -> compare (Gwdb.get_occ p1) (Gwdb.get_occ p2)) pl

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
    match
      Utf8.alphabetic_order (Gwdb.p_surname base p1) (Gwdb.p_surname base p2)
    with
    | 0 -> (
        match
          Utf8.alphabetic_order
            (Gwdb.p_first_name base p1)
            (Gwdb.p_first_name base p2)
        with
        | 0 -> (
            match compare (Gwdb.get_occ p1) (Gwdb.get_occ p2) with
            | 0 -> compare (Gwdb.get_iper p1) (Gwdb.get_iper p2)
            | c -> c)
        | c -> c)
    | c -> c
  in
  sort (fun p1 p2 ->
      if Gwdb.get_iper p1 = Gwdb.get_iper p2 then 0
      else
        match
          match
            ( Date.od_of_cdate (Gwdb.get_birth p1),
              Gwdb.get_death p1,
              Date.od_of_cdate (Gwdb.get_birth p2),
              Gwdb.get_death p2 )
          with
          | Some d1, _, Some d2, _ -> Date.compare_date d1 d2
          | Some d1, _, _, Def.Death (_, d2) ->
              Date.compare_date d1 (Date.date_of_cdate d2)
          | _, Def.Death (_, d1), Some d2, _ ->
              Date.compare_date (Date.date_of_cdate d1) d2
          | _, Def.Death (_, d1), _, Def.Death (_, d2) ->
              Date.compare_date (Date.date_of_cdate d1) (Date.date_of_cdate d2)
          | Some _, _, _, _ -> 1
          | _, Def.Death (_, _), _, _ -> 1
          | _, _, Some _, _ -> -1
          | _, _, _, Def.Death (_, _) -> -1
          | _ -> 0
        with
        | 0 -> default p1 p2
        | c -> c)

let sort_person_list = sort_person_list_aux List.sort
let sort_uniq_person_list = sort_person_list_aux List.sort_uniq

let homonyms ~base ~first_name ~surname =
  let ipl = Gwdb.persons_of_name base (first_name ^ " " ^ surname) in
  let first_name = Name.lower first_name in
  let surname = Name.lower surname in
  ipl
  |> List.filter_map (fun ip ->
      let p = Gwdb.poi base ip in
      Ext_option.return_if
        (first_name = Name.lower (Gwdb.p_first_name base p)
        && surname = Name.lower (Gwdb.p_surname base p))
        (fun () -> ip))

let get_all_occurrence_numbers ~base ~first_name ~surname =
  let ipl = homonyms ~base ~first_name ~surname in
  ipl
  |> List.map (fun ip ->
      let p = Gwdb.poi base ip in
      Gwdb.get_occ p)
  |> Ext_int.Set.of_list

let find_free_occ base f s =
  let occurrence_numbers =
    get_all_occurrence_numbers ~base ~first_name:f ~surname:s
  in
  Occurrence_number.smallest_free occurrence_numbers

let get_birth_death_date p =
  let birth_date, approx =
    match Date.od_of_cdate (Gwdb.get_birth p) with
    | None -> (Date.od_of_cdate (Gwdb.get_baptism p), true)
    | x -> (x, false)
  in
  let death_date, approx =
    match Date.date_of_death (Gwdb.get_death p) with
    | Some d -> (Some d, approx)
    | None -> (
        match Gwdb.get_burial p with
        | Def.Buried cd | Def.Cremated cd -> (Date.od_of_cdate cd, true)
        | Def.UnknownBurial -> (None, approx))
  in
  (birth_date, death_date, approx)
