(* $Id: gutil.ml,v 5.27 2006-10-01 11:30:07 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Def;
open Gwdb;
open Mutil;

value lindex s c =
  pos 0 where rec pos i =
    if i == String.length s then None
    else if s.[i] == c then Some i
    else pos (i + 1)
;

value list_iter_first f al =
  let _ =
    List.fold_left (fun first a -> let () = f first a in False) True al
  in
  ()
;

value string_sub s i len =
  let i = min (String.length s) (max 0 i) in
  let len = min (String.length s - i) (max 0 len) in String.sub s i len
;

value tr c1 c2 s =
  match rindex s c1 with
  [ Some _ ->
      let s' = String.create (String.length s) in
      do {
        for i = 0 to String.length s - 1 do {
          s'.[i] := if s.[i] = c1 then c2 else s.[i]
        };
        s'
      }
  | None -> s ]
;

value utf_8_of_iso_8859_1 str =
  loop 0 0 where rec loop i len =
    if i = String.length str then Buff.get len
    else
      let c = str.[i] in
      if Char.code c < 0x80 then loop (i + 1) (Buff.store len c)
      else if Char.code c < 0xC0 then
        let len = Buff.store len (Char.chr 0xC2) in
        loop (i + 1) (Buff.store len c)
      else 
        let len = Buff.store len (Char.chr 0xC3) in
        loop (i + 1) (Buff.store len (Char.chr (Char.code c - 0x40)))
;

value iso_8859_1_of_utf_8 s =
  loop 0 0 where rec loop i len =
    if i == String.length s then Buff.get len
    else
      let c = s.[i] in
      match Char.code c with
      [ 0xC2 when i + 1 < String.length s ->
          loop (i + 2) (Buff.store len s.[i+1])
      | 0xC3 when i + 1 < String.length s ->
          loop (i + 2) (Buff.store len (Char.chr (Char.code s.[i+1] + 0x40)))
      | _ -> loop (i + 1) (Buff.store len c) ]
;

value utf_8_intern_byte c =
  utf_8_db.val && Char.code c >= 0x80 && Char.code c < 0xC0;

value leap_year a =
  if a mod 100 == 0 then a / 100 mod 4 == 0 else a mod 4 == 0
;

value nb_days_in_month =
  let tb = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |] in
  fun m a ->
    if m == 2 && leap_year a then 29
    else if m >= 1 && m <= 12 then tb.(m - 1)
    else 0
;

value common_prec p1 p2 =
  if p1 = p2 then p1
  else
    match (p1, p2) with
    [ (Sure, _) -> p2
    | (_, Sure) -> p1
    | _ -> Maybe ]
;

value time_gone_by d1 d2 =
  let prec = common_prec d1.prec d2.prec in
  match d1 with
  [ {day = 0; month = 0; year = a1} ->
      {day = 0; month = 0; year = d2.year - a1; prec = prec; delta = 0}
  | {day = 0; month = m1; year = a1} ->
      match d2 with
      [ {day = 0; month = 0; year = a2} ->
          {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
      | {day = 0; month = m2; year = a2} ->
          let r = 0 in
          let (month, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          {day = 0; month = month; year = year; prec = prec; delta = 0}
      | {day = j2; month = m2; year = a2} ->
          let r = 0 in
          let (month, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          {day = 0; month = month; year = year; prec = prec; delta = 0} ]
  | {day = j1; month = m1; year = a1} ->
      match d2 with
      [ {day = 0; month = 0; year = a2} ->
          {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
      | {day = 0; month = m2; year = a2} ->
          let r = 0 in
          let (month, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          {day = 0; month = month; year = year; prec = prec; delta = 0}
      | {day = j2; month = m2; year = a2} ->
          let (day, r) =
            if j1 <= j2 then (j2 - j1, 0)
            else (j2 - j1 + nb_days_in_month m1 a1, 1)
          in
          let (month, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          {day = day; month = month; year = year; prec = prec; delta = 0} ] ]
;

value year_of d = d.year;

value strictly_before_dmy d1 d2 =
  let {day = d; month = m; year = y; prec = p} = time_gone_by d2 d1 in
  if y < 0 then True
  else if y > 0 then False
  else if m < 0 then True
  else if m > 0 then False
  else if d < 0 then True
  else if d > 0 then False
  else if d1.prec = d2.prec then False
  else if d1.prec = Before && d2.prec = After then True
  else False
;

value strictly_before d1 d2 =
  match (d1, d2) with
  [ (Dgreg d1 _, Dgreg d2 _) -> strictly_before_dmy d1 d2
  | _ -> False ]
;

value strictly_after_dmy d1 d2 =
  let {day = d; month = m; year = y; prec = p} = time_gone_by d1 d2 in
  if y < 0 then True
  else if y > 0 then False
  else if m < 0 then True
  else if m > 0 then False
  else if d < 0 then True
  else if d > 0 then False
  else if d2.prec = d1.prec then False
  else if d2.prec = Before && d1.prec = After then True
  else False
;

value strictly_after d1 d2 =
  match (d1, d2) with
  [ (Dgreg d1 _, Dgreg d2 _) -> strictly_after_dmy d1 d2
  | _ -> False ]
;

value designation base p =
  let first_name = p_first_name base p in
  let nom = p_surname base p in
  iso_8859_1_of_utf_8
    (first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ nom)
;

value father = Adef.father;
value mother = Adef.mother;
value couple multi fath moth =
  if not multi then Adef.couple fath moth else Adef.multi_couple fath moth
;
value parent multi parent =
  if not multi then Adef.parent parent else Adef.multi_parent parent
;
value parent_array = Adef.parent_array;

value no_ascend () =
  ascend_of_gen_ascend {parents = None; consang = Adef.fix (-1)}
;

value spouse ip cpl =
  if ip == get_father cpl then get_mother cpl
  else get_father cpl
;

value person_ht_add base s ip = patch_name base s ip;

value person_is_key base p k =
  let k = Name.crush_lower k in
  if k = Name.crush_lower (p_first_name base p ^ " " ^ p_surname base p) then
    True
  else if
    List.exists (fun x -> k = Name.crush_lower x)
      (person_misc_names base p get_titles) then
    True
  else False
;

value person_ht_find_unique base first_name surname occ =
  if first_name = "?" || surname = "?" then raise Not_found
  else
    let first_name = nominative first_name in
    let surname = nominative surname in
    let ipl = persons_of_name base (first_name ^ " " ^ surname) in
    let first_name = Name.lower first_name in
    let surname = Name.lower surname in
    let rec find =
      fun
      [ [ip :: ipl] ->
          let p = poi base ip in
          if occ == get_occ p &&
             first_name = Name.lower (p_first_name base p) &&
             surname = Name.lower (p_surname base p)
          then
            get_key_index p
          else find ipl
      | _ -> raise Not_found ]
    in
    find ipl
;

value find_num s i =
  loop i i where rec loop start i =
    if i == String.length s then None
    else
      match s.[i] with
      [ '0'..'9' -> loop start (i + 1)
      | c ->
          if i == start then
            if c = ' ' then loop (start + 1) (start + 1) else None
          else Some (int_of_string (String.sub s start (i - start)), i) ]
;

value split_key s =
  match lindex s '.' with
  [ Some i ->
      match find_num s (i + 1) with
      [ Some (occ, j) ->
          let first_name = String.sub s 0 i in
          let surname = String.sub s j (String.length s - j) in
          (first_name, occ, surname)
      | None -> raise Not_found ]
  | None -> raise Not_found ]
;

value person_of_key base s =
  try
    let (first_name, occ, surname) = split_key s in
    Some (person_ht_find_unique base first_name surname occ)
  with
  [ Not_found -> None ]
;

value person_ht_find_all base s =
  match person_of_key base s with
  [ Some p ->
      [p]
  | _ ->
      let ipl = persons_of_name base s in
      let rec select =
        fun
        [ [ip :: ipl] ->
            if person_is_key base (poi base ip) s then
              let ipl = select ipl in
              if List.mem ip ipl then ipl else [ip :: ipl]
            else select ipl
        | [] -> [] ]
      in
      select ipl ]
;

value find_same_name base p =
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
            Name.strip_lower (p_surname base p) = s then
           [p :: pl]
         else pl)
      [] ipl
  in
  List.sort (fun p1 p2 -> compare (get_occ p1) (get_occ p2)) pl
;

(* check base *)

type base_error = error person;
type base_warning = warning person descend title;

type visit = [ NotVisited | BeingVisited | Visited ];

value check_noloop base error =
  let tab = Array.create (nb_of_persons base) NotVisited in
  let rec noloop i =
    match tab.(i) with
    [ NotVisited ->
        do {
          match get_parents (aoi base (Adef.iper_of_int i)) with
          [ Some fam ->
              let fath = get_father (coi base fam) in
              let moth = get_mother (coi base fam) in
              do {
                tab.(i) := BeingVisited;
                noloop (Adef.int_of_iper fath);
                noloop (Adef.int_of_iper moth);
                ()
              }
          | None -> () ];
          tab.(i) := Visited;
        }
    | BeingVisited -> error (OwnAncestor (poi base (Adef.iper_of_int i)))
    | Visited -> () ]
  in
  for i = 0 to nb_of_persons base - 1 do {
    match tab.(i) with
    [ NotVisited -> noloop i
    | BeingVisited -> failwith "check_noloop algorithm error"
    | Visited -> () ]
  }
;

value check_noloop_for_person_list base error ipl =
  let tab = Array.create (nb_of_persons base) NotVisited in
  let rec noloop ip =
    let i = Adef.int_of_iper ip in
    match tab.(i) with
    [ NotVisited ->
        do {
          match get_parents (aoi base ip) with
          [ Some ifam ->
              let cpl = coi base ifam in
              do {
                tab.(i) := BeingVisited;
                Array.iter noloop (get_parent_array cpl);
              }
          | None -> () ];
          tab.(i) := Visited;
        }
    | BeingVisited -> error (OwnAncestor (poi base ip))
    | Visited -> () ]
  in
  List.iter noloop ipl
;

value date_of_death =
  fun
  [ Death _ cd -> Some (Adef.date_of_cdate cd)
  | _ -> None ]
;

value roman_of_arabian n =
  let build one five ten =
    fun
    [ 0 -> ""
    | 1 -> one
    | 2 -> one ^ one
    | 3 -> one ^ one ^ one
    | 4 -> one ^ five
    | 5 -> five
    | 6 -> five ^ one
    | 7 -> five ^ one ^ one
    | 8 -> five ^ one ^ one ^ one
    | _ -> one ^ ten ]
  in
  build "M" "M" "M" (n / 1000 mod 10) ^ build "C" "D" "M" (n / 100 mod 10) ^
    build "X" "L" "C" (n / 10 mod 10) ^ build "I" "V" "X" (n mod 10)
;

value arabian_of_roman s =
  let decode_digit one five ten r =
    loop 0 where rec loop cnt i =
      if i >= String.length s then (10 * r + cnt, i)
      else if s.[i] = one then loop (cnt + 1) (i + 1)
      else if s.[i] = five then
        if cnt = 0 then loop 5 (i + 1) else (10 * r + 5 - cnt, i + 1)
      else if s.[i] = ten then (10 * r + 10 - cnt, i + 1)
      else (10 * r + cnt, i)
  in
  let (r, i) = decode_digit 'M' 'M' 'M' 0 0 in
  let (r, i) = decode_digit 'C' 'D' 'M' r i in
  let (r, i) = decode_digit 'X' 'L' 'C' r i in
  let (r, i) = decode_digit 'I' 'V' 'X' r i in
  if i = String.length s then r else raise Not_found
;

value child_born_after_his_parent base error warning x iparent =
  let parent = poi base iparent in
  match
    (Adef.od_of_codate (get_birth parent), Adef.od_of_codate (get_birth x),
     date_of_death (get_death x))
  with
  [ (Some (Dgreg g1 _ as d1), Some (Dgreg g2 _ as d2), _) ->
      if strictly_after d1 d2 then warning (ParentBornAfterChild parent x)
      else
        let a = time_gone_by g1 g2 in
        if year_of a < 11 then warning (ParentTooYoung parent a) else ()
  | (Some (Dgreg g1 _ as d1), _, Some (Dgreg g2 _ as d2)) ->
      if strictly_after d1 d2 then warning (ParentBornAfterChild parent x)
      else
        let a = time_gone_by g1 g2 in
        if year_of a < 11 then warning (ParentTooYoung parent a) else ()
  | _ -> () ]
;

value born_after_his_elder_sibling base error warning x np ifam des =
  match (np, Adef.od_of_codate (get_birth x), get_death x) with
  [ (None, _, _) -> ()
  | (Some (elder, d1), Some d2, _) ->
      if strictly_after d1 d2 then
        warning (ChildrenNotInOrder ifam des elder x)
      else ()
  | (Some (elder, d1), _, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictly_after d1 d2 then
        warning (ChildrenNotInOrder ifam des elder x)
      else ()
  | _ -> () ]
;

value child_born_before_mother_death base warning x imoth =
  let mother = poi base imoth in
  match (Adef.od_of_codate (get_birth x), get_death mother) with
  [ (Some d1, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictly_after d1 d2 then
        warning (MotherDeadAfterChildBirth mother x)
      else ()
  | _ -> () ]
;

value child_has_sex warning child =
  if get_sex child = Neuter then warning (UndefinedSex child) else ()
;

value possible_father base warning x ifath =
  let father = poi base ifath in
  match
    (Adef.od_of_codate (get_birth x), date_of_death (get_death father))
  with
  [ (Some (Dgreg {prec = Before} _), _) |
    (_, Some (Dgreg {prec = After} _)) ->
      ()
  | (Some (Dgreg d1 _), Some (Dgreg d2 _)) ->
      let a2 =
        match d2 with
        [ {prec = YearInt a2} -> a2
        | {prec = OrYear a2} -> a2
        | {year = a} -> a ]
      in
      if year_of d1 > a2 + 1 then warning (DeadTooEarlyToBeFather father x)
      else ()
  | _ -> () ]
;

value birth_before_death base warning p =
  match (Adef.od_of_codate (get_birth p), get_death p) with
  [ (Some d1, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictly_after d1 d2 then warning (BirthAfterDeath p) else ()
  | _ -> () ]
;

value titles_after_birth base warning p t =
  let t_date_start = Adef.od_of_codate t.t_date_start in
  let t_date_end = Adef.od_of_codate t.t_date_end in
  do {
    match (t_date_start, t_date_end) with
    [ (Some d1, Some d2) ->
        if strictly_after d1 d2 then warning (TitleDatesError p t) else ()
    | _ -> () ];
    match Adef.od_of_codate (get_birth p) with
    [ Some d1 ->
        do {
          match t_date_start with
          [ Some d ->
              if strictly_after d1 d then warning (TitleDatesError p t)
              else ()
          | None -> () ];
          match t_date_end with
          [ Some d ->
              if strictly_after d1 d then warning (TitleDatesError p t)
              else ()
          | None -> () ];
          ()
        }
    | _ -> () ];
  }
;

value try_to_fix_relation_sex base warning p_ref = do {
  let p_index = Some (get_key_index p_ref) in
  let fixed = ref 0 in
  let not_fixed = ref 0 in
  let changed_related =
    List.fold_right
      (fun ip changed_related ->
         let p = poi base ip in
         let (rparents, changed, not_changed) =
           List.fold_right
             (fun rel (rparents, changed, not_changed) ->
                let (rel, changed, not_changed) =
                  match (p_index = rel.r_fath, p_index = rel.r_moth) with
                  [ (True, False) ->
                      if get_sex p_ref = Female then
                        match rel.r_moth with
                        [ Some ip ->
                            let oth_p = poi base ip in
                            if get_sex oth_p = Male then
                              let rel =
                                {(rel) with
                                 r_fath = rel.r_moth; r_moth = p_index}
                              in
                              (rel, changed + 1, not_changed)
                            else
                              (rel, changed, not_changed + 1)
                        | None ->
                            let rel =
                              {(rel) with r_fath = None; r_moth = p_index}
                            in
                            (rel, changed + 1, not_changed) ]
                      else (rel, changed, not_changed)
                  | (False, True) ->
                      if get_sex p_ref = Male then
                        match rel.r_fath with
                        [ Some ip ->
                            let oth_p = poi base ip in
                            if get_sex oth_p = Female then
                              let rel =
                                {(rel) with
                                 r_moth = rel.r_fath; r_fath = p_index}
                              in
                              (rel, changed + 1, not_changed)
                            else
                              (rel, changed, not_changed + 1)
                        | None ->
                            let rel =
                              {(rel) with r_moth = None; r_fath = p_index}
                            in
                            (rel, changed + 1, not_changed) ]
                      else (rel, changed, not_changed)
                  | (False, False) -> (rel, changed, not_changed)
                  | (True, True) -> (rel, changed, not_changed + 1) ]
                in
                ([rel :: rparents], changed, not_changed))
             (get_rparents p) ([], 0, 0)
         in
         let _ = do {
           fixed.val := fixed.val + changed;
           not_fixed.val := not_fixed.val + not_changed
         }
         in
         if changed > 0 then
           let p = person_with_rparents p rparents in
           [(ip, p) :: changed_related]
         else changed_related)
      (get_related p_ref) []
  in
  warning (IncoherentSex p_ref fixed.val not_fixed.val);
  if fixed.val > 0 then Some changed_related else None
};

value related_sex_is_coherent base warning p_ref =
  let p_index = Some (get_key_index p_ref) in
  let merge_sex g1 g2 =
    match (g1, g2) with
    [ (Some Male, Some Male) -> Some Male
    | (Some Female, Some Female) -> Some Female
    | (Some Neuter, Some g) -> Some g
    | (Some g, Some Neuter) -> Some g
    | _ -> None ]
  in
  let check_sex sex rparents =
    List.fold_left
      (fun g rel ->
         match (p_index = rel.r_fath, p_index = rel.r_moth) with
         [ (True, False) -> merge_sex g (Some Male)
         | (False, True) -> merge_sex g (Some Female)
         | (False, False) -> g
         | (True, True) -> None ])
      sex rparents
  in
  let new_sex =
    List.fold_left
      (fun g ip ->
         let p = poi base ip in
         check_sex g (get_rparents p))
      (Some (get_sex p_ref)) (get_related p_ref)
  in
  match new_sex with
  [ Some g ->
      if get_sex p_ref != g then
        let p = person_with_sex p_ref g in
        Some [(get_key_index p_ref, p)]
      else None
  | None -> try_to_fix_relation_sex base warning p_ref ]
;

value check_normal_marriage_date_for_someone base error warning fam ip =
  let p = poi base ip in
  match Adef.od_of_codate (get_marriage fam) with
  [ Some d2 ->
      do {
        match Adef.od_of_codate (get_birth p) with
        [ Some d1 ->
            if strictly_before d2 d1 then
              warning (MarriageDateBeforeBirth p)
            else ()
        | _ -> () ];
        match get_death p with
        [ Death _ d3 ->
            let d3 = Adef.date_of_cdate d3 in
            if strictly_after d2 d3 then warning (MarriageDateAfterDeath p)
            else ()
        | _ -> () ];
      }
  | None -> () ]
;

value check_normal_marriage_date base error warning fam =
  let cpl = coi base (get_fam_index fam) in
  do {
    check_normal_marriage_date_for_someone base error warning fam
      (get_father cpl);
    check_normal_marriage_date_for_someone base error warning fam
      (get_mother cpl);
  }
;

(*
 * Semi sort children by birth dates.
 * If all children have birth dates, no problem.
 * Otherwise, sorting groups of consecutive children who have dates.
 * In not possible cases, try to keep order of children of same sex.
 *   ex: G1, B2 being resp. girl and boy with date(G1) < date(B2)
 *       and G and B begin resp. girls boys without dates
 *     if order is ... B2 B B B G1 ... it becomes ... G1 B2 B B B ...
 *     if order is ... B2 G G G G1 ... it becomes ... G G G G1 B2 ...
 *     if order is ... B2 G B G G1 ... no change (a warning appears).
 *)

value semi_sort base a before comp di =
  loop where rec loop i =
    if i < 0 || i >= Array.length a then ()
    else
      let p1 = poi base a.(i) in
      let d1 =
        match Adef.od_of_codate (get_birth p1) with
        [ Some d1 -> Some d1
        | None -> Adef.od_of_codate (get_baptism p1) ]
      in
      match d1 with
      [ Some d1 ->
          loop_j None (i - di) where rec loop_j sex_interm_sib j =
            if j < 0 || j >= Array.length a then loop (i + di)
            else
              let p2 = poi base a.(j) in
              let d2 =
                match Adef.od_of_codate (get_birth p2) with
                [ Some d2 -> Some d2
                | None -> Adef.od_of_codate (get_baptism p2) ]
              in
              match d2 with
              [ Some d2 ->
                  if comp d1 d2 then do {
                    let j =
                      match sex_interm_sib with
                      [ Some s ->
                          if s = get_sex p1 then None
                          else if s = get_sex p2 then Some j
                          else None
                      | None -> Some j ]
                    in
                    match j with
                    [ Some j ->
                        let k =
                          loop_k (j - di) where rec loop_k k =
                            if k < 0 || k >= Array.length a then k + di
                            else
                              let p3 = poi base a.(k) in
                              let d3 =
                                match Adef.od_of_codate (get_birth p3) with
                                [ Some d3 -> Some d3
                                | None -> Adef.od_of_codate (get_baptism p3) ]
                              in
                              match d3 with
                              [ Some d3 ->
                                  if comp d1 d3 then loop_k (k - di)
                                  else k + di
                              | None -> k + di ]
                        in
                        do  {
                          match before.val with
                          [ Some _ -> ()
                          | None -> before.val := Some (Array.copy a) ];
                          let ip = a.(i) in
                          loop_up i where rec loop_up j =
                            if j = k then ()
                            else do {
                              a.(j) := a.(j - di);
                              loop_up (j - di)
                            };
                          a.(k) := ip;
                          loop (i + di)
                        }
                    | None -> loop (i + di) ]
                  }
                  else loop (i + di)
              | None ->
                  match sex_interm_sib with
                  [ Some s ->
                      if s = get_sex p2 then loop_j sex_interm_sib (j - di)
                      else loop (i + di)
                  | None -> loop_j (Some (get_sex p2)) (j - di) ] ]
      | None -> loop (i + di) ]
;

value sort_children base warning ifam des =
  let before = ref None in
  do {
    semi_sort base (get_children des) before strictly_before 1 1;
    semi_sort base (get_children des) before strictly_after ~-1 1;
    semi_sort base (get_children des) before strictly_before 1 1;
    match before.val with
    [ None -> ()
    | Some a -> warning (ChangedOrderOfChildren ifam des a) ];
  }
;

value check_family base error warning fam cpl des =
  let ifam = get_fam_index fam in
  let fath = poi base (get_father cpl) in
  let moth = poi base (get_mother cpl) in
  do {
    match get_sex fath with
    [ Male -> birth_before_death base warning fath
    | Female | Neuter ->
        if get_relation fam = NoSexesCheckNotMarried
        || get_relation fam = NoSexesCheckMarried then ()
        else error (BadSexOfMarriedPerson fath) ];
    match get_sex moth with
    [ Female -> birth_before_death base warning moth
    | Male | Neuter ->
        if get_relation fam = NoSexesCheckNotMarried ||
           get_relation fam = NoSexesCheckMarried then ()
        else error (BadSexOfMarriedPerson moth) ];
    check_normal_marriage_date base error warning fam;
    sort_children base warning ifam des;
    let _ =
      List.fold_left
        (fun np child ->
           let child = poi base child in
           do {
             birth_before_death base warning child;
             born_after_his_elder_sibling base error warning child np ifam
               des;
             child_born_after_his_parent base error warning child
               (get_father cpl);
             child_born_after_his_parent base error warning child
               (get_mother cpl);
             child_born_before_mother_death base warning child
               (get_mother cpl);
             possible_father base warning child (get_father cpl);
             child_has_sex warning child;
             match Adef.od_of_codate (get_birth child) with
             [ Some d -> Some (child, d)
             | _ -> np ]
           })
        None (Array.to_list (get_children des))
    in
    ();
  }
;

value check_person base error warning p = do {
  birth_before_death base warning p;
  List.iter (titles_after_birth base warning p) (get_titles p);
  related_sex_is_coherent base warning p
};

value is_deleted_family fam = get_fam_index fam = Adef.ifam_of_int (-1);

value strip_all_trailing_spaces s =
  let b = Buffer.create (String.length s) in
  let len =
    loop (String.length s - 1) where rec loop i =
      if i < 0 then 0
      else
        match s.[i] with
        [ ' ' | '\t' | '\r' | '\n' -> loop (i - 1)
        | _ -> i + 1 ]
  in
  loop 0 where rec loop i =
    if i = len then Buffer.contents b
    else
      match s.[i] with
      [ '\r' -> loop (i + 1)
      | ' ' | '\t' ->
          loop0 (i + 1) where rec loop0 j =
            if j = len then Buffer.contents b
            else
              match s.[j] with
              [ ' ' | '\t' | '\r' -> loop0 (j + 1)
              | '\n' -> loop j
              | _ -> do { Buffer.add_char b s.[i]; loop (i + 1) } ]
      | c -> do { Buffer.add_char b c; loop (i + 1) } ]
;

value gen_strip_spaces strip_heading str =
  let start =
    if strip_heading then
      loop 0 where rec loop i =
        if i == String.length str then i
        else
          match str.[i] with
          [ ' ' | '\r' | '\n' | '\t' -> loop (i + 1)
          | _ -> i ]
    else 0
  in
  let stop =
    loop (String.length str - 1) where rec loop i =
      if i == -1 then i + 1
      else
        match str.[i] with
        [ ' ' | '\r' | '\n' | '\t' -> loop (i - 1)
        | _ -> i + 1 ]
  in
  if start == 0 && stop == String.length str then str
  else if start > stop then ""
  else String.sub str start (stop - start)
;

value strip_spaces = gen_strip_spaces True;

value alphabetic_utf_8 n1 n2 =
  let rec loop i1 i2 =
    if i1 >= String.length n1 && i2 >= String.length n2 then i1 - i2
    else if i1 >= String.length n1 then -1
    else if i2 >= String.length n2 then 1
    else
      let (cv1, ii1) = Name.unaccent_utf_8 False n1 i1 in
      let (cv2, ii2) = Name.unaccent_utf_8 False n2 i2 in
      let c =
        if cv1 = cv2 then
          compare (String.sub n1 i1 (ii1 - i1)) (String.sub n2 i2 (ii2 - i2))
        else compare cv1 cv2
      in
      if c = 0 then loop ii1 ii2 else c
  in
  if n1 = n2 then 0 else loop 0 0
;

value alphabetic_value =
  let tab = Array.create 256 0 in
  do {
    for i = 0 to 255 do { tab.(i) := 10 * i };
    tab.(Char.code 'à') := tab.(Char.code 'a') + 1;
    tab.(Char.code 'á') := tab.(Char.code 'a') + 2;
    tab.(Char.code 'â') := tab.(Char.code 'a') + 3;
    tab.(Char.code 'è') := tab.(Char.code 'e') + 1;
    tab.(Char.code 'é') := tab.(Char.code 'e') + 2;
    tab.(Char.code 'ê') := tab.(Char.code 'e') + 3;
    tab.(Char.code 'ë') := tab.(Char.code 'e') + 4;
    tab.(Char.code 'ô') := tab.(Char.code 'o') + 1;
    tab.(Char.code 'Á') := tab.(Char.code 'A') + 2;
    tab.(Char.code 'Æ') := tab.(Char.code 'A') + 5;
    tab.(Char.code 'È') := tab.(Char.code 'E') + 1;
    tab.(Char.code 'É') := tab.(Char.code 'E') + 2;
    tab.(Char.code 'Ö') := tab.(Char.code 'O') + 4;
    tab.(Char.code '?') := 3000;
    fun x -> tab.(Char.code x)
  }
;

value alphabetic_iso_8859_1 n1 n2 =
  let rec loop i1 i2 =
    if i1 == String.length n1 && i2 == String.length n2 then i1 - i2
    else if i1 == String.length n1 then -1
    else if i2 == String.length n2 then 1
    else
      let c1 = n1.[i1] in
      let c2 = n2.[i2] in
      if alphabetic_value c1 < alphabetic_value c2 then -1
      else if alphabetic_value c1 > alphabetic_value c2 then 1
      else loop (succ i1) (succ i2)
  in
  if n1 = n2 then 0 else loop (initial n1) (initial n2)
;

value alphabetic n1 n2 =
(*
  if utf_8_db.val then alphabetic_utf_8 n1 n2 else alphabetic_iso_8859_1 n1 n2
*)
  alphabetic_iso_8859_1 n1 n2
(**)
;

value alphabetic_order n1 n2 =
  if utf_8_db.val then alphabetic_utf_8 n1 n2 else alphabetic_iso_8859_1 n1 n2
;

value map_title_strings f t =
  let t_name =
    match t.t_name with
    [ Tmain -> Tmain
    | Tname s -> Tname (f s)
    | Tnone -> Tnone ]
  in
  let t_ident = f t.t_ident in
  let t_place = f t.t_place in
  {t_name = t_name; t_ident = t_ident; t_place = t_place;
   t_date_start = t.t_date_start; t_date_end = t.t_date_end; t_nth = t.t_nth}
;

value map_relation_ps fp fs r =
  {r_type = r.r_type;
   r_fath =
     match r.r_fath with
     [ Some x -> Some (fp x)
     | None -> None ];
   r_moth =
     match r.r_moth with
     [ Some x -> Some (fp x)
     | None -> None ];
   r_sources = fs r.r_sources}
;

value map_person_ps fp fs p =
  {first_name = fs p.first_name; surname = fs p.surname; occ = p.occ;
   image = fs p.image;
   first_names_aliases = List.map fs p.first_names_aliases;
   surnames_aliases = List.map fs p.surnames_aliases;
   public_name = fs p.public_name; qualifiers = List.map fs p.qualifiers;
   titles = List.map (map_title_strings fs) p.titles;
   rparents = List.map (map_relation_ps fp fs) p.rparents;
   related = p.related; aliases = List.map fs p.aliases;
   occupation = fs p.occupation; sex = p.sex; access = p.access;
   birth = p.birth; birth_place = fs p.birth_place;
   birth_src = fs p.birth_src; baptism = p.baptism;
   baptism_place = fs p.baptism_place; baptism_src = fs p.baptism_src;
   death = p.death; death_place = fs p.death_place;
   death_src = fs p.death_src; burial = p.burial;
   burial_place = fs p.burial_place; burial_src = fs p.burial_src;
   notes = fs p.notes; psources = fs p.psources; key_index = p.key_index}
;

value map_family_ps fp fs fam =
  {marriage = fam.marriage; marriage_place = fs fam.marriage_place;
   marriage_src = fs fam.marriage_src; witnesses = Array.map fp fam.witnesses;
   relation = fam.relation; divorce = fam.divorce; comment = fs fam.comment;
   origin_file = fs fam.origin_file; fsources = fs fam.fsources;
   fam_index = fam.fam_index}
;

value map_couple_p multi_parents fp cpl =
  parent multi_parents (Array.map fp (parent_array cpl))
;

value map_descend_p fp des = {children = Array.map fp des.children};

value arg_list_of_string line =
  loop [] 0 0 None where rec loop list i len quote =
    if i == String.length line then
      if len == 0 then List.rev list else List.rev [Buff.get len :: list]
    else
      match (quote, line.[i]) with
      [ (Some c1, c2) ->
          if c1 == c2 then loop list (i + 1) len None
          else loop list (i + 1) (Buff.store len c2) quote
      | (None, ' ') ->
          let list = if len == 0 then list else [Buff.get len :: list] in
          loop list (i + 1) 0 quote
      | (None, ('"' | ''' as c)) -> loop list (i + 1) 0 (Some c)
      | (None, c) -> loop list (i + 1) (Buff.store len c) None ]
;

value sort_person_list base pl =
  List.sort
    (fun p1 p2 ->
       match
         (Adef.od_of_codate (get_birth p1), get_death p1,
          Adef.od_of_codate (get_birth p2), get_death p2)
       with
       [ (Some d1, _, Some d2, _) -> if strictly_before d1 d2 then -1 else 1
       | (Some d1, _, _, Death _ d2) ->
           if strictly_before d1 (Adef.date_of_cdate d2) then -1 else 1
       | (_, Death _ d1, Some d2, _) ->
           if strictly_before (Adef.date_of_cdate d1) d2 then -1 else 1
       | (_, Death _ d1, _, Death _ d2) ->
           if strictly_before (Adef.date_of_cdate d1) (Adef.date_of_cdate d2)
           then -1
           else 1
       | (Some _, _, _, _) -> 1
       | (_, Death _ _, _, _) -> 1
       | (_, _, Some _, _) -> -1
       | (_, _, _, Death _ _) -> -1
       | _ ->
           let c = alphabetic (p_surname base p1) (p_surname base p2) in
           if c == 0 then
             let c =
               alphabetic (p_first_name base p1) (p_first_name base p2)
             in
             if c == 0 then compare (get_occ p1) (get_occ p2) else c
           else c ])
    pl
;

value find_free_occ base f s i =
  let ipl = persons_of_name base (f ^ " " ^ s) in
  let first_name = Name.lower f in
  let surname = Name.lower s in
  let list_occ =
    loop [] ipl where rec loop list =
      fun
      [ [ip :: ipl] ->
          let p = poi base ip in
          if not (List.mem (get_occ p) list) &&
             first_name = Name.lower (p_first_name base p) &&
             surname = Name.lower (p_surname base p) then
            loop [get_occ p :: list] ipl
          else loop list ipl
      | [] -> list ]
  in
  let list_occ = List.sort compare list_occ in
  loop 0 list_occ where rec loop cnt1 =
    fun
    [ [cnt2 :: list] ->
        if cnt1 = cnt2 then loop (cnt1 + 1) list else cnt1
    | [] -> cnt1 ]
;

value input_lexicon lang ht open_fname =
  try
    let ic = open_fname () in
    let derived_lang =
      match lindex lang '-' with
      [ Some i -> String.sub lang 0 i
      | _ -> "" ]
    in
    try
      do {
        try
          while True do {
            let k =
              find_key (input_line ic) where rec find_key line =
                if String.length line < 4 then find_key (input_line ic)
                else if String.sub line 0 4 <> "    " then
                  find_key (input_line ic)
                else line
            in
            let k = String.sub k 4 (String.length k - 4) in
            let rec loop line =
              match lindex line ':' with
              [ Some i ->
                  let line_lang = String.sub line 0 i in
                  do {
                    if line_lang = lang ||
                       line_lang = derived_lang && not (Hashtbl.mem ht k) then
                      let v =
                        if i + 1 = String.length line then ""
                        else
                          String.sub line (i + 2) (String.length line - i - 2)
                      in
                      Hashtbl.add ht k v
                    else ();
                    loop (input_line ic)
                  }
              | None -> () ]
            in
            loop (input_line ic)
          }
        with
        [ End_of_file -> () ];
        close_in ic;
      }
    with e ->
      do { close_in ic; raise e }
  with
  [ Sys_error _ -> () ]
;

value lock_file bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then
      Filename.chop_suffix bname ".gwb"
    else bname
  in
  bname ^ ".lck"
;
