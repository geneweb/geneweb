(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

type date_search = JustSelf | AddSpouse | AddChildren

module StrSet = Mutil.StrSet

let date_interval conf base t x =
  let d1 = ref { day = 0; month = 0; year = max_int; prec = Sure; delta = 0 } in
  let d2 = ref { day = 0; month = 0; year = 0; prec = Sure; delta = 0 } in
  let found = ref false in
  let rec loop t x =
    let set d =
      if Date.compare_dmy d !d1 < 0 then d1 := d;
      if Date.compare_dmy d !d2 > 0 then d2 := d;
      found := true
    in
    Option.iter set (Date.cdate_to_dmy_opt (get_birth x));
    Option.iter set (Date.cdate_to_dmy_opt (get_baptism x));
    let death = get_death x in
    (match Date.dmy_of_death death with
    | Some d -> set d
    | None -> if death = NotDead then set conf.today);
    List.iter
      (fun t ->
        Option.iter set (Date.cdate_to_dmy_opt t.t_date_start);
        Option.iter set (Date.cdate_to_dmy_opt t.t_date_end))
      (Util.nobtit conf base x);
    match t with
    | JustSelf -> ()
    | AddSpouse | AddChildren ->
        let u = pget conf base (get_iper x) in
        Array.iter
          (fun ifam ->
            let fam = foi base ifam in
            let md = get_marriage fam in
            let conj = Gutil.spouse (get_iper x) fam in
            Option.iter set (Date.cdate_to_dmy_opt md);
            loop JustSelf (pget conf base conj);
            match t with
            | AddSpouse | JustSelf -> ()
            | AddChildren ->
                Array.iter
                  (fun e -> loop JustSelf (pget conf base e))
                  (get_children fam))
          (get_family u)
  in
  loop t x;
  if !found then Some (!d1, !d2) else None

let compare_title_dates conf base (x1, t1) (x2, t2) =
  match
    ( ( get_birth x1,
        Date.od_of_cdate t1.t_date_start,
        Date.od_of_cdate t1.t_date_end,
        get_death x1 ),
      ( get_birth x2,
        Date.od_of_cdate t2.t_date_start,
        Date.od_of_cdate t2.t_date_end,
        get_death x2 ) )
  with
  | (_, Some (Dgreg (d1, _)), _, _), (_, Some (Dgreg (d2, _)), _, _) -> (
      match Date.compare_dmy d1 d2 with
      | 0 -> (
          match
            (Date.od_of_cdate t1.t_date_end, Date.od_of_cdate t2.t_date_end)
          with
          | Some d1, Some d2 -> Date.compare_date d1 d2
          | _ -> -1)
      | x -> x)
  | (_, _, Some (Dgreg (_, _) as d1), _), (_, _, Some (Dgreg (_, _) as d2), _)
    ->
      Date.compare_date d1 d2
  | (_, _, _, Death (_, d1)), (_, Some d2, _, _)
    when Date.compare_date (Date.date_of_cdate d1) d2 <= 0 ->
      -1
  | (_, Some (Dgreg (_, _) as d1), _, _), (_, _, _, Death (_, d2))
    when Date.compare_date d1 (Date.date_of_cdate d2) > 0 ->
      1
  | _ -> (
      match
        ( date_interval conf base JustSelf x1,
          date_interval conf base JustSelf x2 )
      with
      | Some (d11, d12), Some (d21, d22) ->
          if Date.compare_dmy d12 d21 <= 0 then -1
          else if Date.compare_dmy d11 d22 >= 0 then 1
          else if Date.compare_dmy d21 d11 > 0 then -1
          else 1
      | _ -> (
          match
            ( date_interval conf base AddSpouse x1,
              date_interval conf base AddSpouse x2 )
          with
          | Some (d11, d12), Some (d21, d22) ->
              if Date.compare_dmy d12 d21 <= 0 then -1
              else if Date.compare_dmy d11 d22 >= 0 then 1
              else if Date.compare_dmy d22 d12 >= 0 then -1
              else 1
          | _ -> (
              match
                ( date_interval conf base AddChildren x1,
                  date_interval conf base AddChildren x2 )
              with
              | Some (d11, d12), Some (d21, d22) ->
                  if Date.compare_dmy d21 d12 >= 0 then -1
                  else if Date.compare_dmy d11 d22 >= 0 then 1
                  else if Date.compare_dmy d22 d12 >= 0 then -1
                  else 1
              | Some _, None -> -1
              | None, Some _ -> 1
              | None, None -> -1)))

let compare_title_order conf base (x1, t1) (x2, t2) =
  if t1.t_nth = 0 || t2.t_nth = 0 || t1.t_nth = t2.t_nth then
    compare_title_dates conf base (x1, t1) (x2, t2)
  else compare t1.t_nth t2.t_nth

(**)

let select_title_place conf base ~absolute title place =
  let list = ref [] in
  let clean_title = ref title in
  let clean_place = ref place in
  let all_names = ref [] in
  let title1 = Name.lower title in
  let place1 = Name.lower place in
  let select x t =
    if
      (absolute && sou base t.t_ident = title && sou base t.t_place = place)
      || (not absolute)
         && Name.lower (sou base t.t_ident) = title1
         && Name.lower (sou base t.t_place) = place1
    then (
      let tn = sou base t.t_ident in
      clean_title := tn;
      clean_place := sou base t.t_place;
      list := (x, t) :: !list;
      if not (List.mem tn !all_names) then all_names := tn :: !all_names)
  in
  Gwdb.Collection.iter
    (fun i ->
      let x = pget conf base i in
      List.iter (select x) (nobtit conf base x))
    (Gwdb.ipers base);
  (!list, !clean_title, !clean_place, !all_names)

let select_all_with_place conf base place =
  let list = ref [] in
  let clean_place = ref place in
  let place = Name.lower place in
  let select x t =
    if Name.lower (sou base t.t_place) = place then (
      clean_place := sou base t.t_place;
      list := (x, t) :: !list)
  in
  Gwdb.Collection.iter
    (fun i ->
      let x = pget conf base i in
      List.iter (select x) (nobtit conf base x))
    (Gwdb.ipers base);
  (!list, !clean_place)

let select_title conf base ~absolute title =
  let set = ref StrSet.empty in
  let clean_name = ref title in
  let all_names = ref [] in
  let title2 = Name.lower title in
  let add_place t =
    let tn = sou base t.t_ident in
    if (absolute && tn = title) || ((not absolute) && Name.lower tn = title2)
    then (
      let pn = sou base t.t_place in
      if not (StrSet.mem pn !set) then (
        clean_name := tn;
        set := StrSet.add pn !set);
      if not (List.mem tn !all_names) then all_names := tn :: !all_names)
  in
  Gwdb.Collection.iter
    (fun i ->
      let x = pget conf base i in
      List.iter add_place (nobtit conf base x))
    (Gwdb.ipers base);
  (StrSet.elements !set, !clean_name, !all_names)

let select_place conf base place =
  let list = ref [] in
  let clean_name = ref place in
  let place2 = Name.lower place in
  let add_title t =
    let pn = sou base t.t_place in
    if Name.lower pn = place2 then
      let tn = sou base t.t_ident in
      if not (List.mem tn !list) then (
        clean_name := pn;
        list := tn :: !list)
  in
  Gwdb.Collection.iter
    (fun i ->
      let x = pget conf base i in
      List.iter add_title (nobtit conf base x))
    (Gwdb.ipers base);
  (!list, !clean_name)

let select_all proj conf base =
  Gwdb.Collection.fold
    (fun acc i ->
      let x = pget conf base i in
      List.fold_left
        (fun s t -> StrSet.add (sou base (proj t)) s)
        acc (nobtit conf base x))
    StrSet.empty (Gwdb.ipers base)
  |> StrSet.elements

let select_all2 proj conf base =
  let ht = Hashtbl.create 1 in
  Gwdb.Collection.iter
    (fun i ->
      let x = pget conf base i in
      List.iter
        (fun t ->
          let s = sou base (proj t) in
          let cnt =
            try Hashtbl.find ht s
            with Not_found ->
              let cnt = ref 0 in
              Hashtbl.add ht s cnt;
              cnt
          in
          incr cnt)
        (nobtit conf base x))
    (Gwdb.ipers base);
  Hashtbl.fold (fun s cnt list -> (s, !cnt) :: list) ht []

let select_all_titles = select_all2 (fun t -> t.t_ident)
let select_all_places = select_all (fun t -> t.t_place)
