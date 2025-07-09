(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Gutil = Geneweb_db.Gutil
module T = Driver.Istr.Table

type date_search = JustSelf | AddSpouse | AddChildren

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
    Option.iter set (Date.cdate_to_dmy_opt (Driver.get_birth x));
    Option.iter set (Date.cdate_to_dmy_opt (Driver.get_baptism x));
    let death = Driver.get_death x in
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
        let u = pget conf base (Driver.get_iper x) in
        Array.iter
          (fun ifam ->
            let fam = Driver.foi base ifam in
            let md = Driver.get_marriage fam in
            let conj = Gutil.spouse (Driver.get_iper x) fam in
            Option.iter set (Date.cdate_to_dmy_opt md);
            loop JustSelf (pget conf base conj);
            match t with
            | AddSpouse | JustSelf -> ()
            | AddChildren ->
                Array.iter
                  (fun e -> loop JustSelf (pget conf base e))
                  (Driver.get_children fam))
          (Driver.get_family u)
  in
  loop t x;
  if !found then Some (!d1, !d2) else None

let compare_title_dates conf base (x1, t1) (x2, t2) =
  match
    ( ( Driver.get_birth x1,
        Date.od_of_cdate t1.t_date_start,
        Date.od_of_cdate t1.t_date_end,
        Driver.get_death x1 ),
      ( Driver.get_birth x2,
        Date.od_of_cdate t2.t_date_start,
        Date.od_of_cdate t2.t_date_end,
        Driver.get_death x2 ) )
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

let to_string_list base ht =
  T.to_seq ht |> Seq.map (fun (i, ()) -> Driver.sou base i) |> List.of_seq

let select_title_place conf base ~absolute title place =
  let names : unit T.t = T.create 17 in
  let select =
    let tl1 = Name.lower title in
    let pl1 = Name.lower place in
    fun t ->
      let tl2 = Driver.sou base t.t_ident in
      let pl2 = Driver.sou base t.t_place in
      (absolute && String.equal title tl2 && String.equal place pl2)
      || (not absolute)
         && String.equal tl1 (Name.lower tl2)
         && String.equal pl1 (Name.lower pl2)
  in
  let l =
    Collection.fold
      (fun acc i ->
        let x = pget conf base i in
        let titles = nobtit conf base x in
        List.fold_left
          (fun acc t ->
            if select t then (
              T.replace names t.t_ident ();
              (x, t) :: acc)
            else acc)
          acc titles)
      []
      (Geneweb_db.Driver.ipers base)
  in
  let names = to_string_list base names in
  (l, names)

let select_all_with_place conf base place =
  let select =
    let p = Name.lower place in
    fun t -> String.equal (Name.lower (Driver.sou base t.t_place)) p
  in
  Collection.fold
    (fun acc i ->
      let x = pget conf base i in
      let titles = nobtit conf base x in
      List.fold_left
        (fun acc t -> if select t then (x, t) :: acc else acc)
        acc titles)
    []
    (Geneweb_db.Driver.ipers base)

let select_title conf base ~absolute title =
  let places : unit T.t = T.create 17 in
  let names : unit T.t = T.create 17 in
  let select =
    let tl = Name.lower title in
    fun t ->
      let tn = Driver.sou base t.t_ident in
      (absolute && String.equal tn title)
      || ((not absolute) && String.equal (Name.lower tn) tl)
  in
  Collection.iter
    (fun i ->
      let x = pget conf base i in
      List.iter
        (fun t ->
          if select t then (
            T.replace names t.t_ident ();
            T.replace places t.t_place ()))
        (nobtit conf base x))
    (Geneweb_db.Driver.ipers base);
  let places = to_string_list base places in
  let names = to_string_list base names in
  (places, names)

let select_place conf base place =
  let names : unit T.t = T.create 17 in
  let select =
    let p = Name.lower place in
    fun t ->
      let pn = Driver.sou base t.t_place in
      String.equal (Name.lower pn) p
  in
  Collection.iter
    (fun i ->
      let x = pget conf base i in
      let titles = nobtit conf base x in
      List.iter (fun t -> if select t then T.replace names t.t_ident ()) titles)
    (Geneweb_db.Driver.ipers base);
  to_string_list base names

let select_all proj conf base =
  let ht : unit T.t = T.create 17 in
  Collection.iter
    (fun i ->
      let x = pget conf base i in
      let titles = nobtit conf base x in
      List.iter (fun t -> T.add ht (proj t) ()) titles)
    (Geneweb_db.Driver.ipers base);
  to_string_list base ht

let select_all_with_counter proj conf base =
  let ht : int T.t = T.create 17 in
  Collection.iter
    (fun i ->
      let x = pget conf base i in
      let titles = nobtit conf base x in
      List.iter
        (fun t ->
          let y = proj t in
          match T.find ht y with
          | i -> T.replace ht y (i + 1)
          | exception Not_found -> T.add ht y 1)
        titles)
    (Geneweb_db.Driver.ipers base);
  T.to_seq ht |> Seq.map (fun (i, c) -> (Driver.sou base i, c)) |> List.of_seq

let select_all_titles = select_all_with_counter (fun t -> t.t_ident)
let select_all_places = select_all (fun t -> t.t_place)
