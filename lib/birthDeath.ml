(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

let get_k conf =
  match p_getint conf.env "k" with
  | Some x -> x
  | _ -> (
      try int_of_string (List.assoc "latest_event" conf.base_env)
      with Not_found | Failure _ -> 20)

let select (type a)
    (module Q : Pqueue.S with type elt = a * Adef.dmy * Adef.calendar) limits
    nb_of iterator get get_date conf base =
  let limits = Array.map (fun k -> min (max 0 k) (nb_of base)) limits in
  let ref_date =
    match p_getint conf.env "by" with
    | Some by ->
        let bm = Option.value ~default:0 (p_getint conf.env "bm") in
        let bd = Option.value ~default:0 (p_getint conf.env "bd") in
        Some { Adef.day = bd; month = bm; year = by; prec = Sure; delta = 0 }
    | None -> None
  in
  let queues = Array.make (Array.length limits) Q.empty in
  let lens = Array.make (Array.length limits) 0 in
  Collection.iter
    (fun i ->
      let x = get base i in
      match get_date x with
      | Some (b, Adef.Dgreg (d, cal)) ->
          let d_greg = Date.approx_gregorian ~from:cal d in
          let aft =
            match ref_date with
            | Some ref_date -> Date.compare_dmy ref_date d_greg <= 0
            | None -> false
          in
          if not aft then
            let q = Q.add (x, d_greg, cal) queues.(b) in
            if lens.(b) < limits.(b) then (
              queues.(b) <- q;
              lens.(b) <- lens.(b) + 1)
            else queues.(b) <- snd (Q.take q)
      | Some (_, Adef.Dtext _) | None -> ())
    (iterator base);
  let rec to_list list q =
    if Q.is_empty q then list
    else
      let e, q = Q.take q in
      to_list (e :: list) q
  in
  Array.mapi (fun b q -> (to_list [] q, lens.(b))) queues

module PQ = Pqueue.Make (struct
  type t = Geneweb_db.Driver.person * Adef.dmy * Adef.calendar

  let leq (_, x, _) (_, y, _) = Date.compare_dmy x y <= 0
end)

module PQ_oldest = Pqueue.Make (struct
  type t = Geneweb_db.Driver.person * Adef.dmy * Adef.calendar

  let leq (_, x, _) (_, y, _) = Date.compare_dmy y x <= 0
end)

let select_person conf base get_date find_oldest =
  let get_date p =
    match get_date p with
    | Some d when authorized_age conf base p -> Some (0, d)
    | Some _ | None -> None
  in
  (select
     (if find_oldest then (module PQ_oldest) else (module PQ))
     [| get_k conf |]
     Driver.nb_of_persons Driver.ipers (pget conf) get_date conf base).(0)

module FQ = Pqueue.Make (struct
  type t = Geneweb_db.Driver.family * Adef.dmy * Adef.calendar

  let leq (_, x, _) (_, y, _) = Date.compare_dmy x y <= 0
end)

module FQ_oldest = Pqueue.Make (struct
  type t = Geneweb_db.Driver.family * Adef.dmy * Adef.calendar

  let leq (_, x, _) (_, y, _) = Date.compare_dmy y x <= 0
end)

let select_families conf base limits get_date find_oldest =
  let auth ip = authorized_age conf base (Driver.poi base ip) in
  let get_date fam =
    match get_date fam with
    | Some _ as d
      when auth (Driver.get_father fam) && auth (Driver.get_mother fam) ->
        d
    | Some _ | None -> None
  in
  select
    (if find_oldest then (module FQ_oldest) else (module FQ))
    limits Driver.nb_of_families Driver.ifams Driver.foi get_date conf base

let select_family conf base get_date find_oldest =
  (select_families conf base
     [| get_k conf |]
     (fun fam -> Option.map (fun d -> (0, d)) (get_date fam))
     find_oldest).(0)

let death_date p = Date.date_of_death (Driver.get_death p)

let make_population_pyramid ~nb_intervals ~interval ~limit ~at_date base =
  let men = Array.make (nb_intervals + 1) 0 in
  let wom = Array.make (nb_intervals + 1) 0 in
  (* TODO? Load person array *)
  Collection.iter
    (fun i ->
      let p = Driver.poi base i in
      let sex = Driver.get_sex p in
      let dea = Driver.get_death p in
      if sex <> Neuter then
        match Date.cdate_to_dmy_opt (Driver.get_birth p) with
        | None -> ()
        | Some dmy ->
            if Date.compare_dmy dmy at_date <= 0 then
              let a = Date.time_elapsed dmy at_date in
              if a.year >= 0 then
                let j = min nb_intervals (a.year / interval) in
                if
                  (dea = NotDead || (dea = DontKnowIfDead && a.year < limit))
                  ||
                  match Date.dmy_of_death dea with
                  | None -> false
                  | Some d -> Date.compare_dmy d at_date > 0
                then
                  if sex = Male then men.(j) <- men.(j) + 1
                  else wom.(j) <- wom.(j) + 1)
    (Driver.ipers base);
  (men, wom)

let make_death_pyramid ~nb_intervals ~interval ~limit ~from_year ~to_year base =
  let men = Array.make (nb_intervals + 1) 0 in
  let wom = Array.make (nb_intervals + 1) 0 in
  Collection.iter
    (fun i ->
      let p = Driver.poi base i in
      let sex = Driver.get_sex p in
      if sex <> Neuter then
        match Date.dmy_of_death (Driver.get_death p) with
        | None -> ()
        | Some dd -> (
            if dd.year >= from_year && dd.year <= to_year then
              match Date.cdate_to_dmy_opt (Driver.get_birth p) with
              | None -> ()
              | Some bd ->
                  let a = Date.time_elapsed bd dd in
                  if a.year >= 0 && (limit = 0 || a.year <= limit) then
                    let j = min nb_intervals (a.year / interval) in
                    if sex = Male then men.(j) <- men.(j) + 1
                    else wom.(j) <- wom.(j) + 1))
    (Driver.ipers base);
  (men, wom)
