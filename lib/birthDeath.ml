(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

let get_k conf =
  match p_getint conf.env "k" with
  | Some x -> x
  | _ -> (
      try int_of_string (List.assoc "latest_event" conf.base_env)
      with Not_found | Failure _ -> 20)

let select (type a) (module Q : Pqueue.S with type elt = a * dmy * calendar)
    nb_of iterator get get_date conf base =
  let n = min (max 0 (get_k conf)) (nb_of base) in
  let ref_date =
    match p_getint conf.env "by" with
    | Some by ->
        let bm = Option.value ~default:(-1) (p_getint conf.env "bm") in
        let bd = Option.value ~default:(-1) (p_getint conf.env "bd") in
        Some { day = bd; month = bm; year = by; prec = Sure; delta = 0 }
    | None -> None
  in
  let q, len =
    Gwdb.Collection.fold
      (fun (q, len) i ->
        let x = get base i in
        match get_date x with
        | Some (Dgreg (d, cal)) ->
            let aft =
              match ref_date with
              | Some ref_date -> Date.compare_dmy ref_date d <= 0
              | None -> false
            in
            if aft then (q, len)
            else
              let e = (x, d, cal) in
              if len < n then (Q.add e q, len + 1)
              else (snd (Q.take (Q.add e q)), len)
        | _ -> (q, len))
      (Q.empty, 0) (iterator base)
  in
  let rec loop list q =
    if Q.is_empty q then (list, len)
    else
      let e, q = Q.take q in
      loop (e :: list) q
  in
  loop [] q

module PQ = Pqueue.Make (struct
  type t = Gwdb.person * Def.dmy * Def.calendar

  let leq (_, x, _) (_, y, _) = Date.compare_dmy x y <= 0
end)

module PQ_oldest = Pqueue.Make (struct
  type t = Gwdb.person * Def.dmy * Def.calendar

  let leq (_, x, _) (_, y, _) = Date.compare_dmy y x <= 0
end)

let select_person conf base get_date find_oldest =
  select
    (if find_oldest then (module PQ_oldest) else (module PQ))
    nb_of_persons Gwdb.ipers (pget conf) get_date conf base

module FQ = Pqueue.Make (struct
  type t = Gwdb.family * Def.dmy * Def.calendar

  let leq (_, x, _) (_, y, _) = Date.compare_dmy x y <= 0
end)

module FQ_oldest = Pqueue.Make (struct
  type t = Gwdb.family * Def.dmy * Def.calendar

  let leq (_, x, _) (_, y, _) = Date.compare_dmy y x <= 0
end)

let select_family conf base get_date find_oldest =
  select
    (if find_oldest then (module FQ_oldest) else (module FQ))
    nb_of_families Gwdb.ifams Gwdb.foi get_date conf base

let death_date p = Date.date_of_death (get_death p)

let make_population_pyramid ~nb_intervals ~interval ~limit ~at_date conf base =
  let men = Array.make (nb_intervals + 1) 0 in
  let wom = Array.make (nb_intervals + 1) 0 in
  (* TODO? Load person array *)
  Gwdb.Collection.iter
    (fun i ->
      let p = pget conf base i in
      let sex = get_sex p in
      let dea = get_death p in
      if sex <> Neuter then
        match Date.cdate_to_dmy_opt (get_birth p) with
        | None -> ()
        | Some dmy ->
            if Date.compare_dmy dmy at_date <= 0 then
              let a = Date.time_elapsed dmy at_date in
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
    (Gwdb.ipers base);
  (men, wom)
