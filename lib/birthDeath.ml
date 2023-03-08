(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

let clamp_to_conf_nb conf nb =
  (* ?? .. *)
  let get_k conf =
    match p_getint conf.env "k" with
    | Some x -> x
    | None -> (
        try int_of_string (List.assoc "latest_event" conf.base_env)
        with Not_found | Failure _ -> 20)
  in
  min (max 0 (get_k conf)) nb

let select (type key value) (module Q : Pqueue.S with type elt = key * value)
    ~iterator ~get_key ~get_value ~nb ~filter =
  let q, len =
    Gwdb.Collection.fold
      (fun (q, len) i ->
        let x = get_key i in
        match get_value x with
        | None -> (q, len)
        | Some item ->
            if not (filter item) then (q, len)
            else
              let e = (x, item) in
              if len < nb then (Q.add e q, len + 1)
              else (snd (Q.take (Q.add e q)), len))
      (Q.empty, 0) iterator
  in
  (* TODO Q.to_list? *)
  let rec loop list q =
    if Q.is_empty q then (list, len)
    else
      let e, q = Q.take q in
      loop (e :: list) q
  in
  loop [] q

(* this is used to filter date before/after a date defined in conf.env *)
let date_filter conf =
  match p_getint conf.env "by" with
  | None -> fun _d -> true
  | Some by ->
      (* TODO making and comparing a date with negative value is a bad idea *)
      let bm = Option.value ~default:(-1) (p_getint conf.env "bm") in
      let bd = Option.value ~default:(-1) (p_getint conf.env "bd") in
      let ref_date =
        Date.{ day = bd; month = bm; year = by; prec = Sure; delta = 0 }
      in
      fun (d, _cal) -> Date.compare_dmy ref_date d > 0

module PQ_date = Pqueue.Make (struct
  type t = Gwdb.person * (Date.dmy * Date.calendar)

  (* TODO leq must define a total order, does Date.compare_dmy really define a total order...? *)
  let leq (_, (x, _)) (_, (y, _)) = Date.compare_dmy x y <= 0
end)

(* TODO have a make_module leq = ... *)
module PQ_date_reverse = Pqueue.Make (struct
  type t = Gwdb.person * (Date.dmy * Date.calendar)

  let leq (_, (x, _)) (_, (y, _)) = Date.compare_dmy y x <= 0
end)

let select_person_by_date conf base get_date ~ascending =
  let iterator = Gwdb.ipers base in
  let get_key = pget conf base in
  let nb = nb_of_persons base |> clamp_to_conf_nb conf in
  let get_value p =
    match get_date p with
    | Some (Date.Dgreg (d, cal)) -> Some (d, cal)
    | Some (Dtext _) | None -> None
  in
  select
    (if ascending then (module PQ_date_reverse) else (module PQ_date))
    ~iterator ~get_key ~get_value ~nb ~filter:(date_filter conf)

module PQ_elapsed = Pqueue.Make (struct
  type t = Gwdb.person * Date.elapsed_time

  let leq (_, x) (_, y) = Date.compare_elapsed_time x y <= 0
end)

(* TODO have a make_module leq = ... *)
module PQ_elapsed_reverse = Pqueue.Make (struct
  type t = Gwdb.person * Date.elapsed_time

  let leq (_, x) (_, y) = Date.compare_elapsed_time y x <= 0
end)

let select_person_by_elapsed_time conf base get_elapsed_time ~ascending =
  let iterator = Gwdb.ipers base in
  let get_key = pget conf base in
  let nb = nb_of_persons base |> clamp_to_conf_nb conf in
  select
    (if ascending then (module PQ_elapsed_reverse) else (module PQ_elapsed))
    ~iterator ~get_key ~get_value:get_elapsed_time ~nb
    ~filter:(fun _item -> true)

module FQ = Pqueue.Make (struct
  type t = Gwdb.family * (Date.dmy * Date.calendar)

  let leq (_, (x, _)) (_, (y, _)) = Date.compare_dmy x y <= 0
end)

module FQ_oldest = Pqueue.Make (struct
  type t = Gwdb.family * (Date.dmy * Date.calendar)

  let leq (_, (x, _)) (_, (y, _)) = Date.compare_dmy y x <= 0
end)

let select_family conf base get_date find_oldest =
  let iterator = Gwdb.ifams base in
  let get_key = Gwdb.foi base in
  let nb = nb_of_families base |> clamp_to_conf_nb conf in
  let get_value fam =
    match get_date fam with
    | Some (Date.Dgreg (d, cal)) -> Some (d, cal)
    | Some (Dtext _) | None -> None
  in
  select
    (if find_oldest then (module FQ_oldest) else (module FQ))
    ~iterator ~get_key ~get_value ~nb ~filter:(date_filter conf)

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
              let age = Date.time_elapsed dmy at_date in
              let j = min nb_intervals (age.nb_year / interval) in
              if
                (dea = NotDead || (dea = DontKnowIfDead && age.nb_year < limit))
                ||
                match Date.dmy_of_death dea with
                | None -> false
                | Some d -> Date.compare_dmy d at_date > 0
              then
                if sex = Male then men.(j) <- men.(j) + 1
                else wom.(j) <- wom.(j) + 1)
    (Gwdb.ipers base);
  (men, wom)
