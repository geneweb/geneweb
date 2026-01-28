(* Copyright (c) 1998-2007 INRIA *)

open Def

type cdate = Adef.cdate =
  | Cgregorian of int
  | Cjulian of int
  | Cfrench of int
  | Chebrew of int
  | Ctext of string
  | Cdate of date
  | Cnone

(* Compress concrete date into integer if simple precision.
   Encoding: (((prec_code * 32 + day) * 13 + month) * 2500 + year)
   - prec_code: 0=Sure, 1=About, 2=Maybe, 3=Before, 4=After
   - day ∈ [0,31], month ∈ [0,12], year ∈ ]0,2500[
   - Limitations: year must be in ]0,2500[, delta must be 0,
     OrYear/YearInt cannot be compressed
   - Note: month can be 13 for some calendars but compression uses
     mod 13 so max storable is 12 *)
let compress d =
  let simple =
    match d.prec with
    | Sure | About | Maybe | Before | After ->
        d.day >= 0 && d.month >= 0 && d.year > 0 && d.year < 2500 && d.delta = 0
    | OrYear _ | YearInt _ -> false
  in
  if simple then
    let p =
      match d.prec with
      | About -> 1
      | Maybe -> 2
      | Before -> 3
      | After -> 4
      | Sure | OrYear _ | YearInt _ -> 0
    in
    Some ((((((p * 32) + d.day) * 13) + d.month) * 2500) + d.year)
  else None

(* uncompress concrete date *)
let uncompress x =
  let year, x = (x mod 2500, x / 2500) in
  let month, x = (x mod 13, x / 13) in
  let day, x = (x mod 32, x / 32) in
  let prec =
    match x with
    | 1 -> About
    | 2 -> Maybe
    | 3 -> Before
    | 4 -> After
    | _ -> Sure
  in
  { day; month; year; prec; delta = 0 }

let date_of_cdate = function
  | Cgregorian i -> Dgreg (uncompress i, Dgregorian)
  | Cjulian i -> Dgreg (uncompress i, Djulian)
  | Cfrench i -> Dgreg (uncompress i, Dfrench)
  | Chebrew i -> Dgreg (uncompress i, Dhebrew)
  | Cdate d -> d
  | Ctext t -> Dtext t
  | Cnone -> failwith "date_of_cdate"

let cdate_of_date = function
  | Dtext t -> Ctext t
  | Dgreg (g, cal) as d -> (
      match compress g with
      | None -> Cdate d
      | Some i -> (
          match cal with
          | Dgregorian -> Cgregorian i
          | Djulian -> Cjulian i
          | Dfrench -> Cfrench i
          | Dhebrew -> Chebrew i))

let cdate_of_od = function Some d -> cdate_of_date d | None -> Cnone
let od_of_cdate = function Cnone -> None | cd -> Some (date_of_cdate cd)
let cdate_None = cdate_of_od None

let dmy_of_dmy2 dmy2 =
  {
    day = dmy2.day2;
    month = dmy2.month2;
    year = dmy2.year2;
    prec = Sure;
    delta = dmy2.delta2;
  }

let leap_year a = (a mod 4 = 0 && a mod 100 <> 0) || a mod 400 = 0

let nb_days_in_month m a =
  if m = 2 && leap_year a then 29
  else if m >= 1 && m <= 12 then
    [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |].(m - 1)
  else 0

(* TODO: Consider using SDN (Serial Day Number) for calendar-neutral
   calculations once Calendars library is updated. Current implementation
   works only for Gregorian calendar and partial dates. *)
let time_elapsed d1 d2 =
  let prec =
    match (d1.prec, d2.prec) with
    | Sure, Sure -> Sure
    | (Maybe | Sure | About), (Maybe | Sure | About) -> Maybe
    | (About | Maybe | Sure | Before), (After | Sure | Maybe | About) -> After
    | (About | Maybe | Sure | After), (Before | Sure | Maybe | About) -> Before
    | _ -> Maybe
  in
  let compute_month_diff m1 m2 =
    if m1 <= m2 then (m2 - m1, 0) else (m2 - m1 + 12, 1)
  in
  let compute_day_diff j1 j2 m1 a1 =
    if j1 <= j2 then (j2 - j1, 0) else (j2 - j1 + nb_days_in_month m1 a1, 1)
  in
  match (d1, d2) with
  | { day = 0; month = 0; year = a1; _ }, { year = a2; _ } ->
      { day = 0; month = 0; year = a2 - a1; prec; delta = 0 }
  | { day = 0; month = _; year = a1; _ }, { day = 0; month = 0; year = a2; _ }
    ->
      { day = 0; month = 0; year = a2 - a1; prec; delta = 0 }
  | { day = 0; month = m1; year = a1; _ }, { day = 0; month = m2; year = a2; _ }
    ->
      let month, r = compute_month_diff m1 m2 in
      { day = 0; month; year = a2 - a1 - r; prec; delta = 0 }
  | { day = 0; month = m1; year = a1; _ }, { month = m2; year = a2; _ } ->
      let month, r = compute_month_diff m1 m2 in
      { day = 0; month; year = a2 - a1 - r; prec; delta = 0 }
  | { day = _; month = _; year = a1; _ }, { day = 0; month = 0; year = a2; _ }
    ->
      { day = 0; month = 0; year = a2 - a1; prec; delta = 0 }
  | { day = _; month = m1; year = a1; _ }, { day = 0; month = m2; year = a2; _ }
    ->
      let month, r = compute_month_diff m1 m2 in
      { day = 0; month; year = a2 - a1 - r; prec; delta = 0 }
  | ( { day = j1; month = m1; year = a1; _ },
      { day = j2; month = m2; year = a2; _ } ) ->
      let day, r1 = compute_day_diff j1 j2 m1 a1 in
      let month, r2 = compute_month_diff (m1 + r1) m2 in
      { day; month; year = a2 - a1 - r2; prec; delta = 0 }

let time_elapsed_opt d1 d2 =
  match (d1.prec, d2.prec) with
  | After, After | Before, Before -> None
  | _ -> Some (time_elapsed d1 d2)

(* TODO use SDN to compare date (?) *)
(* use strict = false to compare date as if they are points on a timeline.
   use strict = true to compare date by taking precision in account. This makes some dates not comparable, do not use to sort a list *)
let rec compare_dmy_opt ?(strict = false) dmy1 dmy2 =
  match compare dmy1.year dmy2.year with
  | 0 -> compare_month_or_day ~is_day:false strict dmy1 dmy2
  | x -> eval_strict strict dmy1 dmy2 x

and compare_month_or_day ~is_day strict dmy1 dmy2 =
  (* compare a known month|day with a unknown one (0) *)
  let compare_with_unknown_value ~strict ~unknown ~known =
    match unknown.prec with
    | After -> Some 1
    | Before -> Some (-1)
    | _other -> if strict then None else compare_prec false unknown known
  in
  (* if we are comparing months the next comparison to do is on days
      else if we are comparing days it is compare_prec *)
  let x, y, next_comparison =
    if is_day then (dmy1.day, dmy2.day, compare_prec)
    else (dmy1.month, dmy2.month, compare_month_or_day ~is_day:true)
  in
  (* 0 means month|day is unknow*)
  match (x, y) with
  | 0, 0 -> compare_prec strict dmy1 dmy2
  | 0, _ -> compare_with_unknown_value ~strict ~unknown:dmy1 ~known:dmy2
  | _, 0 ->
      (* swap dmy1 and dmy2 *)
      Option.map Int.neg
      @@ compare_with_unknown_value ~strict ~unknown:dmy2 ~known:dmy1
  | m1, m2 -> (
      match Int.compare m1 m2 with
      | 0 -> next_comparison strict dmy1 dmy2
      | x -> eval_strict strict dmy1 dmy2 x)

and compare_prec strict dmy1 dmy2 =
  match (dmy1.prec, dmy2.prec) with
  | (Sure | About | Maybe), (Sure | About | Maybe) -> Some 0
  | After, After | Before, Before -> Some 0
  | OrYear dmy1, OrYear dmy2 | YearInt dmy1, YearInt dmy2 ->
      compare_dmy_opt ~strict (dmy_of_dmy2 dmy1) (dmy_of_dmy2 dmy2)
  | _, After | Before, _ -> Some (-1)
  | After, _ | _, Before -> Some 1
  | _ -> Some 0

and eval_strict strict dmy1 dmy2 x =
  if strict then
    match x with
    | -1 when dmy1.prec = After || dmy2.prec = Before -> None
    | 1 when dmy1.prec = Before || dmy2.prec = After -> None
    | x -> Some x
  else Some x

let compare_dmy dmy1 dmy2 =
  match compare_dmy_opt ~strict:false dmy1 dmy2 with
  | None -> assert false
  | Some x -> x

let compare_dmy_strict dmy1 dmy2 = compare_dmy_opt ~strict:true dmy1 dmy2

let compare_date d1 d2 =
  match (d1, d2) with
  | Dgreg (dmy1, _), Dgreg (dmy2, _) -> compare_dmy dmy1 dmy2
  | Dgreg (_, _), Dtext _ -> 1
  | Dtext _, Dgreg (_, _) -> -1
  | Dtext _, Dtext _ -> 0

let compare_date_strict d1 d2 =
  match (d1, d2) with
  | Dgreg (dmy1, _), Dgreg (dmy2, _) -> compare_dmy_strict dmy1 dmy2
  | Dgreg (_, _), Dtext _ | Dtext _, Dgreg (_, _) | Dtext _, Dtext _ -> None

let cdate_to_dmy_opt cdate =
  match od_of_cdate cdate with Some (Dgreg (d, _)) -> Some d | _ -> None

let max_month_of = function
  | Dgregorian | Djulian -> 12
  | Dfrench | Dhebrew -> 13

let partial_date_upper_bound ~from ~day ~month ~year =
  match (day, month) with
  | 0, 0 | _, 0 -> (1, 1, year + 1)
  | 0, month ->
      if month = max_month_of from then (1, 1, year + 1)
      else (1, month + 1, year)
  | day, month -> (day, month, year)

let to_sdn ~from ?(lower = true) d =
  let { day; month; year; delta; _ } = d in
  let day, month, year =
    if lower then (max 1 day, max 1 month, year)
    else partial_date_upper_bound ~from ~day ~month ~year
  in
  let dmy = { d with day; month; year } in
  let base_sdn =
    match from with
    | Dgregorian -> Calendar.sdn_of_gregorian dmy
    | Djulian -> Calendar.sdn_of_julian dmy
    | Dfrench -> Calendar.sdn_of_french dmy
    | Dhebrew -> Calendar.sdn_of_hebrew dmy
  in
  base_sdn + delta

let gregorian_of_sdn ~prec sdn = Calendar.gregorian_of_sdn prec sdn
let julian_of_sdn ~prec sdn = Calendar.julian_of_sdn prec sdn
let french_of_sdn ~prec sdn = Calendar.french_of_sdn prec sdn
let hebrew_of_sdn ~prec sdn = Calendar.hebrew_of_sdn prec sdn

let convert ~from ~to_ dmy =
  if from = to_ then dmy
  else
    let via_gregorian d =
      let g =
        match from with
        | Dgregorian -> d
        | Djulian -> Calendar.gregorian_of_julian d
        | Dfrench -> Calendar.gregorian_of_french d
        | Dhebrew -> Calendar.gregorian_of_hebrew d
      in
      match to_ with
      | Dgregorian -> g
      | Djulian -> Calendar.julian_of_gregorian g
      | Dfrench -> Calendar.french_of_gregorian g
      | Dhebrew -> Calendar.hebrew_of_gregorian g
    in
    let convert_dmy2 d2 =
      Calendar.dmy2_of_dmy (via_gregorian (Calendar.dmy_of_dmy2 d2))
    in
    let prec =
      match dmy.prec with
      | OrYear d2 -> OrYear (convert_dmy2 d2)
      | YearInt d2 -> YearInt (convert_dmy2 d2)
      | p -> p
    in
    { (via_gregorian dmy) with prec }

let cdate_of_death = function
  | Death (_, cd) -> Some cd
  | NotDead | DeadYoung | DeadDontKnowWhen | DontKnowIfDead | OfCourseDead ->
      None

let dmy_of_death death = Option.bind (cdate_of_death death) cdate_to_dmy_opt
let date_of_death death = Option.bind (cdate_of_death death) od_of_cdate
