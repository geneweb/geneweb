type cdate = Adef.cdate =
  | Cgregorian of int
  | Cjulian of int
  | Cfrench of int
  | Chebrew of int
  | Ctext of string
  | Cdate of Adef.date
  | Cnone

type date = Adef.date = Dgreg of dmy * calendar | Dtext of string
and calendar = Adef.calendar = Dgregorian | Djulian | Dfrench | Dhebrew

and dmy = Adef.dmy = {
  day : int;
  month : int;
  year : int;
  prec : precision;
  delta : int;
}

and dmy2 = Adef.dmy2 = { day2 : int; month2 : int; year2 : int; delta2 : int }

and precision = Adef.precision =
  | Sure
  | About
  | Maybe
  | Before
  | After
  | OrYear of dmy2
  (* inteval *)
  | YearInt of dmy2

(* compress concrete date if it's possible *)
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

let cdate_of_date d =
  match d with
  | Dtext t -> Ctext t
  | Dgreg (g, cal) -> (
      match compress g with
      | None -> Cdate d
      | Some i -> (
          match cal with
          | Dgregorian -> Cgregorian i
          | Djulian -> Cjulian i
          | Dfrench -> Cfrench i
          | Dhebrew -> Chebrew i))

let cdate_of_od = function Some d -> cdate_of_date d | None -> Cnone

let od_of_cdate od =
  match od with Cnone -> None | _ -> Some (date_of_cdate od)

let cdate_None = cdate_of_od None

let dmy_of_dmy2 dmy2 =
  {
    day = dmy2.day2;
    month = dmy2.month2;
    year = dmy2.year2;
    prec = Sure;
    delta = dmy2.delta2;
  }

let leap_year a = if a mod 100 = 0 then a / 100 mod 4 = 0 else a mod 4 = 0

let nb_days_in_month m a =
  if m = 2 && leap_year a then 29
  else if m >= 1 && m <= 12 then
    [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |].(m - 1)
  else 0

(* TODO use SDN instead *)
(* elapsed time should not be represented by a date.  *)
let time_elapsed d1 d2 =
  let prec =
    match (d1.prec, d2.prec) with
    | Sure, Sure -> Sure
    | (Maybe | Sure | About), (Maybe | Sure | About) -> Maybe
    | (About | Maybe | Sure | Before), (After | Sure | Maybe | About) -> After
    | (About | Maybe | Sure | After), (Before | Sure | Maybe | About) -> Before
    | _ -> Maybe
  in
  match d1 with
  | { day = 0; month = 0; year = a1 } ->
      { day = 0; month = 0; year = d2.year - a1; prec; delta = 0 }
  | { day = 0; month = m1; year = a1 } -> (
      match d2 with
      | { day = 0; month = 0; year = a2 } ->
          { day = 0; month = 0; year = a2 - a1; prec; delta = 0 }
      | { day = 0; month = m2; year = a2 } ->
          let month, r = if m1 <= m2 then (m2 - m1, 0) else (m2 - m1 + 12, 1) in
          let year = a2 - a1 - r in
          { day = 0; month; year; prec; delta = 0 }
      | { month = m2; year = a2 } ->
          let month, r = if m1 <= m2 then (m2 - m1, 0) else (m2 - m1 + 12, 1) in
          let year = a2 - a1 - r in
          { day = 0; month; year; prec; delta = 0 })
  | { day = j1; month = m1; year = a1 } -> (
      match d2 with
      | { day = 0; month = 0; year = a2 } ->
          { day = 0; month = 0; year = a2 - a1; prec; delta = 0 }
      | { day = 0; month = m2; year = a2 } ->
          let month, r = if m1 <= m2 then (m2 - m1, 0) else (m2 - m1 + 12, 1) in
          let year = a2 - a1 - r in
          { day = 0; month; year; prec; delta = 0 }
      | { day = j2; month = m2; year = a2 } ->
          let day, r =
            if j1 <= j2 then (j2 - j1, 0)
            else (j2 - j1 + nb_days_in_month m1 a1, 1)
          in
          let month, r =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          { day; month; year; prec; delta = 0 })

let time_elapsed_opt d1 d2 =
  match (d1.prec, d2.prec) with
  | After, After | Before, Before -> None
  | _ -> Some (time_elapsed d1 d2)

(* use strict = false to compare date as if they are points on a timeline.
   use strict = true to compare date by taking precision in account. This makes some dates not comparable, do not use to sort a list *)
(* it is always Some _ if strict = false *)
let rec compare_dmy_opt ?(strict = false) dmy1 dmy2 =
  match compare dmy1.year dmy2.year with
  | 0 -> compare_month_or_day ~is_day:false strict dmy1 dmy2
  | x -> eval_strict strict dmy1 dmy2 x

and compare_month_or_day ~is_day strict dmy1 dmy2 =
  (* compare a known month|day with a unknown one (0) *)
  let compare_with_unknown_value ~strict ~unkonwn ~known =
    match unkonwn.prec with
    | After -> Some 1
    | Before -> Some (-1)
    | _other -> if strict then None else compare_prec false unkonwn known
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
  | 0, _ -> compare_with_unknown_value ~strict ~unkonwn:dmy1 ~known:dmy2
  | _, 0 ->
      (* swap dmy1 and dmy2 *)
      Option.map Int.neg
      @@ compare_with_unknown_value ~strict ~unkonwn:dmy2 ~known:dmy1
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
  | None -> (* can't happen *) assert false
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
  match od_of_cdate cdate with
  | Some (Dgreg (d, _)) -> Some d
  | Some (Dtext _) | None -> None

let cdate_of_death = function
  | Def.Death (_, cd) -> Some cd
  | NotDead | DeadYoung | DeadDontKnowWhen | DontKnowIfDead | OfCourseDead ->
      None

let dmy_of_death death = Option.bind (cdate_of_death death) cdate_to_dmy_opt
let date_of_death death = Option.bind (cdate_of_death death) od_of_cdate

(* Calendars library wrapper *)

let max_month_of cal =
  match cal with Dgregorian | Djulian -> 12 | Dfrench | Dhebrew -> 13

let partial_date_upper_bound ~from ~day ~month ~year =
  let day, month, year =
    match (day, month) with
    | 0, 0 -> (1, 1, year + 1)
    | _day, 0 -> (1, 1, year + 1)
    | 0, month ->
        if month = max_month_of from then (1, 1, year + 1)
        else (1, month + 1, year)
    | day, month -> (day, month, year)
  in
  (day, month, year)

let partial_date_lower_bound ~day ~month ~year = (max 1 day, max 1 month, year)

(* In the case of partial date Calendars.make fail.
   So we convert a lower or upper bound of the date depending on [lower] instead *)
let to_calendars ~from ~day ~month ~year ~delta ~lower =
  let bound =
    if lower then partial_date_lower_bound else partial_date_upper_bound ~from
  in
  let day, month, year = bound ~day ~month ~year in
  match from with
  | Dgregorian ->
      Calendars.make Gregorian ~day ~month ~year ~delta |> Result.get_ok
  | Djulian ->
      Calendars.make Julian ~day ~month ~year ~delta
      |> Result.get_ok |> Calendars.to_gregorian
  | Dfrench ->
      Calendars.make French ~day ~month ~year ~delta
      |> Result.get_ok |> Calendars.to_gregorian
  | Dhebrew ->
      Calendars.make Hebrew ~day ~month ~year ~delta
      |> Result.get_ok |> Calendars.to_gregorian

(* [to_sdn] does not work if day|month are unknown
   so we return sdn of [partial_date_lower_bound d] instead *)
let to_sdn ~from d =
  let { day; month; year; delta } = d in
  to_calendars ~from ~day ~month ~year ~delta ~lower:true |> Calendars.to_sdn

let of_calendars_raw ~prec date =
  let { Calendars.day; month; year; delta; _ } = date in
  { day; month; year; delta; prec }

let gregorian_of_sdn ~prec sdn =
  Calendars.gregorian_of_sdn sdn |> of_calendars_raw ~prec

let julian_of_sdn ~prec sdn =
  Calendars.julian_of_sdn sdn |> of_calendars_raw ~prec

let french_of_sdn ~prec sdn =
  Calendars.french_of_sdn sdn |> of_calendars_raw ~prec

let hebrew_of_sdn ~prec sdn =
  Calendars.hebrew_of_sdn sdn |> of_calendars_raw ~prec

(* Geneweb uses day|month=0 for partially known dates, this makes converting dates troublesome
   In the case of partial date we use dmy.delta to define an interval:
     [dmy; dmy.delta = to_sdn(dmy) + d.delta (in SDN) [
   If we convert a partial date and convert it back, we try to recover the fact
   that it was a partial date.

   BUG : with day>0 and month=0 we do not recover a partial date correctly. *)
let convert ~from ~to_ dmy =
  if from = to_ then dmy else
  let dmy_tuple_of_sdn ~to_ sdn =
    let to_tuple date =
      let { Calendars.day; month; year; delta; _ } = date in
      (day, month, year, delta)
    in
    match to_ with
    | Dgregorian -> Calendars.gregorian_of_sdn sdn |> to_tuple
    | Djulian -> Calendars.julian_of_sdn sdn |> to_tuple
    | Dfrench -> Calendars.french_of_sdn sdn |> to_tuple
    | Dhebrew -> Calendars.hebrew_of_sdn sdn |> to_tuple
  in
  (* code addapted from Calendars *)
  let convert_aux ~from ~to_ ~day ~month ~year ~delta =
    let sdn_min =
      to_calendars ~from ~day ~month ~year ~delta ~lower:true
      |> Calendars.to_sdn
    in
    let sdn_max =
      let sdn_max =
        to_calendars ~from ~day ~month ~year ~delta ~lower:false
        |> Calendars.to_sdn
      in
      (* we want sdn_min < sdn_max; add + 1 if needed *)
      if sdn_min = sdn_max then sdn_max + 1 else sdn_max
    in
    let day_min, month_min, year_min, delta_min =
      dmy_tuple_of_sdn ~to_ sdn_min
    in
    let day_max, month_max, year_max, _delta_max =
      dmy_tuple_of_sdn ~to_ (sdn_max + delta)
    in
    (* if min date and max date looks like they came from a partial date,
       reconstitute it as a partial date (with day|month = 0) *)
    if day_min = 1 && day_max = 1 then
      if month_min = 1 && month_max = 1 then
        if year_min + 1 = year_max then (0, 0, year_min, 0)
        else (day_min, month_min, year_min, sdn_max + delta - sdn_min - 1)
      else if
        month_min + 1 = month_max
        || (month_min = max_month_of to_ && year_min + 1 = year_max)
      then (0, month_min, year_min, delta_min)
      else (day_min, month_min, year_min, sdn_max + delta - sdn_min - 1)
    else (day_min, month_min, year_min, sdn_max + delta - sdn_min - 1)
  in

  let convert_dmy2 ~from ~to_ { day2; month2; year2; delta2 } =
    let day2, month2, year2, delta2 =
      convert_aux ~from ~to_ ~day:day2 ~month:month2 ~year:year2 ~delta:delta2
    in
    { day2; month2; year2; delta2 }
  in

  let { day; month; year; delta; prec } = dmy in
  let day, month, year, delta =
    convert_aux ~from ~to_ ~day ~month ~year ~delta
  in
  match prec with
  | (Sure | About | Maybe | Before | After) as p ->
      { day; month; year; delta; prec = p }
  | OrYear dmy2 ->
      { day; month; year; delta; prec = OrYear (convert_dmy2 ~from ~to_ dmy2) }
  | YearInt dmy2 ->
      { day; month; year; delta; prec = YearInt (convert_dmy2 ~from ~to_ dmy2) }

let compare_date d1 d2 =
  match (d1, d2) with
  | Dgreg (dmy1, _), Dgreg (dmy2, _) -> compare_dmy dmy1 dmy2
  | Dgreg (_, _), Dtext _ -> 1
  | Dtext _, Dgreg (_, _) -> -1
  | Dtext _, Dtext _ -> 0

let compare_date_strict d1 d2 =
  match (d1, d2) with
  | Dgreg (dmy1, _), Dgreg (dmy2, _) -> compare_dmy_opt ~strict:true dmy1 dmy2
  | Dgreg (_, _), Dtext _ | Dtext _, Dgreg (_, _) | Dtext _, Dtext _ -> None
