(* Copyright (c) 1998-2007 INRIA *)

open Def

let dmy_of_dmy2 dmy2 =
  {day = dmy2.day2; month = dmy2.month2; year = dmy2.year2; prec = Sure;
   delta = dmy2.delta2}

let leap_year a = if a mod 100 = 0 then a / 100 mod 4 = 0 else a mod 4 = 0

let nb_days_in_month m a =
  if m = 2 && leap_year a
  then 29
  else if m >= 1 && m <= 12
  then [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |].(m-1)
  else 0

let time_elapsed d1 d2 =
  let prec =
    match d1.prec, d2.prec with
    | (Sure, Sure) -> Sure
    | (Maybe | Sure | About), (Maybe | Sure | About) -> Maybe
    | (About | Maybe | Sure | Before), (After | Sure | Maybe | About) -> After
    | (About | Maybe | Sure | After), (Before | Sure | Maybe | About) -> Before
    | _ -> Maybe
  in
  match d1 with
  | {day = 0; month = 0; year = a1} ->
    {day = 0; month = 0; year = d2.year - a1; prec = prec; delta = 0}
  | {day = 0; month = m1; year = a1} ->
    begin match d2 with
        {day = 0; month = 0; year = a2} ->
        {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
      | {day = 0; month = m2; year = a2} ->
        let (month, r) =
          if m1 <= m2 then m2 - m1, 0 else m2 - m1 + 12, 1
        in
        let year = a2 - a1 - r in
        {day = 0; month = month; year = year; prec = prec; delta = 0}
      | {month = m2; year = a2} ->
        let (month, r) =
          if m1 <= m2 then m2 - m1, 0 else m2 - m1 + 12, 1
        in
        let year = a2 - a1 - r in
        {day = 0; month = month; year = year; prec = prec; delta = 0}
    end
  | {day = j1; month = m1; year = a1} ->
    match d2 with
      {day = 0; month = 0; year = a2} ->
      {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
    | {day = 0; month = m2; year = a2} ->
      let (month, r) =
        if m1 <= m2 then m2 - m1, 0 else m2 - m1 + 12, 1
      in
      let year = a2 - a1 - r in
      {day = 0; month = month; year = year; prec = prec; delta = 0}
    | {day = j2; month = m2; year = a2} ->
      let (day, r) =
        if j1 <= j2 then j2 - j1, 0
        else j2 - j1 + nb_days_in_month m1 a1, 1
      in
      let (month, r) =
        if m1 + r <= m2 then m2 - m1 - r, 0 else m2 - m1 - r + 12, 1
      in
      let year = a2 - a1 - r in
      {day = day; month = month; year = year; prec = prec; delta = 0}

let time_elapsed_opt d1 d2 =
  match d1.prec, d2.prec with
  | After, After | Before, Before -> None
  | _ -> Some (time_elapsed d1 d2)

let date_of_death =
  function
  | Death (_, cd) -> Some (Adef.date_of_cdate cd)
  | NotDead | DeadYoung | DeadDontKnowWhen
  | DontKnowIfDead | OfCourseDead -> None

let rec compare_dmy_opt ?(strict=false) dmy1 dmy2 =
  match compare dmy1.year dmy2.year with
  | 0 -> compare_month_or_day ~is_day:false strict dmy1 dmy2
  | x -> eval_strict strict dmy1 dmy2 x
and compare_month_or_day ~is_day strict dmy1 dmy2 =
  (* compare a known month|day with a unknown one (0) *)
  let compare_with_unknown_value ~strict ~unkonwn ~known =
    match unkonwn.prec with
      | After -> Some 1
      | Before -> Some (-1)
      | _other -> if strict then None
      else compare_prec false unkonwn known
  in
  (* if we are comparing months the next comparison to do is on days
      else if we are comparing days it is compare_prec *)
  let x, y, next_comparison =
    if is_day then dmy1.day, dmy2.day, compare_prec
    else dmy1.month, dmy2.month, compare_month_or_day ~is_day:true
  in
  (* 0 means month|day is unknow*)
  match x,y with
  | 0, 0 -> compare_prec strict dmy1 dmy2
  | 0, _ ->
    compare_with_unknown_value ~strict ~unkonwn:dmy1 ~known:dmy2
  | _, 0 ->
    (* swap dmy1 and dmy2 *)
    Option.map Int.neg @@
      compare_with_unknown_value ~strict ~unkonwn:dmy2 ~known:dmy1
  | m1, m2 -> match Int.compare m1 m2 with
    | 0 -> next_comparison strict dmy1 dmy2
    | x -> eval_strict strict dmy1 dmy2 x
and compare_prec strict dmy1 dmy2 =
  match dmy1.prec, dmy2.prec with
  | (Sure|About|Maybe), (Sure|About|Maybe) -> Some 0
  | After, After | Before, Before -> Some 0
  | OrYear dmy1, OrYear dmy2 | YearInt dmy1, YearInt dmy2 ->
    compare_dmy_opt ~strict (dmy_of_dmy2 dmy1) (dmy_of_dmy2 dmy2)
  | _, After | Before, _ -> Some (-1)
  | After, _ | _, Before -> Some 1
  | _ -> Some 0
and eval_strict strict dmy1 dmy2 x =
    if strict then match x with
      | -1 when dmy1.prec = After || dmy2.prec = Before -> None
      | 1 when dmy1.prec = Before || dmy2.prec = After -> None
      | x -> Some x
    else Some x

exception Not_comparable

let compare_dmy ?(strict=false) dmy1 dmy2 =
  match compare_dmy_opt ~strict dmy1 dmy2 with
    | None -> raise Not_comparable
    | Some x -> x

let compare_date ?(strict=false) d1 d2 =
  match d1, d2 with
  | Dgreg (dmy1, _), Dgreg (dmy2, _) -> compare_dmy ~strict dmy1 dmy2
  | Dgreg (_, _), Dtext _ -> if strict then raise Not_comparable else 1
  | Dtext _, Dgreg (_, _) -> if strict then raise Not_comparable else -1
  | Dtext _, Dtext _ -> if strict then raise Not_comparable else 0
