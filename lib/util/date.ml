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
  let sign, d1, d2 =
    if d1.year < d2.year
    then (fun i -> i), d1, d2
    else if d1.month < d2.month
    then (fun i -> i), d1, d2
    else if d1.day > d2.day
    then (fun i -> - i), d2, d1
    else (fun i -> i), d1, d2
  in
  match d1 with
  | {day = 0; month = 0; year = a1} ->
    {day = 0; month = 0; year = sign (d2.year - a1); prec = prec; delta = 0}
  | {day = 0; month = m1; year = a1} ->
    begin match d2 with
        {day = 0; month = 0; year = a2} ->
        {day = 0; month = 0; year = sign (a2 - a1); prec = prec; delta = 0}
      | {day = 0; month = m2; year = a2} ->
        let (month, r) =
          if m1 <= m2 then m2 - m1, 0 else m2 - m1 + 12, 1
        in
        let year = a2 - a1 - r in
        {day = 0; month = sign month; year = sign year; prec = prec; delta = 0}
      | {month = m2; year = a2} ->
        let (month, r) =
          if m1 <= m2 then m2 - m1, 0 else m2 - m1 + 12, 1
        in
        let year = a2 - a1 - r in
        {day = 0; month = sign month; year = sign year; prec = prec; delta = 0}
    end
  | {day = j1; month = m1; year = a1} ->
    match d2 with
      {day = 0; month = 0; year = a2} ->
      {day = 0; month = 0; year = sign (a2 - a1); prec = prec; delta = 0}
    | {day = 0; month = m2; year = a2} ->
      let (month, r) =
        if m1 <= m2 then m2 - m1, 0 else m2 - m1 + 12, 1
      in
      let year = a2 - a1 - r in
      {day = 0; month = sign month; year = sign year; prec = prec; delta = 0}
    | {day = j2; month = m2; year = a2} ->
      let (day, r) =
        if j1 <= j2 then j2 - j1, 0
        else j2 - j1 + nb_days_in_month m1 a1, 1
      in
      let (month, r) =
        if m1 + r <= m2 then m2 - m1 - r, 0 else m2 - m1 - r + 12, 1
      in
      let year = a2 - a1 - r in
      {day = sign day; month = sign month; year = sign year; prec = prec; delta = 0}

let time_elapsed_opt d1 d2 =
  match d1.prec, d2.prec with
  | After, After | Before, Before -> None
  | _ -> Some (time_elapsed d1 d2)

let date_of_death =
  function
    Death (_, cd) -> Some (Adef.date_of_cdate cd)
  | _ -> None

exception Not_comparable

let rec compare_dmy_aux failure success ?(strict=false) dmy1 dmy2 =
  match dmy1.prec, dmy2.prec with
  | Before, (Sure | Maybe | About)
  | (Sure | Maybe | About), After ->
    begin match time_elapsed dmy1 dmy2 with
      | { year = 0 ; month = 0 ; day = -1 ; prec = (Sure | Maybe | After) } ->
        if strict then failure () else success 0
      | _ -> compare_year strict failure success dmy1 dmy2
    end
  | After, (About | Maybe | Sure)
  | (About | Maybe | Sure), Before ->
    begin match time_elapsed dmy1 dmy2 with
      | { year = 0 ; month = 0 ; day = 1 ; prec = (Sure | Maybe | Before) } ->
        if strict then failure () else success 0
      | _ -> compare_year strict failure success dmy1 dmy2
    end
  | _, _ -> compare_year strict failure success dmy1 dmy2
and compare_year strict failure success dmy1 dmy2 =
  match compare dmy1.year dmy2.year with
  | 0 -> compare_month strict failure success dmy1 dmy2
  | x ->
    if strict then match x with
      | -1 when dmy1.prec = After || dmy2.prec = Before -> failure ()
      | 1 when dmy1.prec = Before || dmy2.prec = After -> failure ()
      | x -> success x
    else success x
and compare_month strict failure success dmy1 dmy2 = match dmy1.month, dmy2.month with
  | 0, 0 -> cmp_prec strict failure success dmy1 dmy2
  | 0, _ ->
    if dmy1.prec = After then success 1
    else if strict then failure ()
    else cmp_prec false failure success dmy1 dmy2
  | _, 0 ->
    if dmy2.prec = After then success (-1)
    else if strict then failure ()
    else cmp_prec false failure success dmy1 dmy2
  | m1, m2 -> match compare m1 m2 with
    | 0 -> compare_day strict failure success dmy1 dmy2
    | x -> success x
and compare_day strict failure success dmy1 dmy2 = match dmy1.day, dmy2.day with
  | 0, 0 -> cmp_prec strict failure success dmy1 dmy2
  | 0, _ ->
    if dmy1.prec = After then success 1
    else if strict then failure ()
    else cmp_prec false failure success dmy1 dmy2
  | _, 0 ->
    if dmy2.prec = After then success (-1)
    else if strict then failure ()
    else cmp_prec false failure success dmy1 dmy2
  | d1, d2 -> match compare d1 d2 with
    | 0 -> cmp_prec strict failure success dmy1 dmy2
    | x -> success x
and cmp_prec strict failure success dmy1 dmy2 =
  match dmy1.prec, dmy2.prec with
  | (Sure|About|Maybe), (Sure|About|Maybe) -> success 0
  | After, After | Before, Before -> success 0
  | OrYear dmy1, OrYear dmy2 | YearInt dmy1, YearInt dmy2 ->
    compare_dmy_aux ~strict failure success (dmy_of_dmy2 dmy1) (dmy_of_dmy2 dmy2)
  | _, After | Before, _ -> success (-1)
  | After, _ | _, Before -> success 1
  | _ -> success 0

let compare_dmy = compare_dmy_aux (fun () -> raise Not_comparable) (fun x -> x)
let compare_dmy_opt = compare_dmy_aux (fun () -> None) (fun x -> Some x)

let compare_date ?(strict=false) d1 d2 =
  match d1, d2 with
  | Dgreg (dmy1, _), Dgreg (dmy2, _) -> compare_dmy ~strict dmy1 dmy2
  | Dgreg (_, _), Dtext _ -> if strict then raise Not_comparable else 1
  | Dtext _, Dgreg (_, _) -> if strict then raise Not_comparable else -1
  | Dtext _, Dtext _ -> if strict then raise Not_comparable else 0
