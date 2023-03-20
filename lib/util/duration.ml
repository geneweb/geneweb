type display = { nb_day : int; nb_month : int; nb_year : int }

(* TODO do we really neewd a precision on duration..? *)
type precision = Less | Exact | More | Unknown
type t = { sdn : int; prec : precision; display : display }

let compare a b = Int.compare a.sdn b.sdn

(* compute precission of a duration made from 2 durations *)
let compute_prec p1 p2 =
  match (p1, p2) with
  | Exact, Exact -> Exact
  | (Less | Exact), (Less | Exact) -> Less
  | (More | Exact), (More | Exact) -> More
  | Less, More | More, Less | Unknown, _ | _, Unknown -> Unknown

let date_prec_to_duration_prec = function
  | Date.Sure -> Exact
  | Before -> Less
  | After -> More
  | Maybe | About | YearInt _ | OrYear _ -> Unknown

(*
TODO
let displayable_time_elapsed dmy1 dmy2 =
  time_elapsed dmy1 dmy2 |> to_displayable_duration
  *)
let of_sdn sdn =
  (* TODO better display *)
  { sdn; prec = Exact; display = { nb_day = sdn; nb_month = 0; nb_year = 0 } }

(* I think nb_day depends on the original dates we computed the elapsed_time on ... so we compute displayable_elapsed_time here, so elapsed_time is not juste = sdn
   TODO do we care about this?
*)
let time_elapsed d1 d2 =
  let Date.{ day = j1; month = m1; year = a1 } = d1 in
  let Date.{ day = j2; month = m2; year = a2 } = d2 in
  let nb_day, r =
    if j1 <= j2 then (j2 - j1, 0) else (j2 - j1 + Date.nb_days_in_month m1 a1, 1)
  in
  let nb_month, r =
    if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
  in
  let nb_year = a2 - a1 - r in
  let sdn1 = Date.to_sdn ~from:Dgregorian d1 in
  let sdn2 = Date.to_sdn ~from:Dgregorian d2 in
  let sdn = sdn2 - sdn1 in
  let prec =
    compute_prec
      (date_prec_to_duration_prec d1.prec)
      (date_prec_to_duration_prec d2.prec)
  in
  { sdn; prec; display = { nb_day; nb_month; nb_year } }

let time_elapsed_opt d1 d2 =
  match (d1.Date.prec, d2.Date.prec) with
  | After, After | Before, Before -> None
  | _ -> Some (time_elapsed d1 d2)

let add a b =
  let sdn = a.sdn + b.sdn in
  let prec = compute_prec a.prec b.prec in
  (* TODO better display *)
  { sdn; prec; display = { nb_day = sdn; nb_month = 0; nb_year = 0 } }

(* TODO remove? *)
let div a n =
  let sdn = a.sdn / n in
  (* TODO better display *)
  { sdn; prec = a.prec; display = { nb_day = sdn; nb_month = 0; nb_year = 0 } }
