type display = { nb_day : int; nb_month : int; nb_year : int }
type precision = Less | Exact | More | Undefined

(* TODO should we consider sdn interval instead? ..... *)
type t = { sdn : int; prec : precision; display : display }

let compare a b = Int.compare a.sdn b.sdn

(* compute precission of a duration made from 2 durations *)
let compute_prec p1 p2 =
  match (p1, p2) with
  | Exact, Exact -> Exact
  (* TODO NO Less, Less is undefined *)
  | (Less | Exact), (Less | Exact) -> Less
  | (More | Exact), (More | Exact) -> More
  | Less, More | More, Less | Undefined, _ | _, Undefined -> Undefined

let date_prec_to_duration_prec = function
  | Date.Sure -> Exact
  | Before -> Less
  | After -> More
  | Maybe | About | YearInt _ | OrYear _ -> Undefined

let of_sdn ~prec sdn =
  (* TODO no fix display *)
  let { Date.day; month; year } = Date.gregorian_of_sdn ~prec:Sure sdn in
  { sdn; prec; display = { nb_day = day; nb_month = month; nb_year = year } }

let of_years i = of_sdn ~prec:Exact (365 * i)
let of_months i = of_sdn ~prec:Exact (30 * i)
let of_days i = of_sdn ~prec:Exact i

(* I think nb_day depends on the original dates we computed the elapsed_time on ... so we compute displayable_elapsed_time here, so elapsed_time is not juste = sdn
   TODO do we care about this?
*)
(* TODO check this; maybe just keep old version; maybe it handled unknown date correctly? *)
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
  of_sdn ~prec sdn

(* -- TODO put un Def_show ? -- *)
let prec_to_string = function
  | Exact -> "Exact"
  | Less -> "Less"
  | More -> "More"
  | Undefined -> "Undefined"

let pp_duration_debug fmt d =
  Format.fprintf fmt "{sdn: %d;prec: %s;display: {%d; %d; %d}" d.sdn
    (prec_to_string d.prec) d.display.nb_day d.display.nb_month
    d.display.nb_year
