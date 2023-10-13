(* TODO this is probably buggy,
   because geneweb uses month|day = 0 for incomplete dates *)
(** Convert [Adef.date] to Calendars.d *)
let to_calendars : Def.dmy -> Calendars.d =
 fun { Def.day; month; year; delta; _ } -> { Calendars.day; month; year; delta }

(** Convert Calendars.d to [Adef.date] *)
let of_calendars : ?prec:Def.precision -> Calendars.d -> Def.dmy =
 fun ?(prec = Def.Sure) { Calendars.day; month; year; delta } ->
  { Def.day; month; year; delta; prec }

let sdn_of_gregorian (d : Def.dmy) =
  Calendars.sdn_of_gregorian @@ to_calendars d

let gregorian_of_sdn prec sdn =
  of_calendars ~prec @@ Calendars.gregorian_of_sdn sdn

let sdn_of_julian (d : Def.dmy) = Calendars.sdn_of_julian @@ to_calendars d
let julian_of_sdn prec sdn = of_calendars ~prec @@ Calendars.julian_of_sdn sdn
let sdn_of_french (d : Def.dmy) = Calendars.sdn_of_french @@ to_calendars d
let french_of_sdn prec sdn = of_calendars ~prec @@ Calendars.french_of_sdn sdn
let sdn_of_hebrew (d : Def.dmy) = Calendars.sdn_of_hebrew @@ to_calendars d
let hebrew_of_sdn prec sdn = of_calendars ~prec @@ Calendars.hebrew_of_sdn sdn

let dmy_of_dmy2 : Def.dmy2 -> Def.dmy =
 fun { Def.day2; month2; year2; delta2 } ->
  {
    Def.day = day2;
    month = month2;
    year = year2;
    prec = Def.Sure;
    delta = delta2;
  }

let aux fn (d : Def.dmy) : Def.dmy =
  let aux2 d2 =
    let d = of_calendars @@ fn @@ to_calendars @@ dmy_of_dmy2 d2 in
    {
      Def.day2 = d.Def.day;
      month2 = d.Def.month;
      year2 = d.Def.year;
      delta2 = d.Def.delta;
    }
  in
  let prec =
    match d.Def.prec with
    | Def.OrYear d2 -> Def.OrYear (aux2 d2)
    | Def.YearInt d2 -> Def.YearInt (aux2 d2)
    | prec -> prec
  in
  of_calendars ~prec @@ fn @@ to_calendars d

let gregorian_of_julian = aux Calendars.gregorian_of_julian
let julian_of_gregorian = aux Calendars.julian_of_gregorian
let gregorian_of_french = aux Calendars.gregorian_of_french
let french_of_gregorian = aux Calendars.french_of_gregorian
let gregorian_of_hebrew = aux Calendars.gregorian_of_hebrew
let hebrew_of_gregorian = aux Calendars.hebrew_of_gregorian

type moon_phase = Calendars.moon_phase =
  | NewMoon
  | FirstQuarter
  | FullMoon
  | LastQuarter

let moon_phase_of_sdn = Calendars.moon_phase_of_sdn
