let to_calendar_date kind { Def.day; month; year; delta; _ } =
  let day = max 1 day in
  let month = max 1 month in
  match Calendars.make kind ~day ~month ~year ~delta with
  | Ok d -> d
  | Error err ->
      failwith
        (Printf.sprintf "Invalid date: %s"
           (Calendars.Unsafe.to_string err.value))

let of_calendars : type a. ?prec:Def.precision -> a Calendars.date -> Def.dmy =
 fun ?(prec = Def.Sure) { Calendars.day; month; year; delta; _ } ->
  { Def.day; month; year; delta; prec }

let sdn_of_gregorian d =
  Calendars.to_sdn (to_calendar_date Calendars.Gregorian d)

let gregorian_of_sdn prec sdn =
  of_calendars ~prec (Calendars.gregorian_of_sdn sdn)

let sdn_of_julian d = Calendars.to_sdn (to_calendar_date Calendars.Julian d)
let julian_of_sdn prec sdn = of_calendars ~prec (Calendars.julian_of_sdn sdn)
let sdn_of_french d = Calendars.to_sdn (to_calendar_date Calendars.French d)
let french_of_sdn prec sdn = of_calendars ~prec (Calendars.french_of_sdn sdn)
let sdn_of_hebrew d = Calendars.to_sdn (to_calendar_date Calendars.Hebrew d)
let hebrew_of_sdn prec sdn = of_calendars ~prec (Calendars.hebrew_of_sdn sdn)

let dmy_of_dmy2 { Def.day2; month2; year2; delta2 } =
  {
    Def.day = day2;
    month = month2;
    year = year2;
    prec = Def.Sure;
    delta = delta2;
  }

let convert_via_sdn from_sdn to_of_sdn d =
  let convert_dmy2 d2 =
    if d2.Def.day2 = 0 || d2.Def.month2 = 0 then d2
    else
      let sdn = from_sdn (dmy_of_dmy2 d2) in
      let c = of_calendars (to_of_sdn sdn) in
      {
        Def.day2 = c.Def.day;
        month2 = c.Def.month;
        year2 = c.Def.year;
        delta2 = c.Def.delta;
      }
  in
  let prec =
    match d.Def.prec with
    | Def.OrYear d2 -> Def.OrYear (convert_dmy2 d2)
    | Def.YearInt d2 -> Def.YearInt (convert_dmy2 d2)
    | prec -> prec
  in
  if d.Def.day = 0 || d.Def.month = 0 then { d with Def.prec }
  else
    let sdn = from_sdn d in
    of_calendars ~prec (to_of_sdn sdn)

let gregorian_of_julian d =
  convert_via_sdn sdn_of_julian Calendars.gregorian_of_sdn d

let julian_of_gregorian d =
  convert_via_sdn sdn_of_gregorian Calendars.julian_of_sdn d

let gregorian_of_french d =
  convert_via_sdn sdn_of_french Calendars.gregorian_of_sdn d

let french_of_gregorian d =
  convert_via_sdn sdn_of_gregorian Calendars.french_of_sdn d

let gregorian_of_hebrew d =
  convert_via_sdn sdn_of_hebrew Calendars.gregorian_of_sdn d

let hebrew_of_gregorian d =
  convert_via_sdn sdn_of_gregorian Calendars.hebrew_of_sdn d

type moon_phase = Calendars.moon_phase =
  | NewMoon
  | FirstQuarter
  | FullMoon
  | LastQuarter

let moon_phase_of_sdn = Calendars.moon_phase_of_sdn
