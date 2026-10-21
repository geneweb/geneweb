let of_sdn : type a. a Calendars.kind -> Calendars.sdn -> a Calendars.date =
 fun kind sdn ->
  match kind with
  | Calendars.Gregorian -> Calendars.gregorian_of_sdn sdn
  | Calendars.Julian -> Calendars.julian_of_sdn sdn
  | Calendars.French -> Calendars.french_of_sdn sdn
  | Calendars.Hebrew -> Calendars.hebrew_of_sdn sdn
  | Calendars.Islamic -> Calendars.islamic_of_sdn sdn

let to_calendar_date kind { Adef.day; month; year; delta; _ } =
  let day = max 1 day in
  let month = max 1 month in
  match Calendars.make kind ~day ~month ~year ~delta with
  | Ok d -> d
  | Error { kind = Invalid_day; _ } -> (
      match Calendars.make kind ~day:1 ~month ~year ~delta with
      | Ok d -> of_sdn kind (Calendars.to_sdn d + day - 1)
      | Error err ->
          failwith
            (Printf.sprintf "Invalid date: %s"
               (Calendars.Unsafe.to_string err.value)))
  | Error err ->
      failwith
        (Printf.sprintf "Invalid date: %s"
           (Calendars.Unsafe.to_string err.value))

let of_calendars : type a. ?prec:Adef.precision -> a Calendars.date -> Adef.dmy
    =
 fun ?(prec = Adef.Sure) { Calendars.day; month; year; delta; _ } ->
  { Adef.day; month; year; delta; prec }

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

let dmy_of_dmy2 { Adef.day2; month2; year2; delta2 } =
  {
    Adef.day = day2;
    month = month2;
    year = year2;
    prec = Adef.Sure;
    delta = delta2;
  }

let dmy2_of_dmy { Adef.day; month; year; delta; _ } =
  { Adef.day2 = day; month2 = month; year2 = year; delta2 = delta }

let convert_via_sdn from_sdn to_of_sdn d =
  let convert_dmy2 d2 =
    if d2.Adef.day2 = 0 || d2.Adef.month2 = 0 then d2
    else
      let sdn = from_sdn (dmy_of_dmy2 d2) in
      let c = of_calendars (to_of_sdn sdn) in
      {
        Adef.day2 = c.Adef.day;
        month2 = c.month;
        year2 = c.year;
        delta2 = c.delta;
      }
  in
  let prec =
    match d.Adef.prec with
    | Adef.OrYear d2 -> Adef.OrYear (convert_dmy2 d2)
    | YearInt d2 -> YearInt (convert_dmy2 d2)
    | prec -> prec
  in
  if d.Adef.day = 0 || d.month = 0 then { d with Adef.prec }
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
