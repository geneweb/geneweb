(* $Id: calendar.ml,v 2.2 1999-09-16 08:13:36 ddr Exp $ *)
(* Borrowed from Scott E. Lee http://genealogy.org/~scottlee/; converted his
   C program into an Ocaml program.

   SDN 1 is November 25, 4714 BC Gregorian calendar *)

open Def;

(* gregorian *)

value sdn_offset = 32045;
value days_per_5_months = 153;
value days_per_4_years = 1461;
value days_per_400_years = 146097;

value sdn_of_gregorian d =
  let year = if d.year < 0 then d.year + 4801 else d.year + 4800 in
  let (month, year) =
    if d.month > 2 then (d.month - 3, year) else (d.month + 9, year - 1)
  in
  year / 100 * days_per_400_years / 4 + year mod 100 * days_per_4_years / 4 +
    (month * days_per_5_months + 2) / 5 + d.day - sdn_offset
;

value gregorian_of_sdn prec sdn =
  let temp = (sdn + sdn_offset) * 4 - 1 in
  let century = temp / days_per_400_years in
  let temp = ((temp mod days_per_400_years) / 4) * 4 + 3 in
  let year = (century * 100) + (temp / days_per_4_years) in
  let dayOfYear = (temp mod days_per_4_years) / 4 + 1 in
  let temp = dayOfYear * 5 - 3 in
  let month = temp / days_per_5_months in
  let day = (temp mod days_per_5_months) / 5 + 1 in
  let (month, year) =
    if month < 10 then (month + 3, year) else (month - 9, year + 1)
  in
  let year = year - 4800 in
  let year = if year <= 0 then year - 1 else year in
  {day = day; month = month; year = year; prec = prec}
;

(* julian *)

value sdn_offset = 32083;
value days_per_5_months = 153;
value days_per_4_years = 1461;

value sdn_of_julian d =
  let year = if d.year < 0 then d.year + 4801 else d.year + 4800 in
  let (month, year) =
    if d.month > 2 then (d.month - 3, year) else (d.month + 9, year - 1)
  in
  (year * days_per_4_years) / 4 + (month * days_per_5_months + 2) / 5 +
     d.day - sdn_offset
;

value julian_of_sdn prec sdn =
  let temp = (sdn + sdn_offset) * 4 - 1 in
  let year = temp / days_per_4_years in
  let dayOfYear = (temp mod days_per_4_years) / 4 + 1 in
  let temp = dayOfYear * 5 - 3 in
  let month = temp / days_per_5_months in
  let day = (temp mod days_per_5_months) / 5 + 1 in
  let (month, year) =
    if month < 10 then (month + 3, year) else (month - 9, year + 1)
  in
  let year = year - 4800 in
  let year = if year <= 0 then year - 1 else year in
  {day = day; month = month; year = year; prec = prec}
;

(* french *)

value sdn_offset = 2375474;
value days_per_4_years = 1461;
value days_per_month = 30;

value sdn_of_french d =
  (d.year * days_per_4_years) / 4
  + (d.month - 1) * days_per_month
  + d.day + sdn_offset
;

value french_of_sdn prec sdn =
  let temp = (sdn - sdn_offset) * 4 - 1 in
  let year = temp / days_per_4_years in
  let temp = temp mod days_per_4_years in
  let temp = if temp < 0 then temp + days_per_4_years else temp in
  let dayOfYear = temp / 4 in
  let month = dayOfYear / days_per_month + 1 in
  let day = dayOfYear mod days_per_month + 1 in
  {day = day; month = month; year = year; prec = prec}
;

(* hebrew *)

value halakim_per_hour = 1080;
value halakim_per_day = 25920;
value halakim_per_lunar_cycle = (29 * halakim_per_day) + 13753;
value halakim_per_metonic_cycle = halakim_per_lunar_cycle * (12 * 19 + 7);

value sdn_offset = 347997;
value new_moon_of_creation = 31524;

value sunday = 0;
value monday = 1;
value tuesday = 2;
value wednesday = 3;
value thursday = 4;
value friday = 5;
value saturday = 6;

value noon = 18 * halakim_per_hour;
value am3_11_20 = 9 * halakim_per_hour + 204;
value am9_32_43 = 15 * halakim_per_hour + 589;

value monthsPerYear =
  [| 12; 12; 13; 12; 12; 13; 12; 13; 12; 12;
     13; 12; 12; 13; 12; 12; 13; 12; 13 |];

value yearOffset =
  [| 0; 12; 24; 37; 49; 61; 74; 86; 99; 111; 123;
     136; 148; 160; 173; 185; 197; 210; 222 |];

value ftishri1 metonicYear moladDay moladHalakim =
  let tishri1 = moladDay in
  let dow = tishri1 mod 7 in
  let leapYear =
    match metonicYear with
    [ 2 | 5 | 7 | 10 | 13 | 16 | 18 -> True
    | _ -> False ]
  in
  let lastWasLeapYear =
    match metonicYear with
    [ 3 | 6 | 8 | 11 | 14 | 17 | 0 -> True
    | _ -> False ]
  in
  let (tishri1, dow) =
    if moladHalakim >= noon
    || not leapYear && dow == tuesday && moladHalakim >= am3_11_20
    || lastWasLeapYear && dow == monday && moladHalakim >= am9_32_43
    then
      let tishri1 = tishri1 + 1 in
      let dow = dow + 1 in
      let dow = if dow == 7 then 0 else dow in
      (tishri1, dow)
    else
      (tishri1, dow)
  in
  let tishri1 =
    if dow == wednesday || dow == friday || dow == sunday then tishri1 + 1
    else tishri1
  in
  tishri1
;

value moladOfMetonicCycle metonicCycle =
  let r1 = new_moon_of_creation in
  let r1 = r1 + metonicCycle * (halakim_per_metonic_cycle land 0xFFFF) in
  let r2 = r1 lsr 16 in
  let r2 =
    r2 + metonicCycle * ((halakim_per_metonic_cycle lsr 16) land 0xFFFF)
  in
  let d2 = r2 / halakim_per_day in
  let r2 = r2 - d2 * halakim_per_day in
  let r1 = 4. *. float (r2 lsl 14) +. float (r1 land 0xFFFF) in
  let d1 = truncate (r1 /. (float halakim_per_day) +. 0.5) in
  let r1 = truncate (r1 -. float d1 *. float halakim_per_day +. 0.5) in
  let pMoladDay = (d2 lsl 16) lor d1 in
  let pMoladHalakim = r1 in
  (pMoladDay, pMoladHalakim)
;

value findStartOfYear year =
  let pMetonicCycle = (year - 1) / 19 in
  let pMetonicYear = (year - 1) mod 19 in
  let (pMoladDay, pMoladHalakim) = moladOfMetonicCycle pMetonicCycle in
  let pMoladHalakim =
    pMoladHalakim + halakim_per_lunar_cycle * yearOffset.(pMetonicYear)
  in
  let pMoladDay = pMoladDay + pMoladHalakim / halakim_per_day in
  let pMoladHalakim = pMoladHalakim mod halakim_per_day in
  let pTishri1 = ftishri1 pMetonicYear pMoladDay pMoladHalakim in
  (pMetonicCycle, pMetonicYear, pMoladDay, pMoladHalakim, pTishri1)
;

value sdn_of_hebrew d =
  let sdn =
    match d.month with
    [ 1 | 2 ->
        let (metonicCycle, metonicYear, moladDay, moladHalakim, tishri1) =
          findStartOfYear d.year
        in
        if d.month = 1 then tishri1 + d.day - 1 else tishri1 + d.day + 29
    | 3 ->
        let (metonicCycle, metonicYear, moladDay, moladHalakim, tishri1) =
          findStartOfYear d.year
        in
        let moladHalakim =
          moladHalakim + halakim_per_lunar_cycle * monthsPerYear.(metonicYear)
        in
        let moladDay = moladDay + moladHalakim / halakim_per_day in
        let moladHalakim = moladHalakim mod halakim_per_day in
        let tishri1After =
          ftishri1 ((metonicYear + 1) mod 19) moladDay moladHalakim
        in
        let yearLength = tishri1After - tishri1 in
        if yearLength == 355 || yearLength == 385 then tishri1 + d.day + 59
        else tishri1 + d.day + 58
    | 4 | 5 | 6 ->
        let (metonicCycle, metonicYear, moladDay, moladHalakim, tishri1After) =
          findStartOfYear (d.year + 1)
        in
        let lengthOfAdarIAndII =
          if monthsPerYear.((d.year - 1) mod 19) == 12 then 29 else 59
        in
        if d.month == 4 then
          tishri1After + d.day - lengthOfAdarIAndII - 237
        else if d.month == 5 then
          tishri1After + d.day - lengthOfAdarIAndII - 208
        else
          tishri1After + d.day - lengthOfAdarIAndII - 178
    | _ ->
        let (metonicCycle, metonicYear, moladDay, moladHalakim, tishri1After) =
          findStartOfYear (d.year + 1)
        in
        match d.month with
        [ 7 -> tishri1After + d.day - 207
        | 8 -> tishri1After + d.day - 178
        | 9 -> tishri1After + d.day - 148
        | 10 -> tishri1After + d.day - 119
        | 11 -> tishri1After + d.day - 89
        | 12 -> tishri1After + d.day - 60
        | 13 -> tishri1After + d.day - 30
        | _ -> invalid_arg "sdn_of_hebrew" ] ]
  in
  sdn + sdn_offset
;

(* from and to gregorian *)

value gregorian_of_julian d =
  if d.day = 0 then d else gregorian_of_sdn d.prec (sdn_of_julian d)
;

value julian_of_gregorian d =
  if d.day = 0 then d else julian_of_sdn d.prec (sdn_of_gregorian d)
;

value gregorian_of_french d =
  if d.day = 0 then
    if d.month = 0 then {(d) with year = d.year + 1791}
    else if d.month < 5 then
      {(d) with month = d.month + 8; year = d.year + 1791}
    else
      {(d) with month = d.month - 4; year = d.year + 1792}
  else gregorian_of_sdn d.prec (sdn_of_french d)
;

value french_of_gregorian d =
  if d.day = 0 then
    if d.month = 0 then {(d) with year = d.year - 1791}
    else if d.month > 8 then
      {(d) with month = d.month - 8; year = d.year - 1791}
    else
      {(d) with month = d.month + 4; year = d.year - 1792}

  else french_of_sdn d.prec (sdn_of_gregorian d)
;
