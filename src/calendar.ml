(* $Id: calendar.ml,v 4.1 2001-04-10 23:10:28 ddr Exp $ *)

(* Borrowed from Scott E. Lee http://genealogy.org/~scottlee/;
   converted his C program into this OCaml program.
   SDN 1 is November 25, 4714 BC Gregorian calendar *)
(* Changed gregorian and julian to work always with negative years
   (Scott's version worked only for years > -4800 *)

open Def;

value mydiv x y = if x >= 0 then x / y else (x - y + 1) / y;
value mymod x y = if x >= 0 then x mod y else (x + 1) mod y + y - 1;

(* gregorian *)

value sdn_offset = 1721119;
value days_per_5_months = 153;
value days_per_4_years = 1461;
value days_per_400_years = 146097;

value sdn_of_gregorian d =
  let year = if d.year < 0 then d.year + 1 else d.year in
  let (month, year) =
    if d.month > 2 then (d.month - 3, year) else (d.month + 9, year - 1)
  in
  mydiv (mydiv year 100 * days_per_400_years) 4 +
  mydiv (mymod year 100 * days_per_4_years) 4 +
    (month * days_per_5_months + 2) / 5 + d.day + sdn_offset
;

value gregorian_of_sdn prec sdn =
  let temp = (sdn - sdn_offset) * 4 - 1 in
  let century = mydiv temp days_per_400_years in
  let temp = (mydiv (mymod temp days_per_400_years) 4) * 4 + 3 in
  let year = (century * 100) + (mydiv temp days_per_4_years) in
  let dayOfYear = mydiv (mymod temp days_per_4_years) 4 + 1 in
  let temp = dayOfYear * 5 - 3 in
  let month = mydiv temp days_per_5_months in
  let day = mydiv (mymod temp days_per_5_months) 5 + 1 in
  let (month, year) =
    if month < 10 then (month + 3, year) else (month - 9, year + 1)
  in
  let year = if year <= 0 then year - 1 else year in
  {day = day; month = month; year = year; prec = prec; delta = 0}
;

(* julian *)

value sdn_offset = 1721117;
value days_per_5_months = 153;
value days_per_4_years = 1461;

value sdn_of_julian d =
  let year = if d.year < 0 then d.year + 1 else d.year in
  let (month, year) =
    if d.month > 2 then (d.month - 3, year) else (d.month + 9, year - 1)
  in
  mydiv (year * days_per_4_years) 4 + (month * days_per_5_months + 2) / 5 +
    d.day + sdn_offset
;

value julian_of_sdn prec sdn =
  let temp = (sdn - sdn_offset) * 4 - 1 in
  let year = mydiv temp days_per_4_years in
  let dayOfYear = mydiv (mymod temp days_per_4_years) 4 + 1 in
  let temp = dayOfYear * 5 - 3 in
  let month = mydiv temp days_per_5_months in
  let day = mydiv (mymod temp days_per_5_months) 5 + 1 in
  let (month, year) =
    if month < 10 then (month + 3, year) else (month - 9, year + 1)
  in
  let year = if year <= 0 then year - 1 else year in
  {day = day; month = month; year = year; prec = prec; delta = 0}
;

(* french revolution *)
(* this code comes from Remy Pialat; thanks to him *)

value modulo pAngle pVal =
  let x = truncate (pAngle /. pVal) in
  let y = float x *. pVal in
  pAngle -. y
;

value degVersRad pAngle =
  let a = modulo pAngle 360.0 in
  a *. 3.141592653589793 /. 180.0
;

value sinDeg pAngle = sin (degVersRad pAngle);

value modulo360 pAngle =
  let angle = modulo pAngle 360.0 in
  if angle < 0.0 then angle +. 360.0 else angle
;

value equinoxeAutomne pAnnee =
  let f = 12.0 in
  let q = 30.0 in
  let i = 0 in
  let k = 6.0 in
  let jd = (float pAnnee +. k /. f) *. 365.2422 +. 1721141.3 in
  loop i jd (-1.0) where rec loop i jd jdn =
    if abs_float (jd -. jdn) > 1.0E-12 && i + 1 < 20 then
      let i = i + 1 in
      let jdn = jd in
      let t = (jd -. 2415020.0) /. 36525.0 in
      let t2 = t *. t in
      let t3 = t2 *. t in
      let l = 279.69688 +. 36000.76892 *. t +. 3.025E-4 *. t2 in
      let m = 358.47583 +. 35999.04975 *. t -. 1.5E-4 *. t2 -. 3.3E-6 *. t3 in
      let ll =
        l +. (1.91946 -. 0.004789 *. t -. 1.4E-5 *. t2) *. sinDeg m +.
        (0.020094 -. 1.0E-4 *. t) *. sinDeg (2.0 *. m) +.
        0.00293 *. sinDeg (3.0 *. m)
      in
      let ll = ll -. 0.00569 -. 0.00479 *. sinDeg (259.18 -. 1934.142 *. t) in
      let ll = modulo360 ll in
      let ll = if ll > 350.0 then ll -. 360.0 else ll in
      let jd = jd +. 1.014 *. (k *. q -. ll) in loop i jd jdn
    else
      let d = jd -. floor jd in
      let j = truncate jd in
      if d >= 0.5 then j + 1 else j
;

value french_of_sdn prec sdn =
  let greg_date = gregorian_of_sdn prec sdn in
  let fst_vend_sdn = equinoxeAutomne greg_date.year in
  let (year, fst_vend_sdn) =
    if sdn < fst_vend_sdn then
      let fst_vend_sdn = equinoxeAutomne (greg_date.year - 1) in
      let year = greg_date.year - 1792 in
      (year, fst_vend_sdn)
    else
      let year = greg_date.year - 1791 in
      (year, fst_vend_sdn)
  in
  let ndays = sdn - fst_vend_sdn in
  let month = ndays / 30 + 1 in
  let day = ndays mod 30 + 1 in
  {day = day; month = month; year = year; prec = prec; delta = 0}
;

value sdn_of_french d =
  let greg_year = d.year + 1791 in
  equinoxeAutomne greg_year + (d.month - 1) * 30 + d.day - 1
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

value fTishri1 metonicYear moladDay moladHalakim =
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
  let pTishri1 = fTishri1 pMetonicYear pMoladDay pMoladHalakim in
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
          fTishri1 ((metonicYear + 1) mod 19) moladDay moladHalakim
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

value findTishriMolad inputDay =
  let metonicCycle = (inputDay + 310) / 6940 in
  let (moladDay, moladHalakim) = moladOfMetonicCycle metonicCycle in
  let (moladDay, moladHalakim, metonicCycle) =
    loop moladDay moladHalakim metonicCycle
    where rec loop moladDay moladHalakim metonicCycle =
      if moladDay < inputDay - 6940 + 310 then
        let metonicCycle = metonicCycle + 1 in
        let moladHalakim = moladHalakim + halakim_per_metonic_cycle in
        let moladDay = moladDay + moladHalakim / halakim_per_day in
        let moladHalakim = moladHalakim mod halakim_per_day in
        loop moladDay moladHalakim metonicCycle
      else (moladDay, moladHalakim, metonicCycle)
  in
  let (metonicYear, moladDay, moladHalakim) =
    loop 0 moladDay moladHalakim
    where rec loop metonicYear moladDay moladHalakim =
      if metonicYear < 18 then
        if moladDay > inputDay - 74 then (metonicYear, moladDay, moladHalakim)
        else
          let moladHalakim =
            moladHalakim +
             halakim_per_lunar_cycle * monthsPerYear.(metonicYear)
          in
          let moladDay = moladDay + moladHalakim / halakim_per_day in
          let moladHalakim = moladHalakim mod halakim_per_day in
          loop (metonicYear + 1) moladDay moladHalakim
      else (metonicYear, moladDay, moladHalakim)
  in
  (metonicCycle, metonicYear, moladDay, moladHalakim)
;

value glop inputDay tishri1 tishri1After =
  let yearLength = tishri1After - tishri1 in
  let day = inputDay - tishri1 - 29 in
  if yearLength == 355 || yearLength == 385 then
    if day <= 30 then (2, day)
    else (3, day - 30)
  else
    if day <= 29 then (2, day)
    else (3, day - 29)
;

value hebrew_of_sdn prec sdn =
  let inputDay = sdn - sdn_offset in
  let (year, month, day) =
    if inputDay <= 0 then (0, 0, 0)
    else
      let (metonicCycle, metonicYear, day, halakim) =
        findTishriMolad inputDay
      in
      let init_day = day in
      let tishri1 = fTishri1 metonicYear day halakim in
      if inputDay >= tishri1 then
        let year = metonicCycle * 19 + metonicYear + 1 in
        if inputDay < tishri1 + 59 then
          if inputDay < tishri1 + 30 then (year, 1, inputDay - tishri1 + 1)
          else (year, 2, inputDay - tishri1 - 29)
        else
          let halakim =
            halakim + halakim_per_lunar_cycle * monthsPerYear.(metonicYear)
          in
          let day = day + halakim / halakim_per_day in
          let halakim = halakim mod halakim_per_day in
          let tishri1After = fTishri1 ((metonicYear + 1) mod 19) day halakim in
          let (month, day) = glop inputDay tishri1 tishri1After in
          (year, month, day)
      else
        let year = metonicCycle * 19 + metonicYear in
        if inputDay >= tishri1 - 177 then
          let (month, day) =
            if inputDay > tishri1 - 30 then (13, inputDay - tishri1 + 30)
            else if inputDay > tishri1 - 60 then (12, inputDay - tishri1 + 60)
            else if inputDay > tishri1 - 89 then (11, inputDay - tishri1 + 89)
            else if inputDay > tishri1 - 119 then
              (10, inputDay - tishri1 + 119)
            else if inputDay > tishri1 - 148 then (9, inputDay - tishri1 + 148)
            else (8, inputDay - tishri1 + 178)
          in
          (year, month, day)
        else
          if monthsPerYear.((year - 1) mod 19) == 13 then
            let month = 7 in
            let day = inputDay - tishri1 + 207 in
            if day > 0 then (year, month, day)
            else
              let month = month - 1 in
              let day = day + 30 in
              if day > 0 then (year, month, day)
              else
                let month = month - 1 in
                let day = day + 30 in
                if day > 0 then (year, month, day)
                else
                  let month = month - 1 in
                  let day = day + 29 in
                  if day > 0 then (year, month, day)
                  else
                    let tishri1After = tishri1 in
                    let (metonicCycle, metonicYear, day, halakim) =
                      findTishriMolad (init_day - 365)
                    in
                    let tishri1 = fTishri1 metonicYear day halakim in
                    let (month, day) = glop inputDay tishri1 tishri1After in
                    (year, month, day)
          else
            let month = 6 in
            let day = inputDay - tishri1 + 207 in
            if day > 0 then (year, month, day)
            else
              let month = month - 1 in
              let day = day + 30 in
              if day > 0 then (year, month, day)
              else
                let month = month - 1 in
                let day = day + 29 in
                if day > 0 then (year, month, day)
                else
                  let tishri1After = tishri1 in
                  let (metonicCycle, metonicYear, day, halakim) =
                    findTishriMolad (init_day - 365)
                  in
                  let tishri1 = fTishri1 metonicYear day halakim in
                  let (month, day) = glop inputDay tishri1 tishri1After in
                  (year, month, day)
  in
  {day = day; month = month; year = year; prec = prec; delta = 0}
;

(* from and to gregorian *)

value conv f f_max_month g g_max_month d =
  let sdn =
    if d.day = 0 then
      if d.month = 0 then g {(d) with day = 1; month = 1}
      else g {(d) with day = 1}
    else g d
  in
  let sdn_max =
    if d.day = 0 then
      if d.month = 0 || d.month = g_max_month then
        g {day = 1; month = 1; year = d.year + 1; prec = d.prec; delta = 0}
      else
        g {day = 1; month = d.month + 1; year = d.year; prec = d.prec;
           delta = 0}
    else sdn + 1
  in
  let d1 = f d.prec sdn in
  let d2 = f d.prec (sdn_max + d.delta) in
  if d1.day = 1 && d2.day = 1 then
    if d1.month = 1 && d2.month = 1 then
      if d1.year + 1 = d2.year then
        {day = 0; month = 0; year = d1.year; prec = d.prec; delta = 0}
      else {(d1) with delta = sdn_max + d.delta - sdn - 1}
    else if
      d1.month + 1 = d2.month
    || d1.month = f_max_month && d1.year + 1 = d2.year then
      {(d1) with day = 0}
    else {(d1) with delta = sdn_max + d.delta - sdn - 1}
  else {(d1) with delta = sdn_max + d.delta - sdn - 1}
;

value gregorian_of_julian = conv gregorian_of_sdn 12 sdn_of_julian 12;
value julian_of_gregorian = conv julian_of_sdn 12 sdn_of_gregorian 12;

value gregorian_of_french = conv gregorian_of_sdn 12 sdn_of_french 13;
value french_of_gregorian = conv french_of_sdn 13 sdn_of_gregorian 12;

value gregorian_of_hebrew = conv gregorian_of_sdn 12 sdn_of_hebrew 13;
value hebrew_of_gregorian = conv hebrew_of_sdn 13 sdn_of_gregorian 12;
