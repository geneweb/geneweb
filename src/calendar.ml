(* $Id: calendar.ml,v 2.1 1999-09-14 22:35:28 ddr Exp $ *)
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
