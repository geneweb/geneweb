(* $Id: date.ml,v 2.12 1999-09-16 12:17:24 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Util;
open Gutil;

value nbsp = "&nbsp;";

value code_date conf encoding d m y =
  loop 0 where rec loop i =
    if i = String.length encoding then ""
    else
      let (s, i) =
        match encoding.[i] with
        [ '%' when i + 1 < String.length encoding ->
            let s =
              match encoding.[i+1] with
              [ 'd' -> string_of_int d
              | 'm' -> transl_nth conf "(month)" (m - 1)
              | 'y' -> string_of_int y
              | c -> "%" ^ String.make 1 c ]
            in
            (s, i + 1)
        | ' ' -> (nbsp, i)
        | c -> (String.make 1 c, i) ]
      in
      s ^ loop (i + 1)
;

value french_month =
  let tab =
    [| "vendemiaire"; "brumaire"; "frimaire"; "niv&ocirc;se";
       "pluvi&ocirc;se"; "vent&ocirc;se"; "germinal"; "floreal";
       "prairial"; "messidor"; "thermidor"; "fructidor";
       "jours compl&eacute;mentaires" |]
  in
  fun m -> tab.(m)
;

value code_french_date conf d m y =
  let s =
    if d = 0 then ""
    else string_of_int d ^ (if d = 1 then "<sup>er</sup>" else "")
  in
  let s =
    if m = 0 then ""
    else s ^ (if s = "" then "" else " ") ^ french_month (m - 1)
  in
  s ^ (if s = "" then "" else " ") ^
  " an " ^ string_of_int y
;

value string_of_on_prec_dmy conf sy d =
  match d.prec with
  [ Sure ->
      if d.day = 0 && d.month = 0 then
        transl conf "in (year)" ^ " " ^ sy
      else if d.day = 0 then transl conf "in (month year)" ^ " " ^ sy
      else transl conf "on (day month year)" ^ " " ^ sy
  | About | Before | After ->
      let s = sy in
      if d.prec = About then transl conf "about (date)" ^ " " ^ s
      else if d.prec = Before then transl conf "before (date)" ^ " " ^ s
      else transl conf "after (date)" ^ " " ^ s
  | Maybe ->
      let s =
        if d.day = 0 && d.month = 0 then
          transl conf "in (year)" ^ " " ^ sy
        else if d.day = 0 then transl conf "in (month year)" ^ " " ^ sy
        else transl conf "on (day month year)" ^ " " ^ sy
      in
      transl conf "maybe (date)" ^ " " ^ s
  | OrYear z ->
      let s =
        if d.day = 0 && d.month = 0 then
          transl conf "in (year)" ^ " " ^ sy
        else if d.day = 0 then transl conf "in (month year)" ^ " " ^ sy
        else transl conf "on (day month year)" ^ " " ^ sy
      in
      s ^ " " ^
      transl conf "or" ^ " " ^
      code_date conf (transl_nth conf "(date)" 3) 0 0 z
  | YearInt z ->
      let s =
        if d.day = 0 && d.month = 0 then sy
        else if d.day = 0 then sy
        else transl conf "on (day month year)" ^ " " ^ sy
      in
      transl conf "between (date)" ^ " " ^ s ^ " " ^
      transl conf "and" ^ " " ^
      code_date conf (transl_nth conf "(date)" 3) 0 0 z ]
;

value string_of_on_dmy conf d =
  let encoding =
    let n =
      if d.day = 1 then 0
      else if d.day != 0 then 1
      else if d.month != 0 then 2
      else 3
    in
    transl_nth conf "(date)" n
  in
  let sy = code_date conf encoding d.day d.month d.year in
  string_of_on_prec_dmy conf sy d
;

value string_of_on_french_dmy conf d =
  let sy = code_french_date conf d.day d.month d.year in
  string_of_on_prec_dmy conf sy d
;

value string_of_dmy conf d =
  let encoding =
    let n =
      if d.day = 1 then 0
      else if d.day != 0 then 1
      else if d.month != 0 then 2
      else 3
    in
    transl_nth conf "(date)" n
  in
  let s = code_date conf encoding d.day d.month d.year in
  match d.prec with
  [ Sure -> s
  | About -> transl conf "about (date)" ^ " " ^ s
  | Before -> transl conf "before (date)" ^ " " ^ s
  | After -> transl conf "after (date)" ^ " " ^ s
  | Maybe -> transl conf "maybe (date)" ^ " " ^ s
  | OrYear z ->
      s ^ " " ^
      transl conf "or" ^ " " ^
      code_date conf (transl_nth conf "(date)" 3) 0 0 z
  | YearInt z ->
      transl conf "between (date)" ^ " " ^ s ^ " " ^
      transl conf "and" ^ " " ^
      code_date conf (transl_nth conf "(date)" 3) 0 0 z ]
;

value gregorian_precision conf d =
  if d.delta = 0 then string_of_dmy conf d
  else
    let d2 =
      Calendar.gregorian_of_sdn d.prec
        (Calendar.sdn_of_gregorian d + d.delta)
    in
    transl conf "between (date)" ^ " " ^
    string_of_on_dmy conf d ^ " " ^
    transl conf "and" ^ " " ^
    string_of_on_dmy conf d2
;

value string_of_ondate conf =
  fun
  [ Dgreg d Dgregorian -> string_of_on_dmy conf d
  | Dgreg ({day = 0} as d) Djulian -> string_of_on_dmy conf d
  | Dgreg d Djulian ->
      let cal_prec =
        if d.year < 1582 then ""
        else " (" ^ gregorian_precision conf d ^ ")"
      in
      let d1 = Calendar.julian_of_gregorian d in
      string_of_on_dmy conf d1 ^ " " ^
      transl_nth conf "gregorian/julian/french/hebrew" 1 ^ cal_prec
  | Dgreg d Dfrench ->
      let d1 = Calendar.french_of_gregorian d in
      let s = gregorian_precision conf d in
      string_of_on_french_dmy conf d1 ^ " "
      ^ " (" ^ s ^ ")"
  | Dgreg d Dhebrew -> string_of_on_dmy conf d
  | Dtext t -> "(" ^ t ^ ")" ]
;

value string_of_date conf =
  fun
  [ Dgreg d _ -> string_of_dmy conf d
  | Dtext t -> "(" ^ t ^ ")" ]
;

value print_age conf a =
  match a with
  [ {day = 0; month = 0; year = y} ->
      if y > 1 then
        Wserver.wprint "%d %s" y (transl conf "years old")
      else if y = 1 then
        Wserver.wprint "%s" (transl conf "one year old")
      else
        Wserver.wprint "%s" (transl conf "birth")
  | {day = 0; month = m; year = y} ->
      if y >= 2 then
        Wserver.wprint "%d %s" y (transl conf "years old")
      else if y > 0 || m > 1 then
        Wserver.wprint "%d %s" (y * 12 + m) (transl conf "months old")
      else if m = 1 then
        Wserver.wprint "%s" (transl conf "one month old")
      else
        Wserver.wprint "%s" (transl conf "less than one month old")
  | {day = d; month = m; year = y} ->
      if y >= 2 then
        Wserver.wprint "%d %s" y (transl conf "years old")
      else if y > 0 || m > 1 then
        Wserver.wprint "%d %s" (y * 12 + m) (transl conf "months old")
      else if m = 1 then
        Wserver.wprint "%s" (transl conf "one month old")
      else if d >= 2 then
        Wserver.wprint "%d %s" d (transl conf "days old")
      else if d == 1 then
        Wserver.wprint "%s" (transl conf "one day old")
      else Wserver.wprint "0" ]
;

value year_text d =
  let s =
    match d.prec with
    [ Before -> "/"
    | About | Maybe | OrYear _ | YearInt _ -> "ca "
    | _ -> "" ]
  in
  let s = s ^ string_of_int (annee d) in
  match d.prec with
  [ After -> s ^ "/"
  | _ -> s ]
;

value display_year d =
  Wserver.wprint "%s" (year_text d)
;

value of_course_died conf p =
  match Adef.od_of_codate p.birth with
  [ Some (Dgreg d _) -> conf.Config.today.year - d.year > 120
  | _ -> False ]
;

value short_dates_text conf base p =
  if age_autorise conf base p then
    let birth_date =
      match Adef.od_of_codate p.birth with
      [ None -> Adef.od_of_codate p.baptism
      | x -> x ]
    in
    let s =
      match (birth_date, p.death) with
      [ (Some _, DontKnowIfDead) -> "*"
      | _ -> "" ]
    in
    let s =
      match birth_date with
      [ Some (Dgreg d _) -> s ^ year_text d
      | _ -> s ]
    in
    let s =
      match (birth_date, p.death) with
      [ (Some _, Death _ _ | NotDead) -> s ^ "-"
      | (_, Death _ _) -> if s = "" then "+" else s ^ nbsp ^ "+"
      | (_, DeadDontKnowWhen | DeadYoung) ->
          if of_course_died conf p then s
          else if s = "" then "+" else s ^ nbsp ^ "+"
      | _ -> s ]
    in
    let s =
      match date_of_death p.death with
      [ Some (Dgreg d _) -> s ^ year_text d
      | _ -> s ]
    in
    if s <> "" then " <em>" ^ s ^ "</em>" else s
  else ""
;

value short_marriage_date_text conf base fam p1 p2 =
  if age_autorise conf base p1 && age_autorise conf base p2 then
    match Adef.od_of_codate fam.marriage with
    [ Some (Dgreg d _) -> "<font size=-2>" ^ year_text d ^ "</font>"
    | _ -> "" ]
  else ""
;

(* Deprecated *)

value afficher_dates_courtes conf base p =
  Wserver.wprint "%s" (short_dates_text conf base p)
;
