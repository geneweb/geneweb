(* $Id: date.ml,v 2.5 1999-04-25 16:32:18 ddr Exp $ *)
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

value string_of_ondate conf d =
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
  match d.prec with
  [ Sure ->
      if d.day = 0 && d.month = 0 then
        transl conf "in (year)" ^ nbsp ^ sy
      else if d.day = 0 then transl conf "in (month year)" ^ nbsp ^ sy
      else transl conf "on (day month year)" ^ nbsp ^ sy
  | About | Before | After ->
      let s = sy in
      if d.prec = About then transl conf "about (date)" ^ nbsp ^ s
      else if d.prec = Before then transl conf "before (date)" ^ nbsp ^ s
      else transl conf "after (date)" ^ nbsp ^ s
  | Maybe ->
      let s =
        if d.day = 0 && d.month = 0 then
          transl conf "in (year)" ^ nbsp ^ sy
        else if d.day = 0 then transl conf "in (month year)" ^ nbsp ^ sy
        else transl conf "on (day month year)" ^ nbsp ^ sy
      in
      transl conf "maybe (date)" ^ nbsp ^ s
  | OrYear z ->
      let s =
        if d.day = 0 && d.month = 0 then
          transl conf "in (year)" ^ nbsp ^ sy
        else if d.day = 0 then transl conf "in (month year)" ^ nbsp ^ sy
        else transl conf "on (day month year)" ^ nbsp ^ sy
      in
      s ^ " " ^
      transl conf "or" ^ " " ^
      code_date conf (transl_nth conf "(date)" 3) 0 0 z
  | YearInt z ->
      let s =
        if d.day = 0 && d.month = 0 then sy
        else if d.day = 0 then sy
        else transl conf "on (day month year)" ^ nbsp ^ sy
      in
      transl conf "between (date)" ^ nbsp ^ s ^ " " ^
      transl conf "and" ^ " " ^
      code_date conf (transl_nth conf "(date)" 3) 0 0 z ]
;

value string_of_date conf d =
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
  | About -> transl conf "about (date)" ^ nbsp ^ s
  | Before -> transl conf "before (date)" ^ nbsp ^ s
  | After -> transl conf "after (date)" ^ nbsp ^ s
  | Maybe -> transl conf "maybe (date)" ^ nbsp ^ s
  | OrYear z ->
      s ^ " " ^
      transl conf "or" ^ " " ^
      code_date conf (transl_nth conf "(date)" 3) 0 0 z
  | YearInt z ->
      transl conf "between (date)" ^ nbsp ^ s ^ " " ^
      transl conf "and" ^ " " ^
      code_date conf (transl_nth conf "(date)" 3) 0 0 z ]
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

value afficher_dates conf base p =
  let is = index_of_sex p.sex in
  if age_autorise conf base p then
    let something =
      match (Adef.od_of_codate p.birth, p.death) with
      [ (Some _, _)
      | (_, Death _ _ | DeadYoung | DeadDontKnowWhen) -> True
      | _ -> False ]
    in
    do if something then Wserver.wprint "<em>" else ();
       match Adef.od_of_codate p.birth with
       [ Some d ->
           do Wserver.wprint ",\n%s\n" (transl_nth conf "born" is);
              Wserver.wprint "%s" (string_of_ondate conf d);
           return ()
       | None -> () ];
       match p.death with
       [ Death dr d ->
           let d = Adef.date_of_cdate d in
           let dr_w =
             match dr with
             [ Unspecified -> transl_nth conf "died" is
             | Murdered -> transl_nth conf "murdered" is
             | Killed -> transl_nth conf "killed (in action)" is
             | Executed -> transl_nth conf "executed (legally killed)" is
             | Disappeared -> transl_nth conf "disappeared" is ]
           in
           do Wserver.wprint ",\n%s\n" dr_w;
              Wserver.wprint "%s" (string_of_ondate conf d);
           return ()
       | DeadYoung ->
           Wserver.wprint ",\n%s" (transl_nth conf "dead young" is)
       | DeadDontKnowWhen | DontKnowIfDead | NotDead -> () ];
       if something then Wserver.wprint "</em>" else ();
    return ()
  else ()
;

value year_text d =
  let s =
    match d.prec with
    [ Before -> "/"
    | About | Maybe | OrYear _ | YearInt _ -> "ca&nbsp;"
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
  [ Some d -> conf.Config.today.year - d.year > 120
  | None -> False ]
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
      [ Some d -> s ^ year_text d
      | _ -> s ]
    in
    let s =
      match (birth_date, p.death) with
      [ (Some _, Death _ _ | NotDead) -> s ^ "-"
      | (_, Death _ _) -> s
      | (_, DeadDontKnowWhen | DeadYoung) ->
          if of_course_died conf p then s else s ^ "+"
      | _ -> s ]
    in
    let s =
      match p.death with
      [ Death _ d -> s ^ year_text (Adef.date_of_cdate d)
      | _ -> s ]
    in
    if s <> "" then " <em>" ^ s ^ "</em>" else s
  else ""
;

value afficher_dates_courtes conf base p =
  Wserver.wprint "%s" (short_dates_text conf base p)
;
