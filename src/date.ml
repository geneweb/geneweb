(* $Id: date.ml,v 1.1 1998-09-01 14:32:06 ddr Exp $ *)

open Def;
open Util;
open Gutil;

value nbsp = "&nbsp;";

value string_of_ondate conf d =
  match d with
  [ Djma d m y ->
      transl conf "on (day month year)" ^ nbsp ^
      transl_nth conf "(day)" (d - 1) ^ nbsp ^
      transl_nth conf "(month)" (m - 1) ^ nbsp ^ string_of_int y
  | Dma m y ->
      transl conf "in (month year)" ^ nbsp ^
      transl_nth conf "(month)" (m - 1) ^ nbsp ^ string_of_int y
  | Da Sure y -> transl conf "in (year)" ^ nbsp ^ string_of_int y
  | Da About y -> transl conf "about (year)" ^ nbsp ^ string_of_int y
  | Da Maybe y -> transl conf "maybe in (year)" ^ nbsp ^ string_of_int y
  | Da Before y -> transl conf "before (year)" ^ nbsp ^ string_of_int y
  | Da After y -> transl conf "after (year)" ^ nbsp ^ string_of_int y
  | Da (OrYear z) y ->
      transl conf "in (year)" ^ nbsp ^ string_of_int y ^ nbsp ^
      transl conf "or" ^ nbsp ^ string_of_int z ]
;

value string_of_date conf d =
  match d with
  [ Djma d m y ->
      transl_nth conf "(day)" (d - 1) ^ " " ^
      transl_nth conf "(month)" (m - 1) ^ " " ^ string_of_int y
  | Dma m y ->
      transl_nth conf "(month)" (m - 1) ^ " " ^ string_of_int y
  | Da Sure y -> string_of_int y
  | Da _ _ -> "..." ]
;

value print_age conf a =
  match a with
  [ Djma d m y ->
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
      else Wserver.wprint "0"
  | Dma m y ->
      if y >= 2 then
        Wserver.wprint "%d %s" y (transl conf "years old")
      else if y > 0 || m > 1 then
        Wserver.wprint "%d %s" (y * 12 + m) (transl conf "months old")
      else if m = 1 then
        Wserver.wprint "%s" (transl conf "one month old")
      else
        Wserver.wprint "%s" (transl conf "less than one month old")
  | Da _ y ->
      if y > 1 then
        Wserver.wprint "%d %s" y (transl conf "years old")
      else if y = 1 then
        Wserver.wprint "%s" (transl conf "one year old")
      else
        Wserver.wprint "%s" (transl conf "less than one year old") ]
;

value afficher_dates conf base p =
  let is = index_of_sex p.sexe in
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

value display_year d =
  do match d with
     [ Da Before _ -> Wserver.wprint "/"
     | Da (About | Maybe | OrYear _) _ -> Wserver.wprint "ca&nbsp;"
     | _ -> () ];
     Wserver.wprint "%d" (annee d);
     match d with
     [ Da After _ -> Wserver.wprint "/"
     | _ -> () ];
  return ()
;

value afficher_dates_courtes conf base p =
  if age_autorise conf base p then
    let something =
      match (Adef.od_of_codate p.birth, p.death) with
      [ (Some _, _) | (_, Death _ _) -> True
      | _ -> False ]
    in
    if something then
      do Wserver.wprint " <em>";
         match (Adef.od_of_codate p.birth, p.death) with
         [ (Some _, DeadDontKnowWhen | DontKnowIfDead) ->
             Wserver.wprint "*"
         | _ -> () ];
         match Adef.od_of_codate p.birth with
         [ Some d -> display_year d
         | _ -> () ];
         match (Adef.od_of_codate p.birth, p.death) with
         [ (Some _, Death _ _ | NotDead) -> Wserver.wprint "-"
         | (_, Death _ _) -> Wserver.wprint "+"
         | _ -> () ];
         match p.death with
         [ Death _ d ->
             let d = Adef.date_of_cdate d in
             display_year d
         | _ -> () ];
         Wserver.wprint "</em>";
      return ()
    else ()
  else ()
;
