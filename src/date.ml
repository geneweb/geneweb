(* $Id: date.ml,v 1.2 1998-09-24 16:51:57 ddr Exp $ *)

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
  match d with
  [ Djma 1 m y ->
      let encoding = transl_nth conf "(date)" 0 in
      transl conf "on (day month year)" ^ nbsp ^
      code_date conf encoding 0 m y
  | Djma d m y ->
      let encoding = transl_nth conf "(date)" 1 in
      transl conf "on (day month year)" ^ nbsp ^
      code_date conf encoding d m y
  | Dma m y ->
      let encoding = transl_nth conf "(date)" 2 in
      transl conf "in (month year)" ^ nbsp ^
      code_date conf encoding 0 m y
  | Da p y ->
      let encoding = transl_nth conf "(date)" 3 in
      let sy = code_date conf encoding 0 0 y in
      match p with
      [ Sure -> transl conf "in (year)" ^ nbsp ^ sy
      | About -> transl conf "about (year)" ^ nbsp ^ sy
      | Maybe -> transl conf "maybe in (year)" ^ nbsp ^ sy
      | Before -> transl conf "before (year)" ^ nbsp ^ sy
      | After -> transl conf "after (year)" ^ nbsp ^ sy
      | OrYear z ->
          transl conf "in (year)" ^ nbsp ^ sy ^ " " ^
          transl conf "or" ^ " " ^ code_date conf encoding 0 0 z ] ]
;

value string_of_date conf d =
  match d with
  [ Djma 1 m y ->
      let encoding = transl_nth conf "(date)" 0 in
      code_date conf encoding 0 m y
  | Djma d m y ->
      let encoding = transl_nth conf "(date)" 1 in
      code_date conf encoding d m y
  | Dma m y ->
      let encoding = transl_nth conf "(date)" 2 in
      code_date conf encoding 0 m y
  | Da Sure y ->
      let encoding = transl_nth conf "(date)" 3 in
      code_date conf encoding 0 0 y
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
