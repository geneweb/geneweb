(* camlp4r ./pa_html.cmo *)
(* $Id: date.ml,v 3.7 2000-01-15 18:55:52 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Util;
open Gutil;
open Config;

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

value default_french_month =
  let tab =
    [| "Vendemiaire"; "Brumaire"; "Frimaire"; "Nivose"; "Pluviose"; "Ventose";
       "Germinal"; "Floreal"; "Prairial"; "Messidor"; "Thermidor"; "Fructidor";
       "Extra" |]
  in
  fun m -> tab.(m)
;

value default_hebrew_month =
  let tab =
    [| "Tishri"; "Heshvan"; "Kislev"; "Tevet"; "Shevat"; "AdarI";
       "AdarII"; "Nisan"; "Iyyar"; "Sivan"; "Tammuz"; "Av"; "Elul" |]
  in
  fun m -> tab.(m)
;

value french_month conf m =
  let r = transl_nth conf "(french month)" m in
  if r = "[(french month)]" then "[" ^ default_french_month m ^ "]" else r
;

value hebrew_month conf m =
  let r = transl_nth conf "(hebrew month)" m in
  if r = "[(hebrew month)]" then "[" ^ default_hebrew_month m ^ "]" else r
;

value code_french_date conf d m y =
  let s =
    if d = 0 then ""
    else string_of_int d ^ (if d = 1 then "<sup>er</sup>" else "")
  in
  let s =
    if m = 0 then ""
    else s ^ (if s = "" then "" else " ") ^ french_month conf (m - 1)
  in
  s ^ (if s = "" then "" else " ") ^
  " " ^ transl_nth conf "year/month/day" 0 ^ " " ^ string_of_int y
;

value code_hebrew_date conf d m y =
  let s = if d = 0 then "" else string_of_int d in
  let s =
    if m = 0 then ""
    else s ^ (if s = "" then "" else " ") ^ hebrew_month conf (m - 1)
  in
  s ^ (if s = "" then "" else " ") ^ " " ^ string_of_int y
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

value string_of_on_hebrew_dmy conf d =
  let sy = code_hebrew_date conf d.day d.month d.year in
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
  | Dgreg d Dhebrew ->
      let d1 = Calendar.hebrew_of_gregorian d in
      let s = gregorian_precision conf d in
      string_of_on_hebrew_dmy conf d1 ^ " "
      ^ " (" ^ s ^ ")"
  | Dtext t -> "(" ^ t ^ ")" ]
;

(*
value string_of_ondate conf d =
  match d with
  [ Dgreg {day = day; month = month; year = year} _
    when day <> 0 && month <> 0 && not conf.cancel_links ->
      "<a href=\"" ^ commd conf ^ "m=CAL;yg=" ^ string_of_int year ^ ";mg=" ^
      string_of_int month ^ ";dg=" ^ string_of_int day ^ ";tg=ok\">" ^
      string_of_ondate conf d ^ "</a>"
  | _ -> string_of_ondate conf d ]
;
*)

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

value print_dates conf base p =
  let cap s = ", " ^ s in
  let is = index_of_sex p.sex in
  do let birth_place = sou base p.birth_place in
     do match Adef.od_of_codate p.birth with
        [ Some d ->
            do Wserver.wprint "%s " (cap (transl_nth conf "born" is));
               Wserver.wprint "%s" (string_of_ondate conf d);
               if birth_place <> "" then Wserver.wprint ",\n" else ();
            return ()
        | None ->
            if birth_place <> "" then
              Wserver.wprint "%s\n-&nbsp;" (cap (transl_nth conf "born" is))
            else () ];
        if birth_place <> "" then Wserver.wprint "%s" birth_place else ();
     return ();
     let baptism = Adef.od_of_codate p.baptism in
     let baptism_place = sou base p.baptism_place in
     do match baptism with
        [ Some d ->
            do Wserver.wprint "%s "
                 (cap (transl_nth conf "baptized" is));
               Wserver.wprint "%s" (string_of_ondate conf d);
               if baptism_place <> "" then Wserver.wprint ",\n" else ();
            return ()
        | None ->
            if baptism_place <> "" then
              Wserver.wprint "%s\n-&nbsp;"
                (cap (transl_nth conf "baptized" is))
            else () ];
        if baptism_place <> "" then
          Wserver.wprint "%s" baptism_place
        else ();
     return ();
     let death_place = sou base p.death_place in
     do match p.death with
        [ Death dr d ->
            let dr_w =
              match dr with
              [ Unspecified -> transl_nth conf "died" is
              | Murdered -> transl_nth conf "murdered" is
              | Killed -> transl_nth conf "killed (in action)" is
              | Executed -> transl_nth conf "executed (legally killed)" is
              | Disappeared -> transl_nth conf "disappeared" is ]
            in
            let d = Adef.date_of_cdate d in
            do Wserver.wprint "%s " (cap dr_w);
               Wserver.wprint "%s" (string_of_ondate conf d);
               if death_place <> "" then Wserver.wprint ",\n" else ();
            return ()
        | DeadYoung ->
            do Wserver.wprint "%s" (cap (transl_nth conf "dead young" is));
               if death_place <> "" then Wserver.wprint "\n-&nbsp;" else ();
            return ()
        | DeadDontKnowWhen ->
            match (death_place, p.burial) with
            [ ("", Buried _ | Cremated _) -> ()
            | _ ->
                if death_place <> "" || not (of_course_died conf p) then
                  do Wserver.wprint "%s" (cap (transl_nth conf "died" is));
                     if death_place <> "" then Wserver.wprint "\n-&nbsp;"
                     else ();
                  return ()
                else () ]
        | DontKnowIfDead | NotDead -> () ];
        if death_place <> "" then
          do Wserver.wprint "%s" death_place;
          return ()
        else ();
     return ();
     let sure d = d.prec = Sure in
     match (Adef.od_of_codate p.birth, date_of_death p.death) with
     [ (Some (Dgreg d1 _), Some (Dgreg d2 _)) ->
         if sure d1 && sure d2 && d1 <> d2 then
           let a = temps_ecoule d1 d2 in
           do Wserver.wprint "\n(";
              Wserver.wprint "%s " (transl conf "death age:");
              print_age conf a;
              Wserver.wprint ")";
           return ()
         else ()
     | _ -> () ];
     let burial_date_place cod =
       let place = sou base p.burial_place in
       do match Adef.od_of_codate cod with
          [ Some d ->
              do Wserver.wprint " %s" (string_of_ondate conf d);
                 if place <> "" then Wserver.wprint ",\n" else ();
              return ()
          | None ->
              if place <> "" then Wserver.wprint " -&nbsp;" else () ];
          if place <> "" then Wserver.wprint "%s" place else ();
       return ()
     in
     do match p.burial with
        [ Buried cod ->
            do Wserver.wprint "%s" (cap (transl_nth conf "buried" is));
               burial_date_place cod;
            return ()
        | Cremated cod ->
            do Wserver.wprint "%s"
                 (cap (transl_nth conf "cremated" is));
               burial_date_place cod;
            return ()
        | UnknownBurial -> () ];
     return ();
  return ()
;

(* Calendar request *)

value gregorian_month_name conf n = capitale (transl_nth conf "(month)" n);
value julian_month_name = gregorian_month_name;
value french_month_name conf n = capitale (french_month conf n);
value hebrew_month_name conf n = capitale (hebrew_month conf n);

value print_some_calendar conf date n month_name n_months var =
  do Wserver.wprint "\n";
     tag "tr" begin
       stag "th" begin
         Wserver.wprint "%s\n"
           (capitale (transl_nth conf "gregorian/julian/french/hebrew" n));
       end;
       Wserver.wprint "\n";
       tag "td" begin
         Wserver.wprint "<input type=submit name=y%s1 value=\"&lt;\">" var;
         Wserver.wprint "<input name=y%s size=5 maxlength=5 value=%d>"
           var date.year;
         Wserver.wprint "<input type=submit name=y%s2 value=\"&gt;\">\n" var;
       end;
       tag "td" "align=center" begin
         Wserver.wprint "<input type=submit name=m%s1 value=\"&lt;\">" var;
         stag "select" "name=m%s" var begin
           for i = 1 to n_months do
             Wserver.wprint "<option value=%d%s> %s\n" i
               (if date.month = i then " selected" else "")
               (month_name conf (i - 1));
           done;
         end;
         Wserver.wprint "<input type=submit name=m%s2 value=\"&gt;\">" var;
       end;
       tag "td" begin
         Wserver.wprint "<input type=submit name=d%s1 value=\"&lt;\">" var;
         Wserver.wprint "<input name=d%s size=2 maxlength=2 value=%d>"
           var date.day;
         Wserver.wprint "<input type=submit name=d%s2 value=\"&gt;\">\n" var;
       end;
       tag "td" begin
         Wserver.wprint "<input type=submit name=t%s value=\"=\">\n" var;
       end;
     end;
  return ()
;

value print_calendar_head conf =
  tag "tr" begin
    stag "td" begin Wserver.wprint "&nbsp;"; end; Wserver.wprint "\n";
    for i = 0 to 2 do
      tag "th" begin
        Wserver.wprint "%s" (capitale (transl_nth conf "year/month/day" i));
      end;
    done;
    stag "td" begin Wserver.wprint "&nbsp;"; end; Wserver.wprint "\n";
  end
;

value print_calendar conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl_nth conf "calendar/calendars" 1))
  in
  let getint v =
    match p_getint conf.env v with
    [ Some x -> x
    | _ -> 0 ]
  in
  let sdn =
    List.fold_left
      (fun d (var, conv, max_month) ->
         let yy = getint ("y" ^ var) in
         let mm = getint ("m" ^ var) in
         let dd = getint ("d" ^ var) in
         match p_getenv conf.env ("t" ^ var) with
         [ Some _ ->
             conv {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
         | None ->
             match
               (p_getenv conf.env ("y" ^ var ^ "1"),
                p_getenv conf.env ("y" ^ var ^ "2"),
                p_getenv conf.env ("m" ^ var ^ "1"),
                p_getenv conf.env ("m" ^ var ^ "2"),
                p_getenv conf.env ("d" ^ var ^ "1"),
                p_getenv conf.env ("d" ^ var ^ "2"))
             with
             [ (Some _, _, _, _, _, _) ->
                 let yy = yy - 1 in
                 conv {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
             | (_, Some _, _, _, _, _) ->
                 let yy = yy + 1 in
                 conv {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
             | (_, _, Some _, _, _, _) ->
                 let (yy, mm) =
                   if mm = 1 then (yy - 1, max_month)
                   else (yy, mm - 1)
                 in
                 conv {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
             | (_, _, _, Some _, _, _) ->
                 let (yy, mm) =
                   if mm = max_month then (yy + 1, 1)
                   else (yy, mm + 1)
                 in
                 conv {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
             | (_, _, _, _, Some _, _) ->
                 let dd = dd - 1 in
                 conv {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
             | (_, _, _, _, _, Some _) ->
                 let dd = dd + 1 in
                 conv {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
             | _ -> d ] ])
      (Calendar.sdn_of_gregorian conf.today)
      [("g", Calendar.sdn_of_gregorian, 12);
       ("j", Calendar.sdn_of_julian, 12);
       ("f", Calendar.sdn_of_french, 13);
       ("h", Calendar.sdn_of_hebrew, 13)]
  in
  let date = Calendar.gregorian_of_sdn Sure sdn in
  let wday =
    let sdn_today = Calendar.sdn_of_gregorian conf.today in
    let x = conf.today_wd - sdn_today + sdn in
    if x < 0 then 6 + (x + 1) mod 7 else x mod 7
  in
  do header conf title;
     print_link_to_welcome conf True;
     Wserver.wprint "- %s -\n"
       (capitale (nominative (transl_nth conf "(week day)" wday)));
     html_p conf;
     tag "form" "method=GET action=\"%s\"" conf.command begin
       List.iter
         (fun (k, v) ->
            Wserver.wprint "<input type=hidden name=%s value=%s>\n" k v)
         conf.henv;
       Wserver.wprint "<input type=hidden name=m value=CAL>\n\n";
       tag "table" "border=1" begin
         print_calendar_head conf;
         print_some_calendar conf date 0 gregorian_month_name 12 "g";
         print_some_calendar conf (Calendar.julian_of_gregorian date) 1
           julian_month_name 12 "j";
         print_some_calendar conf (Calendar.french_of_gregorian date) 2
           french_month_name 13 "f";
         print_some_calendar conf (Calendar.hebrew_of_gregorian date) 3
           hebrew_month_name 13 "h";
       end;
       Wserver.wprint "<br><p>%s: " (capitale (transl conf "julian day"));
       Num.print (fun x -> Wserver.wprint "%s" x)
         (transl conf "(thousand separator)")
         (Num.of_int (Calendar.sdn_of_gregorian date));
       Wserver.wprint "\n";
     end;
     trailer conf;
  return ()
;

(* Deprecated *)

value afficher_dates_courtes conf base p =
  Wserver.wprint "%s" (short_dates_text conf base p)
;
