(* camlp4r ./pa_html.cmo *)
(* $Id: date.ml,v 4.23 2004-12-26 18:11:20 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Util;
open Gutil;
open Config;

value nbsp = "&nbsp;";

value code_date conf encoding d m y =
  let apply_date_code =
    fun
    [ 'd' -> string_of_int d
    | 'm' -> transl_nth conf "(month)" (m - 1)
    | 'y' -> string_of_int y
    | c -> "%" ^ String.make 1 c ]
  in
  let rec loop i =
    if i = String.length encoding then ""
    else
      let (s, i) =
        match encoding.[i] with
        [ '%' when i + 1 < String.length encoding ->
            let s = apply_date_code encoding.[i + 1] in (s, i + 1)
        | '['
          when
            i + 5 < String.length encoding && encoding.[i + 3] = ']' &&
            encoding.[i + 4] = '%' ->
            let s = apply_date_code encoding.[i + 5] in
            let s1 =
              if start_with_vowel s then String.make 1 encoding.[i + 2]
              else String.make 1 encoding.[i + 1] ^ " "
            in
            (s1 ^ s, i + 5)
        | c -> (String.make 1 c, i) ]
      in
      s ^ loop (i + 1)
  in
  loop 0
;

value code_dmy conf d =
  let encoding =
    let n =
      if d.day = 1 then 0
      else if d.day != 0 then 1
      else if d.month != 0 then 2
      else 3
    in
    transl_nth conf "(date)" n
  in
  code_date conf encoding d.day d.month d.year
;

value code_year conf y =
  code_date conf (transl_nth conf "(date)" 3) 0 0 y
;

value default_french_month =
  let tab =
    [| "Vendemiaire"; "Brumaire"; "Frimaire"; "Nivose"; "Pluviose"; "Ventose";
       "Germinal"; "Floreal"; "Prairial"; "Messidor"; "Thermidor";
       "Fructidor"; "Extra" |]
  in
  fun m -> tab.(m)
;

value default_hebrew_month =
  let tab =
    [| "Tishri"; "Heshvan"; "Kislev"; "Tevet"; "Shevat"; "AdarI"; "AdarII";
       "Nisan"; "Iyyar"; "Sivan"; "Tammuz"; "Av"; "Elul" |]
  in
  fun m -> tab.(m)
;

value french_month conf m =
  let r = transl_nth conf "(french revolution month)" m in
  if r = "[(french revolution month)]" then "[" ^ default_french_month m ^ "]"
  else r
;

value hebrew_month conf m =
  let r = transl_nth conf "(hebrew month)" m in
  if r = "[(hebrew month)]" then "[" ^ default_hebrew_month m ^ "]" else r
;

value code_french_year conf y =
  transl_nth conf "year/month/day" 3 ^ " " ^
    (if y >= 1 && y < 4000 then roman_of_arabian y else string_of_int y)
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
  s ^ (if s = "" then "" else " ") ^ code_french_year conf y
;

value code_hebrew_date conf d m y =
  let s = if d = 0 then "" else string_of_int d in
  let s =
    if m = 0 then ""
    else s ^ (if s = "" then "" else " ") ^ hebrew_month conf (m - 1)
  in
  s ^ (if s = "" then "" else " ") ^ string_of_int y
;

value string_of_on_prec_dmy_aux conf code_year sy d =
  match d.prec with
  [ Sure ->
      if d.day = 0 && d.month = 0 then transl conf "in (year)" ^ " " ^ sy
      else if d.day = 0 then transl_decline conf "in (month year)" sy
      else transl_decline conf "on (day month year)" sy
  | About | Before | After ->
      let s = sy in
      if d.prec = About then transl_decline conf "about (date)" s
      else if d.prec = Before then transl_decline conf "before (date)" s
      else transl_decline conf "after (date)" s
  | Maybe ->
      let s =
        if d.day = 0 && d.month = 0 then transl conf "in (year)" ^ " " ^ sy
        else if d.day = 0 then transl_decline conf "in (month year)" sy
        else transl_decline conf "on (day month year)" sy
      in
      transl_decline conf "possibly (date)" s
  | OrYear z ->
      let s =
        if d.day = 0 && d.month = 0 then transl conf "in (year)" ^ " " ^ sy
        else if d.day = 0 then transl_decline conf "in (month year)" sy
        else transl_decline conf "on (day month year)" sy
      in
      s ^ " " ^ transl conf "or" ^ " " ^ nominative (code_year conf z)
  | YearInt z ->
      let s =
        if d.day = 0 && d.month = 0 then sy
        else if d.day = 0 then sy
        else transl_decline conf "on (day month year)" sy
      in
      transl conf "between (date)" ^ " " ^ s ^ " " ^
        transl_nth conf "and" 0 ^ " " ^ nominative (code_year conf z) ]
;

value replace_spaces_by_nbsp s =
  loop 0 0 where rec loop i len =
    if i = String.length s then Buff.get len
    else if s.[i] = ' ' then loop (i + 1) (Buff.mstore len "&nbsp;")
    else loop (i + 1) (Buff.store len s.[i])
;

value string_of_on_prec_dmy conf code_dmy sy d =
  let r = string_of_on_prec_dmy_aux conf code_dmy sy d in
  replace_spaces_by_nbsp r
;

value string_of_on_dmy conf d =
  let sy = code_dmy conf d in
  string_of_on_prec_dmy conf code_year sy d
;

value string_of_on_french_dmy conf d =
  let sy = code_french_date conf d.day d.month d.year in
  string_of_on_prec_dmy conf code_french_year sy d
;

value string_of_on_hebrew_dmy conf d =
  let sy = code_hebrew_date conf d.day d.month d.year in
  string_of_on_prec_dmy conf code_year sy d
;

value string_of_prec_dmy conf s d =
  match d.prec with
  [ Sure -> nominative s
  | About -> transl_decline conf "about (date)" s
  | Before -> transl_decline conf "before (date)" s
  | After -> transl_decline conf "after (date)" s
  | Maybe -> transl_decline conf "possibly (date)" s
  | OrYear z ->
      s ^ " " ^ transl conf "or" ^ " " ^
        nominative (code_date conf (transl_nth conf "(date)" 3) 0 0 z)
  | YearInt z ->
      transl conf "between (date)" ^ " " ^ s ^ " " ^
        transl_nth conf "and" 0 ^ " " ^
        nominative (code_date conf (transl_nth conf "(date)" 3) 0 0 z) ]
;

value string_of_dmy conf d =
  let sy = code_dmy conf d in string_of_prec_dmy conf sy d
;

value gregorian_precision conf d =
  if d.delta = 0 then string_of_dmy conf d
  else
    let d2 =
      Calendar.gregorian_of_sdn d.prec (Calendar.sdn_of_gregorian d + d.delta)
    in
    transl conf "between (date)" ^ " " ^ string_of_on_dmy conf d ^ " " ^
      transl_nth conf "and" 0 ^ " " ^ string_of_on_dmy conf d2
;

value string_of_ondate conf =
  fun
  [ Dgreg d Dgregorian -> string_of_on_dmy conf d
  | Dgreg d Djulian ->
      let cal_prec =
        if d.year < 1582 then "" else " (" ^ gregorian_precision conf d ^ ")"
      in
      let d1 = Calendar.julian_of_gregorian d in
      string_of_on_dmy conf d1 ^ " " ^
        transl_nth conf "gregorian/julian/french/hebrew" 1 ^ cal_prec
  | Dgreg d Dfrench ->
      let d1 = Calendar.french_of_gregorian d in
      let s = string_of_on_french_dmy conf d1 in
      match d.prec with
      [ Sure -> s ^ " " ^ " (" ^ gregorian_precision conf d ^ ")"
      | About | Before | After | Maybe | OrYear _ | YearInt _ -> s ]
  | Dgreg d Dhebrew ->
      let d1 = Calendar.hebrew_of_gregorian d in
      let s = string_of_on_hebrew_dmy conf d1 in
      match d.prec with
      [ Sure -> s ^ " " ^ " (" ^ gregorian_precision conf d ^ ")"
      | About | Before | After | Maybe | OrYear _ | YearInt _ -> s ]
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

value string_of_age conf a =
  match a with
  [ {day = 0; month = 0; year = y} ->
      if y > 1 then string_of_int y ^ " " ^ transl conf "years old"
      else if y = 1 then transl conf "one year old"
      else transl conf "birth"
  | {day = 0; month = m; year = y} ->
      if y >= 2 then string_of_int y ^ " " ^ transl conf "years old"
      else if y > 0 || m > 1 then
        string_of_int (y * 12 + m) ^ " " ^ transl conf "months old"
      else if m = 1 then transl conf "one month old"
      else transl conf "less than one month old"
  | {day = d; month = m; year = y} ->
      if y >= 2 then string_of_int y ^ " " ^ transl conf "years old"
      else if y > 0 || m > 1 then
        string_of_int (y * 12 + m) ^ " " ^ transl conf "months old"
      else if m = 1 then transl conf "one month old"
      else if d >= 2 then string_of_int d ^ " " ^ transl conf "days old"
      else if d == 1 then transl conf "one day old"
      else "0" ]
;

value year_text d =
  let s =
    match d.prec with
    [ Before -> "/"
    | About | Maybe -> "ca "
    | _ -> "" ]
  in
  let s = s ^ string_of_int (year_of d) in
  match d.prec with
  [ After -> s ^ "/"
  | OrYear x -> s ^ "/" ^ string_of_int x
  | YearInt x -> s ^ "/" ^ string_of_int x
  | _ -> s ]
;

value of_course_died conf p =
  match Adef.od_of_codate p.birth with
  [ Some (Dgreg d _) -> conf.Config.today.year - d.year > 120
  | _ -> False ]
;

value get_birth_death_date p =
  let (birth_date, approx) =
    match Adef.od_of_codate p.birth with
    [ None -> (Adef.od_of_codate p.baptism, True)
    | x -> (x, False) ]
  in
  let (death_date, approx) =
    match date_of_death p.death with
    [ Some d -> (Some d, approx)
    | _ ->
        match p.burial with
        [ Buried cd -> (Adef.od_of_codate cd, True)
        | Cremated cd -> (Adef.od_of_codate cd, True)
        | _ -> (None, approx) ] ]
  in
  (birth_date, death_date, approx)
;

value short_dates_text conf base p =
  if authorized_age conf base p then
    let (birth_date, death_date, _) = get_birth_death_date p in
    let s = "" in
    let s =
      match birth_date with
      [ Some (Dgreg d _) -> s ^ year_text d
      | _ -> s ]
    in
    let s =
      match (birth_date, death_date) with
      [ (Some _, Some _) -> s ^ "-"
      | (Some _, None) -> if p.death = NotDead then s ^ "-" else s
      | _ ->
          match p.death with
          [ Death _ _ | DeadDontKnowWhen | DeadYoung ->
              if s = "" then "+" else s ^ nbsp ^ "+"
          | _ -> s ] ]
    in
    let s =
      match death_date with
      [ Some (Dgreg d _) -> s ^ year_text d
      | _ -> s ]
    in
    if s <> "" then " <em>" ^ s ^ "</em>" else s
  else ""
;

value short_marriage_date_text conf base fam p1 p2 =
  if authorized_age conf base p1 && authorized_age conf base p2 then
    match Adef.od_of_codate fam.marriage with
    [ Some (Dgreg d _) ->
        "<span style=\"font-size:60%\">" ^ year_text d ^ "</span>"
    | _ -> "" ]
  else ""
;

value string_of_place conf pl =
  Util.string_with_macros conf False [] pl
;

value print_dates conf base p =
  let cap s = ", " ^ s in
  let is = index_of_sex p.sex in
  do {
    let birth_place = sou base p.birth_place in
    match Adef.od_of_codate p.birth with
    [ Some d ->
        do {
          Wserver.wprint "%s " (cap (transl_nth conf "born" is));
          Wserver.wprint "%s" (string_of_ondate conf d);
          if birth_place <> "" then Wserver.wprint ",\n" else ();
        }
    | None ->
        if birth_place <> "" then
          Wserver.wprint "%s\n-&nbsp;" (cap (transl_nth conf "born" is))
        else () ];
    if birth_place <> "" then
      Wserver.wprint "%s" (string_of_place conf birth_place)
    else ();
    let baptism = Adef.od_of_codate p.baptism in
    let baptism_place = sou base p.baptism_place in
    match baptism with
    [ Some d ->
        do {
          Wserver.wprint "%s " (cap (transl_nth conf "baptized" is));
          Wserver.wprint "%s" (string_of_ondate conf d);
          if baptism_place <> "" then Wserver.wprint ",\n" else ();
        }
    | None ->
        if baptism_place <> "" then
          Wserver.wprint "%s\n-&nbsp;"
            (cap (transl_nth conf "baptized" is))
        else () ];
    if baptism_place <> "" then
      Wserver.wprint "%s" (string_of_place conf baptism_place)
    else ();
    let death_place = sou base p.death_place in
    match p.death with
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
        do {
          Wserver.wprint "%s " (cap dr_w);
          Wserver.wprint "%s" (string_of_ondate conf d);
          if death_place <> "" then Wserver.wprint ",\n" else ();
        }
    | DeadYoung ->
        do {
          Wserver.wprint "%s" (cap (transl_nth conf "died young" is));
          if death_place <> "" then Wserver.wprint "\n-&nbsp;" else ();
        }
    | DeadDontKnowWhen ->
        match (death_place, p.burial) with
        [ ("", Buried _ | Cremated _) -> ()
        | _ ->
            if death_place <> "" || not (of_course_died conf p) then do {
              Wserver.wprint "%s" (cap (transl_nth conf "died" is));
              if death_place <> "" then Wserver.wprint "\n-&nbsp;" else ();
            }
            else () ]
    | DontKnowIfDead | NotDead -> () ];
    if death_place <> "" then
      Wserver.wprint "%s" (string_of_place conf death_place)
    else ();
    let burial_date_place cod =
      let place = sou base p.burial_place in
      do {
         match Adef.od_of_codate cod with
         [ Some d ->
             do {
               Wserver.wprint " %s" (string_of_ondate conf d);
               if place <> "" then Wserver.wprint ",\n" else ();
             }
         | None -> if place <> "" then Wserver.wprint " -&nbsp;" else () ];
         if place <> "" then
           Wserver.wprint "%s" (string_of_place conf place)
         else ();
      }
    in
    match p.burial with
    [ Buried cod ->
        do {
          Wserver.wprint "%s" (cap (transl_nth conf "buried" is));
          burial_date_place cod;
        }
    | Cremated cod ->
        do {
          Wserver.wprint "%s" (cap (transl_nth conf "cremated" is));
          burial_date_place cod;
        }
    | UnknownBurial -> () ];
    let (birth_date, death_date, approx) = get_birth_death_date p in
    match (birth_date, death_date) with
    [ (Some (Dgreg ({prec = Sure | About | Maybe} as d1) _),
       Some (Dgreg ({prec = Sure | About | Maybe} as d2) _))
      when d1 <> d2 ->
        let a = time_gone_by d1 d2 in
        if a.year < 0 || a.year = 0 && a.month = 0 then ()
        else do {
          Wserver.wprint "\n(";
          Wserver.wprint "%s " (transl conf "age at death:");
          if not approx && d1.prec = Sure && d2.prec = Sure then ()
          else
            Wserver.wprint "%s " (transl_decline conf "possibly (date)" "");
          Wserver.wprint "%s)" (string_of_age conf a);
        }
    | _ -> () ];
  }
;

(* Calendar request *)

value gregorian_month_name conf n =
  capitale (nominative (transl_nth conf "(month)" n))
;
value julian_month_name = gregorian_month_name;
value french_month_name conf n = capitale (nominative (french_month conf n));
value hebrew_month_name conf n = capitale (nominative (hebrew_month conf n));

value print_year date var =
  do {
    stag "td" begin
      Wserver.wprint "<input type=submit name=y%s1 value=\" &lt; \">" var;
    end;
    Wserver.wprint "\n";
    stag "td" begin
      Wserver.wprint "<input name=y%s size=5 maxlength=5 value=%d>" var
        date.year;
    end;
    Wserver.wprint "\n";
    stag "td" begin
      Wserver.wprint "<input type=submit name=y%s2 value=\" &gt; \">" var;
    end;
    Wserver.wprint "\n";
  }
;

value print_month conf date month_name n_months var =
  do {
    stag "td" begin
      Wserver.wprint "<input type=submit name=m%s1 value=\" &lt; \">" var;
    end;
    Wserver.wprint "\n";
    tag "td" "align=center" begin
      tag "select" "name=m%s" var begin
        for i = 1 to n_months do {
          Wserver.wprint "<option value=%d%s> %s\n" i
            (if date.month = i then " selected" else "")
            (month_name conf (i - 1))
        };
      end;
    end;
    stag "td" begin
      Wserver.wprint "<input type=submit name=m%s2 value=\" &gt; \">" var;
    end;
    Wserver.wprint "\n";
  }
;

value print_day date var =
  do {
    stag "td" begin
      Wserver.wprint "<input type=submit name=d%s1 value=\" &lt; \">" var;
    end;
    Wserver.wprint "\n";
    stag "td" begin
      Wserver.wprint "<input name=d%s size=2 maxlength=2 value=%d>" var
        date.day;
    end;
    Wserver.wprint "\n";
    stag "td" begin
      Wserver.wprint "<input type=submit name=d%s2 value=\" &gt; \">" var;
    end;
    Wserver.wprint "\n";
  }
;

value print_some_calendar conf order date n month_name n_months var =
  do {
    Wserver.wprint "\n";
    tag "tr" "align=left" begin
      stag "th" begin
        Wserver.wprint "%s"
          (capitale (transl_nth conf "gregorian/julian/french/hebrew" n));
      end;
      Wserver.wprint "\n";
      if order = "ddmmyy" then do {
        print_day date var;
        print_month conf date month_name n_months var;
        print_year date var;
      }
      else do {
        print_year date var;
        print_month conf date month_name n_months var;
        print_day date var;
      };
      stag "td" begin
        Wserver.wprint "<input type=submit name=t%s value=\" = \">" var;
      end;
      Wserver.wprint "\n";
    end;
  }
;

value print_calendar_head conf order =
  tag "tr" "align=left" begin
    stag "td" begin Wserver.wprint "&nbsp;"; end;
    Wserver.wprint "\n";
    if order = "ddmmyy" then
      for i = 2 downto 0 do {
        stag "th" "align=center colspan=3" begin
          Wserver.wprint "%s" (capitale (transl_nth conf "year/month/day" i));
        end;
        Wserver.wprint "\n";
      }
    else
      for i = 0 to 2 do {
        stag "th" "align=center" begin
          Wserver.wprint "%s" (capitale (transl_nth conf "year/month/day" i));
        end;
        Wserver.wprint "\n";
      };
    stag "td" begin Wserver.wprint "&nbsp;"; end;
    Wserver.wprint "\n";
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
                 conv
                   {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
             | (_, Some _, _, _, _, _) ->
                 let yy = yy + 1 in
                 conv
                   {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
             | (_, _, Some _, _, _, _) ->
                 let (yy, mm) =
                   if mm = 1 then (yy - 1, max_month) else (yy, mm - 1)
                 in
                 conv
                   {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
             | (_, _, _, Some _, _, _) ->
                 let (yy, mm) =
                   if mm = max_month then (yy + 1, 1) else (yy, mm + 1)
                 in
                 conv
                   {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
             | (_, _, _, _, Some _, _) ->
                 let dd = dd - 1 in
                 conv
                   {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
             | (_, _, _, _, _, Some _) ->
                 let dd = dd + 1 in
                 conv
                   {day = dd; month = mm; year = yy; prec = Sure; delta = 0}
             | _ -> d ] ])
      (Calendar.sdn_of_gregorian conf.today)
      [("g", Calendar.sdn_of_gregorian, 12);
       ("j", Calendar.sdn_of_julian, 12); ("f", Calendar.sdn_of_french, 13);
       ("h", Calendar.sdn_of_hebrew, 13)]
  in
  let date = Calendar.gregorian_of_sdn Sure sdn in
  let wday =
    let sdn_today = Calendar.sdn_of_gregorian conf.today in
    let x = conf.today_wd - sdn_today + sdn in
    if x < 0 then 6 + (x + 1) mod 7 else x mod 7
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    tag "table" "style=\"margin:auto\"" begin
      stag "tbody" begin
        stag "tr" begin
          tag "td" "align=left" begin
            Wserver.wprint "- %s -"
              (capitale (nominative (transl_nth conf "(week day)" wday)));
            if date = conf.today then
              let (hh, mm, ss) = conf.time in
              Wserver.wprint " <tt>%02d:%02d:%02d</tt>" hh mm ss
            else ();
            Wserver.wprint "\n";
          end;
        end;
        stag "tr" begin
          stag "td" "align=left" begin
            Wserver.wprint "&nbsp;";
          end;
        end;
        stag "tr" begin
          tag "td" "align=center" begin
            tag "form" "method=GET action=\"%s\"" conf.command begin
              html_p conf;
              List.iter
                (fun (k, v) ->
                   Wserver.wprint "<input type=hidden name=%s value=%s>\n" k
                     (quote_escaped (decode_varenv v)))
                conf.henv;
              Wserver.wprint "<input type=hidden name=m value=CAL>\n\n";
              let order = transl conf " !dates order" in
              tag "table" "border=1" begin
                print_calendar_head conf order;
                print_some_calendar conf order date 0 gregorian_month_name
                  12 "g";
                print_some_calendar conf order
                  (Calendar.julian_of_gregorian date) 1 julian_month_name
                  12 "j";
                print_some_calendar conf order
                  (Calendar.french_of_gregorian date) 2 french_month_name
                  13 "f";
                print_some_calendar conf order
                  (Calendar.hebrew_of_gregorian date) 3 hebrew_month_name
                  13 "h";
              end;
            end;
          end;
        end;
        stag "tr" begin
          stag "td" "align=left" begin
            Wserver.wprint "&nbsp;";
          end;
        end;
        stag "tr" begin
          tag "td" "align=center" begin
            Wserver.wprint "%s: " (capitale (transl conf "julian day"));
            let jd = Calendar.sdn_of_gregorian date in
            if jd < 0 then Wserver.wprint "%d" jd
            else
              Num.print (fun x -> Wserver.wprint "%s" x)
                (transl conf "(thousand separator)") (Num.of_int jd);
          end;
        end;
      end;
    end;
    trailer conf;
  }
;
