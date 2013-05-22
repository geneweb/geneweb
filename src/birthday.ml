(* camlp5r ./pa_html.cmo *)
(* $Id: birthday.ml,v 5.17 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;
open Hutil;
open Util;

type date_event = [ DeBirth | DeDeath of death_reason ];

value print_anniversary_day conf base dead_people liste =
  tag "ul" begin
    List.iter
      (fun (p, a, date_event, txt_of) ->
         let is = index_of_sex (get_sex p) in
         tag "li" begin
           Wserver.wprint "%s\n" (txt_of conf base p);
           if not dead_people then Wserver.wprint " <em>%d</em>\n" a
           else
             let txt =
               match date_event with
               [ DeBirth -> transl_nth conf "born" is
               | DeDeath Unspecified -> transl_nth conf "died" is
               | DeDeath Killed -> transl_nth conf "killed (in action)" is
               | DeDeath Murdered -> transl_nth conf "murdered" is
               | DeDeath Executed ->
                   transl_nth conf "executed (legally killed)" is
               | DeDeath Disappeared -> transl_nth conf "disappeared" is ]
             in
             Wserver.wprint ", <em>%s %s %d</em>\n" txt
               (transl conf "in (year)") a;
         end)
      liste;
  end
;

value gen_print conf base mois f_scan dead_people =
  let tab = Array.create 31 [] in
  let title _ =
    let lab =
      if dead_people then transl conf "anniversaries"
      else transl conf "birthdays"
    in
    Wserver.wprint "%s %s" (capitale lab)
      (Util.translate_eval (transl_nth conf "(month)" (mois - 1)))
  in
  do {
    try
      while True do {
        let (p, txt_of) = f_scan () in
        if not dead_people then
          match (Adef.od_of_codate (get_birth p), get_death p) with
          [ (Some (Dgreg d _), NotDead | DontKnowIfDead) ->
              if d.prec = Sure && d.day <> 0 && d.month <> 0 &&
                 d.month = mois && d.delta = 0 then
                if authorized_age conf base p then
                  let j = d.day in
                  tab.(pred j) :=
                    [(p, d.year, DeBirth, txt_of) :: tab.(pred j)]
                else ()
              else ()
          | _ -> () ]
        else
          match get_death p with
          [ NotDead | DontKnowIfDead -> ()
          | _ ->
              do {
                match Adef.od_of_codate (get_birth p) with
                [ Some (Dgreg dt _) ->
                    if dt.prec = Sure && dt.day <> 0 && dt.month <> 0 &&
                       dt.month = mois && dt.delta = 0 then
                      if authorized_age conf base p then
                        let j = dt.day in
                        tab.(pred j) :=
                          [(p, dt.year, DeBirth, txt_of) :: tab.(pred j)]
                      else ()
                    else ()
                | _ -> () ];
                match get_death p with
                [ Death dr d ->
                    match Adef.date_of_cdate d with
                    [ Dgreg dt _ ->
                        if dt.prec = Sure && dt.day <> 0 && dt.month <> 0 &&
                           dt.month = mois && dt.delta = 0 then
                          if authorized_age conf base p then
                            let j = dt.day in
                            let a = dt.year in
                            tab.(pred j) :=
                              [(p, a, DeDeath dr, txt_of) :: tab.(pred j)]
                          else ()
                        else ()
                    | _ -> () ]
                | _ -> () ];
              } ]
      }
    with
    [ Not_found -> () ];
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<ul>\n";
    for j = 1 to 31 do {
      if tab.(pred j) <> [] then do {
        tag "li" begin
          Wserver.wprint "%d\n" j;
          let liste =
            List.sort (fun (p1, a1, _, _) (p2, a2, _, _) -> compare a1 a2)
              tab.(pred j)
          in
          print_anniversary_day conf base dead_people liste;
        end
      }
      else ()
    };
    Wserver.wprint "</ul>\n";
    trailer conf;
  }
;

value print_anniversary_list conf base dead_people dt liste =
  let a_ref = dt.year in
  tag "ul" begin
    List.iter
      (fun (p, a, date_event, txt_of) ->
         stagn "li" begin
           if dead_people then do {
             Wserver.wprint "<em>";
             match date_event with
             [ DeBirth -> Wserver.wprint "%s" (transl conf "birth")
             | DeDeath _ -> Wserver.wprint "%s" (transl conf "death") ];
             Wserver.wprint "</em>\n";
             Wserver.wprint "-&gt; ";
             Wserver.wprint "%s" (txt_of conf base p);
             Wserver.wprint "\n<em>%s %d" (transl conf "in (year)") a;
             Wserver.wprint " (";
             Wserver.wprint (ftransl conf "%d years ago")
               (conf.today.year - a);
             Wserver.wprint ")</em>";
           }
           else do {
             Wserver.wprint "%s" (txt_of conf base p);
             match get_death p with
             [ NotDead ->
                 do {
                   Wserver.wprint " <em>";
                   match a_ref - a with
                   [ 0 -> Wserver.wprint "%s" (transl conf "birth")
                   | 1 -> Wserver.wprint "%s" (transl conf "one year old")
                   | n ->
                       Wserver.wprint "%d %s" n (transl conf "years old") ];
                   Wserver.wprint "</em>";
                 }
             | _ -> () ];
           };
         end)
      liste;
  end
;

value f_scan conf base =
  let i = ref (-1) in
  fun () ->
    do {
      incr i;
      if i.val < nb_of_persons base then
        (pget conf base (Adef.iper_of_int i.val),
         referenced_person_title_text)
      else raise Not_found
    }
;
value print_birth conf base mois =
  gen_print conf base mois (f_scan conf base) False
;
value print_dead conf base mois =
  gen_print conf base mois (f_scan conf base) True
;

value print_birth_day conf base day_name fphrase wd dt list =
  match list with
  [ [] ->
      tag "p" begin
        Wserver.wprint "%s %s.\n" (capitale (transl conf "no birthday"))
          day_name;
      end
  | _ ->
      do {
        tag "p" begin
          let txt =
            transl_decline conf "on (weekday day month year)"
              (transl_nth conf "(week day)" wd ^ " " ^ Date.code_dmy conf dt)
          in
          Wserver.wprint fphrase
            (capitale day_name ^ ",\n" ^ std_color conf ("<b>" ^ txt ^ "</b>"))
            (transl conf "the birthday");
          Wserver.wprint "...\n";
        end;
        print_anniversary_list conf base False dt list;
      } ]
;

value propose_months conf mode =
  do {
    begin_centered conf;
    stag "span" begin
      Wserver.wprint "%s" 
        (capitale (transl conf "select a month to see all the anniversaries"));
    end;
    tag "table" "border=\"%d\"" conf.border begin
      tag "tr" begin
        tag "td" begin
          tag "form" "method=\"get\" action=\"%s\"" conf.command begin
            tag "p" begin
              Util.hidden_env conf;
              mode ();
              tag "select" "name=\"v\"" begin
                for i = 1 to 12 do {
                  stagn "option" "value=\"%d\"%s" i
                    (if i = conf.today.month then " selected=\"selected\""
                     else "")
                  begin
                    Wserver.wprint "%s"
                      (capitale
                         (Util.translate_eval
                            (transl_nth conf "(month)" (i - 1))));
                  end
                };
              end;
              xtag "input" "type=\"submit\" value=\"Ok\"";
            end;
          end;
        end;
      end;
    end;
    end_centered conf;
  }
;

value day_after d =
  let (day, r) =
    if d.day >= CheckItem.nb_days_in_month d.month d.year then (1, 1)
    else (succ d.day, 0)
  in
  let (month, r) = if d.month + r > 12 then (1, 1) else (d.month + r, 0) in
  let year = d.year + r in
  {day = day; month = month; year = year; prec = Sure; delta = 0}
;

value print_anniv conf base day_name fphrase wd dt list =
  match list with
  [ [] ->
      tag "p" begin
        Wserver.wprint "%s %s.\n" (capitale (transl conf "no anniversary"))
          day_name;
      end
  | _ ->
      do {
        tag "p" begin
          let txt =
            transl_decline conf "on (weekday day month year)"
              (transl_nth conf "(week day)" wd ^ " " ^ Date.code_dmy conf dt)
          in
          Wserver.wprint fphrase
            (capitale day_name ^ ",\n" ^ std_color conf ("<b>" ^ txt ^ "</b>"))
            (transl conf "the anniversary");
          Wserver.wprint "...\n";
        end;
        print_anniversary_list conf base True dt list;
      } ]
;

value print_marriage conf base month =
  let title _ =
    let lab = transl conf "anniversaries of marriage" in
    Wserver.wprint "%s %s" (capitale lab)
      (transl_decline conf "in (month year)"
         (transl_nth conf "(month)" (month - 1)))
  in
  let tab = Array.create 31 [] in
  do {
    header conf title;
    print_link_to_welcome conf True;
    for i = 0 to nb_of_families base - 1 do {
      let fam = foi base (Adef.ifam_of_int i) in
      if is_deleted_family fam then ()
      else
        match Adef.od_of_codate (get_marriage fam) with
        [ Some (Dgreg {day = d; month = m; year = y; prec = Sure} _)
          when d <> 0 && m <> 0 ->
            let father = pget conf base (get_father fam) in
            let mother = pget conf base (get_mother fam) in
            if m = month &&
               authorized_age conf base father && not (is_hidden father) &&
               authorized_age conf base mother && not (is_hidden mother) then
              tab.(pred d) := [(fam, y) :: tab.(pred d)]
            else ()
        | _ -> () ]
    };
    Wserver.wprint "<ul>";
    for i = 1 to 31 do {
      match tab.(i - 1) with
      [ [] -> ()
      | l ->
          let l = List.sort (fun (fam1, y1) (fam2, y2) -> compare y1 y2) l in
          do {
            Wserver.wprint "\n";
            html_li conf;
            Wserver.wprint "%d\n<ul>" i;
            List.iter
              (fun (fam, year) ->
                 do {
                   html_li conf;
                   Wserver.wprint "%s"
                     (referenced_person_title_text conf base
                        (pget conf base (get_father fam)));
                   Wserver.wprint "\n%s\n" (transl_nth conf "and" 0);
                   Wserver.wprint "%s"
                     (referenced_person_title_text conf base
                        (pget conf base (get_mother fam)));
                   Wserver.wprint ", <em>%s %d</em>\n"
                     (transl conf "in (year)") year;
                 })
              l;
            Wserver.wprint "</ul>\n";
          } ]
    };
    Wserver.wprint "</ul>\n";
    trailer conf;
  }
;

value print_anniversaries_of_marriage conf base y list =
  tag "ul" begin
    List.iter
      (fun (fam, year) ->
         stagn "li" begin
           Wserver.wprint "%s\n"
             (referenced_person_title_text conf base
                (pget conf base (get_father fam)));
           Wserver.wprint "%s\n" (transl_nth conf "and" 0);
           Wserver.wprint "%s"
             (referenced_person_title_text conf base
                (pget conf base (get_mother fam)));
           Wserver.wprint ", <em>%s %d\n(" (transl conf "in (year)") year;
           Wserver.wprint (ftransl conf "%d years ago")
             (conf.today.year - year);
           Wserver.wprint "</em>)";
         end)
      list;
  end
;

value print_marriage_day conf base day_name fphrase wd dt list =
  match list with
  [ [] ->
      tag "p" begin
        Wserver.wprint "%s %s.\n" (capitale (transl conf "no anniversary"))
          day_name;
      end
  | _ ->
      do {
        tag "p" begin
          Wserver.wprint fphrase
            (capitale day_name ^ ",\n" ^
             std_color conf
                ("<b>" ^
                   transl_decline conf "on (weekday day month year)"
                     (transl_nth conf "(week day)" wd ^ " " ^
                        Date.code_dmy conf dt) ^
                   "</b>"))
            (transl conf "the anniversary of marriage");
          Wserver.wprint "...\n";
        end;
        print_anniversaries_of_marriage conf base dt.year list;
      } ]
;

value match_dates conf base p d1 d2 =
  if d1.day = d2.day && d1.month = d2.month then authorized_age conf base p
  else if
    d1.day = 29 && d1.month = 2 && d2.day = 1 && d2.month = 3 &&
    not (CheckItem.leap_year d2.year) then
    authorized_age conf base p
  else False
;

value gen_print_menu_birth conf base f_scan mode =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "birthdays")) in
  let tom = day_after conf.today in
  let aft = day_after tom in
  let list_tod = ref [] in
  let list_tom = ref [] in
  let list_aft = ref [] in
  do {
    match Util.find_person_in_env conf base "" with
    [ Some p -> Perso.interp_notempl_with_menu title "perso_header" conf base p
    | None -> header conf title ];
    print_link_to_welcome conf True;
    try
      while True do {
        let (p, txt_of) = f_scan () in
        match (Adef.od_of_codate (get_birth p), get_death p) with
        [ (Some (Dgreg d _), NotDead | DontKnowIfDead) ->
            if d.prec = Sure && d.day <> 0 && d.month <> 0 then
              if match_dates conf base p d conf.today then
                list_tod.val := [(p, d.year, DeBirth, txt_of) :: list_tod.val]
              else if match_dates conf base p d tom then
                list_tom.val := [(p, d.year, DeBirth, txt_of) :: list_tom.val]
              else if match_dates conf base p d aft then
                list_aft.val := [(p, d.year, DeBirth, txt_of) :: list_aft.val]
              else ()
            else ()
        | _ -> () ]
      }
    with
    [ Not_found -> () ];
    List.iter
      (fun xx ->
         xx.val :=
           List.sort (fun (p1, a1, _, _) (p2, a2, _, _) -> compare a1 a2)
             xx.val)
      [list_tod; list_tom; list_aft];
    print_birth_day conf base (transl conf "today")
      (ftransl conf "%s, it is %s of") conf.today_wd conf.today list_tod.val;
    print_birth_day conf base (transl conf "tomorrow")
      (ftransl conf "%s, it will be %s of") ((conf.today_wd + 1) mod 7) tom
      list_tom.val;
    print_birth_day conf base (transl conf "the day after tomorrow")
      (ftransl conf "%s, it will be %s of") ((conf.today_wd + 2) mod 7) aft
      list_aft.val;
    Wserver.wprint "\n";
    propose_months conf mode;
    Wserver.wprint "\n";
    trailer conf;
  }
;

value print_menu_birth conf base =
  let i = ref (-1) in
  let nb_per = nb_of_persons base in
  let f_scan () =
    do {
      incr i;
      if i.val < nb_per then
        (pget conf base (Adef.iper_of_int i.val),
         referenced_person_title_text)
      else raise Not_found
    }
  in
  let mode () = xtag "input" "type=\"hidden\" name=\"m\" value=\"AN\"" in
  gen_print_menu_birth conf base f_scan mode
;

value gen_print_menu_dead conf base f_scan mode =
  let title _ =
    Wserver.wprint "%s"
      (capitale (transl conf "anniversaries of dead people"))
  in
  let tom = day_after conf.today in
  let aft = day_after tom in
  let list_tod = ref [] in
  let list_tom = ref [] in
  let list_aft = ref [] in
  do {
    header conf title;
    print_link_to_welcome conf True;
    try
      while True do {
        let (p, txt_of) = f_scan () in
        match get_death p with
        [ NotDead | DontKnowIfDead -> ()
        | _ ->
            do {
              match Adef.od_of_codate (get_birth p) with
              [ Some (Dgreg d _) ->
                  if d.prec = Sure && d.day <> 0 && d.month <> 0 then
                    if match_dates conf base p d conf.today then
                      list_tod.val :=
                        [(p, d.year, DeBirth, txt_of) :: list_tod.val]
                    else if match_dates conf base p d tom then
                      list_tom.val :=
                        [(p, d.year, DeBirth, txt_of) :: list_tom.val]
                    else if match_dates conf base p d aft then
                      list_aft.val :=
                        [(p, d.year, DeBirth, txt_of) :: list_aft.val]
                    else ()
                  else ()
              | _ -> () ];
              match get_death p with
              [ Death dr d ->
                  match Adef.date_of_cdate d with
                  [ Dgreg d _ ->
                      if d.prec = Sure && d.day <> 0 && d.month <> 0 then
                        if match_dates conf base p d conf.today then
                          list_tod.val :=
                            [(p, d.year, DeDeath dr, txt_of) :: list_tod.val]
                        else if match_dates conf base p d tom then
                          list_tom.val :=
                            [(p, d.year, DeDeath dr, txt_of) :: list_tom.val]
                        else if match_dates conf base p d aft then
                          list_aft.val :=
                            [(p, d.year, DeDeath dr, txt_of) :: list_aft.val]
                        else ()
                      else ()
                  | _ -> () ]
              | _ -> () ];
            } ]
      }
    with
    [ Not_found -> () ];
    List.iter
      (fun xx ->
         xx.val :=
           List.sort (fun (p1, a1, _, _) (p2, a2, _, _) -> compare a1 a2)
             xx.val)
      [list_tod; list_tom; list_aft];
    print_anniv conf base (transl conf "today")
      (ftransl conf "%s, it is %s of") conf.today_wd conf.today list_tod.val;
    print_anniv conf base (transl conf "tomorrow")
      (ftransl conf "%s, it will be %s of") ((conf.today_wd + 1) mod 7) tom
      list_tom.val;
    print_anniv conf base (transl conf "the day after tomorrow")
      (ftransl conf "%s, it will be %s of") ((conf.today_wd + 2) mod 7) aft
      list_aft.val;
    Wserver.wprint "\n";
    propose_months conf mode;
    Wserver.wprint "\n";
    trailer conf;
  }
;

value print_menu_dead conf base =
  let i = ref (-1) in
  let f_scan () =
    do {
      incr i;
      if i.val < nb_of_persons base then
        (pget conf base (Adef.iper_of_int i.val),
         referenced_person_title_text)
      else raise Not_found
    }
  in
  let mode () = xtag "input" "type=\"hidden\" name=\"m\" value=\"AD\"" in
  gen_print_menu_dead conf base f_scan mode
;

value match_mar_dates conf base cpl d1 d2 =
  if d1.day = d2.day && d1.month = d2.month then
    authorized_age conf base (pget conf base (get_father cpl)) &&
    authorized_age conf base (pget conf base (get_mother cpl))
  else if
    d1.day = 29 && d1.month = 2 && d2.day = 1 && d2.month = 3 &&
    not (CheckItem.leap_year d2.year) then
    authorized_age conf base (pget conf base (get_father cpl)) &&
    authorized_age conf base (pget conf base (get_mother cpl))
  else False
;

value print_menu_marriage conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "anniversaries of marriage"))
  in
  let tom = day_after conf.today in
  let aft = day_after tom in
  let list_tod = ref [] in
  let list_tom = ref [] in
  let list_aft = ref [] in
  do {
    header conf title;
    print_link_to_welcome conf True;
    for i = 0 to nb_of_families base - 1 do {
      let fam = foi base (Adef.ifam_of_int i) in
      if is_deleted_family fam then ()
      else
        match (Adef.od_of_codate (get_marriage fam), get_divorce fam) with
        [ (Some (Dgreg d _), NotDivorced) when d.day <> 0 && d.month <> 0 && d.prec = Sure ->
            let update_list cpl =
              if match_mar_dates conf base cpl d conf.today then
                list_tod.val := [(cpl, d.year) :: list_tod.val]
              else if match_mar_dates conf base cpl d tom then
                list_tom.val := [(cpl, d.year) :: list_tom.val]
              else if match_mar_dates conf base cpl d aft then
                list_aft.val := [(cpl, d.year) :: list_aft.val]
              else ()
            in
            if conf.use_restrict then
              let father = pget conf base (get_father fam) in
              let mother = pget conf base (get_mother fam) in
              if not (is_hidden father) && not (is_hidden mother) then
                update_list fam
              else ()
            else update_list fam
        | _ -> () ]
    };
    List.iter
      (fun xx ->
         xx.val := List.sort (fun (_, y1) (_, y2) -> compare y1 y2) xx.val)
      [list_tod; list_tom; list_aft];
    print_marriage_day conf base (transl conf "today")
      (ftransl conf "%s, it is %s of") conf.today_wd conf.today list_tod.val;
    print_marriage_day conf base (transl conf "tomorrow")
      (ftransl conf "%s, it will be %s of") ((conf.today_wd + 1) mod 7) tom
      list_tom.val;
    print_marriage_day conf base (transl conf "the day after tomorrow")
      (ftransl conf "%s, it will be %s of") ((conf.today_wd + 2) mod 7) aft
      list_aft.val;
    Wserver.wprint "\n";
    let mode () = xtag "input" "type=\"hidden\" name=\"m\" value=\"AM\"" in
    propose_months conf mode;
    Wserver.wprint "\n";
    trailer conf;
  }
;

(* template *)
type env 'a =
  [ Vother of 'a
  | Vnone ]
;

value get_vother = fun [ Vother x -> Some x | _ -> None ];
value set_vother x = Vother x;

value print_anniversaries conf base =
  if p_getenv conf.env "old" = Some "on" then ()
  else
  Hutil.interp conf base "annivmenu"
    {Templ.eval_var _ = raise Not_found;
     Templ.eval_transl _ = Templ.eval_transl conf;
     Templ.eval_predefined_apply _ = raise Not_found;
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach _ = raise Not_found}
    [] ()
;
