(* camlp4r ./pa_html.cmo *)
(* $Id: birthday.ml,v 3.18 2001-03-01 08:21:26 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;
open Util;
open Gutil;

type date_event = [ DeBirth | DeDeath of death_reason ];

value afficher_anniversaires_jour conf base dead_people liste =
  do Wserver.wprint "<ul>\n";
     List.iter
       (fun (p, a, date_event, txt_of) ->
          let is = index_of_sex p.sex in
          do html_li conf;
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
          return ())
       liste;
     Wserver.wprint "</ul>\n";
  return ()
;

value gen_print conf base mois f_scan dead_people =
  let tab = Array.create 31 [] in
  let title _ =
    let lab =
      if dead_people then transl conf "anniversaries"
      else transl conf "birthdays"
    in
    Wserver.wprint "%s %s" (capitale lab)
      (transl_nth conf "(month)" (mois - 1))
  in
  do try
       while True do
         let (p, txt_of) = f_scan () in
         if not dead_people then
           match (Adef.od_of_codate p.birth, p.death) with
           [ (Some (Dgreg d _), NotDead | DontKnowIfDead) ->
               if d.prec = Sure && d.day <> 0 && d.month <> 0
               && d.month = mois && d.delta = 0 then
                 if age_autorise conf base p then
                   let j = d.day in
                   tab.(pred j) :=
                     [(p, d.year, DeBirth, txt_of) :: tab.(pred j)]
                 else ()
               else ()
           | _ -> () ]
         else
           match p.death with
           [ NotDead | DontKnowIfDead -> ()
           | _ ->
               do match Adef.od_of_codate p.birth with
                  [ Some (Dgreg dt _) ->
                      if dt.prec = Sure && dt.day <> 0 && dt.month <> 0
                      && dt.month = mois && dt.delta = 0 then
                        if age_autorise conf base p then
                          let j = dt.day in
                          tab.(pred j) :=
                            [(p, dt.year, DeBirth, txt_of) :: tab.(pred j)]
                        else ()
                      else ()
                  | _ -> () ];
                  match p.death with
                  [ Death dr d ->
                      match Adef.date_of_cdate d with
                      [ Dgreg dt _ ->
                          if dt.prec = Sure && dt.day <> 0 && dt.month <> 0
                          && dt.month = mois && dt.delta = 0 then
                            if age_autorise conf base p then
                              let j = dt.day in
                              let a = dt.year in
                              tab.(pred j) :=
                                [(p, a, DeDeath dr, txt_of) :: tab.(pred j)]
                            else ()
                          else ()
                      | _ -> () ]
                  | _ -> () ];
               return () ];
       done
     with [ Not_found -> () ];
     header conf title;
     print_link_to_welcome conf True;
     Wserver.wprint "<ul>\n";
     for j = 1 to 31 do
       if tab.(pred j) <> [] then
         do html_li conf;
            Wserver.wprint "%d\n" j;
            let liste =
              Sort.list (fun (p1, a1, _, _) (p2, a2, _, _) -> a1 <= a2)
                tab.(pred j)
            in
            afficher_anniversaires_jour conf base dead_people liste;
         return ()
       else ();
     done;
     Wserver.wprint "</ul>\n";
     trailer conf;
  return ()
;

value afficher_liste_anniversaires conf base dead_people dt liste =
  let a_ref = dt.year in
  do Wserver.wprint "<ul>\n";
     List.iter
       (fun (p, a, date_event, txt_of) ->
          do html_li conf;
             if dead_people then
               do Wserver.wprint "<em>";
                  match date_event with
                  [ DeBirth ->
                      Wserver.wprint "%s" (transl conf "of the birth")
                  | DeDeath (Unspecified | Killed) ->
                      Wserver.wprint "%s" (transl conf "of the death")
                  | DeDeath Murdered ->
                       Wserver.wprint "%s" (transl conf "of the murder")
                  | DeDeath Executed ->
                      Wserver.wprint "%s" (transl conf "of the execution")
                  | DeDeath Disappeared ->
                      Wserver.wprint "%s"
                        (transl conf "of the disappearance") ];
                  Wserver.wprint "</em>\n";
                  Wserver.wprint "-&gt; ";
                  Wserver.wprint "%s" (txt_of conf base p);
                  Wserver.wprint "\n<em>%s %d" (transl conf "in (year)") a;
                  Wserver.wprint " (";
                  Wserver.wprint (ftransl conf "%d years ago")
                    (conf.today.year - a);
                  Wserver.wprint ")</em>";
               return ()
             else
               do Wserver.wprint "\n%s" (txt_of conf base p);
                  match p.death with
                  [ NotDead ->
                      do Wserver.wprint " <em>";
                         match a_ref - a with
                         [ 0 -> Wserver.wprint "%s" (transl conf "birth")
                         | 1 ->
                             Wserver.wprint "%s" (transl conf "one year old")
                         | n ->
                             Wserver.wprint "%d %s" n
                               (transl conf "years old") ];
                         Wserver.wprint "</em>";
                      return ()
                  | _ -> () ];
               return ();
             Wserver.wprint "\n";
          return ())
       liste;
     Wserver.wprint "</ul>\n";
  return ()
;

value f_scan conf base =
  let i = ref (-1) in
  fun () ->
    do incr i; return
    if i.val < base.data.persons.len then
      (base.data.persons.get i.val, referenced_person_title_text)
    else raise Not_found
;
value print_birth conf base mois =
  gen_print conf base mois (f_scan conf base) False;
value print_dead conf base mois =
  gen_print conf base mois (f_scan conf base) True;

value print_birth_day conf base day_name verb wd dt list =
  do Wserver.wprint "\n"; html_p conf; return
  match list with
  [ [] ->
      Wserver.wprint "%s %s.\n"
        (capitale (transl conf "no birthday")) day_name
  | _ ->
      do Wserver.wprint "%s,\n" (capitale day_name);
         Wserver.wprint "%s%s\n"
           (std_color conf
              ("<b>" ^
               transl_decline conf "on (day month year)"
                 (transl_nth conf "(week day)" wd ^ " " ^
                  Date.code_dmy conf dt) ^
               "</b>"))
           verb;
         Wserver.wprint "%s\n"
           (transl_decline2 conf "%1 of %2" (transl conf "the birthday")
            "...");
         afficher_liste_anniversaires conf base False dt list;
      return () ]
;

value propose_months conf mode =
  tag "center" begin
    tag "form" "method=get action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      mode ();
      tag "select" "name=v" begin
        for i = 1 to 12 do
          Wserver.wprint "<option value=%d%s>%s\n" i
            (if i = conf.today.month then " selected" else "")
            (capitale (nominative (transl_nth conf "(month)" (i - 1))));
        done;
      end;
      Wserver.wprint "<input type=submit value=\"Ok\">\n";
    end;
  end
;

value day_after d =
  let (day, r) =
    if d.day >= nb_jours_dans_mois d.month d.year then (1, 1)
    else (succ d.day, 0)
  in
  let (month, r) = if d.month + r > 12 then (1, 1) else (d.month + r, 0) in
  let year = d.year + r in
  {day = day; month = month; year = year; prec = Sure; delta = 0}
;

value print_anniv conf base day_name verb wd dt list =
  do Wserver.wprint "\n"; html_p conf; return
  match list with
  [ [] ->
      Wserver.wprint "%s %s.\n"
        (capitale (transl conf "no anniversary")) day_name
  | _ ->
      do Wserver.wprint "%s, %s%s %s:\n"
           (capitale day_name)
           (std_color conf
              ("<b>" ^
               transl_decline conf "on (day month year)"
                 (transl_nth conf "(week day)" wd ^ " " ^
                  Date.code_dmy conf dt) ^
              "</b>"))
           verb (transl conf "the anniversary");
         afficher_liste_anniversaires conf base True dt list;
      return () ]
;

value print_marriage conf base month =
  let title _ =
    let lab = transl conf "anniversaries of marriage" in
    Wserver.wprint "%s %s" (capitale lab)
      (transl_decline conf "in (month year)"
         (transl_nth conf "(month)" (month - 1)))
  in
  let tab = Array.create 31 [] in
  do header conf title;
     print_link_to_welcome conf True;
     for i = 0 to base.data.families.len - 1 do
       let fam = base.data.families.get i in
       if is_deleted_family fam then ()
       else
         match Adef.od_of_codate fam.marriage with
         [ Some (Dgreg {day = d; month = m; year = y; prec = Sure} _) when
           d <> 0 && m <> 0 ->
             let cpl = base.data.couples.get i in
             if m == month && age_autorise conf base (poi base cpl.father)
             && age_autorise conf base (poi base cpl.mother) then
               tab.(pred d) := [(cpl, y) :: tab.(pred d)]
             else ()
         | _ -> () ];
     done;
     Wserver.wprint "<ul>";
     for i = 1 to 31 do
       match tab.(i - 1) with
       [ [] -> ()
       | l ->
           let l = Sort.list (fun (fam1, y1) (fam2, y2) -> y1 < y2) l in
           do Wserver.wprint "\n";
              html_li conf;
              Wserver.wprint "%d\n<ul>" i;
              List.iter
                (fun (fam, year) ->
                   do Wserver.wprint "\n";
                      html_li conf;
                      afficher_personne_titre_referencee conf base
                        (poi base fam.father);
                      Wserver.wprint "\n%s\n" (transl conf "and");
                      afficher_personne_titre_referencee conf base
                        (poi base fam.mother);
                      Wserver.wprint ", <em>%s %d</em>\n"
                        (transl conf "in (year)") year;
                   return ())
               l;
               Wserver.wprint "</ul>\n";
           return () ];
     done;
     Wserver.wprint "</ul>\n";
     trailer conf;
  return ()
;

value print_anniversaries_of_marriage conf base y list =
  do Wserver.wprint "<ul>";
     List.iter
       (fun (fam, year) ->
          do Wserver.wprint "\n";
             html_li conf;
             afficher_personne_titre_referencee conf base
               (poi base fam.father);
             Wserver.wprint "\n%s\n" (transl conf "and");
             afficher_personne_titre_referencee conf base
               (poi base fam.mother);
             Wserver.wprint ", <em>%s %d\n("
               (transl conf "in (year)") year;
             Wserver.wprint (ftransl conf "%d years ago")
               (conf.today.year - year);
             Wserver.wprint "</em>)\n";
          return ())
       list;
     Wserver.wprint "</ul>\n";
  return ()
;

value print_marriage_day conf base day_name verb wd dt list =
  do Wserver.wprint "\n"; html_p conf; return
  match list with
  [ [] ->
      Wserver.wprint "%s %s.\n"
        (capitale (transl conf "no anniversary")) day_name
  | _ ->
      do Wserver.wprint "%s,\n" (capitale day_name);
         Wserver.wprint "%s%s\n"
           (std_color conf
              ("<b>" ^
               transl_decline conf "on (day month year)"
                 (transl_nth conf "(week day)" wd ^ " " ^
                  Date.code_dmy conf dt) ^
               "</b>"))
           verb;
         Wserver.wprint "%s\n"
           (transl_decline2 conf "%1 of %2"
              (transl conf "the anniversary of marriage") "...");
         print_anniversaries_of_marriage conf base dt.year list;
      return () ]
;

value match_dates conf base p d1 d2 =
  (* dont factorize "age_autorise": it is slow *)
  if d1.day == d2.day && d1.month == d2.month then
    age_autorise conf base p
  else if d1.day == 29 && d1.month == 2 && d2.day == 1 && d2.month = 3 &&
    not (leap_year d2.year) then age_autorise conf base p
  else False
;

value gen_print_menu_birth conf base f_scan mode =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "birthdays"))
  in
  let tom = day_after conf.today in
  let aft = day_after tom in
  let list_tod = ref [] in
  let list_tom = ref [] in
  let list_aft = ref [] in
  do header conf title;
     print_link_to_welcome conf True;
     try
       while True do
         let (p, txt_of) = f_scan () in
         match (Adef.od_of_codate p.birth, p.death) with
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
         | _ -> () ];
       done
     with
     [ Not_found -> () ];
     List.iter
       (fun xx ->
          xx.val :=
            Sort.list (fun (p1, a1, _, _) (p2, a2, _, _) -> a1 <= a2) xx.val)
       [list_tod; list_tom; list_aft];
     print_birth_day conf base (transl conf "today") (transl conf ", it is")
       conf.today_wd conf.today list_tod.val;
     print_birth_day conf base (transl conf "tomorrow")
       (transl conf ", it will be") ((conf.today_wd + 1) mod 7)
       tom list_tom.val;
     print_birth_day conf base (transl conf "the day after tomorrow")
       (transl conf ", it will be") ((conf.today_wd + 2) mod 7)
       aft list_aft.val;
     Wserver.wprint "\n";
     html_p conf;
     propose_months conf mode;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_menu_birth conf base =
(*
  let _ = base.data.persons.array () in
*)
  let i = ref (-1) in
  let f_scan () =
    do incr i; return
    if i.val < base.data.persons.len then
      (base.data.persons.get i.val, referenced_person_title_text)
    else raise Not_found
  in
  let mode () =
    Wserver.wprint "<input type=hidden name=m value=AN>\n"
  in
  gen_print_menu_birth conf base f_scan mode
;

value print_menu_dead conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "anniversaries of dead people"))
  in
  let tom = day_after conf.today in
  let aft = day_after tom in
  let list_tod = ref [] in
  let list_tom = ref [] in
  let list_aft = ref [] in
(*
  let _ = base.data.persons.array () in
*)
  do header conf title;
     print_link_to_welcome conf True;
     for i = 0 to base.data.persons.len - 1 do
       let p = base.data.persons.get i in
       match p.death with
       [ NotDead | DontKnowIfDead -> ()
       | _ ->
           do match Adef.od_of_codate p.birth with
              [ Some (Dgreg d _) ->
                  if d.prec = Sure && d.day <> 0 && d.month <> 0 then
                    if match_dates conf base p d conf.today then
                      list_tod.val :=
                        [(p, d.year, DeBirth, referenced_person_title_text) ::
                         list_tod.val]
                    else if match_dates conf base p d tom then
                      list_tom.val :=
                        [(p, d.year, DeBirth, referenced_person_title_text) ::
                         list_tom.val]
                    else if match_dates conf base p d aft then
                      list_aft.val :=
                        [(p, d.year, DeBirth, referenced_person_title_text) ::
                         list_aft.val]
                    else ()
                  else ()
              | _ -> () ];
              match p.death with
              [ Death dr d ->
                  match Adef.date_of_cdate d with
                  [ Dgreg d _ ->
                      if d.prec = Sure && d.day <> 0 && d.month <> 0 then
                        if match_dates conf base p d conf.today then
                          list_tod.val :=
                            [(p, d.year, DeDeath dr,
                              referenced_person_title_text) :: list_tod.val]
                        else if match_dates conf base p d tom then
                          list_tom.val :=
                            [(p, d.year, DeDeath dr,
                              referenced_person_title_text) :: list_tom.val]
                        else if match_dates conf base p d aft then
                          list_aft.val :=
                            [(p, d.year, DeDeath dr,
                              referenced_person_title_text) :: list_aft.val]
                        else ()
                      else ()
                  | _ -> () ]
              | _ -> () ];
           return () ];
     done;
     List.iter
       (fun xx ->
          xx.val :=
            Sort.list (fun (p1, a1, _, _) (p2, a2, _, _) -> a1 <= a2) xx.val)
       [list_tod; list_tom; list_aft];
     print_anniv conf base (transl conf "today") (transl conf ", it is")
       conf.today_wd conf.today list_tod.val;
     print_anniv conf base (transl conf "tomorrow")
       (transl conf ", it will be") ((conf.today_wd + 1) mod 7)
       tom list_tom.val;
     print_anniv conf base (transl conf "the day after tomorrow")
       (transl conf ", it will be") ((conf.today_wd + 2) mod 7)
       aft list_aft.val;
     Wserver.wprint "\n";
     html_p conf;
     let mode () = Wserver.wprint "<input type=hidden name=m value=AD>\n" in
     propose_months conf mode;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value match_mar_dates conf base cpl d1 d2 =
  (* to programmer: dont factorize "age_autorise": it is slow *)
  if d1.day == d2.day && d1.month == d2.month then
    age_autorise conf base (poi base cpl.father) &&
    age_autorise conf base (poi base cpl.mother)
  else if d1.day == 29 && d1.month == 2 && d2.day == 1 && d2.month = 3 &&
       not (leap_year d2.year)
    then
    age_autorise conf base (poi base cpl.father) &&
    age_autorise conf base (poi base cpl.mother)
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
(*
  let _ = base.data.families.array () in
*)
  do header conf title;
     print_link_to_welcome conf True;
     for i = 0 to base.data.families.len - 1 do
       let fam = base.data.families.get i in
       if is_deleted_family fam then ()
       else
         match Adef.od_of_codate fam.marriage with
         [ Some (Dgreg d _) when d.day <> 0 && d.month <> 0 && d.prec = Sure ->
             let cpl = base.data.couples.get i in
             if match_mar_dates conf base cpl d conf.today then
               list_tod.val := [(cpl, d.year) :: list_tod.val]
             else if match_mar_dates conf base cpl d tom then
               list_tom.val := [(cpl, d.year) :: list_tom.val]
             else if match_mar_dates conf base cpl d aft then
               list_aft.val := [(cpl, d.year) :: list_aft.val]
             else ()
         | _ -> () ];
     done;
     List.iter
       (fun xx -> xx.val := Sort.list (fun (_, y1) (_, y2) -> y1 <= y2) xx.val)
       [list_tod; list_tom; list_aft];
     print_marriage_day conf base (transl conf "today")
      (transl conf ", it is") conf.today_wd conf.today list_tod.val;
     print_marriage_day conf base (transl conf "tomorrow")
       (transl conf ", it will be") ((conf.today_wd + 1) mod 7)
       tom list_tom.val;
     print_marriage_day conf base (transl conf "the day after tomorrow")
       (transl conf ", it will be") ((conf.today_wd + 2) mod 7)
       aft list_aft.val;
     Wserver.wprint "\n";
     html_p conf;
     let mode () = Wserver.wprint "<input type=hidden name=m value=AM>\n" in
     propose_months conf mode;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;
