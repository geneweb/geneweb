(* camlp4r ./pa_html.cmo *)
(* $Id: birthday.ml,v 1.3 1998-11-27 20:09:39 ddr Exp $ *)

open Def;
open Config;
open Util;
open Gutil;

type date_event = [ DeBirth | DeDeath of death_reason ];

value afficher_anniversaires_jour conf base dead_people liste =
  do Wserver.wprint "<ul>\n";
     List.iter
       (fun (p, a, date_event) ->
          let is = index_of_sex p.sexe in
          do Wserver.wprint "<li>\n";
             afficher_personne_titre_referencee conf base p;
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

value gen_print conf base mois dead_people =
  let tab = Array.create 31 [] in
  let title _ =
    let lab =
      if dead_people then transl conf "anniversaries"
      else transl conf "birthdays"
    in
    Wserver.wprint "%s %s %s" (capitale lab)
      (transl conf "in (month year)")
      (transl_nth conf "(month)" (mois - 1))
  in
  do for i = 0 to base.persons.len - 1 do
       let p = base.persons.get i in
       if not dead_people then
         match (Adef.od_of_codate p.birth, p.death) with
         [ (Some d, NotDead | DontKnowIfDead) ->
             if d.prec = Sure && d.day <> 0 && d.month <> 0
             && d.month = mois then
               if age_autorise conf base p then
                 let j = d.day in
                 tab.(pred j) := [(p, d.year, DeBirth) :: tab.(pred j)]
               else ()
             else ()
         | _ -> () ]
       else
         match p.death with
         [ NotDead | DontKnowIfDead -> ()
         | _ ->
             do match Adef.od_of_codate p.birth with
                [ Some dt ->
                    if dt.prec = Sure && dt.day <> 0 && dt.month <> 0
                    && dt.month = mois then
                      if age_autorise conf base p then
                        let j = dt.day in
                        tab.(pred j) := [(p, dt.year, DeBirth) :: tab.(pred j)]
                      else ()
                    else ()
                | _ -> () ];
                match p.death with
                [ Death dr d ->
                    let dt = Adef.date_of_cdate d in
                    if dt.prec = Sure && dt.day <> 0 && dt.month <> 0
                    && dt.month = mois then
                      if age_autorise conf base p then
                        let j = dt.day in
                        let a = dt.year in
                        tab.(pred j) := [(p, a, DeDeath dr) :: tab.(pred j)]
                      else ()
                    else ()
                | _ -> () ];
             return () ];
     done;
     header conf title;
     Wserver.wprint "<ul>\n";
     for j = 1 to 31 do
       if tab.(pred j) <> [] then
         do Wserver.wprint "<li> %d\n" j;
            let liste =
              Sort.list (fun (p1, a1, _) (p2, a2, _) -> a1 <= a2) tab.(pred j)
            in
            afficher_anniversaires_jour conf base dead_people liste;
         return ()
       else ();
     done;
     Wserver.wprint "</ul>\n";
     trailer conf;
  return ()
;

value anniversaire_du conf base dead_people jj mm =
  let xx = ref [] in
  do for i = 0 to base.persons.len - 1 do
       let p = base.persons.get i in
       if not dead_people then
         match (Adef.od_of_codate p.birth, p.death) with
         [ (Some d, NotDead | DontKnowIfDead) ->
             if d.prec = Sure && d.day <> 0 && d.month <> 0 then
               if d.day == jj && d.month == mm then
                 if age_autorise conf base p then
                   xx.val := [(p, d.year, DeBirth) :: xx.val]
                 else ()
               else ()
             else ()
         | _ -> () ]
       else
         match p.death with
         [ NotDead | DontKnowIfDead -> ()
         | _ ->
             do match Adef.od_of_codate p.birth with
                [ Some d ->
                    if d.prec = Sure && d.day <> 0 && d.month <> 0 then
                      if d.day == jj && d.month == mm then
                        if age_autorise conf base p then
                          xx.val := [(p, d.year, DeBirth) :: xx.val]
                        else ()
                      else ()
                    else ()
                | _ -> () ];
                match p.death with
                [ Death dr d ->
                    let dt = Adef.date_of_cdate d in
                    if dt.prec = Sure && dt.day <> 0 && dt.month <> 0 then
                      if dt.day == jj && dt.month == mm then
                        if age_autorise conf base p then
                          xx.val := [(p, dt.year, DeDeath dr) :: xx.val]
                        else ()
                      else ()
                    else ()
                | _ -> () ];
             return () ];
     done;
     xx.val := Sort.list (fun (p1, a1, _) (p2, a2, _) -> a1 <= a2) xx.val;
  return xx.val
;

value afficher_liste_anniversaires conf base dead_people a_ref liste =
  do Wserver.wprint "<ul>\n";
     List.iter
       (fun (p, a, date_event) ->
          do Wserver.wprint "<li>\n";
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
                  Wserver.wprint "%s " (transl_nth conf "of" 0);
                  afficher_personne_titre_referencee conf base p;
                  Wserver.wprint "\n<em>%s %d" (transl conf "in (year)") a;
                  Wserver.wprint " (";
                  Wserver.wprint (ftransl conf "%d years ago")
                    (conf.today.year - a);
                  Wserver.wprint ")\n</em>";
               return ()
             else
               do afficher_personne_titre_referencee conf base p;
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

value print conf base mois = gen_print conf base mois False;
value print_dead conf base mois = gen_print conf base mois True;

value print_birth_day conf base day_name verb wd d m y list =
  match list with
  [ [] ->
      Wserver.wprint "\n<p>\n%s %s.\n"
        (capitale (transl conf "no birthday")) day_name
  | _ ->
      let dt = {day = d; month = m; year = y; prec = Sure} in
      do Wserver.wprint "\n<p>\n%s, %s %s%s %s %s:\n"
           (capitale day_name) (transl_nth conf "(week day)" wd)
           (Date.string_of_date conf dt) verb
           (transl conf "the birthday")
           (transl_nth conf "of" 0);
         afficher_liste_anniversaires conf base False y list;
      return () ]
;

value propose_months conf mode =
  tag "form" "method=get action=\"%s\"" conf.command begin
    Srcfile.hidden_env conf;
    Wserver.wprint "\n<input type=hidden name=m value=%s>" mode;
    tag "select" "name=v" begin
      for i = 1 to 12 do
        Wserver.wprint "\n<option value=%d>%s" i
          (capitale (transl_nth conf "(month)" (i - 1)));
      done;
    end;
    Wserver.wprint "\n<input type=submit value=\"Ok\">";
  end
;

value menu_print conf base =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "birthdays")) in
  do header conf title;
     let (tom_d, tom_m, tom_y) =
       lendemain (conf.today.day, conf.today.month, conf.today.year)
     in
     let (aft_d, aft_m, aft_y) = lendemain (tom_d, tom_m, tom_y) in
     let list_today =
       anniversaire_du conf base False conf.today.day conf.today.month
     in
     let list_tom = anniversaire_du conf base False tom_d tom_m in
     let list_aft = anniversaire_du conf base False aft_d aft_m in
     do print_birth_day conf base (transl conf "today") (transl conf ", it is")
          conf.today_wd conf.today.day conf.today.month conf.today.year
          list_today;
        print_birth_day conf base (transl conf "tomorrow")
          (transl conf ", it will be") ((conf.today_wd + 1) mod 7)
          tom_d tom_m tom_y list_tom;
        print_birth_day conf base (transl conf "the day after tomorrow")
          (transl conf ", it will be") ((conf.today_wd + 2) mod 7)
          aft_d aft_m aft_y list_aft;
     return ();
     Wserver.wprint "\n<p>";
     propose_months conf "AN";
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_anniv conf base day_name verb wd d m y list =
  match list with
  [ [] ->
      Wserver.wprint "\n<p>\n%s %s.\n"
        (capitale (transl conf "no anniversary")) day_name
  | _ ->
      let dt = {day = d; month = m; year = y; prec = Sure} in
      do Wserver.wprint "\n<p>\n%s, %s %s%s %s:"
           (capitale day_name) (transl_nth conf "(week day)" wd)
           (Date.string_of_date conf dt) verb
           (transl conf "the anniversary");
         afficher_liste_anniversaires conf base True y list;
      return () ]
;

value menu_print_dead conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "anniversaries of dead"))
  in
  do header conf title;
     let (tom_d, tom_m, tom_y) =
       lendemain (conf.today.day, conf.today.month, conf.today.year)
     in
     let (aft_d, aft_m, aft_y) = lendemain (tom_d, tom_m, tom_y) in
     let list_today =
       anniversaire_du conf base True conf.today.day conf.today.month
     in
     let list_tom = anniversaire_du conf base True tom_d tom_m in
     let list_aft = anniversaire_du conf base True aft_d aft_m in
     do print_anniv conf base (transl conf "today") (transl conf ", it is")
          conf.today_wd conf.today.day conf.today.month conf.today.year
          list_today;
        print_anniv conf base (transl conf "tomorrow")
          (transl conf ", it will be") ((conf.today_wd + 1) mod 7)
          tom_d tom_m tom_y list_tom;
        print_anniv conf base (transl conf "the day after tomorrow")
          (transl conf ", it will be") ((conf.today_wd + 2) mod 7)
          aft_d aft_m aft_y list_aft;
     return ();
     Wserver.wprint "\n<p>";
     propose_months conf "AD";
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_marriage conf base month =
  let title _ =
    let lab = transl conf "anniversaries of marriage" in
    Wserver.wprint "%s %s %s" (capitale lab)
      (transl conf "in (month year)")
      (transl_nth conf "(month)" (month - 1))
  in
  let tab = Array.create 31 [] in
  do header conf title;
     for i = 0 to base.families.len - 1 do
       let fam = base.families.get i in
       if is_deleted_family fam then ()
       else
         match Adef.od_of_codate fam.marriage with
         [ Some {day = d; month = m; year = y; prec = Sure} when
           d <> 0 && m <> 0 ->
             let cpl = base.couples.get i in
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
           do Wserver.wprint "\n<li>\n";
              Wserver.wprint "%d\n<ul>" i;
              List.iter
                (fun (fam, year) ->
                   do Wserver.wprint "\n<li>\n";
                      afficher_personne_titre_referencee conf base
                        (poi base fam.father);
                      Wserver.wprint "\n<br>%s\n" (transl conf "and");
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

value anniversary_of_marriage_of_day conf base dd mm =
  let xx = ref [] in
  do for i = 0 to base.families.len - 1 do
       let fam = base.families.get i in
       if is_deleted_family fam then ()
       else
         match Adef.od_of_codate fam.marriage with
         [ Some {day = d; month = m; year = y; prec = Sure} when
           d <> 0 && m <> 0 ->
             let cpl = base.couples.get i in
             if age_autorise conf base (poi base cpl.father)
             && age_autorise conf base (poi base cpl.mother)
             && d == dd && m == mm then xx.val := [(cpl, y) :: xx.val]
             else ()
         | _ -> () ];
     done;
     xx.val := Sort.list (fun (fam1, y1) (fam2, y2) -> y1 <= y2) xx.val;
  return xx.val
;

value print_anniversaries_of_marriage conf base y list =
  do Wserver.wprint "<ul>";
     List.iter
       (fun (fam, year) ->
          do Wserver.wprint "\n<li>\n";
             afficher_personne_titre_referencee conf base
               (poi base fam.father);
             Wserver.wprint "\n<br>%s\n" (transl conf "and");
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

value print_marriage_day conf base day_name verb wd d m y list =
  match list with
  [ [] ->
      Wserver.wprint "\n<p>\n%s %s.\n"
        (capitale (transl conf "no anniversary")) day_name
  | _ ->
      let dt = {day = d; month = m; year = y; prec = Sure} in
      do Wserver.wprint "\n<p>\n%s, %s %s%s %s %s:\n"
           (capitale day_name) (transl_nth conf "(week day)" wd)
           (Date.string_of_date conf dt) verb
           (transl conf "the anniversary of marriage")
           (transl_nth conf "of" 0);
         print_anniversaries_of_marriage conf base y list;
      return () ]
;

value print_menu_marriage conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "anniversaries of marriage"))
  in
  do header conf title;
     let (tom_d, tom_m, tom_y) =
       lendemain (conf.today.day, conf.today.month, conf.today.year)
     in
     let (aft_d, aft_m, aft_y) = lendemain (tom_d, tom_m, tom_y) in
     let list_today =
       anniversary_of_marriage_of_day conf base conf.today.day conf.today.month
     in
     let list_tomorrow =
       anniversary_of_marriage_of_day conf base tom_d tom_m
     in
     let list_after =
       anniversary_of_marriage_of_day conf base aft_d aft_m
     in
     do print_marriage_day conf base (transl conf "today")
          (transl conf ", it is") conf.today_wd conf.today.day conf.today.month
          conf.today.year list_today;
        print_marriage_day conf base (transl conf "tomorrow")
          (transl conf ", it will be") ((conf.today_wd + 1) mod 7)
          tom_d tom_m tom_y list_tomorrow;
        print_marriage_day conf base (transl conf "the day after tomorrow")
          (transl conf ", it will be") ((conf.today_wd + 2) mod 7)
          aft_d aft_m aft_y list_after;
       return ();
     Wserver.wprint "\n<p>";
     propose_months conf "AM";
     Wserver.wprint "\n";
     trailer conf;
  return ()
;
