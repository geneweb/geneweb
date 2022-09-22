(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

type date_event =
    DeBirth
  | DeDeath of death_reason

let print_anniversary_day conf base dead_people liste =
  Output.print_sstring conf "<ul>";
  List.iter begin fun (p, a, date_event, txt_of) ->
    let is = index_of_sex (get_sex p) in
    Output.print_sstring conf "<li>";
    Output.print_string conf (txt_of conf base p);
    if not dead_people then begin
      Output.print_sstring conf " <em>" ;
      Output.print_sstring conf (string_of_int a) ;
      Output.print_sstring conf "</em>" ;
    end else begin
      Output.print_sstring conf ", <em>" ;
      begin Output.print_sstring conf @@ match date_event with
        | DeBirth -> transl_nth conf "born" is
        | DeDeath Unspecified -> transl_nth conf "died" is
        | DeDeath Killed -> transl_nth conf "killed (in action)" is
        | DeDeath Murdered -> transl_nth conf "murdered" is
        | DeDeath Executed ->
          transl_nth conf "executed (legally killed)" is
        | DeDeath Disappeared -> transl_nth conf "disappeared" is
      end ;
      Output.print_sstring conf " " ;
      Output.print_sstring conf (transl conf "in (year)") ;
      Output.print_sstring conf " " ;
      Output.print_sstring conf (string_of_int a) ;
      Output.print_sstring conf "</em>" ;
    end ;
    Output.print_sstring conf "</li>"
  end liste;
  Output.print_sstring conf "</ul>"

let gen_print conf base mois f_scan dead_people =
  let tab = Array.make 31 [] in
  let title _ =
    let lab =
      if dead_people then transl conf "anniversaries"
      else transl conf "birthdays"
    in
    Output.printf conf "%s %s" (Utf8.capitalize_fst lab)
      (Util.translate_eval (transl_nth conf "(month)" (mois - 1)))
  in
  begin try
    while true do
      let (p, txt_of) = f_scan () in
      if not dead_people then
        match Adef.od_of_cdate (get_birth p), get_death p with
          Some (Dgreg (d, _)), (NotDead | DontKnowIfDead) ->
            if d.prec = Sure && d.day <> 0 && d.month <> 0 &&
               d.month = mois && d.delta = 0
            then
              if authorized_age conf base p then
                let j = d.day in
                tab.(pred j) <- (p, d.year, DeBirth, txt_of) :: tab.(pred j)
        | _ -> ()
      else
        match get_death p with
          NotDead | DontKnowIfDead -> ()
        | _ ->
            begin match Adef.od_of_cdate (get_birth p) with
              Some (Dgreg (dt, _)) ->
                if dt.prec = Sure && dt.day <> 0 && dt.month <> 0 &&
                   dt.month = mois && dt.delta = 0
                then
                  if authorized_age conf base p then
                    let j = dt.day in
                    tab.(pred j) <-
                      (p, dt.year, DeBirth, txt_of) :: tab.(pred j)
            | _ -> ()
            end;
            match get_death p with
              Death (dr, d) ->
                begin match Adef.date_of_cdate d with
                  Dgreg (dt, _) ->
                    if dt.prec = Sure && dt.day <> 0 && dt.month <> 0 &&
                       dt.month = mois && dt.delta = 0
                    then
                      if authorized_age conf base p then
                        let j = dt.day in
                        let a = dt.year in
                        tab.(pred j) <-
                          (p, a, DeDeath dr, txt_of) :: tab.(pred j)
                | _ -> ()
                end
            | _ -> ()
    done
  with Not_found -> ()
  end;
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  if Array.for_all ((=) []) tab then
    begin
      Output.print_sstring conf "<p>\n";
      Output.printf conf "%s.\n" (Utf8.capitalize_fst (transl conf "no anniversary"));
      Output.print_sstring conf "</p>\n"
    end;
  Output.print_sstring conf "<ul>\n";
  for j = 1 to 31 do
    if tab.(pred j) <> [] then
      begin
        Output.print_sstring conf "<li>\n";
        Output.printf conf "%d\n" j;
        begin let liste =
          List.sort (fun (_, a1, _, _) (_, a2, _, _) -> compare a1 a2)
            tab.(pred j)
        in
          print_anniversary_day conf base dead_people liste
        end;
        Output.print_sstring conf "</li>\n"
      end
  done;
  Output.print_sstring conf "</ul>\n";
  Hutil.trailer conf

let print_anniversary_list conf base dead_people dt liste =
  let a_ref = dt.year in
  Output.print_sstring conf "<ul>\n";
  List.iter
    (fun (p, a, date_event, txt_of) ->
       Output.print_sstring conf "<li>";
       if dead_people then begin
         Output.print_sstring conf "<em>";
         begin match date_event with
           | DeBirth -> Output.print_sstring conf (transl conf "birth")
           | DeDeath _ -> Output.print_sstring conf (transl conf "death")
         end;
         Output.print_sstring conf "</em> -&gt; ";
         Output.print_string conf (txt_of conf base p);
         Output.print_sstring conf " <em>" ;
         Output.print_sstring conf (transl conf "in (year)") ;
         Output.print_sstring conf " " ;
         Output.print_sstring conf (string_of_int a );
         Output.print_sstring conf " (";
         Output.print_sstring conf
           (Printf.sprintf (ftransl conf "%d years ago") (conf.today.year - a) );
         Output.print_sstring conf ")</em>"
       end else begin
         Output.print_string conf (txt_of conf base p);
         match get_death p with
         | NotDead ->
           Output.print_sstring conf " <em>";
           begin match a_ref - a with
             | 0 -> Output.print_sstring conf (transl conf "birth")
             | 1 -> Output.print_sstring conf (transl conf "one year old")
             | n ->
               Output.print_sstring conf (string_of_int n) ;
               Output.print_sstring conf " " ;
               Output.print_sstring conf (transl conf "years old") ;
           end ;
           Output.print_sstring conf "</em>"
         | _ -> ()
       end ;
       Output.print_sstring conf "</li>")
    liste;
  Output.print_sstring conf "</ul>"

let f_scan conf base =
  let next = Gwdb.Collection.iterator (Gwdb.ipers base) in
  fun () -> match next () with
    | Some i -> (pget conf base i, referenced_person_title_text)
    | None -> raise Not_found

let print_birth conf base mois =
  gen_print conf base mois (f_scan conf base) false
let print_dead conf base mois =
  gen_print conf base mois (f_scan conf base) true

let print_birth_day conf base day_name fphrase wd dt list =
  match list with
  | [] ->
    Output.print_sstring conf "<p>";
    Output.print_sstring conf (Utf8.capitalize_fst (transl conf "no birthday")) ;
    Output.print_sstring conf " " ;
    Output.print_string conf day_name ;
    Output.print_sstring conf ".</p>"
  | _ ->
    Output.print_sstring conf "<p>\n";
    let txt =
      (transl_nth conf "(week day)" wd ^ " " ^ DateDisplay.code_dmy conf dt)
      |> transl_decline conf "on (weekday day month year)"
      |> Adef.safe
    in
    Output.printf conf fphrase
      (Utf8.capitalize_fst (day_name : Adef.safe_string :> string)
       ^<^ ",\n" ^<^ std_color conf ("<b>" ^<^ txt ^>^ "</b>")
       : Adef.safe_string :> string)
      (transl conf "the birthday") ;
    Output.print_sstring conf "...</p>";
    print_anniversary_list conf base false dt list

let propose_months conf mode =
  begin_centered conf;
  Output.print_sstring conf "<span>";
  transl conf "select a month to see all the anniversaries"
  |> Utf8.capitalize_fst
  |> Output.print_sstring conf ;
  Output.print_sstring conf "</span>";
  Output.print_sstring conf {|<table border="|} ;
  Output.print_sstring conf (string_of_int conf.border) ;
  Output.print_sstring conf {|"><tr><td>|};
  Output.print_sstring conf {|<form class="form-inline" method="get" action="|} ;
  Output.print_sstring conf conf.command ;
  Output.print_sstring conf {|"><p>|} ;
  Util.hidden_env conf ;
  mode () ;
  Output.print_sstring conf {|<select class="form-control form-control-lg" name="v">|} ;
  for i = 1 to 12 do
    Output.print_sstring conf {|<option value="|} ;
    Output.print_sstring conf (string_of_int i) ;
    Output.print_sstring conf {|"|} ;
    Output.print_sstring conf (if i = conf.today.month then {| selected="selected">|} else ">");
    transl_nth conf "(month)" (i - 1)
    |> Util.translate_eval
    |> Utf8.capitalize_fst
    |> Output.print_sstring conf ;
    Output.print_sstring conf "</option>"
  done;
  Output.print_sstring conf "</select>";
  Output.print_sstring conf {|<button type="submit" class="btn btn-secondary btn-lg">|};
  transl_nth conf "validate/delete" 0
  |> Utf8.capitalize_fst
  |> Output.print_sstring conf ;
  Output.print_sstring conf "</button></p></form></td></tr></table>";
  end_centered conf

let day_after d =
  let (day, r) =
    if d.day >= Date.nb_days_in_month d.month d.year then 1, 1
    else succ d.day, 0
  in
  let (month, r) = if d.month + r > 12 then 1, 1 else d.month + r, 0 in
  let year = d.year + r in
  {day = day; month = month; year = year; prec = Sure; delta = 0}

let print_anniv conf base day_name fphrase wd dt = function
  | [] ->
    Output.print_sstring conf "<p>";
    transl conf "no anniversary"
    |> Utf8.capitalize_fst
    |> Output.print_sstring conf ;
    Output.print_sstring conf " " ;
    Output.print_string conf day_name ;
    Output.print_sstring conf ".</p>"
  | list ->
    Output.print_sstring conf "<p>";
    let txt =
      (transl_nth conf "(week day)" wd ^ " " ^ DateDisplay.code_dmy conf dt)
      |> transl_decline conf "on (weekday day month year)"
      |> Adef.safe
    in
    Output.printf conf fphrase
      (Utf8.capitalize_fst (day_name : Adef.safe_string :> string)
       ^<^ ",\n" ^<^ std_color conf ("<b>" ^<^ txt ^>^ "</b>")
       : Adef.safe_string :> string)
      (transl conf "the anniversary") ;
    Output.print_sstring conf "...</p>";
    print_anniversary_list conf base true dt list

let list_aux conf base list cb =
  Output.print_sstring conf "<ul>";
  List.iter begin fun (fam, year) ->
    Output.print_sstring conf "<li>";
    Output.print_string conf
      (referenced_person_title_text conf base (pget conf base (get_father fam)));
    Output.print_sstring conf " " ;
    Output.print_sstring conf (transl_nth conf "and" 0);
    Output.print_sstring conf " " ;
    Output.print_string conf
      (referenced_person_title_text conf base (pget conf base (get_mother fam)));
    Output.print_sstring conf ", <em>";
    Output.print_sstring conf (transl conf "in (year)") ;
    Output.print_sstring conf " " ;
    Output.print_sstring conf (string_of_int year) ;
    cb conf year ;
    Output.print_sstring conf "</em></li>"
  end list;
  Output.print_sstring conf "</ul>"

let print_marriage conf base month =
  let title _ =
    let lab = transl conf "anniversaries of marriage" in
    Output.printf conf "%s %s" (Utf8.capitalize_fst lab)
      (transl_decline conf "in (month year)"
         (transl_nth conf "(month)" (month - 1)))
  in
  let tab = Array.make 31 [] in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Gwdb.Collection.iter (fun ifam ->
    let fam = foi base ifam in
      match Adef.od_of_cdate (get_marriage fam) with
      | Some (Dgreg ({day = d; month = m; year = y; prec = Sure}, _))
        when d <> 0 && m <> 0 ->
        let father = pget conf base (get_father fam) in
        let mother = pget conf base (get_mother fam) in
        if m = month && authorized_age conf base father
           && not (is_hidden father) && authorized_age conf base mother
           && not (is_hidden mother)
        then
          tab.(pred d) <- (fam, y) :: tab.(pred d)
      | _ -> ()
    ) (Gwdb.ifams base) ;
  Output.print_sstring conf "<ul>";
  for i = 1 to 31 do
    match tab.(i-1) with
    | [] -> ()
    | list ->
      let list = List.sort (fun (_, y1) (_, y2) -> compare y1 y2) list in
      Output.print_sstring conf " <li>" ;
      Output.print_sstring conf (string_of_int i);
      list_aux conf base list (fun _ _ -> ()) ;
      Output.print_sstring conf " </li>" ;
  done;
  Output.print_sstring conf "</ul>";
  Hutil.trailer conf

let print_anniversaries_of_marriage conf base list =
  list_aux conf base list begin fun conf year ->
    Output.print_sstring conf " (" ;
    Printf.sprintf (ftransl conf "%d years ago") (conf.today.year - year)
    |> Output.print_sstring conf ;
    Output.print_sstring conf ")" ;
  end

let print_marriage_day conf base day_name fphrase wd dt = function
  | [] ->
    Output.print_sstring conf "<p>";
    transl conf "no anniversary"
    |> Utf8.capitalize_fst
    |> Output.print_sstring conf ;
    Output.print_sstring conf " " ;
    Output.print_string conf day_name;
    Output.print_sstring conf ".</p>"
  | list ->
    Output.print_sstring conf "<p>";
    Output.printf conf fphrase
      (Utf8.capitalize_fst (day_name : Adef.safe_string :> string)
       ^<^ ",\n"
       ^<^ std_color conf
         ("<b>"
          ^ transl_decline conf "on (weekday day month year)"
            (transl_nth conf "(week day)" wd ^ " " ^ DateDisplay.code_dmy conf dt)
          ^ "</b>"
          |> Adef.safe)
       : Adef.safe_string :> string)
      (transl conf "the anniversary of marriage") ;
    Output.print_sstring conf "...</p>" ;
    print_anniversaries_of_marriage conf base list

let match_dates conf base p d1 d2 =
  if d1.day = d2.day && d1.month = d2.month then authorized_age conf base p
  else if d1.day = 29 && d1.month = 2 && d2.day = 1 && d2.month = 3
          && not (Date.leap_year d2.year)
  then authorized_age conf base p
  else false

let gen_print_menu_birth conf base f_scan mode =
  let title _ = transl conf "birthdays" |> Utf8.capitalize_fst |> Output.print_sstring conf in
  let tom = day_after conf.today in
  let aft = day_after tom in
  let list_tod = ref [] in
  let list_tom = ref [] in
  let list_aft = ref [] in
  begin match Util.find_person_in_env conf base "" with
    | Some p ->
      Perso.interp_notempl_with_menu title "perso_header" conf base p;
      Output.print_sstring conf "<h2>";
      title false;
      Output.print_sstring conf "</h2>"
    | None -> Hutil.header conf title
  end ;
  Hutil.print_link_to_welcome conf true;
  begin try
    while true do
      let (p, txt_of) = f_scan () in
      match Adef.od_of_cdate (get_birth p), get_death p with
        Some (Dgreg (d, _)), (NotDead | DontKnowIfDead) ->
          if d.prec = Sure && d.day <> 0 && d.month <> 0 then
            if match_dates conf base p d conf.today then
              list_tod := (p, d.year, DeBirth, txt_of) :: !list_tod
            else if match_dates conf base p d tom then
              list_tom := (p, d.year, DeBirth, txt_of) :: !list_tom
            else if match_dates conf base p d aft then
              list_aft := (p, d.year, DeBirth, txt_of) :: !list_aft
      | _ -> ()
    done
  with Not_found -> ()
  end;
  List.iter begin fun xx ->
    xx := List.sort (fun (_, a1, _, _) (_, a2, _, _) -> compare a1 a2) !xx
  end [list_tod; list_tom; list_aft];
  print_birth_day conf base (transl conf "today" |> Adef.safe)
    (ftransl conf "%s, it is %s of") conf.today_wd conf.today !list_tod;
  print_birth_day conf base (transl conf "tomorrow" |> Adef.safe)
    (ftransl conf "%s, it will be %s of") ((conf.today_wd + 1) mod 7) tom
    !list_tom;
  print_birth_day conf base (transl conf "the day after tomorrow" |> Adef.safe)
    (ftransl conf "%s, it will be %s of") ((conf.today_wd + 2) mod 7) aft
    !list_aft;
  Output.print_sstring conf " ";
  propose_months conf mode;
  Output.print_sstring conf " ";
  Hutil.trailer conf

let print_menu_birth conf base =
  let f_scan =
    let next = Gwdb.Collection.iterator (Gwdb.ipers base) in
    fun () -> match next () with
      | Some i -> (pget conf base i, referenced_person_title_text)
      | None -> raise Not_found
  in
  let mode () =
    Output.print_sstring conf "<input type=\"hidden\" name=\"m\" value=\"AN\">\n"
  in
  gen_print_menu_birth conf base f_scan mode

let gen_print_menu_dead conf base f_scan mode =
  let title _ =
    transl conf "anniversaries of dead people"
    |> Utf8.capitalize_fst
    |> Output.print_sstring conf
  in
  let tom = day_after conf.today in
  let aft = day_after tom in
  let list_tod = ref [] in
  let list_tom = ref [] in
  let list_aft = ref [] in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  begin try
    while true do
      let (p, txt_of) = f_scan () in
      match get_death p with
        NotDead | DontKnowIfDead -> ()
      | _ ->
          begin match Adef.od_of_cdate (get_birth p) with
            Some (Dgreg (d, _)) ->
              if d.prec = Sure && d.day <> 0 && d.month <> 0 then
                if match_dates conf base p d conf.today then
                  list_tod := (p, d.year, DeBirth, txt_of) :: !list_tod
                else if match_dates conf base p d tom then
                  list_tom := (p, d.year, DeBirth, txt_of) :: !list_tom
                else if match_dates conf base p d aft then
                  list_aft := (p, d.year, DeBirth, txt_of) :: !list_aft
          | _ -> ()
          end;
          match get_death p with
            Death (dr, d) ->
              begin match Adef.date_of_cdate d with
                Dgreg (d, _) ->
                  if d.prec = Sure && d.day <> 0 && d.month <> 0 then
                    if match_dates conf base p d conf.today then
                      list_tod := (p, d.year, DeDeath dr, txt_of) :: !list_tod
                    else if match_dates conf base p d tom then
                      list_tom := (p, d.year, DeDeath dr, txt_of) :: !list_tom
                    else if match_dates conf base p d aft then
                      list_aft := (p, d.year, DeDeath dr, txt_of) :: !list_aft
              | _ -> ()
              end
          | _ -> ()
    done
  with Not_found -> ()
  end;
  List.iter begin fun xx ->
    xx := List.sort (fun (_, a1, _, _) (_, a2, _, _) -> compare a1 a2) !xx
  end [list_tod; list_tom; list_aft];
  print_anniv conf base (transl conf "today" |> Adef.safe)
    (ftransl conf "%s, it is %s of")
    conf.today_wd conf.today !list_tod;
  print_anniv conf base (transl conf "tomorrow" |> Adef.safe)
    (ftransl conf "%s, it will be %s of") ((conf.today_wd + 1) mod 7) tom
    !list_tom;
  print_anniv conf base (transl conf "the day after tomorrow" |> Adef.safe)
    (ftransl conf "%s, it will be %s of") ((conf.today_wd + 2) mod 7) aft
    !list_aft;
  Output.print_sstring conf "\n";
  propose_months conf mode;
  Output.print_sstring conf "\n";
  Hutil.trailer conf

let print_menu_dead conf base =
  let f_scan =
    let next = Gwdb.Collection.iterator (Gwdb.ipers base) in
    fun () -> match next () with
      | Some i -> (pget conf base i, referenced_person_title_text)
      | None -> raise Not_found
  in
  gen_print_menu_dead conf base f_scan (fun () -> Util.hidden_input conf "m" @@ Adef.encoded "AD")

let match_mar_dates conf base cpl d1 d2 =
  if d1.day = d2.day && d1.month = d2.month then
    authorized_age conf base (pget conf base (get_father cpl)) &&
    authorized_age conf base (pget conf base (get_mother cpl))
  else if
    d1.day = 29 && d1.month = 2 && d2.day = 1 && d2.month = 3 &&
    not (Date.leap_year d2.year)
  then
    authorized_age conf base (pget conf base (get_father cpl)) &&
    authorized_age conf base (pget conf base (get_mother cpl))
  else false

let print_menu_marriage conf base =
  let title _ =
    transl conf "anniversaries of marriage"
    |> Utf8.capitalize_fst
    |> Output.print_sstring conf
  in
  let tom = day_after conf.today in
  let aft = day_after tom in
  let list_tod = ref [] in
  let list_tom = ref [] in
  let list_aft = ref [] in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Gwdb.Collection.iter (fun ifam ->
    let fam = foi base ifam in
      match Adef.od_of_cdate (get_marriage fam), get_divorce fam with
      | Some (Dgreg (d, _)), NotDivorced
        when d.day <> 0 && d.month <> 0 && d.prec = Sure ->
          let update_list cpl =
            if match_mar_dates conf base cpl d conf.today then
              list_tod := (cpl, d.year) :: !list_tod
            else if match_mar_dates conf base cpl d tom then
              list_tom := (cpl, d.year) :: !list_tom
            else if match_mar_dates conf base cpl d aft then
              list_aft := (cpl, d.year) :: !list_aft
          in
          if conf.use_restrict then
            let father = pget conf base (get_father fam) in
            let mother = pget conf base (get_mother fam) in
            (if not (is_hidden father) && not (is_hidden mother) then
               update_list fam)
          else update_list fam
      | _ -> ()
    ) (Gwdb.ifams base) ;
  List.iter
    (fun xx -> xx := List.sort (fun (_, y1) (_, y2) -> compare y1 y2) !xx)
    [list_tod; list_tom; list_aft];
  print_marriage_day conf base (transl conf "today" |> Adef.safe)
    (ftransl conf "%s, it is %s of") conf.today_wd conf.today !list_tod;
  print_marriage_day conf base (transl conf "tomorrow" |> Adef.safe)
    (ftransl conf "%s, it will be %s of") ((conf.today_wd + 1) mod 7) tom
    !list_tom;
  print_marriage_day conf base (transl conf "the day after tomorrow" |> Adef.safe)
    (ftransl conf "%s, it will be %s of") ((conf.today_wd + 2) mod 7) aft
    !list_aft;
  Output.print_sstring conf "\n";
  propose_months conf (fun () -> Util.hidden_input conf "m" @@ Adef.encoded "AM");
  Output.print_sstring conf "\n";
  Hutil.trailer conf

(* template *)
type 'a env =
    Vother of 'a

let get_vother =
  function
    Vother x -> Some x

let set_vother x = Vother x

let print_anniversaries conf =
  if p_getenv conf.env "old" = Some "on" then ()
  else
    Hutil.interp conf "annivmenu"
      {Templ.eval_var = (fun _ -> raise Not_found);
       Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
       Templ.eval_predefined_apply = (fun _ -> raise Not_found);
       Templ.get_vother = get_vother; Templ.set_vother = set_vother;
       Templ.print_foreach = fun _ -> raise Not_found}
      [] ()
