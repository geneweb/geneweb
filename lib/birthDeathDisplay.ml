(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util
open BirthDeath

let month_txt conf d cal =
  let d = DateDisplay.string_of_date conf (Dgreg ({ d with day = 0 }, cal)) in
  (d : Adef.safe_string :> string) |> Utf8.capitalize_fst |> Adef.safe

let print_birth conf base =
  let list, len =
    select_person conf base (fun p -> Date.od_of_cdate (get_birth p)) false
  in
  let title _ =
    Output.printf conf (fcapitale (ftransl conf "the latest %d births")) len
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<ul>\n";
  ignore
  @@ List.fold_left
       (fun (last_month_txt, was_future) (p, d, cal) ->
         let month_txt = month_txt conf d cal in
         let future = Date.compare_dmy d conf.today = 1 in
         if (not future) && was_future then (
           Output.print_sstring conf "</li></ul></li></ul><p><ul><li>";
           Output.print_string conf month_txt;
           Output.print_sstring conf "<ul>")
         else if month_txt <> last_month_txt then (
           if (last_month_txt :> string) <> "" then
             Output.print_sstring conf "</ul></li>";
           Output.print_sstring conf "<li>";
           Output.print_string conf month_txt;
           Output.print_sstring conf "<ul>");
         Output.print_sstring conf "<li><b>";
         Output.print_string conf
           (NameDisplay.referenced_person_text conf base p);
         Output.print_sstring conf "</b>, ";
         if future then (
           Output.print_sstring conf "<em>";
           Output.print_string conf
             (DateDisplay.string_of_date conf (Dgreg (d, cal)));
           Output.print_sstring conf "</em>.")
         else (
           Output.print_sstring conf
             (transl_nth conf "born" (index_of_sex (get_sex p)));
           Output.print_sstring conf " <em>";
           Output.print_string conf
             (DateDisplay.string_of_ondate conf (Dgreg (d, cal)));
           Output.print_sstring conf "</em>.");
         Output.print_sstring conf "</li>";
         (month_txt, future))
       (Adef.safe "", false)
       list;
  Output.print_sstring conf "</ul></li></ul>";
  Hutil.trailer conf

let print_death conf base =
  let list, len = select_person conf base death_date false in
  let title _ =
    Printf.sprintf
      (fcapitale (ftransl conf "the latest %t deaths"))
      (fun _ -> string_of_int len)
    |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  if list <> [] then (
    Output.print_sstring conf "<ul>";
    let _, ages_sum, ages_nb =
      List.fold_left
        (fun (last_month_txt, ages_sum, ages_nb) (p, d, cal) ->
          let month_txt = month_txt conf d cal in
          if month_txt <> last_month_txt then (
            if (last_month_txt :> string) <> "" then
              Output.print_sstring conf "</ul>\n</li>\n";
            Output.print_sstring conf "<li>";
            Output.print_string conf month_txt;
            Output.print_sstring conf "<ul>");
          let age, ages_sum, ages_nb =
            let sure d = d.Date.prec = Sure in
            match Date.cdate_to_dmy_opt (get_birth p) with
            | None -> (None, ages_sum, ages_nb)
            | Some d1 ->
                if sure d1 && sure d && d1 <> d then
                  let a = Date.time_elapsed d1 d in
                  let ages_sum =
                    match get_sex p with
                    | Male -> (fst ages_sum + a.year, snd ages_sum)
                    | Female -> (fst ages_sum, snd ages_sum + a.year)
                    | Neuter -> ages_sum
                  in
                  let ages_nb =
                    match get_sex p with
                    | Male -> (fst ages_nb + 1, snd ages_nb)
                    | Female -> (fst ages_nb, snd ages_nb + 1)
                    | Neuter -> ages_nb
                  in
                  (Some a, ages_sum, ages_nb)
                else (None, ages_sum, ages_nb)
          in
          Output.print_sstring conf "<li><b>";
          Output.print_string conf
            (NameDisplay.referenced_person_text conf base p);
          Output.print_sstring conf "</b>, ";
          Output.print_sstring conf
            (transl_nth conf "died" (index_of_sex (get_sex p)));
          Output.print_sstring conf " <em>";
          Output.print_string conf
            (DateDisplay.string_of_ondate conf (Dgreg (d, cal)));
          Output.print_sstring conf "</em>";
          Option.iter
            (fun a ->
              Output.print_sstring conf " <em>(";
              Output.print_string conf (DateDisplay.string_of_age conf a);
              Output.print_sstring conf ")</em>")
            age;
          Output.print_sstring conf "</li>";
          (month_txt, ages_sum, ages_nb))
        (Adef.safe "", (0, 0), (0, 0))
        list
    in
    Output.print_sstring conf "</ul></li></ul>";
    let aux sex nb sum =
      if nb >= 3 then (
        transl conf "average age at death"
        |> Utf8.capitalize_fst |> Output.print_sstring conf;
        Output.print_sstring conf " (";
        Output.print_sstring conf (transl_nth conf "M/F" sex);
        Output.print_sstring conf ") : ";
        Output.print_string conf
          (DateDisplay.string_of_age conf
             { day = 0; month = 0; year = sum / nb; delta = 0; prec = Sure });
        Output.print_sstring conf "<br>")
    in
    aux 0 (fst ages_nb) (fst ages_sum);
    aux 1 (snd ages_nb) (snd ages_sum);
    Output.print_sstring conf
      {|<br><div align="center"><hr width="50%"></div><br>|};
    let aux name def =
      string_of_int
      @@
      match p_getenv conf.env name with
      | Some s -> int_of_string s
      | None -> def
    in
    let by = aux "by" conf.today.year in
    let bm = aux "bm" conf.today.month in
    let bd = aux "bd" conf.today.day in
    Output.print_sstring conf {|<form method="get" action="|};
    Output.print_sstring conf conf.command;
    Output.print_sstring conf {|"><p>|};
    Util.hidden_env conf;
    Util.hidden_input conf "m" (Adef.encoded "LD");
    Output.print_sstring conf
    @@ Printf.sprintf
         (fcapitale (ftransl conf "the latest %t deaths"))
         (fun _ ->
           {|<input name="k" value="|} ^ string_of_int len
           ^ {|" size="4" maxlength="4">|});
    Output.print_sstring conf "\n... (";
    Output.print_sstring conf (transl conf "before");
    Output.print_sstring conf "...\n";
    let aux name value size =
      Output.print_sstring conf {|<input name="|};
      Output.print_sstring conf name;
      Output.print_sstring conf {|" value="|};
      Output.print_sstring conf value;
      Output.print_sstring conf {|" size="|};
      Output.print_sstring conf size;
      Output.print_sstring conf {|" maxlength="|};
      Output.print_sstring conf size;
      Output.print_sstring conf {|">|}
    in
    aux "by" by "4";
    aux "bm" bm "2";
    aux "bd" bd "2";
    Output.print_sstring conf ")";
    Output.print_sstring conf
      {|<button type="submit" class="btn btn-secondary btn-lg">|};
    transl_nth conf "validate/delete" 0
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf "</button></p></form>");
  Hutil.trailer conf

let print_oldest_alive conf base =
  let limit = match p_getint conf.env "lim" with Some x -> x | _ -> 0 in
  let get_oldest_alive p =
    match get_death p with
    | NotDead -> Date.od_of_cdate (get_birth p)
    | DontKnowIfDead when limit > 0 -> (
        match Date.od_of_cdate (get_birth p) with
        | Some (Dgreg (d, _)) as x when conf.today.year - d.year <= limit -> x
        | Some _ | None -> None)
    | Death _ | DontKnowIfDead | DeadYoung | DeadDontKnowWhen | OfCourseDead ->
        None
  in
  let list, len = select_person conf base get_oldest_alive true in
  let title _ =
    Printf.sprintf
      (fcapitale (ftransl conf "the %d oldest perhaps still alive"))
      len
    |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<ul>\n";
  List.iter
    (fun (p, d, cal) ->
      Output.print_sstring conf "<li><b>";
      Output.print_string conf (NameDisplay.referenced_person_text conf base p);
      Output.print_sstring conf "</b>, ";
      Output.print_sstring conf
        (transl_nth conf "born" (index_of_sex (get_sex p)));
      Output.print_sstring conf " <em>";
      Output.print_string conf
        (DateDisplay.string_of_ondate conf (Dgreg (d, cal)));
      Output.print_sstring conf "</em>";
      if get_death p = NotDead && d.prec = Sure then (
        let a = Date.time_elapsed d conf.today in
        Output.print_sstring conf " <em>(";
        Output.print_string conf (DateDisplay.string_of_age conf a);
        Output.print_sstring conf ")</em>");
      Output.print_sstring conf ".</li>")
    list;
  Output.print_sstring conf "</ul>";
  Hutil.trailer conf

let print_longest_lived conf base =
  let get_longest p =
    if Util.authorized_age conf base p then
      match (Date.cdate_to_dmy_opt (get_birth p), get_death p) with
      | Some bd, Death (_, cd) -> (
          match Date.cdate_to_dmy_opt cd with
          | None -> None
          | Some dd -> Some (Date.Dgreg (Date.time_elapsed bd dd, Dgregorian)))
      | _ -> None
    else None
  in
  let list, len = select_person conf base get_longest false in
  let title _ =
    Printf.sprintf (fcapitale (ftransl conf "the %d who lived the longest")) len
    |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<ul>";
  List.iter
    (fun (p, d, _) ->
      Output.print_sstring conf "<li><strong>";
      Output.print_string conf (NameDisplay.referenced_person_text conf base p);
      Output.print_sstring conf "</strong>";
      Output.print_string conf (DateDisplay.short_dates_text conf base p);
      Output.print_sstring conf " (";
      Output.print_sstring conf (string_of_int d.Date.year);
      Output.print_sstring conf " ";
      Output.print_sstring conf (transl conf "years old");
      Output.print_sstring conf ")";
      Output.print_sstring conf ".";
      Output.print_sstring conf "</li>")
    list;
  Output.print_sstring conf "</ul>";
  Hutil.trailer conf

let print_marr_or_eng conf base title list =
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<ul>\n";
  ignore
  @@ List.fold_left
       (fun (last_month_txt, was_future) (fam, d, cal) ->
         let month_txt = month_txt conf d cal in
         let future = Date.compare_dmy d conf.today > 0 in
         if (not future) && was_future then (
           Output.print_sstring conf "</ul></li></ul><ul><li>";
           Output.print_string conf month_txt;
           Output.print_sstring conf "<ul>")
         else if month_txt <> last_month_txt then (
           if (last_month_txt :> string) = "" then
             Output.print_sstring conf "</ul></li>";
           Output.print_sstring conf "<li>";
           Output.print_string conf month_txt;
           Output.print_sstring conf "<ul>");
         Output.print_sstring conf "<li><b>";
         Output.print_string conf
           (NameDisplay.referenced_person_text conf base
              (pget conf base (get_father fam)));
         Output.print_sstring conf "</b> ";
         Output.print_sstring conf (transl_nth conf "and" 0);
         Output.print_sstring conf " <b>";
         Output.print_string conf
           (NameDisplay.referenced_person_text conf base
              (pget conf base (get_mother fam)));
         Output.print_sstring conf "</b>, ";
         if future then (
           Output.print_sstring conf "<em>";
           Output.print_string conf
             (DateDisplay.string_of_date conf (Dgreg (d, cal)));
           Output.print_sstring conf "</em>")
         else (
           (match get_relation fam with
           | NotMarried | NoSexesCheckNotMarried ->
               Output.print_sstring conf
               @@ transl_nth conf "relation/relations" 0
           | Married | NoSexesCheckMarried ->
               Output.print_sstring conf @@ transl conf "married"
           | Engaged -> Output.print_sstring conf @@ transl conf "engaged"
           | MarriageBann | MarriageContract | MarriageLicense | Pacs
           | Residence | NoMention ->
               ());
           Output.print_sstring conf " <em>";
           Output.print_string conf
             (DateDisplay.string_of_ondate conf (Dgreg (d, cal)));
           Output.print_sstring conf "</em>.");
         Output.print_sstring conf "</li>";
         (month_txt, future))
       (Adef.safe "", false)
       list;
  Output.print_sstring conf "</ul></li></ul>";
  Hutil.trailer conf

let print_marriage conf base =
  let list, len =
    select_family conf base
      (fun fam ->
        let rel = get_relation fam in
        if rel = Married || rel = NoSexesCheckMarried then
          Date.od_of_cdate (get_marriage fam)
        else None)
      false
  in
  let title _ =
    Printf.sprintf (fcapitale (ftransl conf "the latest %d marriages")) len
    |> Output.print_sstring conf
  in
  print_marr_or_eng conf base title list

let print_oldest_engagements conf base =
  let list, len =
    select_family conf base
      (fun fam ->
        if get_relation fam = Engaged then
          let husb = pget conf base (get_father fam) in
          let wife = pget conf base (get_mother fam) in
          match (get_death husb, get_death wife) with
          | (NotDead | DontKnowIfDead), (NotDead | DontKnowIfDead) ->
              Date.od_of_cdate (get_marriage fam)
          | _ -> None
        else None)
      true
  in
  let title _ =
    Printf.sprintf
      (fcapitale
         (ftransl conf "the %d oldest couples perhaps still alive and engaged"))
      len
    |> Output.print_sstring conf
  in
  print_marr_or_eng conf base title list

let old_print_statistics conf =
  let title _ =
    transl conf "statistics" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  let n =
    try int_of_string (List.assoc "latest_event" conf.base_env)
    with Not_found | Failure _ -> 20
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<ul>";
  let aux m label =
    Output.print_sstring conf {|<li><a href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf {|m=|};
    Output.print_sstring conf m;
    Output.print_sstring conf {|&k=|};
    Output.print_sstring conf (string_of_int n);
    Output.print_sstring conf {|">|};
    Output.print_sstring conf (Printf.sprintf (ftransl conf label) n);
    Output.print_sstring conf {|</a></li>|}
  in
  if conf.wizard || conf.friend then (
    aux "LB" "the latest %d births";
    aux "LD" "the latest %d deaths";
    (* FIXME *)
    aux "LM" "the latest %d marriages";
    aux "OE" "the %d oldest couples perhaps still alive and engaged";
    aux "OA" "the %d oldest perhaps still alive");
  aux "LL" "the %d who lived the longest";
  Output.print_sstring conf "</ul>\n";
  Hutil.trailer conf

(* *)

type 'a env = Vother of 'a

let get_vother = function Vother x -> Some x
let set_vother x = Vother x

let print_statistics conf =
  if p_getenv conf.env "old" = Some "on" then old_print_statistics conf
  else
    Hutil.interp conf "stats"
      {
        Templ.eval_var = (fun _ -> raise Not_found);
        Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
        Templ.eval_predefined_apply = (fun _ -> raise Not_found);
        Templ.get_vother;
        Templ.set_vother;
        Templ.print_foreach = (fun _ -> raise Not_found);
      }
      [] ()

let print_population_pyramid conf base =
  let interval =
    match p_getint conf.env "int" with Some i -> max 1 i | None -> 5
  in
  let limit = match p_getint conf.env "lim" with Some x -> x | _ -> 0 in
  let at_date =
    match p_getint conf.env "y" with
    | None -> conf.today
    | Some i -> { day = 31; month = 12; year = i; prec = Sure; delta = 0 }
  in
  let nb_intervals = 150 / interval in
  let men, wom =
    make_population_pyramid ~nb_intervals ~interval ~limit ~at_date conf base
  in
  let at_year = at_date.year in
  let string_of_nb n =
    Mutil.string_of_int_sep (transl conf "(thousand separator)") n
  in
  let title _ =
    transl conf "population pyramid"
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf " (";
    Output.print_sstring conf (string_of_int at_year);
    Output.print_sstring conf ")"
  in
  let print_image doit sex iname =
    Output.print_sstring conf "<td>";
    if doit then (
      Output.print_sstring conf {|<img src="|};
      Output.print_string conf (Image.prefix conf);
      Output.print_sstring conf "/";
      Output.print_string conf iname;
      Output.print_sstring conf {|" alt="|};
      Output.print_sstring conf (transl_nth conf "M/F" sex);
      Output.print_sstring conf {|" title="|};
      Output.print_sstring conf (transl_nth conf "M/F" sex);
      Output.print_sstring conf {|">|})
    else Output.print_sstring conf "&nbsp;";
    Output.print_sstring conf "</td>"
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  let max_hum =
    let max_men = Array.fold_left max 0 men in
    let max_wom = Array.fold_left max 0 wom in
    max 1 (max max_men max_wom)
  in
  let max_size = 70 in
  let band_size n = ((2 * max_size * n) + max_hum) / (2 * max_hum) in
  let first_interv =
    let rec loop i =
      if i <= 0 then 0
      else if men.(i) > 0 || wom.(i) > 0 then i
      else loop (i - 1)
    in
    loop nb_intervals
  in
  Output.print_sstring conf "<div>\n";
  Output.print_sstring conf {|<table id="table_pop_pyr" border="|};
  Output.print_sstring conf (string_of_int conf.border);
  Output.print_sstring conf
    {|" cellspacing="0" cellpadding="0" style="margin:auto">|};
  for i = first_interv downto 0 do
    let nb_men = men.(i) in
    let nb_wom = wom.(i) in
    Output.print_sstring conf "<tr><td class=\"pyramid_year\">";
    Output.print_sstring conf (string_of_int @@ (at_year - (i * interval)));
    Output.print_sstring conf "</td><td>&nbsp;</td>";
    print_image (i = 0) 0 (Adef.safe "male.png");
    Output.print_sstring conf "<td>&nbsp;</td><td align=\"right\">\n";
    Output.printf conf
      {|<table cellspacing="0" cellpadding="0"><tr><td class="pyramid_nb">|};
    if nb_men <> 0 then Output.print_sstring conf (string_of_int nb_men);
    Output.print_sstring conf "&nbsp;</td><td>";
    let aux_img nb img =
      if nb <> 0 then (
        let n = max 1 (band_size nb) in
        Output.print_sstring conf {|<img src="images/|};
        Output.print_string conf img;
        Output.print_sstring conf {|" width="|};
        Output.print_sstring conf (string_of_int @@ (n * 3));
        Output.print_sstring conf {|" height="14">|})
    in
    aux_img nb_men (Adef.encoded "pyr_male.png");
    Output.print_sstring conf {|</td></tr></table></td><td align="center">|};
    if i = nb_intervals then Output.print_sstring conf "&nbsp;"
    else Output.print_sstring conf (string_of_int @@ ((i + 1) * interval));
    Output.print_sstring conf
      {|</td><td align="left"><table cellspacing="0" cellpadding="0"><tr><td>|};
    aux_img nb_wom (Adef.encoded "pyr_female.png");
    Output.print_sstring conf {|</td><td class="pyramid_nb">&nbsp;|};
    if nb_wom <> 0 then Output.print_sstring conf (string_of_int nb_wom);
    Output.print_sstring conf "</td></tr></table></td><td>&nbsp;</td>\n";
    print_image (i = 0) 1 (Adef.safe "female.png");
    Output.print_sstring conf {|<td>&nbsp;</td><td class="pyramid_year">|};
    Output.print_sstring conf (string_of_int @@ (at_year - (i * interval)));
    Output.print_sstring conf "</td></tr>"
  done;
  Output.print_sstring conf "</table>";
  Output.print_sstring conf "</div>";
  let sum_men = Array.fold_left ( + ) 0 men in
  let sum_wom = Array.fold_left ( + ) 0 wom in
  Output.print_sstring conf "<p>";
  transl conf "number of living persons:"
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf " ";
  Output.print_sstring conf (string_of_nb (sum_men + sum_wom));
  Output.print_sstring conf {|</p><p><form method="get" action="|};
  Output.print_string conf (commd conf);
  Output.print_sstring conf {|">|};
  hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "POP_PYR");
  Util.hidden_input conf "int" (Adef.encoded @@ string_of_int interval);
  Util.hidden_input conf "lim" (Adef.encoded @@ string_of_int limit);
  Output.print_sstring conf (transl_nth conf "year/month/day" 0);
  Output.print_sstring conf " ";
  Output.print_sstring conf {|<input name="y" value="|};
  Output.print_sstring conf (string_of_int at_year);
  Output.print_sstring conf {|" size="5"></form></p>|};
  Hutil.trailer conf
