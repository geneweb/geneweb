(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
open BirthDeath
module Driver = Geneweb_db.Driver

let month_txt conf d cal =
  let d = DateDisplay.string_of_date conf (Dgreg ({ d with day = 0 }, cal)) in
  (d : Adef.safe_string :> string) |> Utf8.capitalize_fst |> Adef.safe

let list_aux_1 conf d cal last_month_txt was_future =
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
  (month_txt, future)

let print_birth conf base =
  let list, len =
    select_person conf base
      (fun p -> Date.od_of_cdate (Driver.get_birth p))
      false
  in
  let title _ =
    Output.printf conf (fcapitale (ftransl conf "the latest %d births")) len
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul>\n";
  ignore
  @@ List.fold_left
       (fun (last_month_txt, was_future) (p, d, cal) ->
         let month_txt, future =
           list_aux_1 conf d cal last_month_txt was_future
         in
         Output.print_sstring conf "<li><b>";
         Output.print_string conf (referenced_person_text conf base p);
         Output.print_sstring conf "</b>, ";
         if future then (
           Output.print_sstring conf "<em>";
           Output.print_string conf
             (DateDisplay.string_of_date conf (Dgreg (d, cal)));
           Output.print_sstring conf "</em>.")
         else (
           Output.print_sstring conf
             (transl_nth conf "born" (index_of_sex (Driver.get_sex p)));
           Output.print_sstring conf " <em>";
           Output.print_string conf
             (DateDisplay.string_of_ondate conf (Dgreg (d, cal)));
           Output.print_sstring conf "</em>.");
         Output.print_sstring conf "</li>";
         (month_txt, future))
       (Adef.safe "", false)
       list;
  Output.print_sstring conf "</ul></li>";
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
            let sure d = d.prec = Sure in
            match Date.cdate_to_dmy_opt (Driver.get_birth p) with
            | None -> (None, ages_sum, ages_nb)
            | Some d1 ->
                if sure d1 && sure d && d1 <> d then
                  let a = Date.time_elapsed d1 d in
                  let ages_sum =
                    match Driver.get_sex p with
                    | Male -> (fst ages_sum + a.year, snd ages_sum)
                    | Female -> (fst ages_sum, snd ages_sum + a.year)
                    | Neuter -> ages_sum
                  in
                  let ages_nb =
                    match Driver.get_sex p with
                    | Male -> (fst ages_nb + 1, snd ages_nb)
                    | Female -> (fst ages_nb, snd ages_nb + 1)
                    | Neuter -> ages_nb
                  in
                  (Some a, ages_sum, ages_nb)
                else (None, ages_sum, ages_nb)
          in
          Output.print_sstring conf "<li><b>";
          Output.print_string conf (referenced_person_text conf base p);
          Output.print_sstring conf "</b>, ";
          Output.print_sstring conf
            (transl_nth conf "died" (index_of_sex (Driver.get_sex p)));
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
    let by =
      aux "by" (if conf.predictable_mode then 1971 else conf.today.year)
    in
    let bm = aux "bm" (if conf.predictable_mode then 1 else conf.today.month) in
    let bd = aux "bd" (if conf.predictable_mode then 1 else conf.today.day) in
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
      {|<button type="submit" class="btn btn-primary btn-lg">|};
    transl_nth conf "validate/delete" 0
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf "</button></p></form>");
  Hutil.trailer conf

let print_oldest_alive conf base =
  let limit = match p_getint conf.env "lim" with Some x -> x | _ -> 0 in
  let get_oldest_alive p =
    match Driver.get_death p with
    | NotDead -> Date.od_of_cdate (Driver.get_birth p)
    | DontKnowIfDead when limit > 0 -> (
        match Date.od_of_cdate (Driver.get_birth p) with
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
  Output.print_sstring conf "<ul>\n";
  List.iter
    (fun (p, d, cal) ->
      Output.print_sstring conf "<li><b>";
      Output.print_string conf (referenced_person_text conf base p);
      Output.print_sstring conf "</b>, ";
      Output.print_sstring conf
        (transl_nth conf "born" (index_of_sex (Driver.get_sex p)));
      Output.print_sstring conf " <em>";
      Output.print_string conf
        (DateDisplay.string_of_ondate conf (Dgreg (d, cal)));
      Output.print_sstring conf "</em>";
      if Driver.get_death p = NotDead && d.prec = Sure then (
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
      match
        (Date.cdate_to_dmy_opt (Driver.get_birth p), Driver.get_death p)
      with
      | Some bd, Death (_, cd) -> (
          match Date.cdate_to_dmy_opt cd with
          | None -> None
          | Some dd -> Some (Dgreg (Date.time_elapsed bd dd, Dgregorian)))
      | _ -> None
    else None
  in
  let list, len = select_person conf base get_longest false in
  let title _ =
    Printf.sprintf (fcapitale (ftransl conf "the %d who lived the longest")) len
    |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul>";
  List.iter
    (fun (p, d, _) ->
      Output.print_sstring conf "<li><strong>";
      Output.print_string conf (referenced_person_text conf base p);
      Output.print_sstring conf "</strong>";
      Output.print_string conf (DateDisplay.short_dates_text conf base p);
      Output.print_sstring conf " (";
      Output.print_sstring conf (string_of_int d.year);
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
  Output.print_sstring conf "<ul>\n";
  ignore
  @@ List.fold_left
       (fun (last_month_txt, was_future) (fam, d, cal) ->
         let month_txt, future =
           list_aux_1 conf d cal last_month_txt was_future
         in
         Output.print_sstring conf "<li><b>";
         Output.print_string conf
           (referenced_person_text conf base
              (pget conf base (Driver.get_father fam)));
         Output.print_sstring conf "</b> ";
         Output.print_sstring conf (transl_nth conf "and" 0);
         Output.print_sstring conf " <b>";
         Output.print_string conf
           (referenced_person_text conf base
              (pget conf base (Driver.get_mother fam)));
         Output.print_sstring conf "</b>, ";
         if future then (
           Output.print_sstring conf "<em>";
           Output.print_string conf
             (DateDisplay.string_of_date conf (Dgreg (d, cal)));
           Output.print_sstring conf "</em>")
         else (
           (match Driver.get_relation fam with
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
  Output.print_sstring conf "</ul></li>";
  Hutil.trailer conf

let print_marriage conf base =
  let list, len =
    select_family conf base
      (fun fam ->
        let rel = Driver.get_relation fam in
        if rel = Married || rel = NoSexesCheckMarried then
          Date.od_of_cdate (Driver.get_marriage fam)
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
        if Driver.get_relation fam = Engaged then
          let husb = pget conf base (Driver.get_father fam) in
          let wife = pget conf base (Driver.get_mother fam) in
          match (Driver.get_death husb, Driver.get_death wife) with
          | (NotDead | DontKnowIfDead), (NotDead | DontKnowIfDead) ->
              Date.od_of_cdate (Driver.get_marriage fam)
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
    let ifun =
      Templ.
        {
          eval_var = (fun _ -> raise Not_found);
          eval_transl = (fun _ -> Templ.eval_transl conf);
          eval_predefined_apply = (fun _ -> raise Not_found);
          get_vother;
          set_vother;
          print_foreach = (fun _ -> raise Not_found);
        }
    in
    Templ.output conf ifun Templ.Env.empty () "stats"

let print_pyramid_table conf ~men ~wom ~nb_intervals ~interval ~row_label_side =
  let sep = transl conf "(thousand separator)" in
  let nbstr n = Mutil.string_of_int_sep sep n in
  let max_hum =
    max 1 (max (Array.fold_left max 0 men) (Array.fold_left max 0 wom))
  in
  let band n = max 1 (((140 * n) + max_hum) / (2 * max_hum)) * 3 in
  let first =
    let rec loop i =
      if i <= 0 then 0
      else if men.(i) > 0 || wom.(i) > 0 then i
      else loop (i - 1)
    in
    loop nb_intervals
  in
  let compact = interval < 3 in
  let h = if compact then max 8 (interval * 8) else 24 in
  let pfx = Util.images_prefix conf in
  let bar nb file =
    if nb = 0 then ""
    else
      Printf.sprintf {|<img src="%s/%s" width="%d" height="%d">|} pfx file
        (band nb) h
  in
  let nb_html nb =
    if nb = 0 then "" else Printf.sprintf {|<i>%s</i>|} (nbstr nb)
  in
  let side_html i =
    match row_label_side with
    | Some f -> Printf.sprintf {|<td class="text-muted px-2">%s</td>|} (f i)
    | None -> ""
  in
  let cls = if compact then " pyr-compact" else "" in
  Output.printf conf {|<div><table id="table_pop_pyr" class="%s">|} cls;
  for i = first downto 0 do
    let nm = men.(i) and nw = wom.(i) in
    let center =
      if i = nb_intervals then "&nbsp;" else string_of_int ((i + 1) * interval)
    in
    Output.printf conf
      {|<tr>%s
  <td class="text-right">%s %s</td>
  <td class="text-center px-1">%s</td>
  <td>%s %s</td>
%s</tr>|}
      (side_html i) (nb_html nm) (bar nm "pyr_male.png") center
      (bar nw "pyr_female.png") (nb_html nw) (side_html i)
  done;
  Output.print_sstring conf {|</table>
</div>|}

let print_pyramid_totals conf ~string_of_nb ~label_key ~sum_men ~sum_wom =
  Output.printf conf
    {|<div class="mt-3">%s%s %s (%s &#9794; + %s &#9792;).</div>|}
    (Utf8.capitalize_fst (transl conf label_key))
    (transl conf ":")
    (string_of_nb (sum_men + sum_wom))
    (string_of_nb sum_men) (string_of_nb sum_wom)

let print_pyramid_form_tail conf ~interval ~limit =
  Output.printf conf
    {|<label for="int" class="ml-3">%s</label>
<input type="number" id="int" name="int"
       value="%d" class="form-control col-1 ml-2"
       step="1" min="1" max="130">
<label for="lim" class="ml-3">%s</label>
<input type="number" id="lim" name="lim"
       value="%d" class="form-control col-1 ml-2"
       step="1" min="0">
<button type="submit"
        class="btn btn-primary ml-3">OK</button>|}
    (Utf8.capitalize_fst (transl conf "interval"))
    interval
    (Utf8.capitalize_fst (transl conf "limit"))
    limit

let print_population_pyramid conf base =
  let death_mode = p_getenv conf.env "t" = Some "D" in
  let interval =
    match p_getint conf.env "int" with Some i -> max 1 i | None -> 5
  in
  let limit = match p_getint conf.env "lim" with Some x -> x | _ -> 0 in
  let nb_intervals = 150 / interval in
  let string_of_nb n =
    Mutil.string_of_int_sep (transl conf "(thousand separator)") n
  in
  let commd_s = (commd conf :> string) in
  let open_form hidden_extra =
    Output.printf conf
      {|<form method="get" class="mt-2" action="%s">
<div class="form-inline">|}
      commd_s;
    hidden_env conf;
    Util.hidden_input conf "m" (Adef.encoded "POP_PYR");
    Output.print_sstring conf hidden_extra
  in
  if death_mode then (
    let from_year =
      match p_getint conf.env "from" with
      | Some y -> y
      | None -> conf.today.year
    in
    let to_year =
      match p_getint conf.env "to" with Some y -> y | None -> from_year
    in
    let men, wom =
      make_death_pyramid ~nb_intervals ~interval ~limit ~from_year ~to_year conf
        base
    in
    let range =
      if to_year <> from_year then Printf.sprintf "%d-%d" from_year to_year
      else string_of_int from_year
    in
    let title _ =
      Output.printf conf
        {|<a href="%sm=POP_PYR" title="%s"
  class="btn btn-secondary btn-sm mb-1 mr-3">&#176;</a>%s (%s)|}
        commd_s
        (Utf8.capitalize_fst (transl conf "population pyramid"))
        (Utf8.capitalize_fst (transl conf "death pyramid"))
        range
    in
    Hutil.header conf title;
    print_pyramid_table conf ~men ~wom ~nb_intervals ~interval
      ~row_label_side:None;
    let sum_men = Array.fold_left ( + ) 0 men in
    let sum_wom = Array.fold_left ( + ) 0 wom in
    print_pyramid_totals conf ~string_of_nb
      ~label_key:"number of deceased persons" ~sum_men ~sum_wom;
    open_form {|<input type="hidden" name="t" value="D">|};
    Output.printf conf
      {|<label for="from" class="mr-1">%s</label>
<input type="number" id="from" name="from" value="%d"
  class="form-control col-1 ml-1" step="1">
<label for="to" class="ml-3 mr-1">… %s</label>
<input type="number" id="to" name="to" value="%d"
  class="form-control col-1 ml-1" step="1">|}
      (Utf8.capitalize_fst (transl_nth conf "from/to (date year)" 0))
      from_year
      (transl_nth conf "from/to (date year)" 1)
      to_year;
    print_pyramid_form_tail conf ~interval ~limit;
    Output.print_sstring conf {|</div></form>|};
    Hutil.trailer conf)
  else
    let at_date =
      match p_getint conf.env "y" with
      | Some i -> { year = i; month = 31; day = 12; prec = Sure; delta = 0 }
      | None -> conf.today
    in
    let men, wom =
      make_population_pyramid ~nb_intervals ~interval ~limit ~at_date conf base
    in
    let at_year = at_date.year in
    let title _ =
      Output.printf conf
        {|<a href="%sm=POP_PYR&t=D&from=1900&to=%d" title="%s"
  class="btn btn-secondary btn-sm mb-1 mr-3">&dagger;</a>%s (%d)|}
        commd_s conf.today.year
        (Utf8.capitalize_fst (transl conf "death pyramid"))
        (Utf8.capitalize_fst (transl conf "population pyramid"))
        at_year
    in
    Hutil.header conf title;
    print_pyramid_table conf ~men ~wom ~nb_intervals ~interval
      ~row_label_side:(Some (fun i -> string_of_int (at_year - (i * interval))));
    let sum_men = Array.fold_left ( + ) 0 men in
    let sum_wom = Array.fold_left ( + ) 0 wom in
    print_pyramid_totals conf ~string_of_nb
      ~label_key:"number of living persons" ~sum_men ~sum_wom;
    open_form "";
    Output.printf conf
      {|<label for="yr">%s</label>
<input type="number" id="yr" name="y" value="%d"
  class="form-control col-1 ml-2" step="1">|}
      (Utf8.capitalize_fst (transl_nth conf "year/month/day" 0))
      at_year;
    print_pyramid_form_tail conf ~interval ~limit;
    Output.print_sstring conf {|</div></form>|};
    Hutil.trailer conf
