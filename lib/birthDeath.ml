(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

let get_k conf =
  match p_getint conf.env "k" with
    Some x -> x
  | _ ->
      try int_of_string (List.assoc "latest_event" conf.base_env) with
        Not_found | Failure _ -> 20

let select conf base get_date find_oldest =
  let module Q =
    Pqueue.Make
      (struct
         type t = Gwdb.person * Def.dmy * Def.calendar
         let leq (_, x, _) (_, y, _) =
           if find_oldest then Date.before_date x y else Date.before_date y x
       end)
  in
  let n = min (max 0 (get_k conf)) (nb_of_persons base) in
  let ref_date =
    match p_getint conf.env "by" with
      Some by ->
        let bm =
          match p_getint conf.env "bm" with
            Some x -> x
          | None -> -1
        in
        let bd =
          match p_getint conf.env "bd" with
            Some x -> x
          | None -> -1
        in
        Some {day = bd; month = bm; year = by; prec = Sure; delta = 0}
    | None -> None
  in
  let (q, len) =
    Gwdb.Collection.fold (fun (q, len) i ->
        let p = pget conf base i in
        match get_date p with
        | Some (Dgreg (d, cal)) ->
          let aft =
            match ref_date with
            | Some ref_date -> Date.before_date d ref_date
            | None -> false
          in
          if aft then (q, len)
          else
            let e = p, d, cal in
            if len < n then ((Q.add e q), (len + 1))
            else ((snd (Q.take (Q.add e q))), len)
        | _ -> (q, len)
      ) (Q.empty, 0) (Gwdb.ipers base)
  in
  let rec loop list q =
    if Q.is_empty q then list, len
    else let (e, q) = Q.take q in loop (e :: list) q
  in
  loop [] q

(* TODO? Factorize with select_person? *)
let select_family conf base get_date find_oldest =
  let module QF =
    Pqueue.Make
      (struct
         type t = Gwdb.ifam * Gwdb.family * Def.dmy * Def.calendar
         let leq (_, _, x, _) (_, _, y, _) =
           if find_oldest then Date.before_date x y else Date.before_date y x
       end)
  in
  let n = min (max 0 (get_k conf)) (nb_of_families base) in
  let ref_date =
    match p_getint conf.env "by" with
      Some by ->
        let bm =
          match p_getint conf.env "bm" with
            Some x -> x
          | None -> -1
        in
        let bd =
          match p_getint conf.env "bd" with
            Some x -> x
          | None -> -1
        in
        Some {day = bd; month = bm; year = by; prec = Sure; delta = 0}
    | None -> None
  in
  let (q, len) =
    Gwdb.Collection.fold (fun (q, len) i ->
        let fam = foi base i in
        match get_date i fam with
          | Some (Dgreg (d, cal)) ->
            let aft =
              match ref_date with
              | Some ref_date -> Date.before_date d ref_date
              | None -> false
            in
            if aft then (q, len)
            else
              let e = i, fam, d, cal in
              if len < n then (QF.add e q, len + 1)
              else (snd (QF.take (QF.add e q)), len)
          | _ -> (q, len)
      ) (QF.empty, 0) (Gwdb.ifams base)
  in
  let rec loop list q =
    if QF.is_empty q then list, len
    else let (e, q) = QF.take q in loop (e :: list) q
  in
  loop [] q

let print_birth conf base =
  let (list, len) =
    select conf base (fun p -> Adef.od_of_cdate (get_birth p)) false
  in
  let title _ =
    Wserver.printf (fcapitale (ftransl conf "the latest %d births")) len
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<ul>\n";
  let _ =
    List.fold_left
      (fun (last_month_txt, was_future) (p, d, cal) ->
         let month_txt =
           let d = {d with day = 0} in
           capitale (Date.string_of_date conf (Dgreg (d, cal)))
         in
         let future = CheckItem.strictly_after_dmy d conf.today in
         if not future && was_future then
           begin
             Wserver.printf "</li>\n</ul>\n</li>\n</ul>\n<p>\n<ul>\n";
             Wserver.printf "<li>%s\n" month_txt;
             Wserver.printf "<ul>\n"
           end
         else if month_txt <> last_month_txt then
           begin
             if last_month_txt = "" then ()
             else Wserver.printf "</ul>\n</li>\n";
             Wserver.printf "<li>%s\n" month_txt;
             Wserver.printf "<ul>\n"
           end;
         Wserver.printf "<li>";
         Wserver.printf "<b>";
         Wserver.printf "%s" (referenced_person_text conf base p);
         Wserver.printf "</b>";
         Wserver.printf ",\n";
         if future then
           Wserver.printf "<em>%s</em>.\n"
             (Date.string_of_date conf (Dgreg (d, cal)))
         else
           Wserver.printf "%s <em>%s</em>.\n"
             (transl_nth conf "born" (index_of_sex (get_sex p)))
             (Date.string_of_ondate conf (Dgreg (d, cal)));
         Wserver.printf "</li>\n";
         month_txt, future)
      ("", false) list
  in
  Wserver.printf "</ul>\n</li>\n</ul>\n"; Hutil.trailer conf

let death_date p =
  match get_death p with
    Death (_, cd) -> Some (Adef.date_of_cdate cd)
  | _ -> None

let print_death conf base =
  let (list, len) = select conf base death_date false in
  let title _ =
    Wserver.printf (fcapitale (ftransl conf "the latest %t deaths"))
      (fun _ -> string_of_int len)
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  if list <> [] then
    begin
      Wserver.printf "<ul>\n";
      let (_, ages_sum, ages_nb) =
        List.fold_left
          (fun (last_month_txt, ages_sum, ages_nb) (p, d, cal) ->
             let month_txt =
               let d = {d with day = 0} in
               capitale (Date.string_of_date conf (Dgreg (d, cal)))
             in
             if month_txt <> last_month_txt then
               begin
                 if last_month_txt = "" then ()
                 else Wserver.printf "</ul>\n</li>\n";
                 Wserver.printf "<li>%s\n" month_txt;
                 Wserver.printf "<ul>\n"
               end;
             let (age, ages_sum, ages_nb) =
               let sure d = d.prec = Sure in
               match Adef.od_of_cdate (get_birth p) with
                 Some (Dgreg (d1, _)) ->
                   if sure d1 && sure d && d1 <> d then
                     let a = CheckItem.time_elapsed d1 d in
                     let ages_sum =
                       match get_sex p with
                         Male -> fst ages_sum + a.year, snd ages_sum
                       | Female -> fst ages_sum, snd ages_sum + a.year
                       | Neuter -> ages_sum
                     in
                     let ages_nb =
                       match get_sex p with
                         Male -> fst ages_nb + 1, snd ages_nb
                       | Female -> fst ages_nb, snd ages_nb + 1
                       | Neuter -> ages_nb
                     in
                     Some a, ages_sum, ages_nb
                   else None, ages_sum, ages_nb
               | _ -> None, ages_sum, ages_nb
             in
             Wserver.printf "<li>";
             Wserver.printf "<b>";
             Wserver.printf "%s" (referenced_person_text conf base p);
             Wserver.printf "</b>";
             Wserver.printf ", %s <em>%s</em>"
               (transl_nth conf "died" (index_of_sex (get_sex p)))
               (Date.string_of_ondate conf (Dgreg (d, cal)));
             begin match age with
               Some a ->
                 Wserver.printf " <em>(%s)</em>" (Date.string_of_age conf a)
             | None -> ()
             end;
             Wserver.printf "</li>\n";
             month_txt, ages_sum, ages_nb)
          ("", (0, 0), (0, 0)) list
      in
      Wserver.printf "</ul>\n</li>\n</ul>\n";
      if fst ages_nb >= 3 then
        Wserver.printf "%s (%s) : %s<br%s>\n"
          (capitale (transl conf "average age at death"))
          (transl_nth conf "M/F" 0)
          (Date.string_of_age conf
             {day = 0; month = 0; year = fst ages_sum / fst ages_nb;
              delta = 0; prec = Sure})
          conf.xhs;
      if snd ages_nb >= 3 then
        Wserver.printf "%s (%s) : %s<br%s>\n"
          (capitale (transl conf "average age at death"))
          (transl_nth conf "M/F" 1)
          (Date.string_of_age conf
             {day = 0; month = 0; year = snd ages_sum / snd ages_nb;
              delta = 0; prec = Sure})
          conf.xhs;
      Wserver.printf "<br%s>\n" conf.xhs;
      Wserver.printf "<div align=\"center\">\n";
      Wserver.printf "<hr width=\"50%%\"%s>\n" conf.xhs;
      Wserver.printf "</div>\n";
      Wserver.printf "<br%s>\n" conf.xhs;
      let by =
        match p_getenv conf.env "by" with
          Some s -> s
        | None -> string_of_int conf.today.year
      in
      let bm =
        match p_getenv conf.env "bm" with
          Some s -> s
        | None -> string_of_int conf.today.month
      in
      let bd =
        match p_getenv conf.env "bd" with
          Some s -> s
        | None -> string_of_int conf.today.day
      in
      Wserver.printf "<form method=\"get\" action=\"%s\">\n" conf.command;
      Wserver.printf "<p>\n";
      Util.hidden_env conf;
      Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"LD\"%s>\n"
        conf.xhs;
      begin let ds =
        Printf.sprintf
          "<input name=\"k\" value=\"%d\" size=\"4\" maxlength=\"4\"%s>" len
          conf.xhs
      in
        Wserver.printf (fcapitale (ftransl conf "the latest %t deaths"))
          (fun _ -> ds)
      end;
      Wserver.printf "\n... (%s...\n" (transl conf "before");
      Wserver.printf
        "<input name=\"by\" value=\"%s\" size=\"4\" maxlength=\"4\"%s>\n" by
        conf.xhs;
      Wserver.printf
        "<input name=\"bm\" value=\"%s\" size=\"2\" maxlength=\"2\"%s>\n" bm
        conf.xhs;
      Wserver.printf
        "<input name=\"bd\" value=\"%s\" size=\"2\" maxlength=\"2\"%s>\n" bd
        conf.xhs;
      Wserver.printf ")\n";
      Wserver.printf
        "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
      Wserver.printf "%s" (capitale (transl_nth conf "validate/delete" 0));
      Wserver.printf "</button>\n";
      Wserver.printf "</p>\n";
      Wserver.printf "</form>\n"
    end;
  Hutil.trailer conf

let print_oldest_alive conf base =
  let limit =
    match p_getint conf.env "lim" with
      Some x -> x
    | _ -> 0
  in
  let get_oldest_alive p =
    match get_death p with
      NotDead -> Adef.od_of_cdate (get_birth p)
    | DontKnowIfDead when limit > 0 ->
        begin match Adef.od_of_cdate (get_birth p) with
          Some (Dgreg (d, _)) as x when conf.today.year - d.year <= limit -> x
        | _ -> None
        end
    | _ -> None
  in
  let (list, len) = select conf base get_oldest_alive true in
  let title _ =
    Wserver.printf
      (fcapitale (ftransl conf "the %d oldest perhaps still alive")) len
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<ul>\n";
  List.iter
    (fun (p, d, cal) ->
       Wserver.printf "<li>\n";
       Wserver.printf "<b>%s</b>,\n" (referenced_person_text conf base p);
       Wserver.printf "%s <em>%s</em>"
         (transl_nth conf "born" (index_of_sex (get_sex p)))
         (Date.string_of_ondate conf (Dgreg (d, cal)));
       if get_death p = NotDead && d.prec = Sure then
         begin let a = CheckItem.time_elapsed d conf.today in
           Wserver.printf " <em>(%s)</em>" (Date.string_of_age conf a)
         end;
       Wserver.printf ".";
       Wserver.printf "</li>\n")
    list;
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

let print_longest_lived conf base =
  let get_longest p =
    if Util.authorized_age conf base p then
      match Adef.od_of_cdate (get_birth p), get_death p with
        Some (Dgreg (bd, _)), Death (_, cd) ->
          begin match Adef.date_of_cdate cd with
            Dgreg (dd, _) ->
              Some (Dgreg (CheckItem.time_elapsed bd dd, Dgregorian))
          | _ -> None
          end
      | _ -> None
    else None
  in
  let (list, len) = select conf base get_longest false in
  let title _ =
    Wserver.printf (fcapitale (ftransl conf "the %d who lived the longest"))
      len
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<ul>\n";
  List.iter
    (fun (p, d, _) ->
       Wserver.printf "<li>\n";
       Wserver.printf "<strong>\n";
       Wserver.printf "%s" (referenced_person_text conf base p);
       Wserver.printf "</strong>%s" (Date.short_dates_text conf base p);
       Wserver.printf "\n(%d %s)" d.year (transl conf "years old");
       Wserver.printf ".";
       Wserver.printf "</li>\n")
    list;
  Wserver.printf "</ul>\n\n";
  Hutil.trailer conf

let print_marr_or_eng conf base title list =
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<ul>\n";
  let _ =
    List.fold_left
      (fun (last_month_txt, was_future) (ifam, fam, d, cal) ->
         let month_txt =
           let d = {d with day = 0} in
           capitale (Date.string_of_date conf (Dgreg (d, cal)))
         in
         let cpl = foi base ifam in
         let future = CheckItem.strictly_after_dmy d conf.today in
         if not future && was_future then
           begin
             Wserver.printf "</ul>\n</li>\n</ul>\n<ul>\n";
             Wserver.printf "<li>%s\n" month_txt;
             Wserver.printf "<ul>\n"
           end
         else if month_txt <> last_month_txt then
           begin
             if last_month_txt = "" then ()
             else Wserver.printf "</ul>\n</li>\n";
             Wserver.printf "<li>%s\n" month_txt;
             Wserver.printf "<ul>\n"
           end;
         Wserver.printf "<li>";
         Wserver.printf "<b>";
         Wserver.printf "%s"
           (referenced_person_text conf base
              (pget conf base (get_father cpl)));
         Wserver.printf "</b>\n";
         Wserver.printf "%s\n" (transl_nth conf "and" 0);
         Wserver.printf "<b>";
         Wserver.printf "%s"
           (referenced_person_text conf base
              (pget conf base (get_mother cpl)));
         Wserver.printf "</b>";
         Wserver.printf ",\n";
         if future then
           Wserver.printf "<em>%s</em>."
             (Date.string_of_date conf (Dgreg (d, cal)))
         else
           Wserver.printf "%s <em>%s</em>."
             (match get_relation fam with
                NotMarried | NoSexesCheckNotMarried ->
                  transl_nth conf "relation/relations" 0
              | Married | NoSexesCheckMarried -> transl conf "married"
              | Engaged -> transl conf "engaged"
              | MarriageBann
              | MarriageContract
              | MarriageLicense
              | Pacs
              | Residence
              | NoMention -> "")
             (Date.string_of_ondate conf (Dgreg (d, cal)));
         Wserver.printf "</li>\n";
         month_txt, future)
      ("", false) list
  in
  Wserver.printf "</ul>\n</li>\n</ul>\n"; Hutil.trailer conf

let print_marriage conf base =
  let (list, len) =
    select_family conf base
      (fun _ fam ->
          let rel = get_relation fam in
          if rel = Married || rel = NoSexesCheckMarried then
            Adef.od_of_cdate (get_marriage fam)
          else None)
      false
  in
  let title _ =
    Wserver.printf (fcapitale (ftransl conf "the latest %d marriages")) len
  in
  print_marr_or_eng conf base title list

let print_oldest_engagements conf base =
  let (list, len) =
    select_family conf base
      (fun _ fam ->
         if get_relation fam = Engaged then
           let husb = pget conf base (get_father fam) in
           let wife = pget conf base (get_mother fam) in
           match get_death husb, get_death wife with
             (NotDead | DontKnowIfDead), (NotDead | DontKnowIfDead) ->
               Adef.od_of_cdate (get_marriage fam)
           | _ -> None
         else None)
      true
  in
  let title _ =
    Wserver.printf
      (fcapitale
         (ftransl conf
            "the %d oldest couples perhaps still alive and engaged"))
      len
  in
  print_marr_or_eng conf base title list

let old_print_statistics conf =
  let title _ = Wserver.printf "%s" (capitale (transl conf "statistics")) in
  let n =
    try int_of_string (List.assoc "latest_event" conf.base_env) with
      Not_found | Failure _ -> 20
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<ul>\n";
  if conf.wizard || conf.friend then
    begin
      begin
        Wserver.printf "<li>";
        Wserver.printf "<a href=\"%sm=LB&k=%d\">" (commd conf) n;
        Wserver.printf (ftransl conf "the latest %d births") n;
        Wserver.printf "</a>";
        Wserver.printf "</li>\n"
      end;
      begin
        Wserver.printf "<li>";
        Wserver.printf "<a href=\"%sm=LD&k=%d\">" (commd conf) n;
        Wserver.printf (ftransl conf "the latest %t deaths")
          (fun _ -> string_of_int n);
        Wserver.printf "</a>";
        Wserver.printf "</li>\n"
      end;
      begin
        Wserver.printf "<li>";
        Wserver.printf "<a href=\"%sm=LM&k=%d\">" (commd conf) n;
        Wserver.printf (ftransl conf "the latest %d marriages") n;
        Wserver.printf "</a>";
        Wserver.printf "</li>\n"
      end;
      begin
        Wserver.printf "<li>";
        Wserver.printf "<a href=\"%sm=OE&k=%d\">" (commd conf) n;
        Wserver.printf
          (ftransl conf
             "the %d oldest couples perhaps still alive and engaged")
          n;
        Wserver.printf "</a>";
        Wserver.printf "</li>\n"
      end;
      begin
        Wserver.printf "<li>";
        Wserver.printf "<a href=\"%sm=OA&k=%d&lim=0\">" (commd conf) n;
        Wserver.printf (ftransl conf "the %d oldest perhaps still alive") n;
        Wserver.printf "</a>";
        Wserver.printf "</li>\n"
      end
    end;
  Wserver.printf "<li>";
  Wserver.printf "<a href=\"%sm=LL&k=%d\">" (commd conf) n;
  Wserver.printf (ftransl conf "the %d who lived the longest") n;
  Wserver.printf "</a>";
  Wserver.printf "</li>\n";
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

(* *)

type 'a env =
    Vother of 'a
  | Vnone

let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x

let print_statistics conf =
  if p_getenv conf.env "old" = Some "on" then old_print_statistics conf
  else
    Hutil.interp conf "stats"
      {Templ.eval_var = (fun _ -> raise Not_found);
       Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
       Templ.eval_predefined_apply = (fun _ -> raise Not_found);
       Templ.get_vother = get_vother; Templ.set_vother = set_vother;
       Templ.print_foreach = fun _ -> raise Not_found}
      [] ()

let make_population_pyramid ~nb_intervals ~interval ~limit ~at_date conf base =
  let men = Array.make (nb_intervals + 1) 0 in
  let wom = Array.make (nb_intervals + 1) 0 in
  (* TODO? Load person array *)
  Gwdb.Collection.iter (fun i ->
      let p = pget conf base i in
      let sex = get_sex p in
      let dea = get_death p in
      if sex <> Neuter then
        match Adef.od_of_cdate (get_birth p) with
        | Some (Dgreg (dmy, _)) ->
          if not (Date.before_date dmy at_date) then
            let a = CheckItem.time_elapsed dmy at_date in
            let j = min nb_intervals (a.year / interval) in
            if (dea = NotDead || dea = DontKnowIfDead && a.year < limit)
            || match dea with
            | Death (_, cd) ->
              begin match Adef.date_of_cdate cd with
                | Dgreg (d, _) -> Date.before_date d at_date
                | _ -> false
              end
            | _ -> false
            then
              if sex = Male then men.(j) <- men.(j) + 1
              else wom.(j) <- wom.(j) + 1
        | Some (Dtext _) | None -> ()
    ) (Gwdb.ipers base) ;
  (men, wom)

let print_population_pyramid conf base =
  let interval =
    match p_getint conf.env "int" with
      Some i -> max 1 i
    | None -> 5
  in
  let limit =
    match p_getint conf.env "lim" with
      Some x -> x
    | _ -> 0
  in
  let at_date =
    match p_getint conf.env "y" with
      Some i -> {year = i; month = 31; day = 12; prec = Sure; delta = 0}
    | None -> conf.today
  in
  let nb_intervals = 150 / interval in
  let men, wom = make_population_pyramid ~nb_intervals ~interval ~limit ~at_date conf base in
  let at_year = at_date.year in
  let string_of_nb n =
    Mutil.string_of_int_sep (transl conf "(thousand separator)") n
  in
  let title _ =
    Wserver.printf "%s (%d)" (capitale (transl conf "population pyramid"))
      at_year
  in
  let print_image doit sex iname =
    Wserver.printf "<td>";
    if doit then
      Wserver.printf "<img src=\"%s/%s\" alt=\"%s\" title=\"%s\"%s>\n"
        (Util.image_prefix conf) iname (transl_nth conf "M/F" sex)
        (transl_nth conf "M/F" sex) conf.xhs
    else Wserver.printf "&nbsp;";
    Wserver.printf "</td>\n"
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  let max_hum =
    let max_men = Array.fold_left max 0 men in
    let max_wom = Array.fold_left max 0 wom in max 1 (max max_men max_wom)
  in
  let max_size = 70 in
  let band_size n = (2 * max_size * n + max_hum) / (2 * max_hum) in
  let first_interv =
    let rec loop i =
      if i <= 0 then 0
      else if men.(i) > 0 || wom.(i) > 0 then i
      else loop (i - 1)
    in
    loop nb_intervals
  in
  Wserver.printf "<div>\n";
  begin let c = " cellspacing=\"0\" cellpadding=\"0\"" in
    Wserver.printf
      "<table id=\"table_pop_pyr\" border=\"%d\"%s style=\"margin: auto\">\n"
      conf.border c;
    for i = first_interv downto 0 do
      let nb_men = men.(i) in
      let nb_wom = wom.(i) in
      Wserver.printf "<tr>\n";
      Wserver.printf "<td class=\"pyramid_year\">";
      Wserver.printf "%d" (at_year - i * interval);
      Wserver.printf "</td>\n";
      Wserver.printf "<td>";
      Wserver.printf "&nbsp;";
      Wserver.printf "</td>\n";
      print_image (i = 0) 0 "male.png";
      Wserver.printf "<td>";
      Wserver.printf "&nbsp;";
      Wserver.printf "</td>\n";
      Wserver.printf "<td align=\"right\">\n";
      Wserver.printf "<table %s>\n" c;
      Wserver.printf "<tr>\n";
      Wserver.printf "<td class=\"pyramid_nb\">";
      if nb_men <> 0 then Wserver.printf "%d" nb_men;
      Wserver.printf "&nbsp;";
      Wserver.printf "</td>\n";
      Wserver.printf "<td>";
      if nb_men = 0 then ()
      else
        begin let n = max 1 (band_size nb_men) in
          (* On multiplie par 3 parce que c'est *)
          (* la largeur de l'image : 3 x 14     *)
          Wserver.printf
            "<img src=\"images/pyr_male.png\" width=%d height=%d />" (n * 3)
            14
        end;
      Wserver.printf "</td>\n";
      Wserver.printf "</tr>\n";
      Wserver.printf "</table>\n";
      Wserver.printf "</td>\n";
      Wserver.printf "<td align=\"center\">";
      if i = nb_intervals then Wserver.printf "&nbsp;"
      else Wserver.printf "%d" ((i + 1) * interval);
      Wserver.printf "</td>\n";
      Wserver.printf "<td align=\"left\">\n";
      Wserver.printf "<table %s>\n" c;
      Wserver.printf "<tr>\n";
      Wserver.printf "<td>";
      if nb_wom = 0 then ()
      else
        begin let n = max 1 (band_size nb_wom) in
          (* On multiplie par 3 parce que c'est *)
          (* la largeur de l'image : 3 x 14     *)
          Wserver.printf
            "<img src=\"images/pyr_female.png\" width=%d height=%d />" (n * 3)
            14
        end;
      Wserver.printf "</td>\n";
      Wserver.printf "<td class=\"pyramid_nb\">";
      Wserver.printf "&nbsp;";
      if nb_wom <> 0 then Wserver.printf "%d" nb_wom;
      Wserver.printf "</td>\n";
      Wserver.printf "</tr>\n";
      Wserver.printf "</table>\n";
      Wserver.printf "</td>\n";
      Wserver.printf "<td>";
      Wserver.printf "&nbsp;";
      Wserver.printf "</td>\n";
      print_image (i = 0) 1 "female.png";
      Wserver.printf "<td>";
      Wserver.printf "&nbsp;";
      Wserver.printf "</td>\n";
      Wserver.printf "<td class=\"pyramid_year\">";
      Wserver.printf "%d" (at_year - i * interval);
      Wserver.printf "</td>\n";
      Wserver.printf "</tr>\n"
    done;
    Wserver.printf "</table id=\"table_pop_pyr\">\n"
  end;
  Wserver.printf "</div>\n";
  let sum_men = Array.fold_left (+) 0 men in
  let sum_wom = Array.fold_left (+) 0 wom in
  Wserver.printf "<p>\n";
  Wserver.printf "%s %s" (capitale (transl conf "number of living persons:"))
    (string_of_nb (sum_men + sum_wom));
  Wserver.printf "</p>\n";
  Wserver.printf "<p>\n";
  Wserver.printf "<form method=\"get\" action=\"%s\">\n" (commd conf);
  hidden_env conf;
  Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"POP_PYR\"%s>\n"
    conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"int\" value=\"%d\"%s>\n"
    interval conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"lim\" value=\"%d\"%s>\n" limit
    conf.xhs;
  Wserver.printf "%s\n" (transl_nth conf "year/month/day" 0);
  Wserver.printf "<input name=\"y\" value=\"%d\" size=\"5\"%s>\n" at_year
    conf.xhs;
  Wserver.printf "</form>\n";
  Wserver.printf "</p>\n";
  Hutil.trailer conf
