(* Copyright (c) 1998-2007 INRIA *)

let month_txt conf d cal =
  let d = DateDisplay.string_of_date conf (Dgreg ({ d with day = 0 }, cal)) in
  (d : Adef.safe_string :> string) |> Utf8.capitalize_fst |> Adef.safe

let print_death conf base =
  let list, len =
    BirthDeath.select_person conf base BirthDeath.death_date false
  in
  let title _ =
    Printf.sprintf
      (Util.fcapitale (Util.ftransl conf "the latest %t deaths"))
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
            match Date.cdate_to_dmy_opt (Gwdb.get_birth p) with
            | None -> (None, ages_sum, ages_nb)
            | Some d1 ->
                if sure d1 && sure d && d1 <> d then
                  let a = Date.time_elapsed d1 d in
                  let ages_sum =
                    match Gwdb.get_sex p with
                    | Def.Male -> (fst ages_sum + a.year, snd ages_sum)
                    | Def.Female -> (fst ages_sum, snd ages_sum + a.year)
                    | Def.Neuter -> ages_sum
                  in
                  let ages_nb =
                    match Gwdb.get_sex p with
                    | Def.Male -> (fst ages_nb + 1, snd ages_nb)
                    | Def.Female -> (fst ages_nb, snd ages_nb + 1)
                    | Def.Neuter -> ages_nb
                  in
                  (Some a, ages_sum, ages_nb)
                else (None, ages_sum, ages_nb)
          in
          Output.print_sstring conf "<li><b>";
          Output.print_string conf
            (NameDisplay.referenced_person_text conf base p);
          Output.print_sstring conf "</b>, ";
          Output.print_sstring conf
            (Util.transl_nth conf "died" (Util.index_of_sex (Gwdb.get_sex p)));
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
        Util.transl conf "average age at death"
        |> Utf8.capitalize_fst |> Output.print_sstring conf;
        Output.print_sstring conf " (";
        Output.print_sstring conf (Util.transl_nth conf "M/F" sex);
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
      match Util.p_getenv conf.Config.env name with
      | Some s -> int_of_string s
      | None -> def
    in
    let by = aux "by" conf.Config.today.year in
    let bm = aux "bm" conf.Config.today.month in
    let bd = aux "bd" conf.Config.today.day in
    Output.print_sstring conf {|<form method="get" action="|};
    Output.print_sstring conf conf.Config.command;
    Output.print_sstring conf {|"><p>|};
    Util.hidden_env conf;
    Util.hidden_input conf "m" (Adef.encoded "LD");
    Output.print_sstring conf
    @@ Printf.sprintf
         (Util.fcapitale (Util.ftransl conf "the latest %t deaths"))
         (fun _ ->
           {|<input name="k" value="|} ^ string_of_int len
           ^ {|" size="4" maxlength="4">|});
    Output.print_sstring conf "\n... (";
    Output.print_sstring conf (Util.transl conf "before");
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
    Util.transl_nth conf "validate/delete" 0
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf "</button></p></form>");
  Hutil.trailer conf

let old_print_statistics conf =
  let title _ =
    Util.transl conf "statistics"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  let n =
    Option.value ~default:20
      (Option.bind
         (List.assoc_opt "latest_event" conf.Config.base_env)
         int_of_string_opt)
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<ul>";
  let aux m label =
    Output.print_sstring conf {|<li><a href="|};
    Output.print_string conf (Util.commd conf);
    Output.print_sstring conf {|m=|};
    Output.print_sstring conf m;
    Output.print_sstring conf {|&k=|};
    Output.print_sstring conf (string_of_int n);
    Output.print_sstring conf {|">|};
    Output.print_sstring conf (Printf.sprintf (Util.ftransl conf label) n);
    Output.print_sstring conf {|</a></li>|}
  in
  if conf.Config.wizard || conf.Config.friend then (
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
  if Util.p_getenv conf.Config.env "old" = Some "on" then
    old_print_statistics conf
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
