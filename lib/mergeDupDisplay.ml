(* Copyright (c) 2007 INRIA *)

let print_link ?(with_occurrence_number = true) ?(with_life_dates = true)
    ?(with_main_title = true) conf base p =
  Output.print_sstring conf "<a href=\"";
  Output.print_string conf (Util.commd conf);
  Output.print_string conf (Util.acces conf base p);
  Output.print_sstring conf "\">";
  Output.print_string conf
    (Gwdb.get_first_name p |> Gwdb.sou base |> Util.escape_html);
  if with_occurrence_number then (
    Output.print_sstring conf ".";
    Output.print_sstring conf (Gwdb.get_occ p |> string_of_int));
  Output.print_sstring conf " ";
  Output.print_string conf
    (Gwdb.get_surname p |> Gwdb.sou base |> Util.escape_html);
  Output.print_sstring conf "</a>";
  if with_life_dates then
    Output.print_string conf (DateDisplay.short_dates_text conf base p);
  match Util.main_title conf base p with
  | Some t ->
      if with_main_title then
        Output.print_string conf (Util.one_title_text base t)
  | None -> ()

let print_no_candidate conf base p =
  let title _ =
    Util.transl conf "possible duplications"
    |> Util.transl_decline conf "merge"
    |> Geneweb_util.Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Util.transl conf "duplicate_merge_end_explanation"
  |> Output.printf conf "<p>%s</p>";
  Output.print_sstring conf "<p>";
  Output.print_sstring conf (Util.transl conf "duplicate_merge_end_go_back");
  Output.print_sstring conf " ";
  print_link ~with_occurrence_number:false ~with_life_dates:false
    ~with_main_title:false conf base p;
  Output.print_sstring conf "</p>";
  Hutil.trailer conf

let input_excl string_of_i excl =
  List.fold_left
    (fun (s : Adef.encoded_string) (i1, i2) ->
      let t =
        let open Def in
        string_of_i i1 ^^^ "," ^<^ string_of_i i2
      in
      if (s :> string) = "" then t
      else
        let open Def in
        s ^^^ "," ^<^ t)
    (Adef.encoded "") excl

let print_input_excl conf string_of_i excl excl_name =
  let s = input_excl string_of_i excl in
  if (s :> string) <> "" then Util.hidden_input conf excl_name s

let print_submit conf name value =
  Output.print_sstring conf {|<input type="submit" name="|};
  Output.print_sstring conf name;
  Output.print_sstring conf {|" value="|};
  Output.print_sstring conf (Util.transl_nth conf "Y/N" value);
  Output.print_sstring conf {|" style="margin-right:4px">|}

let print_cand_ind conf base (ip, p) (iexcl, fexcl) ip1 ip2 =
  let title _ =
    Util.transl conf "merge" |> Geneweb_util.Utf8.capitalize_fst
    |> Output.print_sstring conf
  in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Output.print_sstring conf "<h2>";
  title false;
  Output.print_sstring conf "</h2>";
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<ul><li>";
  print_link conf base (Gwdb.poi base ip1);
  Output.print_sstring conf "</li><li>";
  print_link conf base (Gwdb.poi base ip2);
  Output.print_sstring conf "</li></ul><p>";
  Util.transl conf "merge" |> Geneweb_util.Utf8.capitalize_fst
  |> Output.print_sstring conf;
  Output.print_sstring conf " ?\n";
  (* FIXME: trans *)
  Output.print_sstring conf {|<form method="post" action="|};
  Output.print_sstring conf conf.Config.command;
  Output.print_sstring conf {|">|};
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "MRG_DUP_IND_Y_N");
  Util.hidden_input conf "ip"
    (Gwdb.string_of_iper ip |> Geneweb_util.Mutil.encode);
  print_input_excl conf
    (fun x -> Gwdb.string_of_iper x |> Geneweb_util.Mutil.encode)
    ((ip1, ip2) :: iexcl) "iexcl";
  print_input_excl conf
    (fun x -> Gwdb.string_of_ifam x |> Geneweb_util.Mutil.encode)
    fexcl "fexcl";
  Util.hidden_input conf "i"
    (Gwdb.string_of_iper ip1 |> Geneweb_util.Mutil.encode);
  Util.hidden_input conf "select"
    (Gwdb.string_of_iper ip2 |> Geneweb_util.Mutil.encode);
  print_submit conf "answer_y" 0;
  print_submit conf "answer_n" 1;
  Output.print_sstring conf "</form></p>";
  Hutil.trailer conf

let print_cand_fam conf base (ip, p) (iexcl, fexcl) ifam1 ifam2 =
  let title _ =
    Util.transl_nth conf "family/families" 1
    |> Util.transl_decline conf "merge"
    |> Geneweb_util.Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Output.print_sstring conf "<h2>";
  title false;
  Output.print_sstring conf "</h2>";
  Hutil.print_link_to_welcome conf true;
  let ip1, ip2 =
    let cpl = Gwdb.foi base ifam1 in
    (Gwdb.get_father cpl, Gwdb.get_mother cpl)
  in
  Output.print_sstring conf "<ul><li>";
  print_link conf base (Gwdb.poi base ip1);
  Output.print_sstring conf " &amp; ";
  print_link conf base (Gwdb.poi base ip2);
  Output.print_sstring conf "</li><li>";
  print_link conf base (Gwdb.poi base ip1);
  Output.print_sstring conf " &amp; ";
  print_link conf base (Gwdb.poi base ip2);
  Output.print_sstring conf "</li></ul><p>";
  Output.print_sstring conf
    (Geneweb_util.Utf8.capitalize_fst (Util.transl conf "merge"));
  Output.print_sstring conf " ? ";
  Output.print_sstring conf {|<form method="post" action="|};
  Output.print_sstring conf conf.Config.command;
  Output.print_sstring conf {|">|};
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "MRG_DUP_FAM_Y_N");
  Util.hidden_input conf "ip"
    (Gwdb.string_of_iper ip |> Geneweb_util.Mutil.encode);
  print_input_excl conf
    (fun x -> Gwdb.string_of_iper x |> Geneweb_util.Mutil.encode)
    iexcl "iexcl";
  print_input_excl conf
    (fun x -> Gwdb.string_of_ifam x |> Geneweb_util.Mutil.encode)
    ((ifam1, ifam2) :: fexcl) "fexcl";
  Util.hidden_input conf "i"
    (Gwdb.string_of_ifam ifam1 |> Geneweb_util.Mutil.encode);
  Util.hidden_input conf "i2"
    (Gwdb.string_of_ifam ifam2 |> Geneweb_util.Mutil.encode);
  print_submit conf "answer_y" 0;
  print_submit conf "answer_n" 1;
  Output.print_sstring conf "</form></p>";
  Hutil.trailer conf

let main_page conf base =
  let ipp =
    match Util.p_getenv conf.Config.env "ip" with
    | Some i ->
        let i = Gwdb.iper_of_string i in
        Some (i, Gwdb.poi base i)
    | None -> None
  in
  let excl = Perso.excluded_possible_duplications conf in
  match ipp with
  | Some (ip, p) -> (
      match Perso.first_possible_duplication base ip excl with
      | Perso.DupInd (ip1, ip2) -> print_cand_ind conf base (ip, p) excl ip1 ip2
      | Perso.DupFam (ifam1, ifam2) ->
          print_cand_fam conf base (ip, p) excl ifam1 ifam2
      | Perso.NoDup -> print_no_candidate conf base p)
  | None -> Hutil.incorrect_request conf

let answ_ind_y_n conf base =
  let yes = Util.p_getenv conf.Config.env "answer_y" <> None in
  if yes then MergeIndDisplay.print conf base else main_page conf base

let answ_fam_y_n conf base =
  let yes = Util.p_getenv conf.Config.env "answer_y" <> None in
  if yes then MergeFamDisplay.print conf base else main_page conf base
