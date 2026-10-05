(* Copyright (c) 2007 INRIA *)

let print_link ?(with_occurrence_number = true) ?(with_life_dates = true)
    ?(with_main_title = true) conf base p =
  Output.print_sstring conf "<a href=\"";
  Output.print_url conf (Util.commd' conf ~query:(Util.acces conf base p));
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
  match Person.main_title conf base p with
  | Some t ->
      if with_main_title then
        Output.print_string conf (Util.one_title_text base t)
  | None -> ()

let print_no_candidate conf base p =
  let title _ =
    Util.transl conf "possible duplications"
    |> Util.transl_decline conf "merge"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
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

let next_step_buttons ~conf ~cancel_url next_url =
  Output.printf conf
    {|<p><a class="button secondary" style="margin-right:4px" href="%s">%s</a><a class="button bare" href="%s">%s</a></p>|}
    (Localized_url.to_string next_url)
    (Utf8.capitalize_fst @@ Util.transl conf "merge")
    (Localized_url.to_string cancel_url)
    (Utf8.capitalize_fst @@ Util.transl_nth conf "user/password/cancel" 2)

let print_cand_ind conf base (ip, p) (iexcl, fexcl) ip1 ip2 =
  let title _ =
    Util.transl conf "merge" |> Utf8.capitalize_fst |> Output.print_sstring conf
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
  Output.print_sstring conf "</li></ul>";
  let make_url mode =
    let open Ext_list.Infix in
    Util.commd conf
      ~query:
        (("m", [ mode ])
        @:: ("ip", [ Gwdb.string_of_iper ip ])
        @:: ( "iexcl",
              List.concat_map
                (fun (p1, p2) ->
                  [ Gwdb.string_of_iper p1; Gwdb.string_of_iper p2 ])
                ((ip1, ip2) :: iexcl) )
        @:: Ext_option.return_if (fexcl <> []) (fun () ->
                ( "fexcl",
                  List.concat_map
                    (fun (f1, f2) ->
                      [ Gwdb.string_of_ifam f1; Gwdb.string_of_ifam f2 ])
                    fexcl ))
        @?: ("i", [ Gwdb.string_of_iper ip1 ])
        @:: [ ("select", [ Gwdb.string_of_iper ip2 ]) ])
  in

  next_step_buttons ~conf ~cancel_url:(make_url "MRG_DUP")
    (make_url "MRG_DUP_IND_Y_N");
  Hutil.trailer conf

let print_cand_fam conf base (ip, p) (iexcl, fexcl) ifam1 ifam2 =
  let title _ =
    Util.transl_nth conf "family/families" 1
    |> Util.transl_decline conf "merge"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
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
  Output.print_sstring conf "</li></ul>";
  let make_url mode =
    let open Ext_list.Infix in
    Util.commd conf
      ~query:
        (("m", [ mode ])
        @:: ("ip", [ Gwdb.string_of_iper ip ])
        @:: Ext_option.return_if (iexcl <> []) (fun () ->
                ( "iexcl",
                  List.concat_map
                    (fun (p1, p2) ->
                      [ Gwdb.string_of_iper p1; Gwdb.string_of_iper p2 ])
                    iexcl ))
        @?: ( "fexcl",
              List.concat_map
                (fun (f1, f2) ->
                  [ Gwdb.string_of_ifam f1; Gwdb.string_of_ifam f2 ])
                ((ifam1, ifam2) :: fexcl) )
        @:: ("i", [ Gwdb.string_of_ifam ifam1 ])
        @:: [ ("i2", [ Gwdb.string_of_ifam ifam2 ]) ])
  in
  next_step_buttons ~conf ~cancel_url:(make_url "MRG_DUP")
    (make_url "MRG_DUP_FAM_Y_N");
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
