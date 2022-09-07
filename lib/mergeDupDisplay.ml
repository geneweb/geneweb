(* Copyright (c) 2007 INRIA *)

open Def
open Config
open Gwdb
open Util

let print_link conf base p =
  Output.print_sstring conf "<a href=\"";
  Output.print_string conf (commd conf);
  Output.print_string conf (acces conf base p);
  Output.print_sstring conf "\">";
  Output.print_string conf (get_first_name p |> sou base |> escape_html);
  Output.print_sstring conf ".";
  Output.print_sstring conf (get_occ p |> string_of_int);
  Output.print_sstring conf " ";
  Output.print_string conf (get_surname p |> sou base |> escape_html);
  Output.print_sstring conf "</a>";
  Output.print_string conf (DateDisplay.short_dates_text conf base p);
  match main_title conf base p with
  | Some t -> Output.print_string conf (one_title_text base t)
  | None -> ()

let print_no_candidate conf base p =
  let title _ =
    transl conf "possible duplications"
    |> transl_decline conf "merge"
    |> Utf8.capitalize_fst
    |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  transl conf "not found" |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "<ul><li>";
  print_link conf base p;
  Output.print_sstring conf "</li></ul>";
  Hutil.trailer conf

let input_excl string_of_i excl =
  List.fold_left begin fun (s : Adef.encoded_string) (i1, i2) ->
    let t = string_of_i i1 ^^^ "," ^<^ string_of_i i2 in
    if (s :> string) = "" then t else s ^^^ "," ^<^ t
  end (Adef.encoded "") excl

let print_input_excl conf string_of_i excl excl_name =
  let s = input_excl string_of_i excl in
  if (s :> string) <> "" then Util.hidden_input conf excl_name s

let print_submit conf name value =
  Output.print_sstring conf {|<input type="submit" name="|};
  Output.print_sstring conf name;
  Output.print_sstring conf {|" value="|};
  Output.print_sstring conf (transl_nth conf "Y/N" value);
  Output.print_sstring conf {|">|}

let print_cand_ind conf base (ip, p) (iexcl, fexcl) ip1 ip2 =
  let title _ = transl conf "merge" |> Utf8.capitalize_fst |> Output.print_sstring conf in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Output.print_sstring conf "<h2>";
  title false;
  Output.print_sstring conf "</h2>";
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<ul><li>";
  print_link conf base (poi base ip1);
  Output.print_sstring conf "</li><li>";
  print_link conf base (poi base ip2);
  Output.print_sstring conf "</li></ul><p>";
  transl conf "merge" |> Utf8.capitalize_fst |> Output.print_sstring conf ;
  Output.print_sstring conf " ?\n" ; (* FIXME: trans *)
  Output.print_sstring conf {|<form method="post" action="|} ;
  Output.print_sstring conf conf.command;
  Output.print_sstring conf {|">|} ;
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "MRG_DUP_IND_Y_N");
  Util.hidden_input conf "ip" (string_of_iper ip |> Mutil.encode);
  print_input_excl conf (fun x -> string_of_iper x |> Mutil.encode) ((ip1, ip2) :: iexcl) "iexcl";
  print_input_excl conf (fun x -> string_of_ifam x |> Mutil.encode) fexcl "fexcl";
  Util.hidden_input conf "i" (string_of_iper ip1 |> Mutil.encode);
  Util.hidden_input conf "select" (string_of_iper ip2 |> Mutil.encode);
  print_submit conf "answer_y" 0;
  print_submit conf "answer_n" 1;
  Output.print_sstring conf "</form></p>";
  Hutil.trailer conf

let print_cand_fam conf base (ip, p) (iexcl, fexcl) ifam1 ifam2 =
  let title _ =
    transl_nth conf "family/families" 1
    |> transl_decline conf "merge"
    |> Utf8.capitalize_fst
    |> Output.print_sstring conf
  in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Output.print_sstring conf "<h2>";
  title false;
  Output.print_sstring conf "</h2>";
  Hutil.print_link_to_welcome conf true;
  let (ip1, ip2) =
    let cpl = foi base ifam1 in Gwdb.get_father cpl, Gwdb.get_mother cpl
  in
  Output.print_sstring conf "<ul><li>";
  print_link conf base (poi base ip1);
  Output.print_sstring conf " &amp; ";
  print_link conf base (poi base ip2);
  Output.print_sstring conf "</li><li>";
  print_link conf base (poi base ip1);
  Output.print_sstring conf " &amp; ";
  print_link conf base (poi base ip2);
  Output.print_sstring conf "</li></ul><p>";
  Output.print_sstring conf (Utf8.capitalize_fst (transl conf "merge"));
  Output.print_sstring conf " ? ";
  Output.print_sstring conf {|<form method="post" action="|};
  Output.print_sstring conf conf.command;
  Output.print_sstring conf {|">|};
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "MRG_DUP_FAM_Y_N") ;
  Util.hidden_input conf "ip" (string_of_iper ip |> Mutil.encode) ;
  print_input_excl conf (fun x -> string_of_iper x |> Mutil.encode) iexcl "iexcl";
  print_input_excl conf (fun x -> string_of_ifam x |> Mutil.encode) ((ifam1, ifam2) :: fexcl) "fexcl";
  Util.hidden_input conf "i" (string_of_ifam ifam1 |> Mutil.encode) ;
  Util.hidden_input conf "i2" (string_of_ifam ifam2 |> Mutil.encode) ;
  print_submit conf "answer_y" 0;
  print_submit conf "answer_n" 1;
  Output.print_sstring conf "</form></p>";
  Hutil.trailer conf

let main_page conf base =
  let ipp =
    match p_getenv conf.env "ip" with
    | Some i -> let i = iper_of_string i in Some (i, poi base i)
    | None -> None
  in
  let excl = Perso.excluded_possible_duplications conf in
  match ipp with
    Some (ip, p) ->
      begin match Perso.first_possible_duplication base ip excl with
        Perso.DupInd (ip1, ip2) ->
          print_cand_ind conf base (ip, p) excl ip1 ip2
      | Perso.DupFam (ifam1, ifam2) ->
          print_cand_fam conf base (ip, p) excl ifam1 ifam2
      | Perso.NoDup -> print_no_candidate conf base p
      end
  | None -> Hutil.incorrect_request conf

let answ_ind_y_n conf base =
  let yes = p_getenv conf.env "answer_y" <> None in
  if yes then MergeIndDisplay.print conf base else main_page conf base

let answ_fam_y_n conf base =
  let yes = p_getenv conf.env "answer_y" <> None in
  if yes then MergeFamDisplay.print conf base else main_page conf base
