(* camlp5r ./pa_html.cmo *)
(* $Id: mergeDup.ml,v 5.9 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config;
open Gwdb;
open Hutil;
open Util;

value print_link conf base p = do {
  stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p) begin
    Wserver.wprint "%s.%d %s" (sou base (get_first_name p))
      (get_occ p) (sou base (get_surname p));
  end;
  Wserver.wprint "%s" (Date.short_dates_text conf base p);
  match main_title conf base p with
  [ Some t -> Wserver.wprint "%s" (one_title_text conf base p t)
  | None -> () ];
};

value print_no_candidate conf base (ip, p) = do {
  let title _ =
    Wserver.wprint "%s\n"
      (capitale
         (transl_decline conf "merge" (transl conf "possible duplications")))
  in
  Wserver.wrap_string.val := Util.xml_pretty_print;
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf True;
  Wserver.wprint "%s\n" (capitale (transl conf "not found"));
  tag "ul" begin
    tag "li" begin print_link conf base p; end;
  end;
  Hutil.trailer conf;
};

value input_excl int_of_i excl =
  List.fold_left
    (fun s (i1, i2) ->
       let t =
         string_of_int (int_of_i i1) ^ "," ^ string_of_int (int_of_i i2)
       in
       if s = "" then t else s ^ "," ^ t)
    "" excl
;

value print_input_excl conf int_of_i excl excl_name =
  let s = input_excl int_of_i excl in
  if s = "" then ()
  else xtag "input" "type=\"hidden\" name=\"%s\" value=\"%s\"" excl_name s
;

value print_cand_ind conf base (ip, p) (iexcl, fexcl) ip1 ip2 = do {
  let title _ =
    Wserver.wprint "%s\n" (capitale (transl conf "merge"))
  in
  Wserver.wrap_string.val := Util.xml_pretty_print;
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  tag "h2" begin title False; end;
  Hutil.print_link_to_welcome conf True;
  tag "ul" begin
    tag "li" begin print_link conf base (poi base ip1); end;
    tag "li" begin print_link conf base (poi base ip2); end;
  end;
  tag "p" begin
    Wserver.wprint "%s ?\n" (capitale (transl conf "merge"));
    tag "form" "method=\"post\" action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      xtag "input" "type=\"hidden\" name=\"m\" value=\"MRG_DUP_IND_Y_N\"";
      xtag "input" "type=\"hidden\" name=\"ip\" value=\"%d\""
        (Adef.int_of_iper ip);
      print_input_excl conf Adef.int_of_iper [(ip1, ip2) :: iexcl] "iexcl";
      print_input_excl conf Adef.int_of_ifam fexcl "fexcl";
      xtag "input" "type=\"hidden\" name=\"i\" value=\"%d\""
        (Adef.int_of_iper ip1);
      xtag "input" "type=\"hidden\" name=\"select\" value=\"%d\""
        (Adef.int_of_iper ip2);
      xtag "input" "type=\"submit\" name=\"answer_y\" value=\"%s\""
        (transl_nth conf "Y/N" 0);
      xtag "input" "type=\"submit\" name=\"answer_n\" value=\"%s\""
        (transl_nth conf "Y/N" 1);
    end;
  end;
  Hutil.trailer conf;
};

value print_cand_fam conf base (ip, p) (iexcl, fexcl) ifam1 ifam2 = do {
  let title _ =
    Wserver.wprint "%s\n"
      (capitale
         (transl_decline conf "merge" (transl_nth conf "family/families" 1)))
  in
  Wserver.wrap_string.val := Util.xml_pretty_print;
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  tag "h2" begin title False; end;
  Hutil.print_link_to_welcome conf True;
  let (ip1, ip2) =
    let cpl = foi base ifam1 in
    (Gwdb.get_father cpl, Gwdb.get_mother cpl)
  in
  tag "ul" begin
    tag "li" begin
      print_link conf base (poi base ip1);
      Wserver.wprint "\n&amp;\n";
      print_link conf base (poi base ip2);
    end;
    tag "li" begin
      print_link conf base (poi base ip1);
      Wserver.wprint "\n&amp;\n";
      print_link conf base (poi base ip2);
    end;
  end;
  tag "p" begin
    Wserver.wprint "%s ?\n" (capitale (transl conf "merge"));
    tag "form" "method=\"post\" action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      xtag "input" "type=\"hidden\" name=\"m\" value=\"MRG_DUP_FAM_Y_N\"";
      xtag "input" "type=\"hidden\" name=\"ip\" value=\"%d\""
        (Adef.int_of_iper ip);
      print_input_excl conf Adef.int_of_iper iexcl "iexcl";
      print_input_excl conf Adef.int_of_ifam [(ifam1, ifam2) :: fexcl]
        "fexcl";
      xtag "input" "type=\"hidden\" name=\"i\" value=\"%d\""
        (Adef.int_of_ifam ifam1);
      xtag "input" "type=\"hidden\" name=\"i2\" value=\"%d\""
        (Adef.int_of_ifam ifam2);
      xtag "input" "type=\"submit\" name=\"answer_y\" value=\"%s\""
        (transl_nth conf "Y/N" 0);
      xtag "input" "type=\"submit\" name=\"answer_n\" value=\"%s\""
        (transl_nth conf "Y/N" 1);
    end;
  end;
  Hutil.trailer conf;
};

value main_page conf base =
  let ipp =
    match p_getint conf.env "ip" with
    [ Some i -> Some (Adef.iper_of_int i, poi base (Adef.iper_of_int i))
    | None -> None ]
  in
  let excl = Perso.excluded_possible_duplications conf in
  match ipp with
  [ Some (ip, p) ->
      match Perso.first_possible_duplication base ip excl with
      [ Perso.DupInd ip1 ip2 ->
          print_cand_ind conf base (ip, p) excl ip1 ip2
      | Perso.DupFam ifam1 ifam2 ->
          print_cand_fam conf base (ip, p) excl ifam1 ifam2
      | Perso.NoDup ->
          print_no_candidate conf base (ip, p) ]
 | None ->
      incorrect_request conf ]
;

value answ_ind_y_n conf base =
  let yes = p_getenv conf.env "answer_y" <> None in
  if yes then MergeInd.print conf base else main_page conf base
;

value answ_fam_y_n conf base =
  let yes = p_getenv conf.env "answer_y" <> None in
  if yes then MergeFam.print conf base else main_page conf base
;
