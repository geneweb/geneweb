(* camlp4r ./pa_html.cmo *)
(* $Id: mergeChil.ml,v 5.2 2007-01-11 15:29:56 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config;
open Gwdb;
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
         (transl_decline conf "merge"
            (sprintf "%s %s %s"
               (transl_nth conf "spouse/spouses" 1)
               (transl conf "or")
               (transl_nth conf "child/children" 1))))
  in
  Util.header conf title;
  Util.print_link_to_welcome conf True;
  Wserver.wprint "%s\n" (capitale (transl conf "not found"));
  tag "ul" begin
    tag "li" begin print_link conf base p; end;
  end;
  Util.trailer conf;
};

value print_candidates conf base (ip, p) iexcl (ip1, p1) (ip2, p2) = do {
  let title _ =
    Wserver.wprint "%s\n"
      (capitale
         (transl_decline conf "merge"
            (sprintf "%s %s %s"
               (transl_nth conf "spouse/spouses" 1)
               (transl conf "or")
               (transl_nth conf "child/children" 1))))
  in
  Util.header conf title;
  Util.print_link_to_welcome conf True;
  tag "ul" begin
    tag "li" begin print_link conf base p1; end;
    tag "li" begin print_link conf base p2; end;
  end;
  tag "p" begin
    Wserver.wprint "%s ?\n" (capitale (transl conf "merge"));
    tag "form" begin
      Util.hidden_env conf;
      xtag "input" "type=\"hidden\" name=\"m\" value=\"MRG_CHN_Y_N\"";
      xtag "input" "type=\"hidden\" name=\"ip\" value=\"%d\""
        (Adef.int_of_iper ip);
      let s =
        List.fold_left
          (fun s (i1, i2) ->
             let t =
               string_of_int (Adef.int_of_iper i1) ^ "," ^
               string_of_int (Adef.int_of_iper i2)
             in
             if s = "" then t else s ^ "," ^ t)
          "" [(ip1, ip2) :: iexcl]
      in
      if s = "" then ()
      else xtag "input" "type=\"hidden\" name=\"iexcl\" value=\"%s\"" s;
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
  Util.trailer conf;
};

value main_page conf base =
  let ipp =
    match p_getint conf.env "ip" with
    [ Some i -> Some (Adef.iper_of_int i, poi base (Adef.iper_of_int i))
    | None -> None ]
  in
  let iexcl = Perso.excluded_mergeable_candidates conf in
  match ipp with
  [ Some (ip, p) ->
      match Perso.first_mergeable_candidates base ip iexcl with
      [ Some (ipp1, ipp2) ->
          print_candidates conf base (ip, p) iexcl ipp1 ipp2
      | None -> 
          print_no_candidate conf base (ip, p) ]
 | None ->
      incorrect_request conf ]
;

value answered_y_n conf base =
  let yes = p_getenv conf.env "answer_y" <> None in
  if yes then MergeInd.print conf base
  else main_page conf base
;
