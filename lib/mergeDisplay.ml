(* $Id: merge.ml, v7-exp 2018-09-26 07:34:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

let print_someone conf base p =
  Output.printf conf "%s%s %s" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)

let print conf base p =
  let title h =
    Output.print_string conf (Utf8.capitalize (transl_decline conf "merge" ""));
    if h then ()
    else
      begin
        Output.printf conf " ";
        print_someone conf base p;
        Output.printf conf " %s%s" (transl_decline conf "with" "")
          (transl conf ":");
      end
  in
  let list = Gutil.find_same_name base p in
  let list =
    List.fold_right
      (fun p1 pl ->
         if get_iper p1 = get_iper p then pl else p1 :: pl)
      list []
  in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Output.printf conf "<h2>";
  title false;
  Output.printf conf "</h2>\n";
  Output.printf conf "<form method=\"get\" action=\"%s\" \
class=\"mx-3 mb-3\">\n" conf.command;
  Util.hidden_env conf;
  Output.printf conf "<input type=\"hidden\" name=\"m\" value=\"MRG_IND\">\n";
  Output.printf conf "<input type=\"hidden\" name=\"i\" value=\"%s\">\n"
    (string_of_iper (get_iper p));
  Output.printf conf "<span class=\"form-row align-items-center\">\n";
  Output.printf conf "<span class=\"col-auto\">\n";
  Output.printf conf "<span class=\"custom-control custom-radio\">\n";
  Output.printf conf "  <input type=\"radio\" class=\"custom-control-input\" \
name=\"select\" id=\"input\" value=\"input\" checked>\n";
  Output.printf conf "  <label class=\"custom-control-label\" \
for=\"input\">%s</label>\n" (transl conf "any individual in the base");
  Output.printf conf "</span>\n</span>\n";
  Output.printf conf "<span class=\"col-auto\">\n";
  Output.printf conf "<input type=\"text\" class=\"form-control\" \
name=\"n\" placeholder=\"%s.%s %s\" title=\"%s.%s %s\" \
size=\"50\" id=\"inlineinput\" autofocus>\n</span>\n"
    (transl_nth conf "first name/first names" 0)
    (transl conf "number") (transl_nth conf "surname/surnames" 0)
    (transl_nth conf "first name/first names" 0)
    (transl conf "number") (transl_nth conf "surname/surnames" 0);
  Output.printf conf "</span>\n";
  if list <> [] then
    List.iter
      (fun p ->
         Output.printf conf "<div class=\"custom-control custom-radio\">\n";
         Output.printf conf
           "  <input type=\"radio\" class=\"custom-control-input\" \
name=\"select\" id=\"%s\" value=\"%s\">\n" (string_of_iper (get_iper p))
(string_of_iper (get_iper p));
         Output.printf conf "  <label class=\"custom-control-label\" \
for=\"%s\">" (string_of_iper (get_iper p));
         Update.print_person_parents_and_spouse conf base p;
         Output.printf conf "  </label>\n</div>\n";)
    list;
  Output.printf conf "<button type=\"submit\" \
 class=\"btn btn-primary btn-lg mt-2\">";
  Output.print_string conf (Utf8.capitalize (transl_nth conf "validate/delete" 0));
  Output.printf conf "</button>\n";
  Output.printf conf "</form>\n";
  Hutil.trailer conf

let print_possible_continue_merging conf base =
  match p_getenv conf.env "ini1", p_getenv conf.env "ini2" with
    Some ini1, Some ini2 ->
      let ini1 = iper_of_string ini1 in
      let ini2 = iper_of_string ini2 in
      let p1 = poi base ini1 in
      let p2 = poi base ini2 in
      Output.printf conf {|<p><a href="%sm=MRG_IND&i=%s&i2=%s">%s</a>\n|}
        (commd conf)
        (string_of_iper ini1)
        (string_of_iper ini2)
        (Utf8.capitalize (transl conf "continue merging"));
      print_someone conf base p1;
      Output.printf conf "\n%s\n" (transl_nth conf "and" 0);
      print_someone conf base p2;
      Output.printf conf "</p>\n"
  | _ ->
      match p_getenv conf.env "ip" with
        Some ip ->
          let ip = iper_of_string ip in
          let s1 =
            match p_getenv conf.env "iexcl" with
              Some "" | None -> ""
            | Some s -> "&iexcl=" ^ s
          in
          let s2 =
            match p_getenv conf.env "fexcl" with
              Some "" | None -> ""
            | Some s -> "&fexcl=" ^ s
          in
          if s1 <> "" || s2 <> "" then
            begin
              let p = poi base ip in
              let s = person_text conf base p in
              Output.printf conf {|<p><a href="%sm=MRG_DUP&ip=%s%s%s">%s</a> (%s)</p>|}
                (commd conf) (string_of_iper ip) s1 s2
                (Utf8.capitalize (transl conf "continue merging"))
                (transl_a_of_b conf
                   (transl conf "possible duplications")
                   (reference conf base p s) s)
            end
      | None -> ()
