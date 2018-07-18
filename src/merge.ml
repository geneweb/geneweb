(* $Id: merge.ml,v 5.13 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Hutil
open Util

let print_someone conf base p =
  Wserver.printf "%s%s %s" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)

let print conf base p =
  let title h =
    Wserver.printf "%s" (capitale (transl_decline conf "merge" ""));
    if h then ()
    else
      begin
        Wserver.printf "%s " (transl conf ":");
        print_someone conf base p
      end
  in
  let list = Gutil.find_same_name base p in
  let list =
    List.fold_right
      (fun p1 pl ->
         if get_key_index p1 = get_key_index p then pl else p1 :: pl)
      list []
  in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Wserver.printf "<h2>\n";
  title false;
  Wserver.printf "</h2>\n";
  Wserver.printf "\n";
  Wserver.printf "<form method=\"get\" action=\"%s\">\n" conf.command;
  Wserver.printf "<p>\n";
  Util.hidden_env conf;
  Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"MRG_IND\"%s>\n"
    conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"i\" value=\"%d\"%s>\n"
    (Adef.int_of_iper (get_key_index p)) conf.xhs;
  Wserver.printf "%s " (capitale (transl_decline conf "with" ""));
  if list <> [] then
    begin
      Wserver.printf "%s" (transl conf ":");
      Wserver.printf "<br%s>\n" conf.xhs;
      Wserver.printf "<input \
type=\"radio\" class=\"form-control\" name=\"select\" value=\"input\" checked%s>\n"
        conf.xhs
    end;
  Wserver.printf "(%s.%s %s)%s\n" (transl_nth conf "first name/first names" 0)
    (transl conf "number") (transl_nth conf "surname/surnames" 0)
    (transl conf ":");
  Wserver.printf
    "<input class=\"form-control\" name=\"n\" size=\"30\" maxlength=\"200\"%s>\n"
    conf.xhs;
  Wserver.printf "<br%s>\n" conf.xhs;
  Wserver.printf "</p>\n";
  if list <> [] then
    Wserver.printf
      "<table border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\n";
  List.iter
    (fun p ->
       Wserver.printf "<tr align=\"%s\">\n" conf.left;
       Wserver.printf "<td valign=\"top\">\n";
       Wserver.printf
         "<input type=\"radio\" name=\"select\" value=\"%d\"%s>\n"
         (Adef.int_of_iper (get_key_index p)) conf.xhs;
       Wserver.printf "</td>\n";
       Wserver.printf "<td>\n";
       Update.print_person_parents_and_spouses conf base p;
       Wserver.printf "<br%s>\n" conf.xhs;
       Wserver.printf "</td>\n";
       Wserver.printf "</tr>\n")
    list;
  if list <> [] then Wserver.printf "</table>\n";
  Wserver.printf
    "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
  Wserver.printf "%s" (capitale (transl_nth conf "validate/delete" 0));
  Wserver.printf "</button>\n";
  Wserver.printf "</form>\n";
  trailer conf

let print_possible_continue_merging conf base =
  match p_getint conf.env "ini1", p_getint conf.env "ini2" with
    Some ini1, Some ini2 ->
      let p1 = poi base (Adef.iper_of_int ini1) in
      let p2 = poi base (Adef.iper_of_int ini2) in
      Wserver.printf "\n";
      html_p conf;
      Wserver.printf "<a href=%sm=MRG_IND;i=%d;i2=%d>" (commd conf) ini1 ini2;
      Wserver.printf "%s" (capitale (transl conf "continue merging"));
      Wserver.printf "</a>";
      Wserver.printf "\n";
      print_someone conf base p1;
      Wserver.printf "\n%s\n" (transl_nth conf "and" 0);
      print_someone conf base p2;
      Wserver.printf "\n"
  | _ ->
      match p_getint conf.env "ip" with
        Some ip ->
          let s1 =
            match p_getenv conf.env "iexcl" with
              Some "" | None -> ""
            | Some s -> ";iexcl=" ^ s
          in
          let s2 =
            match p_getenv conf.env "fexcl" with
              Some "" | None -> ""
            | Some s -> ";fexcl=" ^ s
          in
          if s1 <> "" || s2 <> "" then
            begin
              Wserver.printf "<p>\n";
              begin
                Wserver.printf "<a href=%sm=MRG_DUP;ip=%d%s%s>" (commd conf)
                  ip s1 s2;
                Wserver.printf "%s"
                  (capitale (transl conf "continue merging"));
                Wserver.printf "</a>"
              end;
              Wserver.printf "\n(%s)\n"
                (transl_a_of_b conf (transl conf "possible duplications")
                   (referenced_person_text conf base
                      (poi base (Adef.iper_of_int ip))));
              Wserver.printf "</p>\n"
            end
      | None -> ()
