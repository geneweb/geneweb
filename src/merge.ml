(* camlp5r ./pa_html.cmo *)
(* $Id: merge.ml,v 5.13 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open Util;

value print_someone conf base p =
  Wserver.wprint "%s%s %s" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)
;

value print conf base p =
  let title h =
    do {
      Wserver.wprint "%s" (capitale (transl_decline conf "merge" ""));
      if h then ()
      else do { Wserver.wprint ": "; print_someone conf base p; };
    }
  in
  let list = Gutil.find_same_name base p in
  let list =
    List.fold_right
      (fun p1 pl ->
         if get_key_index p1 = get_key_index p then pl else [p1 :: pl])
      list []
  in
  do {
    header conf title;
    Wserver.wprint "\n";
    tag "form" "method=\"get\" action=\"%s\"" conf.command begin
      tag "p" begin
        Util.hidden_env conf;
        xtag "input" "type=\"hidden\" name=\"m\" value=\"MRG_IND\"";
        xtag "input" "type=\"hidden\" name=\"i\" value=\"%d\""
          (Adef.int_of_iper (get_key_index p));
        Wserver.wprint "%s " (capitale (transl_decline conf "with" ""));
        if list <> [] then do {
          Wserver.wprint ":";
          xtag "br";
          xtag "input" "\
type=\"radio\" name=\"select\" value=\"input\" checked=\"checked\"";
        }
        else ();
        Wserver.wprint "(%s . %s %s):\n"
          (transl_nth conf "first name/first names" 0) (transl conf "number")
          (transl_nth conf "surname/surnames" 0);
        xtag "input" "name=\"n\" size=\"30\" maxlength=\"200\"";
        xtag "br";
      end;
      if list <> [] then
        Wserver.wprint
          "<table border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\n"
      else ();
      List.iter
        (fun p ->
           tag "tr" "align=\"%s\"" conf.left begin
             tag "td" "valign=\"top\"" begin
               xtag "input" "type=\"radio\" name=\"select\" value=\"%d\""
                 (Adef.int_of_iper (get_key_index p));
             end;
             tag "td" begin
               stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p) begin
                 Wserver.wprint "%s.%d %s" (sou base (get_first_name p))
                   (get_occ p) (sou base (get_surname p));
               end;
               Wserver.wprint "%s" (Date.short_dates_text conf base p);
               match main_title conf base p with
               [ Some t -> Wserver.wprint "%s" (one_title_text conf base p t)
               | None -> () ];
               match get_parents p with
               [ Some ifam ->
                   let cpl = foi base ifam in
                   Wserver.wprint ",\n%s"
                     (Util.translate_eval
                       (transl_a_of_b conf
                          (transl_nth conf "son/daughter/child"
                             (index_of_sex (get_sex p)))
                          (person_title_text conf base
                             (poi base (get_father cpl)) ^
                             " " ^ transl_nth conf "and" 0 ^ " " ^
                             person_title_text conf base
                               (poi base (get_mother cpl)))))
               | None -> () ];
               xtag "br";
             end;
           end)
        list;
      if list <> [] then Wserver.wprint "</table>\n" else ();
      tag "p" begin
        xtag "input" "type=\"submit\" value=\"Ok\"";
      end;
    end;
    trailer conf;
  }
;

value print_possible_continue_merging conf base =
  match (p_getint conf.env "ini1", p_getint conf.env "ini2") with
  [ (Some ini1, Some ini2) -> do {
      let p1 = poi base (Adef.iper_of_int ini1) in
      let p2 = poi base (Adef.iper_of_int ini2) in
      Wserver.wprint "\n";
      html_p conf;
      stag "a" "href=%sm=MRG_IND;i=%d;i2=%d" (commd conf) ini1 ini2 begin
        Wserver.wprint "%s" (capitale (transl conf "continue merging"));
      end;
      Wserver.wprint "\n";
      print_someone conf base p1;
      Wserver.wprint "\n%s\n" (transl_nth conf "and" 0);
      print_someone conf base p2;
      Wserver.wprint "\n";
    }
  | _ ->
      match p_getint conf.env "ip" with
      [ Some ip ->
          let s1 =
            match p_getenv conf.env "iexcl" with
            [ Some "" | None -> ""
            | Some s -> ";iexcl=" ^ s ]
          in
          let s2 =
            match p_getenv conf.env "fexcl" with
            [ Some "" | None -> ""
            | Some s -> ";fexcl=" ^ s ]
          in
          if s1 <> "" || s2 <> "" then
            tag "p" begin
              stag "a" "href=%sm=MRG_DUP;ip=%d%s%s" (commd conf) ip s1 s2
              begin
                Wserver.wprint "%s"
                  (capitale (transl conf "continue merging"));
              end;
              Wserver.wprint "\n(%s)\n"
                (transl_a_of_b conf
                   (transl conf "possible duplications")
                   (referenced_person_text conf base
                      (poi base (Adef.iper_of_int ip))));
            end
          else ()
      | None -> () ] ]
;
