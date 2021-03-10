(* Copyright (c) 1998-2018 INRIA *)

open Config
open Def
open Gwdb
open Util
open ChangeChildren

let print_child_person conf base p =
  let var = "c" ^ string_of_iper (get_iper p) in
  let first_name =
    match p_getenv conf.env (var ^ "_first_name") with
      Some v -> v
    | None -> p_first_name base p
  in
  let surname =
    match p_getenv conf.env (var ^ "_surname") with
      Some v -> v
    | None -> p_surname base p
  in
  let occ =
    match p_getint conf.env (var ^ "_occ") with
      Some i -> i
    | None -> get_occ p
  in
  Output.print_string conf "<table class=\"m1-2\">\n";
  Output.print_string conf "<tbody>\n";
  Output.printf conf "<tr align=\"%s\">\n" conf.left;
  Output.print_string conf "<td>";
  Output.printf conf "<label for=\"%s_fn\" class=\"mx-2 mb-0\">%s</label>"
    var (Utf8.capitalize_fst (transl_nth conf "first name/first names" 0));
  Output.print_string conf "</td>\n";
  Output.print_string conf "<td colspan=\"3\">\n";
  Output.printf conf "<input name=\"%s_first_name\" class=\"form-control\" \
size=\"23\" maxlength=\"200\" id=\"%s_fn\" value=\"%s\">\n" var var
(Util.escape_html first_name);
  Output.print_string conf "</td>\n";
  Output.printf conf "<td align=\"%s\">" conf.right;
  Output.printf conf "<label for=\"%s_occ\" class=\"mx-2 mb-0\">%s</label>"
    var (Utf8.capitalize_fst (transl conf "number"));
  Output.print_string conf "</td>\n";
  Output.print_string conf "<td>\n";
  Output.printf conf "<input class=\"form-control\" id=\"%s_occ\" name=\"%s_occ\" \
size=\"5\" maxlength=\"8\"%s>\n" var var
(if occ = 0 then "" else " value=\"" ^ string_of_int occ ^ "\"");
  Output.print_string conf "</td>\n";
  Output.print_string conf "</tr>\n";
  Output.printf conf "<tr align=\"%s\">\n" conf.left;
  Output.print_string conf "<td>";
  Output.printf conf "<label for=\"%s_sn\" class=\"mx-2 mb-0\">%s</label>"
    var (Utf8.capitalize_fst (transl_nth conf "surname/surnames" 0));
  Output.print_string conf "</td>\n";
  Output.print_string conf "<td colspan=\"5\">\n";
  Output.printf conf "<input name=\"%s_surname\" class=\"form-control\" \
size=\"40\" maxlength=\"200\" id=\"%s_sn\" value=\"%s\">\n" var var surname;
  Output.print_string conf "</td>\n";
  Output.print_string conf "</tr>\n";
  Output.print_string conf "</tbody>\n";
  Output.print_string conf "</table>\n"


let print_children conf base ipl =
  Output.print_string conf "<ul>\n";
  List.iter
    (fun ip ->
       let p = poi base ip in
       Output.print_string conf "<li class=\"mt-3\">\n";
       Output.printf conf "<span class=\"ml-2\">%s"
         (reference conf base p (person_text conf base p));
       Output.printf conf "%s</span>\n" (DateDisplay.short_dates_text conf base p);
       print_child_person conf base p;
       Output.print_string conf "</li>\n")
    ipl;
  Output.print_string conf "</ul>\n"

let print_change conf base p =
  let title _ =
    let s = transl conf "change children's names" in
    Output.print_string conf (Utf8.capitalize_fst s)
  in
  let children = children_of_p base p in
  let digest = digest_children base children in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Output.print_string conf "<h2>";
    title false;
  begin
    let s = person_text conf base p in
    Output.print_string conf (Util.transl_a_of_b conf "" (reference conf base p s) s)
  end ;
  Output.printf conf " %s" (DateDisplay.short_dates_text conf base p);
  Output.print_string conf "</h2>\n";
  Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Util.hidden_env conf;
  Output.printf conf "<input type=\"hidden\" name=\"ip\" value=\"%s\">\n"
    (string_of_iper (get_iper p));
  Output.printf conf "<input type=\"hidden\" name=\"digest\" value=\"%s\">\n"
    digest;
  Output.print_string conf "<input type=\"hidden\" name=\"m\" value=\"CHG_CHN_OK\">\n";
  print_children conf base children;
  Output.print_string conf "\n";
  Output.print_string conf
    "<button type=\"submit\" class=\"btn btn-primary btn-lg ml-5 mb-2\">";
  Output.print_string conf (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0));
  Output.print_string conf "</button>\n";
  Output.print_string conf "</form>\n";
  Output.print_string conf "\n";
  Hutil.trailer conf

let print conf base =
  match p_getenv conf.env "ip" with
    Some i ->
      let p = poi base (iper_of_string i) in print_change conf base p
  | _ -> Hutil.incorrect_request conf

let print_children_list conf base u =
  Output.print_string conf "<h4>";
  Output.print_string conf (Utf8.capitalize_fst (transl_nth conf "child/children" 1));
  Output.print_string conf "</h4>";
  Output.print_string conf "\n<p>\n";
  Output.print_string conf "<ul>\n";
  Array.iter
    (fun ifam ->
       let des = foi base ifam in
       Array.iter
         (fun ip ->
            let p = poi base ip in
            Output.print_string conf "<li>" ;
            Output.printf conf "\n%s"
              (reference conf base p (person_text conf base p));
            Output.printf conf "%s\n" (DateDisplay.short_dates_text conf base p))
         (get_children des))
    (get_family u);
  Output.print_string conf "</ul>\n"

let print_change_done conf base p =
  let title _ =
    let s = transl conf "children's names changed" in
    Output.print_string conf (Utf8.capitalize_fst s)
  in
  Hutil.header conf title;
  Output.printf conf "\n%s" (reference conf base p (person_text conf base p));
  Output.printf conf "%s\n" (DateDisplay.short_dates_text conf base p);
  print_children_list conf base p;
  Hutil.trailer conf

let print_conflict conf base ip_var p =
  let title _ = Output.print_string conf (Utf8.capitalize_fst (transl conf "error")) in
  Hutil.rheader conf title;
  Update.print_error conf base (AlreadyDefined p);
  let free_n =
    Gutil.find_free_occ base (p_first_name base p) (p_surname base p) 0
  in
  Output.print_string conf "<ul>\n";
  Output.print_string conf "<li>";
  Output.printf conf "%s%s %d.\n" (Utf8.capitalize_fst (transl conf "first free number"))
    (Util.transl conf ":") free_n;
  Output.printf conf (fcapitale (ftransl conf "click on \"%s\""))
    (transl conf "create");
  Output.printf conf " %s.\n" (transl conf "to try again with this number");
  Output.print_string conf "</li>";
  Output.print_string conf "<li>";
  Output.printf conf "%s " (Utf8.capitalize_fst (transl conf "or"));
  Output.printf conf (ftransl conf "click on \"%s\"") (transl conf "back");
  Output.printf conf " %s %s." (transl_nth conf "and" 0)
    (transl conf "change it (the number) yourself");
  Output.print_string conf "</li>";
  Output.print_string conf "</ul>\n";
  Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       Output.printf conf "<input type=\"hidden\" name=\"%s\" value=\"%s\">\n" x
         (Util.escape_html (Mutil.decode v)))
    (conf.henv @ conf.env);
  begin let var = "c" ^ string_of_iper ip_var in
    Output.printf conf "<input type=\"hidden\" name=\"field\" value=\"%s\">\n" var
  end;
  Output.printf conf "<input type=\"hidden\" name=\"free_occ\" value=\"%d\">\n"
    free_n;
  Output.printf conf  "<button type=\"submit\" name=\"create\" \
class=\"btn btn-primary btn-lg\">%s</button>\n" (Utf8.capitalize_fst (transl conf "create"));
  Output.printf conf "<button type=\"submit\" name=\"return\" \
class=\"btn btn-primary btn-lg\">%s</button>\n" (Utf8.capitalize_fst (transl conf "back"));
  Output.print_string conf "</form>\n";
  Update.print_same_name conf base p;
  Hutil.trailer conf;
  raise @@ Update.ModErr (__FILE__ ^ " " ^ string_of_int __LINE__)

let error_person conf err =
  let title _ = Output.print_string conf (Utf8.capitalize_fst (transl conf "error")) in
  Hutil.rheader conf title;
  Output.printf conf "%s\n" (Utf8.capitalize_fst err);
  Hutil.trailer conf;
  raise @@ Update.ModErr (__FILE__ ^ " " ^ string_of_int __LINE__)


let print_update_child conf base =
  match p_getenv conf.env "m" with
    Some "CHG_CHN_OK" -> print conf base
  | _ -> Hutil.incorrect_request conf

let print_change_ok conf base p =
  let ipl = children_of_p base p in
  let parent_surname = p_surname base p in
  let redisp =
    match p_getenv conf.env "return" with
      Some _ -> true
    | _ -> false
  in
  if redisp then print_update_child conf base
  else begin
    check_digest conf (digest_children base ipl);
    let changed =
      try change_children conf base parent_surname ipl
      with
      | ChangeChildrenConflict (p, p') -> print_conflict conf base (get_iper p) p'
      | FirstNameMissing _ -> error_person conf (transl conf "first name missing")
    in
    Util.commit_patches conf base;
    let changed =
      U_Change_children_name
        (Util.string_gen_person base (gen_person_of_person p), changed)
    in
    History.record conf base changed "cn";
    print_change_done conf base p
  end

let print_ok o_conf base =
  let conf = Update.update_conf o_conf in
  match p_getenv conf.env "ip" with
    Some i ->
      let p = poi base (iper_of_string i) in print_change_ok conf base p
  | _ -> Hutil.incorrect_request conf
