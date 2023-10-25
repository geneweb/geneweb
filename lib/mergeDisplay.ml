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
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_decline conf "merge" ""));
    if not h then (
      Output.print_sstring conf " ";
      print_someone conf base p;
      Output.print_sstring conf " ";
      Output.print_sstring conf (transl_decline conf "with" "");
      Output.print_sstring conf (transl conf ":"))
  in
  let list = Gutil.find_same_name base p in
  let list =
    List.fold_right
      (fun p1 pl -> if get_iper p1 = get_iper p then pl else p1 :: pl)
      list []
  in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Output.print_sstring conf "<h2>";
  title false;
  Output.print_sstring conf "</h2>";
  Output.print_sstring conf {|<form method="get" action="|};
  Output.print_sstring conf conf.command;
  Output.print_sstring conf {|"class="mx-3 mb-3">|};
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "MRG_IND");
  Util.hidden_input conf "i" (get_iper p |> string_of_iper |> Mutil.encode);
  Output.print_sstring conf
    "<span class=\"form-row align-items-center\"><span \
     class=\"col-auto\"><span class=\"custom-control custom-radio\"><input \
     type=\"radio\" class=\"custom-control-input\" name=\"select\" \
     id=\"input\" value=\"input\" checked><label \
     class=\"custom-control-label\"for=\"input\">";
  Output.print_sstring conf (transl conf "any individual in the base");
  Output.print_sstring conf
    "</label></span></span><span class=\"col-auto\"><input type=\"text\" \
     class=\"form-control\" name=\"n\" placeholder=\"";
  Output.print_sstring conf (transl_nth conf "first name/first names" 0);
  Output.print_sstring conf ".";
  Output.print_sstring conf (transl conf "number");
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl_nth conf "surname/surnames" 0);
  Output.print_sstring conf "\" title=\"";
  Output.print_sstring conf (transl_nth conf "first name/first names" 0);
  Output.print_sstring conf ".";
  Output.print_sstring conf (transl conf "number");
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl_nth conf "surname/surnames" 0);
  Output.print_sstring conf
    "\" size=\"50\" id=\"inlineinput\" autofocus></span></span>";
  if list <> [] then
    List.iter
      (fun p ->
        Output.print_sstring conf "<div class=\"custom-control custom-radio\">";
        Output.print_sstring conf
          "<input type=\"radio\" class=\"custom-control-input\" \
           name=\"select\" id=\"";
        Output.print_string conf (get_iper p |> string_of_iper |> Mutil.encode);
        Output.print_sstring conf "\" value=\"";
        Output.print_string conf (get_iper p |> string_of_iper |> Mutil.encode);
        Output.print_sstring conf "\">\n";
        Output.print_sstring conf "<label class=\"custom-control-label\" for=\"";
        Output.print_string conf (get_iper p |> string_of_iper |> Mutil.encode);
        Output.print_sstring conf "\">";
        let cop = (Util.child_of_parent conf base p :> string) in
        let cop = if cop = "" then "" else ", " ^ cop in
        let hbw = (Util.husband_wife conf base p true :> string) in
        let hbw = if hbw = "" then "" else ", " ^ hbw in
        Output.print_sstring conf
          (Printf.sprintf "%s.%d %s%s%s"
             (get_first_name p |> sou base)
             (get_occ p)
             (get_surname p |> sou base)
             cop hbw);
        Output.print_sstring conf "</label></div>")
      list;
  Output.print_sstring conf
    {|<button type="submit" class="btn btn-primary btn-lg mt-2">|};
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0));
  Output.print_sstring conf "</button></form>\n";
  Hutil.trailer conf

let print_possible_continue_merging conf base =
  let open Adef in
  match (p_getenv conf.env "ini1", p_getenv conf.env "ini2") with
  | Some ini1, Some ini2 ->
      let ini1 = iper_of_string ini1 in
      let ini2 = iper_of_string ini2 in
      let p1 = poi base ini1 in
      let p2 = poi base ini2 in
      Output.print_sstring conf {|<p><a href="|};
      Output.print_string conf (commd conf);
      Output.print_sstring conf {|m=MRG_IND&i=|};
      Output.print_string conf (string_of_iper ini1 |> Mutil.encode);
      Output.print_sstring conf {|&i2=|};
      Output.print_string conf (string_of_iper ini2 |> Mutil.encode);
      Output.print_sstring conf {|">|};
      Output.print_sstring conf
        (Utf8.capitalize_fst (transl conf "continue merging"));
      Output.print_sstring conf {|</a> |};
      print_someone conf base p1;
      Output.print_sstring conf " ";
      Output.print_sstring conf (transl_nth conf "and" 0);
      Output.print_sstring conf " ";
      print_someone conf base p2;
      Output.print_sstring conf "</p>"
  | _ -> (
      match p_getenv conf.env "ip" with
      | Some ip ->
          let ip = iper_of_string ip in
          let s1 =
            match p_getenv conf.env "iexcl" with
            | Some "" | None -> Adef.encoded ""
            | Some s -> "&iexcl=" ^<^ Mutil.encode s
          in
          let s2 =
            match p_getenv conf.env "fexcl" with
            | Some "" | None -> Adef.encoded ""
            | Some s -> "&fexcl=" ^<^ Mutil.encode s
          in
          if s1 <^> Adef.encoded "" || s2 <^> Adef.encoded "" then (
            let p = poi base ip in
            let s = gen_person_text conf base p in
            Output.print_sstring conf {|<p><a href="|};
            Output.print_string conf (commd conf);
            Output.print_sstring conf {|m=MRG_DUP&ip=|};
            Output.print_string conf (string_of_iper ip |> Mutil.encode);
            Output.print_string conf s1;
            Output.print_string conf s2;
            Output.print_sstring conf {|">|};
            Output.print_sstring conf
              (Utf8.capitalize_fst (transl conf "continue merging"));
            Output.print_sstring conf {| (|};
            Output.print_sstring conf
              (transl_a_of_b conf
                 (transl conf "possible duplications")
                 (reference conf base p s :> string)
                 (s :> string));
            Output.print_sstring conf {|)</p>|})
      | None -> ())
