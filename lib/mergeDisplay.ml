(* $Id: merge.ml, v7.1 04/03/2026 00:11:35 *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let print_someone conf base p =
  Output.printf conf "%s%s %s"
    (Driver.p_first_name base p)
    (if Driver.get_occ p = 0 then "" else "." ^ string_of_int (Driver.get_occ p))
    (Driver.p_surname base p)

let print_person_info conf base p =
  Output.print_sstring conf {|<a href="|};
  Output.print_string conf (commd conf);
  Output.print_string conf (acces conf base p);
  Output.print_sstring conf {|">|};
  Output.print_string conf (escape_html (Driver.p_first_name base p));
  if Driver.get_occ p <> 0 then (
    Output.print_sstring conf ".";
    Output.print_sstring conf (string_of_int (Driver.get_occ p)));
  Output.print_sstring conf " ";
  Output.print_string conf (escape_html (Driver.p_surname base p));
  Output.print_sstring conf "</a>";
  let dates = DateDisplay.short_dates_text conf base p in
  if (dates :> string) <> "" then
    Output.print_string conf (mod_ind_link conf p dates);
  let cop = (child_of_parent conf base p :> string) in
  if cop <> "" then (
    Output.print_sstring conf ", ";
    Output.print_sstring conf cop);
  let hw = (husband_wife conf base p true :> string) in
  if hw <> "" then Output.print_sstring conf hw

let print conf base p =
  let list = Gutil.find_same_name base p in
  let list =
    List.fold_right
      (fun p1 pl ->
        if Driver.get_iper p1 = Driver.get_iper p then pl else p1 :: pl)
      list []
  in
  let title _ =
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_decline conf "merge" ""))
  in
  Hutil.header conf title;
  Output.print_sstring conf "<h2 class=\"h4\">\n";
  print_person_info conf base p;
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl_decline conf "with" "");
  Output.print_sstring conf (transl conf ":");
  Output.print_sstring conf "\n</h2>\n";
  Output.print_sstring conf
    (Format.sprintf {|<form method="get" action="%s" class="mx-3 mb-3">|}
       (conf.command :> string));
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "MRG_IND");
  Util.hidden_input conf "i"
    (Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode);
  let ph =
    Format.sprintf "%s.%s %s"
      (transl_nth conf "first name/first names" 0)
      (transl conf "number")
      (transl_nth conf "surname/surnames" 0)
  in
  Output.print_sstring conf
    {|<div class="form-check d-flex align-items-center gap-2 mb-2">|};
  Output.print_sstring conf
    {|<input class="form-check-input" type="radio" name="select" id="input" value="input" checked>|};
  Output.print_sstring conf
    {|<label class="form-check-label text-nowrap" for="input">|};
  Output.print_sstring conf (transl conf "any individual in the base");
  Output.print_sstring conf {|</label>|};
  Output.printf conf
    {|<input type="text" class="form-control form-control-sm w-auto" name="n" id="inlineinput" placeholder="%s" title="%s" size="50" autofocus>|}
    ph ph;
  Output.print_sstring conf {|</div>|};
  if list <> [] then
    List.iter
      (fun p ->
        Output.print_sstring conf {|<div class="form-check ms-1">|};
        Output.print_sstring conf
          {|<input type="radio" class="form-check-input" name="select" id="|};
        Output.print_string conf
          (Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode);
        Output.print_sstring conf {|" value="|};
        Output.print_string conf
          (Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode);
        Output.print_sstring conf {|"><label class="form-check-label" for="|};
        Output.print_string conf
          (Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode);
        Output.print_sstring conf {|">|};
        print_person_info conf base p;
        Output.print_sstring conf {|</label></div>|})
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
      let ini1 = Driver.Iper.of_string ini1 in
      let ini2 = Driver.Iper.of_string ini2 in
      let p1 = Driver.poi base ini1 in
      let p2 = Driver.poi base ini2 in
      Output.print_sstring conf {|<p><a href="|};
      Output.print_string conf (commd conf);
      Output.print_sstring conf {|m=MRG_IND&i=|};
      Output.print_string conf (Driver.Iper.to_string ini1 |> Mutil.encode);
      Output.print_sstring conf {|&i2=|};
      Output.print_string conf (Driver.Iper.to_string ini2 |> Mutil.encode);
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
          let ip = Driver.Iper.of_string ip in
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
            let p = Driver.poi base ip in
            let s = gen_person_text conf base p in
            Output.print_sstring conf {|<p><a href="|};
            Output.print_string conf (commd conf);
            Output.print_sstring conf {|m=MRG_DUP&ip=|};
            Output.print_string conf (Driver.Iper.to_string ip |> Mutil.encode);
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
