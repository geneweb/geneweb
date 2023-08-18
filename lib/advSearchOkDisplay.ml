(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util

let print_result conf base max_answers (list, len) =
  let list =
    if len > max_answers then Util.reduce_list max_answers list else list
  in
  if len = 0 then (
    Output.print_sstring conf (Utf8.capitalize_fst (transl conf "no match"));
    Output.print_sstring conf " ")
  else
    let () = SosaCache.build_sosa_ht conf base in
    Output.print_sstring conf "<ul>\n";
    List.iter
      (fun p ->
        Output.print_sstring conf "<li>";
        SosaCache.print_sosa conf base p true;
        Output.print_sstring conf " ";
        Output.print_string conf (referenced_person_text conf base p);
        Output.print_string conf
          (mod_ind_link conf p (DateDisplay.short_dates_text conf base p));
        Output.print_sstring conf "<em>";
        specify_homonymous conf base p false;
        Output.print_sstring conf "</em>")
      list;
    if len > max_answers then Output.print_sstring conf "<li>&hellip;</li>";
    Output.print_sstring conf "</ul>";
    Output.print_sstring conf
      (Format.sprintf "%s%s %d<br>"
         (transl conf "total" |> Utf8.capitalize_fst)
         (transl conf ":") len)

let print conf base =
  let title _ =
    transl_nth conf "advanced request" 0
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  let max_answers =
    match p_getint conf.env "max" with Some n -> n | None -> 100
  in
  Hutil.header conf title;
  Output.print_sstring conf "<p>";
  Output.print_sstring conf (Utf8.capitalize_fst (transl conf "searching all"));
  Output.print_sstring conf (transl conf ":");
  Output.print_sstring conf " ";
  let search =
    (AdvSearchOk.searching_fields conf base :> string) |> String.trim
  in
  Output.print_sstring conf search;
  Output.print_sstring conf ".</p>";
  let list = AdvSearchOk.advanced_search conf base max_answers in
  print_result conf base max_answers list;
  Hutil.trailer conf
