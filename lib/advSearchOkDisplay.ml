(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util

let print_result conf base max_answers (list, len) =
  let list =
    if len > max_answers then Ext_list.take list max_answers else list
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
        Output.print_string conf
          (NameDisplay.referenced_person_text conf base p);
        Output.print_string conf (DateDisplay.short_dates_text conf base p);
        Output.print_sstring conf "<em>";
        NameDisplay.specify_homonymous conf base p false;
        Output.print_sstring conf "</em>")
      list;
    if len > max_answers then Output.print_sstring conf "<li>...</li>";
    Output.print_sstring conf "</ul>"

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
  Output.print_sstring conf " ";
  Output.print_string conf (AdvSearchOk.searching_fields conf base);
  Output.print_sstring conf ".</p>";
  let list = AdvSearchOk.advanced_search conf base max_answers in
  print_result conf base max_answers list;
  Hutil.trailer conf
