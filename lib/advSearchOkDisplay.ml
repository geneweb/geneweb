(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util

let print_result conf base max_answers (list, len) =
  let list =
    if len > max_answers then Util.reduce_list max_answers list else list
  in
  if len = 0 then Output.printf conf "%s\n" (Utf8.capitalize (transl conf "no match"))
  else
    let () = Perso.build_sosa_ht conf base in
    Output.printf conf "<ul>\n";
    List.iter
      (fun p ->
         Output.printf conf "<li>" ;
         Perso.print_sosa conf base p true;
         Output.printf conf "\n%s%s<em>"
           (referenced_person_text conf base p)
           (DateDisplay.short_dates_text conf base p) ;
         specify_homonymous conf base p false;
         Output.printf conf "</em>")
      list;
    if len > max_answers then Output.printf conf "<li>...</li>";
    Output.printf conf "</ul>\n"


let print conf base =
  let title _ =
    Output.print_string conf (Utf8.capitalize (transl_nth conf "advanced request" 0))
  in
  let max_answers =
    match p_getint conf.env "max" with
      Some n -> n
    | None -> 100
  in
  Hutil.header conf title;
  Output.printf conf "<p>\n";
  Output.printf conf "%s %s." (Utf8.capitalize (transl conf "searching all"))
    (AdvSearchOk.searching_fields conf base);
  Output.printf conf "</p>\n";
  let list = AdvSearchOk.advanced_search conf base max_answers in
  print_result conf base max_answers list; Hutil.trailer conf
