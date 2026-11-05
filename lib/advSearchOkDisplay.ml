(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
open Def
module Driver = Geneweb_db.Driver

let acces_with_occ conf base p =
  let acc = acces conf base p in
  let occ = Driver.get_occ p in
  if occ = 0 || not (String.starts_with ~prefix:"i=" (acc :> string)) then acc
  else acc ^>^ "&oc=" ^ string_of_int occ

let reference_with_occ conf base p (s : Adef.safe_string) =
  let cgl = p_getenv conf.env "cgl" = Some "on" in
  if (not (GWPARAM.p_auth conf base p)) || cgl then s
  else
    "<a href=\""
    ^<^ (commd conf ^^^ acces_with_occ conf base p :> Adef.safe_string)
    ^^^ "\" id=\"i"
    ^<^ Driver.Iper.to_string (Driver.get_iper p)
    ^<^ "\">" ^<^ s ^>^ "</a>"

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
        Output.print_string conf
          (reference_with_occ conf base p (gen_person_text conf base p));
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
