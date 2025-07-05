open Config

let print_nav_button conf current_data data icon title =
  let href =
    Printf.sprintf "%sm=CHK_DATA&data=%s" (Util.commd conf :> string) data
  in
  let active = if current_data = data then "active" else "" in
  Output.printf conf
    {|<a href="%s" class="btn btn-outline-primary %s">
       <i class="fa fa-%s mr-1"></i>%s
     </a>|}
    href active icon
    (Utf8.capitalize_fst (Util.transl_nth conf title 1))

let print_nav_buttons conf current_data =
  Output.printf conf "<div class=\"d-flex btn-group mt-3\" role=\"group\">\n";
  print_nav_button conf current_data "fn" "child" "first name/first names";
  print_nav_button conf current_data "sn" "signature" "surname/surnames";
  print_nav_button conf current_data "place" "map-location-dot" "place/places";
  print_nav_button conf current_data "pubn" "pen" "public name/public names";
  print_nav_button conf current_data "qual" "comment" "qualifier/qualifiers";
  print_nav_button conf current_data "alias" "mask" "alias/aliases";
  print_nav_button conf current_data "occu" "user-doctor"
    "occupation/occupations";
  print_nav_button conf current_data "title" "crown" "title/titles";
  print_nav_button conf current_data "domain" "chess-rook" "domain/domains";
  print_nav_button conf current_data "src" "box-archive" "source/sources";
  Output.printf conf "</div>\n"

let display_error_section conf data entries error_type error_title =
  let filtered_entries =
    List.filter_map
      (fun (entry, errors) ->
        if List.mem error_type errors then
          Some (CheckData.make_error_html conf data entry error_type)
        else None)
      entries
  in
  if filtered_entries <> [] then (
    Output.printf conf "<h3 class=\"mt-3\">%s</h3>" error_title;
    Output.print_sstring conf "<ul class=\"list-group\">";
    List.iter
      (fun html ->
        Output.printf conf "<li class=\"list-group-item\">%s</li>" html)
      filtered_entries;
    Output.print_sstring conf "</ul>");
  List.length filtered_entries

let print conf base =
  let data = Util.p_getenv conf.env "data" |> Option.value ~default:"" in

  let dict =
    match data with
    | "fn" -> Some CheckData.Fnames
    | "sn" -> Some CheckData.Snames
    | "place" -> Some CheckData.Places
    | "pubn" -> Some CheckData.PubNames
    | "qual" -> Some CheckData.Qualifiers
    | "alias" -> Some CheckData.Aliases
    | "occu" -> Some CheckData.Occupation
    | "title" -> Some CheckData.Titles
    | "domain" -> Some CheckData.Estates
    | "src" -> Some CheckData.Sources
    | _ -> None
  in

  let entries =
    match dict with
    | Some dict_type -> CheckData.collect_all_errors base dict_type
    | None -> []
  in

  let title _ =
    Output.print_sstring conf (Util.transl conf "data typographic checker")
  in
  Hutil.header conf title;

  print_nav_buttons conf data;

  let total_errors =
    let count1 =
      display_error_section conf data entries CheckData.InvisibleCharacters
        (Util.transl conf "chk_data invisible characters")
    in
    let count2 =
      display_error_section conf data entries CheckData.BadCapitalization
        (Util.transl conf "chk_data bad capitalization")
    in
    let count3 =
      display_error_section conf data entries CheckData.MultipleSpaces
        (Util.transl conf "chk_data multiple spaces")
    in
    let count4 =
      display_error_section conf data entries CheckData.NonBreakingSpace
        (Util.transl conf "chk_data non-breaking spaces")
    in
    count1 + count2 + count3 + count4
  in

  if total_errors = 0 && dict <> None then
    Output.printf conf "<h3 class=\"mt-3\">%s</h3>"
      (Utf8.capitalize_fst (Util.transl conf "no match"));

  Hutil.trailer conf
