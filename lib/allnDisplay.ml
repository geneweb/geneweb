let default_max_cnt = Alln.default_max_cnt

(* tools *)

let particle_at_the_end base is_surnames s =
  if is_surnames then
    Util.surname_without_particle base s ^ Util.surname_particle base s
  else s

let compare_particle_at_the_end base is_surnames a b =
  Utf8.alphabetic_order
    (particle_at_the_end base is_surnames a)
    (particle_at_the_end base is_surnames b)

(* print *)

let print_title conf base is_surnames ini len h =
  let conf = Config.Trimmed.to_config conf in
  if len >= 2 then
    if is_surnames then
      if h then
        Output.print_sstring conf
          (Utf8.capitalize_fst (Util.transl conf "all the names"))
      else
        Printf.sprintf
          (Util.fcapitale (Util.ftransl conf "%s surnames list title"))
          (Mutil.string_of_int_sep
             (Util.transl conf "(thousand separator)")
             len)
        |> Output.print_sstring conf
    else
      Printf.sprintf
        (Util.fcapitale (Util.ftransl conf "the %d first names"))
        len
      |> Output.print_sstring conf
  else if is_surnames then
    Util.transl conf "surname list title"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  else
    Util.transl_nth conf "first name/first names" 0
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
  if ini <> "" then (
    Output.print_sstring conf " ";
    Output.print_sstring conf (Util.transl conf "starting with");
    Output.print_sstring conf " ";
    Output.print_string conf (Util.escape_html ini))
  else if not h then (
    Output.print_sstring conf " (";
    Output.print_sstring conf
      (Mutil.string_of_int_sep
         (Util.transl conf "(thousand separator)")
         (Gwdb.nb_of_real_persons base));
    Output.print_sstring conf " ";
    Output.print_sstring conf
      (Util.translate_eval ("@(c)" ^ Util.transl_nth conf "person/persons" 1));
    Output.print_sstring conf ")")

let tr c1 s2 s =
  let rec loop i len =
    if i = String.length s then Buff.get len
    else if String.unsafe_get s i = c1 then loop (i + 1) (Buff.mstore len s2)
    else loop (i + 1) (Buff.store len (String.unsafe_get s i))
  in
  loop 0 0

let surname_list_meta_description conf base =
  [
    Hutil.
      {
        name = "description";
        content =
          Utf8.capitalize_fst
            (Printf.sprintf
               (Util.ftransl
                  (Config.Trimmed.to_config conf)
                  "%s all_names_meta_description")
               (Gwdb.bname base));
      };
  ]

let print_alphabetic_big conf base is_surnames ini list len too_big =
  let title = print_title conf base is_surnames ini len in
  let mode = if is_surnames then Adef.encoded "N" else Adef.encoded "P" in
  let meta =
    if is_surnames then surname_list_meta_description conf base else []
  in
  let conf = Config.Trimmed.to_config conf in
  Hutil.header_with_meta conf title meta;
  Output.print_sstring conf {|<p class="search_name">|};
  List.iter
    (fun ini_k ->
      if ini_k = ini then (
        Output.print_sstring conf {|<a href="|};
        Output.print_string conf (Util.commd conf);
        Output.print_sstring conf "m=";
        Output.print_string conf mode;
        Output.print_sstring conf "&tri=A&v=";
        Output.print_string conf (Mutil.encode ini_k);
        Output.print_sstring conf {|">|})
      else (
        Output.print_sstring conf {|<a href="|};
        Output.print_string conf (Util.commd conf);
        Output.print_sstring conf "m=";
        Output.print_string conf mode;
        Output.print_sstring conf "&tri=A&k=";
        Output.print_string conf (Mutil.encode ini_k);
        Output.print_sstring conf {|">|});
      Output.print_string conf (tr '_' "&nbsp;" ini_k |> Util.escape_html);
      Output.print_sstring conf "</a>\n")
    list;
  if not too_big then (
    Output.print_sstring conf "</p><p>";
    Util.transl conf "the whole list"
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf (Util.transl conf ":");
    Output.print_sstring conf "</p><ul><li>";
    Output.print_sstring conf {|<a href="|};
    Output.print_string conf (Util.commd conf);
    Output.print_sstring conf "m=";
    Output.print_string conf mode;
    Output.print_sstring conf "&tri=A&o=A&k=";
    Output.print_string conf (Mutil.encode ini);
    Output.print_sstring conf {|">|};
    Output.print_sstring conf (Util.transl conf "long display");
    Output.print_sstring conf "</a></li><li>";
    Output.print_sstring conf {|<a href="|};
    Output.print_string conf (Util.commd conf);
    Output.print_sstring conf "m=";
    Output.print_string conf mode;
    Output.print_sstring conf "&tri=S&o=A&k=";
    Output.print_string conf (Mutil.encode ini);
    Output.print_sstring conf {|">|};
    Output.print_sstring conf (Util.transl conf "short display");
    Output.print_sstring conf "</a></li><li>";
    Output.print_sstring conf {|<a href="|};
    Output.print_string conf (Util.commd conf);
    Output.print_sstring conf "m=";
    Output.print_string conf mode;
    Output.print_sstring conf "&tri=S&o=A&cgl=on&k=";
    Output.print_string conf (Mutil.encode ini);
    Output.print_sstring conf {|">|};
    Output.print_sstring conf (Util.transl conf "short display");
    Output.print_sstring conf " + ";
    Output.print_sstring conf (Util.transl conf "cancel GeneWeb links");
    Output.print_sstring conf "</a></li></ul>");
  Hutil.trailer conf

let print_alphabetic_all conf base is_surnames ini list len =
  let title = print_title conf base is_surnames ini len in
  let mode = Adef.encoded (if is_surnames then "N" else "P") in
  let meta =
    if is_surnames then surname_list_meta_description conf base else []
  in
  let conf = Config.Trimmed.to_config conf in
  Hutil.header_with_meta conf title meta;
  Output.print_sstring conf {|<p class="search_name">|};
  List.iter
    (fun (ini_k, _) ->
      Output.print_sstring conf "<a href=\"#a";
      Output.print_string conf (Mutil.encode ini_k);
      Output.print_sstring conf "\">";
      Output.print_string conf (Ext_string.tr '_' ' ' ini_k |> Adef.safe);
      Output.print_sstring conf "</a>\n")
    list;
  Output.print_sstring conf "</p><ul>";
  List.iter
    (fun (ini_k, l) ->
      Output.print_sstring conf "<li><a id=\"a";
      Output.print_string conf (Mutil.encode ini_k);
      Output.print_sstring conf "\">";
      Output.print_string conf (Ext_string.tr '_' ' ' ini_k |> Adef.safe);
      Output.print_sstring conf "</a><ul>";
      List.iter
        (fun (s, cnt) ->
          Output.print_sstring conf "<li>";
          let href =
            let open Def in
            "m=" ^<^ mode ^^^ "&v=" ^<^ Mutil.encode s ^>^ "&t=A"
          in
          Util.wprint_geneweb_link conf
            (href :> Adef.escaped_string)
            (particle_at_the_end base is_surnames s |> Util.escape_html
              :> Adef.safe_string);
          Output.print_sstring conf " (";
          Output.print_sstring conf (string_of_int cnt);
          Output.print_sstring conf ")</li>")
        (List.sort
           (fun (a, _) (b, _) ->
             compare_particle_at_the_end base is_surnames a b)
           l);
      Output.print_sstring conf "</ul></li>")
    list;
  Output.print_sstring conf "</ul>";
  Hutil.trailer conf

let print_alphabetic_small conf base is_surnames ini list len =
  let title = print_title conf base is_surnames ini len in
  let mode = Adef.encoded (if is_surnames then "N" else "P") in
  let meta =
    if is_surnames then surname_list_meta_description conf base else []
  in
  let conf = Config.Trimmed.to_config conf in
  Hutil.header_with_meta conf title meta;
  if list <> [] then (
    Output.print_sstring conf "<ul>";
    List.iter
      (fun (_, s, cnt) ->
        Output.print_sstring conf "<li>";
        Output.print_sstring conf "<a href=\"";
        Output.print_string conf (Util.commd conf);
        Output.print_sstring conf "m=";
        Output.print_string conf mode;
        Output.print_sstring conf "&v=";
        Output.print_string conf (Mutil.encode s);
        Output.print_sstring conf "&t=A\">";
        Output.print_string conf
          (particle_at_the_end base is_surnames s |> Util.escape_html);
        Output.print_sstring conf "</a> (";
        Output.print_sstring conf (string_of_int cnt);
        Output.print_sstring conf ")</li>")
      (List.sort
         (fun (_, a, _) (_, b, _) ->
           compare_particle_at_the_end base is_surnames a b)
         list);
    Output.print_sstring conf "</ul>");
  Hutil.trailer conf

let print_frequency_any conf base is_surnames list len =
  let title = print_title conf base is_surnames "" len in
  let mode = Adef.encoded (if is_surnames then "N" else "P") in
  let n = ref 0 in
  let conf = Config.Trimmed.to_config conf in
  Hutil.header conf title;
  Output.print_sstring conf "<ul>";
  List.iter
    (fun (cnt, l) ->
      if !n <= default_max_cnt then (
        Output.print_sstring conf "<li>";
        Output.print_sstring conf (string_of_int cnt);
        Output.print_sstring conf "<ul>";
        List.iter
          (fun s ->
            Output.print_sstring conf "<li><a href=\"";
            Output.print_string conf (Util.commd conf);
            Output.print_sstring conf "m=";
            Output.print_string conf mode;
            Output.print_sstring conf "&v=";
            Output.print_string conf (Mutil.encode (Name.lower s));
            Output.print_sstring conf "\">";
            Output.print_string conf
              (particle_at_the_end base is_surnames s |> Util.escape_html);
            Output.print_sstring conf "</a></li>";
            incr n)
          l;
        Output.print_sstring conf "</ul>";
        Output.print_sstring conf "</li>"))
    list;
  Output.print_sstring conf "</ul>";
  Hutil.trailer conf

let print_frequency ~at_least conf base is_surnames =
  let () = Gwdb.load_strings_array base in
  let list, len =
    Alln.select_names ~at_least conf base is_surnames "" max_int
  in
  let list = Alln.groupby_count list in
  print_frequency_any conf base is_surnames list len

let print_alphabetic ~prefix ~all ~at_least ~fast ~index conf base is_surnames =
  let ini = match prefix with Some k -> k | None -> "" in
  if (fast || Gwdb.nb_of_persons base >= 100_000) && ini = "" then (
    Gwdb.load_strings_array base;
    let list =
      List.sort Utf8.alphabetic_order (List.map Utf8.uchar_to_string index)
    in
    print_alphabetic_big conf base is_surnames ini list 1 true)
  else if String.length ini < 2 then Gwdb.load_strings_array base;
  let list, len =
    Alln.select_names ~at_least conf base is_surnames ini
      (if all then max_int else 50)
  in
  match list with
  | Alln.Specify keys ->
      let keys = List.sort Utf8.alphabetic_order keys in
      let too_big = (not all) && List.length keys > Alln.default_max_cnt in
      print_alphabetic_big conf base is_surnames ini keys len too_big
  | Alln.Result list ->
      if len >= 50 || ini = "" then
        let list = Alln.groupby_ini (Utf8.length ini + 1) list in
        print_alphabetic_all conf base is_surnames ini list len
      else print_alphabetic_small conf base is_surnames ini list len

(* short print *)

let print_alphabetic_short conf base is_surnames ini list len =
  let title = print_title conf base is_surnames ini len in
  let mode = Adef.encoded (if is_surnames then "N" else "P") in
  let need_ref = len >= 250 in
  let meta =
    if is_surnames then surname_list_meta_description conf base else []
  in
  let conf = Config.Trimmed.to_config conf in
  Hutil.header_with_meta conf title meta;
  if need_ref then (
    Output.print_sstring conf "<p>";
    List.iter
      (fun (ini_k, _) ->
        Output.print_sstring conf "<a href=\"#a";
        Output.print_string conf (Mutil.encode ini_k);
        Output.print_sstring conf "\">";
        Output.print_string conf
          (Ext_string.tr '_' ' ' ini_k |> Util.escape_html);
        Output.print_sstring conf "</a>\n")
      list;
    Output.print_sstring conf "</p>");
  List.iter
    (fun (ini_k, l) ->
      Output.print_sstring conf "<p>";
      Ext_list.iter_first
        (fun first (s, cnt) ->
          let href =
            let open Def in
            " href=\"" ^<^ Util.commd conf
            ^^^ ("m=" ^<^ mode ^^^ "&v=" ^<^ Mutil.encode s ^>^ "&t=A\""
                  :> Adef.escaped_string)
          in
          let name =
            Adef.encoded
              (if first && need_ref then " id=\"a" ^ ini_k ^ "\"" else "")
          in
          if not first then Output.print_sstring conf ",";
          Output.print_sstring conf "\n<a";
          Output.print_string conf href;
          Output.print_string conf name;
          Output.print_sstring conf ">";
          Output.print_string conf
            (particle_at_the_end base is_surnames s |> Util.escape_html);
          Output.print_sstring conf "</a>";
          Output.print_sstring conf " (";
          Output.print_sstring conf (string_of_int cnt);
          Output.print_sstring conf ")")
        (List.sort (fun (a, _) (b, _) -> Utf8.alphabetic_order a b) l);
      Output.print_sstring conf "</p>")
    list;
  Hutil.trailer conf

let print_short ~prefix ~at_least conf base is_surnames =
  let ini = match prefix with Some k -> k | None -> "" in
  let () = if String.length ini < 2 then Gwdb.load_strings_array base in
  match Alln.select_names ~at_least conf base is_surnames ini max_int with
  | Alln.Specify _, _ -> Hutil.incorrect_request (Config.Trimmed.to_config conf)
  | Alln.Result list, len ->
      let list = Alln.groupby_ini (Utf8.length ini + 1) list in
      print_alphabetic_short conf base is_surnames ini list len
