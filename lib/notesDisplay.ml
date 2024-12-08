(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
open Notes

let print_search_form conf from_note =
  Output.print_sstring conf "<div class=\"form-group mt-3\">\n";
  Output.printf conf "<form method=\"get\" action=\"%s\">\n" conf.command;
  hidden_env conf;
  Output.print_sstring conf "<div class=\"form-check form-check-inline\">";
  Output.print_sstring conf
    {|<input type="hidden" name="m" value="MISC_NOTES_SEARCH">|};
  (match from_note with
  | Some n ->
      Output.print_sstring conf {|<input type="hidden" name="z" value="|};
      Output.print_string conf (Util.escape_html n);
      Output.print_sstring conf {|">|}
  | None -> ());
  Output.print_sstring conf {|<input type="text" name="s" size="40"|};
  Output.print_sstring conf {| class="form-control col-8" value="|};
  (match p_getenv conf.env "s" with
  | Some s -> Output.print_string conf (Util.escape_html s)
  | None -> ());
  Output.print_sstring conf "\">";
  Output.print_sstring conf "<input type=\"checkbox\" name=\"c\" value=\"on\"";
  Output.printf conf " class=\"form-check-input ml-2\" id=\"case\"%s>"
    (match p_getenv conf.env "c" with
    | Some "on" -> " checked"
    | Some _ | None -> "");
  Output.print_sstring conf "<label class=\"form-check-label\" for=\"case\">";
  Output.printf conf "%s" (transl_nth conf "search/case sensitive" 1);
  Output.print_sstring conf "</label></div>\n";
  Output.print_sstring conf {|<button type="submit" class="btn btn-primary">|};
  transl_nth conf "search/case sensitive" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "</button>\n</form>\n</div>"

let print_whole_notes conf base fnotes (title : Adef.safe_string) s ho =
  Hutil.header_without_title conf;
  let title_html =
    if (title :> string) <> "" then
      let title_text =
        match ho with
        | Some (case_sens, h) ->
            html_highlight case_sens h (title : Adef.safe_string :> string)
            |> Adef.safe
        | None -> title
      in
      Format.sprintf "<h1>%s</h1>" (title_text :> string)
    else ""
  in
  Output.printf conf {|<div class="d-flex mb-3">%s%s</div>|} title_html
    (Wiki.make_edit_button conf fnotes ());
  let file_path = file_path conf base in
  let s = Util.string_with_macros conf [] s in
  let edit_opt = Some (conf.wizard, "NOTES", fnotes) in
  let s =
    let wi =
      {
        Wiki.wi_mode = "NOTES";
        Wiki.wi_file_path = file_path;
        Wiki.wi_person_exists = person_exists conf base;
        Wiki.wi_always_show_link = conf.wizard || conf.friend;
      }
    in
    Wiki.html_with_summary_of_tlsw conf wi edit_opt s
  in
  let s = Util.safe_html s in
  Output.print_string conf s;
  if ho <> None then print_search_form conf (Some fnotes);
  Hutil.trailer conf

(* TODO: DO†WE†NEED†ME?
   let what_links_page () =
     if fnotes <> "" then (
       Output.print_sstring conf {|<a href="|};
       Output.print_string conf (commd conf);
       Output.print_sstring conf {|m=NOTES&f=|};
       Output.print_string conf (Mutil.encode fnotes);
       Output.print_sstring conf {|&ref=on" class="mx-2">(|};
       Output.print_sstring conf (transl conf "linked pages");
       Output.print_sstring conf ")</a>\n")
   in*)

let print_notes_part conf base fnotes (title : Adef.safe_string) s cnt0 =
  Hutil.header_with_title conf (fun _ ->
      if (title :> string) = "" then
        Output.print_string conf (Util.escape_html fnotes)
      else Output.print_string conf title);
  if cnt0 = 0 && (title :> string) <> "" then (
    Output.print_sstring conf "<br><br><h1>";
    Output.print_string conf title;
    Output.print_sstring conf "</h1>");
  let s = string_with_macros conf [] s in
  let lines = Wiki.extract_sub_part s cnt0 in
  let mode = "NOTES" in
  let wi =
    {
      Wiki.wi_mode = mode;
      Wiki.wi_file_path = file_path conf base;
      Wiki.wi_person_exists = person_exists conf base;
      Wiki.wi_always_show_link = conf.wizard || conf.friend;
    }
  in
  Wiki.print_sub_part conf wi conf.wizard mode fnotes cnt0 lines;
  Hutil.trailer conf

let fmt_fnote_title conf base fnotes =
  let nenv, _ = read_notes base fnotes in
  let no_title = Utf8.capitalize_fst (transl conf "note without title") in
  let no_title = no_title |> Format.sprintf "<i>%s</i>" in
  try
    let fnote_title = List.assoc "TITLE" nenv in
    if String.trim fnote_title = "" then no_title else fnote_title
  with Not_found -> no_title

let linked_page_rows conf base pg =
  let typ = p_getenv conf.env "type" in
  match (pg, typ) with
  | Def.NLDB.PgInd ip, _ ->
      let p = pget conf base ip in
      if conf.wizard then
        Output.print_sstring conf
          (Format.sprintf
             {|
<td class="text-center">
  <a href="%sm=MOD_IND&i=%s#notes" title="%s">
    <i class="fa fa-user"></i></a></td>|}
             (commd conf :> string)
             (Gwdb.string_of_iper ip)
             (Utf8.capitalize_fst (transl conf "modify note")));
      Output.print_sstring conf
        (Format.sprintf {|
<td>%s%s</td>
<td><i>%s</i></td>|}
           (Util.referenced_person_title_text conf base p :> string)
           (DateDisplay.short_dates_text conf base p :> string)
           (Utf8.capitalize_fst (transl conf "individual notes")))
  | Def.NLDB.PgFam ifam, _ ->
      let fam = Gwdb.foi base ifam in
      let fath = pget conf base (Gwdb.get_father fam) in
      let moth = pget conf base (Gwdb.get_mother fam) in
      if conf.wizard then
        Output.print_sstring conf
          (Format.sprintf
             {|
<td class="align-middle">
  <a href="%sm=MOD_FAM&i=%s&ip=%s#events" title="%s %s %s">
    <i class="fa fa-user fa-sm"></i><i class="fa fa-user fa-sm"></i></a></td>|}
             (commd conf :> string)
             (Gwdb.string_of_ifam ifam)
             (Gwdb.get_iper fath |> Gwdb.string_of_iper)
             (Utf8.capitalize_fst (transl conf "modify"))
             (transl_nth conf "event/events" 0)
             (transl_nth conf "family/families" 0));
      Output.print_sstring conf
        (Format.sprintf
           {|
<td>%s%s<br>& %s%s</td>
<td class="align-middle"><i>%s %s</i></td>|}
           (Util.referenced_person_title_text conf base fath :> string)
           (DateDisplay.short_dates_text conf base fath :> string)
           (Util.referenced_person_title_text conf base moth :> string)
           (DateDisplay.short_dates_text conf base moth :> string)
           (Utf8.capitalize_fst (transl conf "comment"))
           (transl_nth conf "family/families" 0))
  | Def.NLDB.PgNotes, _ ->
      let fnote_title = fmt_fnote_title conf base "" in
      if conf.wizard then
        Output.print_sstring conf
          (Format.sprintf
             {|
<td class="text-center">
  <a href="%sm=MOD_NOTES" title="%s">
    <i class="far fa-file-lines"></i></a></td>|}
             (commd conf :> string)
             (Utf8.capitalize_fst (transl conf "modify note")));
      Output.print_sstring conf
        (Format.sprintf {|
<td><a href="%sm=NOTES">%s</a></td>
<td>%s</td>|}
           (commd conf :> string)
           (Utf8.capitalize (transl conf "base notes"))
           (Util.safe_html fnote_title :> string))
  | Def.NLDB.PgMisc fnotes, typ ->
      let nenv, _ = read_notes base fnotes in
      let n_type = try List.assoc "TYPE" nenv with Not_found -> "" in
      if match typ with Some t when t = "" -> t = n_type | _ -> true then (
        let fnote_title = fmt_fnote_title conf base fnotes in
        if conf.wizard then
          Output.print_sstring conf
            (Format.sprintf
               {|
<td class="text-center">
  <a href="%sm=MOD_NOTES&f=%s" title="%s">
    <i class="%s"></i></a></td>|}
               (commd conf :> string)
               (Util.uri_encode fnotes)
               (Utf8.capitalize_fst
                  (transl conf
                     (if n_type = "gallery" then "modify gallery"
                     else if n_type = "album" then "modify album"
                     else "modify note")))
               (if n_type = "gallery" || n_type = "album" then
                "far fa-image fa-fw"
               else "far fa-file-lines fa-fw"));
        Output.print_sstring conf
          (Format.sprintf
             {|
<td><a href="%sm=NOTES&f=%s">%s</a></td>
<td>%s</td>|}
             (commd conf :> string)
             (Util.uri_encode fnotes)
             (fnotes :> string)
             (Util.safe_html fnote_title :> string)))
  | Def.NLDB.PgWizard wizname, _ ->
      if conf.wizard then
        Output.print_sstring conf
          (Format.sprintf
             {|
<td class="text-center">
  <a href="%sm=MOD_WIZNOTES&f=%s" title="%s %s">
    <i class="fas fa-hat-wizard"></i></a></td>|}
             (commd conf :> string)
             (Util.uri_encode wizname)
             (Utf8.capitalize_fst (transl conf "modify"))
             (transl conf "base wizard notes"));
      Output.print_sstring conf
        (Format.sprintf
           {|
<td><a href="%sm=WIZNOTES&f=%s">%s</a></td>
<td><i>%s</i></td>|}
           (commd conf :> string)
           (Util.uri_encode wizname)
           (wizname :> string)
           (Utf8.capitalize_fst (transl conf "base wizard notes")))

let create_gallery_item conf fnotes nenv s =
  let img_url, img_name = Notes.json_extract_img conf s
  and title = try List.assoc "TITLE" nenv with Not_found -> "" in
  Printf.sprintf
    {|<div class="imap-gallery"><a href="%sm=NOTES&f=%s"><img src="%s" \
       title="%s | %s" alt="%s"></a>%s</div>|}
    (commd conf :> string)
    fnotes img_url fnotes img_name img_name title

let print_linked_list_gallery conf base pgl =
  Wserver.printf "<div class=\"d-flex flex-wrap mt-3\">\n";
  List.iter
    (function
      | Def.NLDB.PgMisc fnotes ->
          let nenv, s = read_notes base fnotes in
          let typ = try List.assoc "TYPE" nenv with Not_found -> "" in
          if typ = "gallery" || typ = "album" then
            Wserver.printf "%s" (create_gallery_item conf fnotes nenv s)
      | _ -> ())
    pgl;
  Wserver.printf "</div>\n"

let print_linked_list_standard conf base pgl =
  Output.print_sstring conf
    "\n<table class=\"table table-borderless table-striped w-auto mt-3\">";
  List.iter
    (fun pg ->
      Output.print_sstring conf "\n<tr>";
      linked_page_rows conf base pg;
      Output.print_sstring conf "</tr>\n")
    pgl;
  Output.print_sstring conf "</table>"

let print_linked_list conf base pgl =
  match p_getenv conf.env "type" with
  | Some "gallery" -> print_linked_list_gallery conf base pgl
  | Some "album" -> print_linked_list_gallery conf base pgl
  | _ -> print_linked_list_standard conf base pgl

let print_what_links conf base fnotes =
  let title h =
    Output.print_sstring conf (Utf8.capitalize_fst (transl conf "linked pages"));
    Output.print_sstring conf " ";
    if h then (
      Output.print_sstring conf "[";
      Output.print_string conf (Util.escape_html fnotes);
      Output.print_sstring conf "]")
    else (
      Output.print_sstring conf {|<span><a href="|};
      Output.print_string conf (commd conf);
      Output.print_sstring conf "m=NOTES&f=";
      Output.print_string conf (Mutil.encode fnotes);
      Output.print_sstring conf {|">|};
      Output.print_sstring conf fnotes;
      Output.print_sstring conf "</a></span>")
  in
  let db = notes_links_db conf base false in
  Hutil.header conf title;
  Option.iter (print_linked_list conf base) (List.assoc_opt fnotes db);
  Hutil.trailer conf

let print conf base =
  let fnotes =
    match p_getenv conf.env "f" with
    | Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  match p_getenv conf.env "ref" with
  | Some "on" -> print_what_links conf base fnotes
  | _ -> (
      let nenv, s = read_notes base fnotes in
      let typ = try List.assoc "TYPE" nenv with Not_found -> "" in
      let templ, typ =
        if typ = "" then (None, "")
        else if typ = "album" || typ = "gallery" then
          try (Util.open_etc_file conf "notes_gallery", typ)
          with Not_found -> (None, "")
        else (None, "")
      in
      match templ with
      | Some (ic, _fname) -> (
          match p_getenv conf.env "ajax" with
          | Some "on" ->
              let charset =
                if conf.charset = "" then "utf-8" else conf.charset
              in
              Wserver.header
                (Format.sprintf "Content-type: application/json; charset=%s"
                   charset);
              Wserver.printf "%s"
                (match typ with
                | "gallery" -> Notes.safe_gallery conf s
                | "album" -> Notes.safe_gallery conf s
                | _ -> s)
          | _ -> Templ.copy_from_templ conf [] ic)
      | None -> (
          let title = try List.assoc "TITLE" nenv with Not_found -> "" in
          let title = Util.safe_html title in
          match p_getint conf.env "v" with
          | Some cnt0 -> print_notes_part conf base fnotes title s cnt0
          | None -> print_whole_notes conf base fnotes title s None))

let print_mod conf base =
  let fnotes =
    match p_getenv conf.env "f" with
    | Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  let nenv, s = read_notes base fnotes in
  let typ = try List.assoc "TYPE" nenv with Not_found -> "" in
  let templ =
    if typ = "" then None
    else if typ = "gallery" || typ = "album" then
      Util.open_etc_file conf ("notes_upd_" ^ typ)
    else None
  in
  let title _ =
    Output.printf conf "%s - %s%s"
      (Utf8.capitalize_fst (transl conf "base notes"))
      conf.bname
      (if fnotes = "" then "" else " (" ^ fnotes ^ ")")
  in
  match (templ, p_getenv conf.env "notmpl") with
  | Some _, Some "on" ->
      Wiki.print_mod_view_page conf true (Adef.encoded "NOTES") fnotes title
        nenv s
  | Some (ic, _fname), _ -> (
      match p_getenv conf.env "ajax" with
      | Some "on" ->
          let s_digest =
            List.fold_left (fun s (k, v) -> s ^ k ^ "=" ^ v ^ "\n") "" nenv ^ s
          in
          let digest = Mutil.digest s_digest in
          let charset = if conf.charset = "" then "utf-8" else conf.charset in
          Wserver.header
            (Format.sprintf "Content-type: application/json; charset=%s" charset);
          Wserver.printf "{\"digest\":\"%s\",\"r\":%s}" digest s
      | _ -> Templ.copy_from_templ conf [] ic)
  | _ ->
      Wiki.print_mod_view_page conf true (Adef.encoded "NOTES") fnotes title
        nenv s

let print_mod_ok conf base =
  let fname = function
    | Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  let edit_mode _ = if conf.wizard then Some "NOTES" else None in
  let mode = "NOTES" in
  let read_string = read_notes base in
  let commit = Notes.commit_notes conf base in
  let string_filter = string_with_macros conf [] in
  let file_path = file_path conf base in
  let wi =
    {
      Wiki.wi_mode = mode;
      Wiki.wi_file_path = file_path;
      Wiki.wi_person_exists = person_exists conf base;
      Wiki.wi_always_show_link = conf.wizard || conf.friend;
    }
  in
  Wiki.print_mod_ok conf wi edit_mode fname read_string commit string_filter
    true

let begin_text_without_html_tags lim s =
  let rec loop i size len =
    if i >= String.length s then Buff.get len
    else if size > lim && String.length s > i + 3 then Buff.get len ^ "..."
    else if s.[i] = '<' then
      let i =
        let rec loop i =
          if i = String.length s then i
          else if s.[i] = '>' then i + 1
          else loop (i + 1)
        in
        loop (i + 1)
      in
      loop i size len
    else if s.[i] = '=' then loop (i + 1) size len
    else
      let nbc = Utf8.nbc s.[i] in
      loop (i + nbc) (size + 1) (Buff.mstore len (String.sub s i nbc))
  in
  loop 0 0 0

(* Sorting directories first then files alphabetically *)
let sort_entries db =
  List.sort
    (fun (r1, opt1) (r2, opt2) ->
      match (opt1, opt2) with
      | None, Some _ -> -1
      | Some _, None -> 1
      | _ -> Gutil.alphabetic_order (Name.lower r1) (Name.lower r2))
    db

(* Build path hierarchy as directory entries *)
let build_path_hierarchy d =
  let parts = String.split_on_char NotesLinks.char_dir_sep d in
  let rec build_paths acc current = function
    | [] -> List.rev acc
    | part :: rest ->
        let new_current =
          if current = "" then part
          else current ^ String.make 1 NotesLinks.char_dir_sep ^ part
        in
        build_paths ((new_current, part) :: acc) new_current rest
  in
  build_paths [] "" parts

(* Format directory entry with proper indentation level *)
let format_folder_entry conf depth r path_to is_current is_path =
  Format.sprintf {|<div class="my-1" style="margin-left: %.1fem;">%s</div>|}
    (1.5 *. float_of_int depth)
    (if is_current then
     Format.sprintf
       {|<span class="text-muted">
             <i class="far fa-folder-open fa-fw mr-2"></i>%s
          </span>|}
       (Util.escape_html r :> string)
    else
      let title_attr =
        let back_txt = Utf8.capitalize_fst (transl conf "back") in
        if is_path then
          if r = ".." then Format.sprintf {| title="%s .."|} back_txt
          else
            Format.sprintf {| title="%s %s"|} back_txt
              (Util.escape_html r :> string)
        else ""
      in
      Format.sprintf
        {|<a href="%sm=MISC_NOTES&d=%s"%s>
             <i class="far fa-folder%s fa-fw mr-2"></i>%s
          </a>|}
        (commd conf :> string)
        (Mutil.encode path_to :> string)
        title_attr
        (if is_path then "-open" else "")
        (Util.escape_html r :> string))

(* Format file entry with proper indentation level *)
let format_file_entry conf depth d f n_type title =
  let icon = match n_type with "gallery" -> "image" | _ -> "file-lines" in
  Format.sprintf
    {|<div class="py-1" style="margin-left: %.1fem;">
         <a href="%sm=NOTES&f=%s"><i class="far fa-%s fa-fw mr-2"></i>%s</a>
         %s
       </div>|}
    (1.5 *. float_of_int depth)
    (commd conf :> string)
    (Mutil.encode (if d = "" then f else d ^ ":" ^ f) :> string)
    icon
    (Util.escape_html f :> string)
    (if (title :> string) <> "" then
     Format.sprintf {|<span class="text-muted ml-2">%s</span>|}
       (title :> string)
    else "")

(* Format back button *)
let format_back_button conf d =
  Format.sprintf
    {|<div class="mb-3">
        <a href="%sm=MISC_NOTES%s" class="btn btn-outline-primary">
          <i class="fa fa-arrow-left mr-2"></i>%s
        </a>
      </div>|}
    (commd conf :> string)
    (match String.rindex_opt d NotesLinks.char_dir_sep with
    | Some i -> "&d=" ^ String.sub d 0 i
    | None -> "")
    (Utf8.capitalize_fst (transl conf "back"))

(* Format simple filename by removing parent directory *)
let format_simple_filename d f =
  let prefix = d ^ String.make 1 NotesLinks.char_dir_sep in
  let prefix_len = String.length prefix in
  if String.length f > prefix_len && String.sub f 0 prefix_len = prefix then
    String.sub f prefix_len (String.length f - prefix_len)
  else f

let print_misc_notes conf base =
  let d = match p_getenv conf.env "d" with Some d -> d | None -> "" in
  Hutil.header conf (fun _ -> ());

  Format.sprintf
    {|<h1 class="mb-3"><i class="far fa-clipboard fa-sm mr-3"></i>%s</h1>|}
    (if d <> "" then d
    else
      transl conf "miscellaneous notes"
      |> Util.translate_eval |> Utf8.capitalize_fst)
  |> Output.print_sstring conf;

  if d <> "" then format_back_button conf d |> Output.print_sstring conf;

  let db = notes_links_db conf base true in
  let db =
    List.fold_right
      (fun (f, _) list ->
        if String.length f >= String.length d then
          if String.sub f 0 (String.length d) = d then
            let r =
              String.sub f (String.length d) (String.length f - String.length d)
            in
            if d = "" || (r <> "" && r.[0] = NotesLinks.char_dir_sep) then
              let r =
                if d = "" then r else String.sub r 1 (String.length r - 1)
              in
              try
                let i = String.index r NotesLinks.char_dir_sep in
                let r = String.sub r 0 i in
                match list with
                | (r', None) :: _ when r = r' -> list
                | _ -> (r, None) :: list
              with Not_found -> (r, Some f) :: list
            else list
          else list
        else list)
      db []
  in
  let db = sort_entries db in

  if db <> [] then (
    Output.print_sstring conf {|<div class="px-1">|};

    if d <> "" then
      format_folder_entry conf 0 ".." "" false true |> Output.print_sstring conf;

    if d <> "" then
      List.iteri
        (fun i (path_to, dirname) ->
          let parts = String.split_on_char NotesLinks.char_dir_sep d in
          let is_current = i = List.length parts - 1 in
          format_folder_entry conf (i + 1) dirname path_to is_current true
          |> Output.print_sstring conf)
        (build_path_hierarchy d);

    let current_depth =
      if d = "" then 0
      else List.length (String.split_on_char NotesLinks.char_dir_sep d) + 1
    in

    List.iter
      (function
        | _, Some f ->
            let txt =
              let n, s = read_notes base f in
              let t = try List.assoc "TITLE" n with Not_found -> "" in
              if t <> "" then (Util.escape_html t :> string)
              else if s = "" then ""
              else
                "<em>"
                ^ (begin_text_without_html_tags 50 s |> Util.escape_html
                    :> string)
                ^ "</em>"
            in
            let n, _ = read_notes base f in
            let n_type = try List.assoc "TYPE" n with Not_found -> "" in
            let simple_f = format_simple_filename d f in
            format_file_entry conf current_depth d simple_f n_type txt
            |> Output.print_sstring conf
        | r, None ->
            format_folder_entry conf current_depth r
              (if d = "" then r
              else d ^ String.make 1 NotesLinks.char_dir_sep ^ r)
              false false
            |> Output.print_sstring conf)
      db;
    Output.print_sstring conf "</div>");

  if d = "" then print_search_form conf None;
  Hutil.trailer conf

(* searching *)

let search_text conf base s =
  let s = if s = "" then " " else s in
  let case_sens = p_getenv conf.env "c" = Some "on" in
  let db =
    let db = notes_links_db conf base true in
    let db = "" :: List.map fst db in
    match p_getenv conf.env "z" with
    | None -> db
    | Some f ->
        let rec loop = function
          | fnotes :: list -> if f = fnotes then list else loop list
          | [] -> []
        in
        loop db
  in
  let noteo =
    let rec loop = function
      | fnotes :: list ->
          let nenv, nt = read_notes base fnotes in
          let tit = try List.assoc "TITLE" nenv with Not_found -> "" in
          if in_text case_sens s tit || in_text case_sens s nt then
            Some (fnotes, tit, nt)
          else loop list
      | [] -> None
    in
    loop db
  in
  match noteo with
  | Some (fnotes, tit, nt) ->
      print_whole_notes conf base fnotes (Util.safe_html tit) nt
        (Some (case_sens, s))
  | None -> print_misc_notes conf base

let print_misc_notes_search conf base =
  match try Some (List.assoc "s" conf.env) with Not_found -> None with
  | Some s -> search_text conf base (Mutil.gen_decode false s)
  | None -> print_misc_notes conf base
