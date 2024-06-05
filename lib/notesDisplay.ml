(* Copyright (c) 1998-2007 INRIA *)

open Def
open Config
open Util
open Notes

let print_search_form conf from_note =
  Output.print_sstring conf "<div class=\"form-group\">\n";
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
  Hutil.header_with_title conf (fun _ -> ());
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
  Output.print_sstring conf {|<div class="d-flex justify-content-between">|};
  if (title :> string) <> "" then (
    let title =
      match ho with
      | Some (case_sens, h) ->
          html_highlight case_sens h (title : Adef.safe_string :> string)
          |> Adef.safe
      | None -> title
    in
    Output.print_sstring conf {|<h1 class="my-3">|};
    Output.print_string conf title;
    Output.print_sstring conf {|</h1>|});
  Output.printf conf "</div>\n";
  Util.include_template conf [] "summary" (fun () -> ());
  let file_path = file_path conf base in
  let s = string_with_macros conf [] s in
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
  let s =
    match ho with
    | Some (case_sens, h) ->
        html_highlight case_sens h (s : Adef.safe_string :> string) |> Adef.safe
    | None -> s
  in
  Output.print_string conf s;
  if ho <> None then print_search_form conf (Some fnotes);
  Hutil.trailer conf

let print_notes_part conf base fnotes (title : Adef.safe_string) s cnt0 =
  Hutil.header_with_title conf (fun _ ->
      if (title :> string) = "" then
        Output.print_string conf (Util.escape_html fnotes)
      else Output.print_string conf title);
  Util.include_template conf [] "summary" (fun () -> ());
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
  <a href="%sm=MOD_FAM&i=%s&ip=%s#comments" title="%s %s %s">
    <i class="fa fa-user fa-sm"></i><i class="fa fa-user fa-sm"></i></a></td>|}
             (commd conf :> string)
             (Gwdb.string_of_ifam ifam)
             (Gwdb.get_iper fath |> Gwdb.string_of_iper)
             (Utf8.capitalize_fst (transl conf "modify"))
             (transl conf "comment")
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
      if
        match typ with
        | Some t ->
            let nenv, _ = read_notes base fnotes in
            let n_type = try List.assoc "TYPE" nenv with Not_found -> "" in
            t = n_type
        | None -> true
      then (
        let fnote_title = fmt_fnote_title conf base fnotes in
        if conf.wizard then
          Output.print_sstring conf
            (Format.sprintf
               {|
<td class="text-center">
  <a href="%sm=MOD_NOTES&f=%s" title="%s">
    <i class="far fa-file-lines"></i></a></td>|}
               (commd conf :> string)
               (Util.uri_encode fnotes)
               (Utf8.capitalize_fst (transl conf "modify note")));
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

let print_linked_list_gallery conf base pgl =
  Wserver.printf "<div class=\"d-flex flex-wrap\">\n";
  List.iter
    (fun pg ->
      match pg with
      | Def.NLDB.PgMisc fnotes ->
          let nenv, s = read_notes base fnotes in
          if (try List.assoc "TYPE" nenv with Not_found -> "") = "gallery"
          then
            let img_url, img_name = Notes.json_extract_img conf s in
            Wserver.printf
              "<div class=\"item_gallery\"><a href=\"%sm=NOTES&f=%s\"><img \
               src=\"%s\" title=\"%s | %s\"></a></div>\n"
              (commd conf :> string)
              fnotes img_url fnotes img_name
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
      let templ, typ =
        try
          let typ = List.assoc "TYPE" nenv in
          let fname = "notes_" ^ typ in
          (Util.open_etc_file conf fname, typ)
        with Not_found -> (None, "")
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
  let env, s = read_notes base fnotes in
  let typ = try List.assoc "TYPE" env with Not_found -> "" in
  let templ =
    let fname = "notes_upd_" ^ typ in
    Util.open_etc_file conf fname
  in
  let title _ =
    Output.printf conf "%s - %s%s"
      (Utf8.capitalize_fst (transl conf "base notes"))
      conf.bname
      (if fnotes = "" then "" else " (" ^ fnotes ^ ")")
  in
  match (templ, p_getenv conf.env "notmpl") with
  | Some _, Some "on" ->
      Wiki.print_mod_view_page conf true (Adef.encoded "NOTES") fnotes title env
        s
  | Some (ic, _fname), _ -> (
      match p_getenv conf.env "ajax" with
      | Some "on" ->
          let s_digest =
            List.fold_left (fun s (k, v) -> s ^ k ^ "=" ^ v ^ "\n") "" env ^ s
          in
          let digest = Mutil.digest s_digest in
          let charset = if conf.charset = "" then "utf-8" else conf.charset in
          Wserver.header
            (Format.sprintf "Content-type: application/json; charset=%s" charset);
          Wserver.printf "{\"digest\":\"%s\",\"r\":%s}" digest s
      | _ -> Templ.copy_from_templ conf [] ic)
  | _ ->
      Wiki.print_mod_view_page conf true (Adef.encoded "NOTES") fnotes title env
        s

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

let print_misc_notes conf base =
  let d = match p_getenv conf.env "d" with Some d -> d | None -> "" in
  let title h =
    Output.print_string conf
      (if d = "" then
       transl conf "miscellaneous notes"
       |> Util.translate_eval |> Utf8.capitalize_fst |> Adef.escaped
      else if h then "- " ^<^ Util.escape_html d ^>^ " -"
      else "<tt>- " ^<^ Util.escape_html d ^>^ " -</tt>")
  in
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
  Hutil.header conf title;
  if db <> [] then (
    Output.print_sstring conf "<ul>";
    if d <> "" then
      Format.sprintf
        {|<a href="%sm=MISC_NOTES%s"><i class="fa fa-arrow-left"></i></a>|}
        (commd conf :> string)
        (match String.rindex_opt d NotesLinks.char_dir_sep with
        | Some i -> "&d=" ^ String.sub d 0 i
        | None -> "")
      |> Output.print_sstring conf;
    List.iter
      (function
        | r, Some f ->
            let txt =
              let n, s = read_notes base f in
              let t = try List.assoc "TITLE" n with Not_found -> "" in
              if t <> "" then Util.escape_html t
              else if s = "" then Adef.escaped ""
              else
                "<em>"
                ^<^ (begin_text_without_html_tags 50 s |> Util.escape_html)
                ^>^ "</em>"
            in
            let c =
              let f = file_path conf base (path_of_fnotes f) in
              if Sys.file_exists f then "" else " style=\"color:red\""
            in
            Output.print_sstring conf {|<li class="file"><a href="|};
            Output.print_string conf (commd conf);
            Output.print_sstring conf {|m=NOTES&f=|};
            Output.print_string conf (Mutil.encode f);
            Output.print_sstring conf {|"|};
            Output.print_sstring conf c;
            Output.print_sstring conf ">";
            Output.print_string conf (Util.escape_html r);
            Output.print_sstring conf "</a>";
            if (txt :> string) <> "" then (
              Output.print_sstring conf (transl conf ":");
              Output.print_sstring conf " ";
              Output.print_string conf txt);
            Output.print_sstring conf "</li>"
        | r, None ->
            Output.print_sstring conf {|<li class="folder"><tt><a href="|};
            Output.print_string conf (commd conf);
            Output.print_sstring conf "m=MISC_NOTES&d=";
            if d = "" then Output.print_string conf (Mutil.encode r)
            else (
              Output.print_string conf (Mutil.encode d);
              Output.print_sstring conf (String.make 1 NotesLinks.char_dir_sep);
              Output.print_string conf (Mutil.encode r));
            Output.print_sstring conf {|">|};
            Output.print_string conf (Util.escape_html r);
            Output.printf conf " --&gt;</a></tt></li>")
      db;
    Output.print_sstring conf "</ul>");
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
