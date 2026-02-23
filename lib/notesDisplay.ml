(* Copyright (c) 1998-2007 INRIA *)

let print_search_form conf from_note =
  Output.print_sstring conf "<table>\n";
  Output.print_sstring conf "<tr>\n";
  Output.printf conf "<td align=\"%s\">\n" conf.Config.right;
  Output.printf conf "<form method=\"get\" action=\"%s\">\n" conf.Config.command;
  Output.print_sstring conf "<p>\n";
  Util.hidden_env conf;
  Output.print_sstring conf
    {|<input type="hidden" name="m" value="MISC_NOTES_SEARCH">|};
  Output.print_sstring conf {|<input name="s" size="30" maxlength="40" value="|};
  (match Util.p_getenv conf.Config.env "s" with
  | Some s -> Output.print_string conf (Util.escape_html s)
  | None -> ());
  Output.print_sstring conf {|">|};
  (match from_note with
  | Some n ->
      Output.print_sstring conf {|<input type="hidden" name="z" value="|};
      Output.print_string conf (Util.escape_html n);
      Output.print_sstring conf {|">|}
  | None -> ());
  Output.print_sstring conf "<br>\n";
  Output.print_sstring conf "<label>\n";
  Output.printf conf "<input type=\"checkbox\" name=\"c\" value=\"on\"%s>\n"
    (match Util.p_getenv conf.Config.env "c" with
    | Some "on" -> " checked=\"checked\""
    | Some _ | None -> "");
  Output.printf conf "%s\n" (Util.transl_nth conf "search/case sensitive" 1);
  Output.print_sstring conf "</label>\n";
  Output.print_sstring conf
    {|<button type="submit" class="btn btn-secondary btn-lg">|};
  Util.transl_nth conf "search/case sensitive" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "</button></p></form></td></tr></table>"

let print_whole_notes conf base fnotes (title : Adef.safe_string) s ho =
  Hutil.header_no_page_title conf (fun _ ->
      if (title :> string) = "" then
        Output.print_string conf (Util.escape_html fnotes)
      else Output.print_string conf title);
  let what_links_page () =
    if fnotes <> "" then (
      Output.print_sstring conf {|<a href="|};
      Output.print_string conf (Util.commd conf);
      Output.print_sstring conf {|m=NOTES&f=|};
      Output.print_string conf (Mutil.encode fnotes);
      Output.print_sstring conf {|&ref=on" class="mx-2">(|};
      Output.print_sstring conf (Util.transl conf "linked pages");
      Output.print_sstring conf ")</a>\n")
  in
  Hutil.gen_print_link_to_welcome what_links_page conf true;
  if (title :> string) <> "" then (
    let title =
      match ho with
      | Some (case_sens, h) ->
          Util.html_highlight case_sens h (title : Adef.safe_string :> string)
          |> Adef.safe
      | None -> title
    in
    Output.print_sstring conf {|<h1>|};
    Output.print_string conf title;
    Output.print_sstring conf {|</h1>|});
  Util.include_template conf [] "summary" (fun () -> ());
  let file_path = Notes.file_path conf base in
  let s =
    Util.string_with_macros ~with_links_target_attribute:false ~conf ~env:[] s
  in
  let edit_opt = Some (conf.Config.wizard, "NOTES", fnotes) in
  let s =
    let wi =
      {
        Wiki.wi_mode = "NOTES";
        Wiki.wi_file_path = file_path;
        Wiki.wi_person_exists = Util.person_exists conf base;
        Wiki.wi_always_show_link = conf.Config.wizard || conf.Config.friend;
      }
    in
    Wiki.html_with_summary_of_tlsw conf base wi edit_opt s
  in
  let s = Util.safe_html s in
  let s =
    match ho with
    | Some (case_sens, h) ->
        Util.html_highlight case_sens h (s : Adef.safe_string :> string)
        |> Adef.safe
    | None -> s
  in
  Output.print_string conf s;
  if ho <> None then print_search_form conf (Some fnotes);
  Hutil.trailer conf

let print_notes_part conf base fnotes (title : Adef.safe_string) s cnt0 =
  Hutil.header_no_page_title conf (fun _ ->
      if (title :> string) = "" then
        Output.print_string conf (Util.escape_html fnotes)
      else Output.print_string conf title);
  Hutil.print_link_to_welcome conf true;
  Util.include_template conf [] "summary" (fun () -> ());
  if cnt0 = 0 && (title :> string) <> "" then (
    Output.print_sstring conf "<br><br><h1>";
    Output.print_string conf title;
    Output.print_sstring conf "</h1>");
  let s =
    Util.string_with_macros ~with_links_target_attribute:false ~conf ~env:[] s
  in
  let lines = Wiki.extract_sub_part s cnt0 in
  let mode = "NOTES" in
  let wi =
    {
      Wiki.wi_mode = mode;
      Wiki.wi_file_path = Notes.file_path conf base;
      Wiki.wi_person_exists = Util.person_exists conf base;
      Wiki.wi_always_show_link = conf.Config.wizard || conf.Config.friend;
    }
  in
  Wiki.print_sub_part conf base wi conf.Config.wizard mode fnotes cnt0 lines;
  Hutil.trailer conf

let print_linked_list conf base pgl =
  Output.print_sstring conf "<ul>";
  List.iter
    (fun pg ->
      Output.print_sstring conf "<li>";
      (match pg with
      | Def.NLDB.PgInd ip ->
          Output.print_sstring conf "<tt>";
          if conf.Config.wizard then (
            Output.print_sstring conf {|<a href="|};
            Output.print_string conf (Util.commd conf);
            Output.print_sstring conf "&i=";
            Output.print_string conf (Gwdb.string_of_iper ip |> Mutil.encode);
            Output.print_sstring conf
              {|"><sup><i class="fa fa-cog"></i></sup></a>|});
          let p = Util.pget conf base ip in
          Output.print_sstring conf "<span class=\"mx-2\">";
          Output.print_string conf
            (NameDisplay.referenced_person_title_text conf base p);
          Output.print_string conf (DateDisplay.short_dates_text conf base p);
          Output.print_sstring conf "</span></tt>"
      | Def.NLDB.PgFam ifam ->
          let fam = Gwdb.foi base ifam in
          let fath = Util.pget conf base (Gwdb.get_father fam) in
          let moth = Util.pget conf base (Gwdb.get_mother fam) in
          Output.print_sstring conf "<tt>";
          if conf.Config.wizard then (
            Output.print_sstring conf {|<a class="mx-2" href="|};
            Output.print_string conf (Util.commd conf);
            Output.print_sstring conf "m=MOD_FAM&i=";
            Output.print_string conf (Gwdb.string_of_ifam ifam |> Mutil.encode);
            Output.print_sstring conf "&ip=";
            Output.print_string conf
              (Gwdb.get_iper fath |> Gwdb.string_of_iper |> Mutil.encode);
            Output.print_sstring conf
              {|"><sup><i class="fa fa-cog"></i></sup></a>|});
          Output.print_sstring conf "<span class=\"mx-2\">";
          Output.print_string conf
            (NameDisplay.referenced_person_title_text conf base fath);
          Output.print_string conf (DateDisplay.short_dates_text conf base fath);
          Output.print_sstring conf " &amp; ";
          Output.print_string conf
            (NameDisplay.referenced_person_title_text conf base moth);
          Output.print_sstring conf " ";
          Output.print_string conf (DateDisplay.short_dates_text conf base moth);
          Output.print_sstring conf "</span></tt>"
      | Def.NLDB.PgNotes ->
          Output.print_sstring conf "<tt>";
          if conf.Config.wizard then (
            Output.print_sstring conf {|<a class="mx-2" href="|};
            Output.print_string conf (Util.commd conf);
            Output.print_sstring conf
              {|m=MOD_NOTES"><sup><i class="fa fa-cog"></i></sup></a>|});
          Output.print_sstring conf "<a class=\"mx-2\" href=\"";
          Output.print_string conf (Util.commd conf);
          Output.print_sstring conf "m=NOTES\">";
          Output.print_sstring conf (Util.transl_nth conf "note/notes" 1);
          Output.print_sstring conf "</a></tt>"
      | Def.NLDB.PgMisc fnotes ->
          let nenv, _ = Notes.read_notes base fnotes in
          let title = Option.value (List.assoc_opt "TITLE" nenv) ~default:"" in
          let title = Util.safe_html title in
          Output.print_sstring conf "<tt>";
          if conf.Config.wizard then (
            Output.print_sstring conf {|<a class="mx-2" href="|};
            Output.print_string conf (Util.commd conf);
            Output.print_sstring conf {|m=MOD_NOTES&f=|};
            Output.print_string conf (Mutil.encode fnotes);
            Output.print_sstring conf
              {|"><sup><i class="fa fa-cog"></i></sup></a>|});
          Output.print_sstring conf {|<a class="mx-2" href="|};
          Output.print_string conf (Util.commd conf);
          Output.print_sstring conf {|m=NOTES&f=|};
          Output.print_string conf (Mutil.encode fnotes);
          Output.print_sstring conf {|">|};
          Output.print_string conf (Util.escape_html fnotes);
          Output.print_sstring conf "</a>";
          if (title :> string) <> "" then (
            Output.print_sstring conf "(";
            Output.print_string conf title;
            Output.print_sstring conf ")");
          Output.print_sstring conf "</tt>"
      | Def.NLDB.PgWizard wizname ->
          Output.print_sstring conf "<tt>";
          if conf.Config.wizard then (
            Output.print_sstring conf {|<a class="mx-2" href="|};
            Output.print_string conf (Util.commd conf);
            Output.print_sstring conf {|m=MOD_WIZNOTES&f=|};
            Output.print_string conf (Mutil.encode wizname);
            Output.print_sstring conf
              {|"><sup><i class="fa fa-cog"></i></sup></a>|});
          Output.print_sstring conf {|<a class="mx-2" href="|};
          Output.print_string conf (Util.commd conf);
          Output.print_sstring conf {|m=WIZNOTES&f=|};
          Output.print_string conf (Mutil.encode wizname);
          Output.print_sstring conf {|">|};
          Output.print_string conf (Util.escape_html wizname);
          Output.print_sstring conf "</a><i>(";
          Output.print_sstring conf
            (Util.transl_nth conf "wizard/wizards/friend/friends/exterior" 0);
          Output.print_sstring conf ")</i></tt>");
      Output.print_sstring conf "</li>")
    pgl;
  Output.print_sstring conf "</ul>"

let print_what_links conf base fnotes =
  let title h =
    Output.print_sstring conf
      (Utf8.capitalize_fst (Util.transl conf "linked pages"));
    Output.print_sstring conf " ";
    if h then (
      Output.print_sstring conf "[";
      Output.print_string conf (Util.escape_html fnotes);
      Output.print_sstring conf "]")
    else (
      Output.print_sstring conf {|<tt>[<a href="|};
      Output.print_string conf (Util.commd conf);
      Output.print_sstring conf "m=NOTES&f=";
      Output.print_string conf (Mutil.encode fnotes);
      Output.print_sstring conf {|">|};
      Output.print_string conf (Util.escape_html fnotes);
      Output.print_sstring conf "</a>]</tt>")
  in
  let db = Notes.notes_links_db conf base false in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Option.iter (print_linked_list conf base) (List.assoc_opt fnotes db);
  Hutil.trailer conf

let print conf base =
  let fnotes =
    match Util.p_getenv conf.Config.env "f" with
    | Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  match Util.p_getenv conf.Config.env "ref" with
  | Some "on" -> print_what_links conf base fnotes
  | _ -> (
      let nenv, s = Notes.read_notes base fnotes in
      let title = Option.value (List.assoc_opt "TITLE" nenv) ~default:"" in
      let title = Util.safe_html title in
      match Util.p_getint conf.Config.env "v" with
      | Some cnt0 -> print_notes_part conf base fnotes title s cnt0
      | None -> print_whole_notes conf base fnotes title s None)

let print_mod conf base =
  let fnotes =
    match Util.p_getenv conf.Config.env "f" with
    | Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  let title _ =
    Output.printf conf "%s - %s%s"
      (Utf8.capitalize_fst (Util.transl conf "base notes"))
      conf.Config.bname
      (if fnotes = "" then "" else " (" ^ fnotes ^ ")")
  in
  let env, s = Notes.read_notes ~limit:false base fnotes in
  Wiki.print_mod_view_page conf true (Adef.encoded "NOTES") fnotes title env s

let print_mod_ok conf base =
  let fname = function
    | Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  let edit_mode _ = if conf.Config.wizard then Some "NOTES" else None in
  let mode = "NOTES" in
  let read_string = Notes.read_notes ~limit:false base in
  let commit = Notes.commit_notes conf base in
  let string_filter = Util.string_with_macros ~conf ~env:[] in
  let file_path = Notes.file_path conf base in
  let wi =
    {
      Wiki.wi_mode = mode;
      Wiki.wi_file_path = file_path;
      Wiki.wi_person_exists = Util.person_exists conf base;
      Wiki.wi_always_show_link = conf.Config.wizard || conf.Config.friend;
    }
  in
  Wiki.print_mod_ok conf base wi edit_mode fname read_string commit
    string_filter true

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
  let d =
    match Util.p_getenv conf.Config.env "d" with Some d -> d | None -> ""
  in
  let title h =
    Output.print_string conf
      (if d = "" then
         Util.transl conf "miscellaneous notes"
         |> Util.translate_eval |> Utf8.capitalize_fst |> Adef.escaped
       else if h then
         let open Def in
         "- " ^<^ Util.escape_html d ^>^ " -"
       else
         let open Def in
         "<tt>- " ^<^ Util.escape_html d ^>^ " -</tt>")
  in
  let db = Notes.notes_links_db conf base true in
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
  Hutil.header_link_welcome conf title;
  if db <> [] then (
    Output.print_sstring conf "<ul>";
    if d <> "" then (
      Output.print_sstring conf {|<li class="parent">|};
      Output.print_sstring conf {|<a href="|};
      Output.print_string conf (Util.commd conf);
      Output.print_sstring conf "m=MISC_NOTES";
      (match String.rindex_opt d NotesLinks.char_dir_sep with
      | Some i ->
          let open Def in
          Output.print_string conf @@ "&d=" ^<^ Mutil.encode (String.sub d 0 i)
      | None -> ());
      Output.print_sstring conf "<tt>&lt;--</tt></a></li>");
    List.iter
      (function
        | r, Some f ->
            let txt =
              let n, s = Notes.read_notes base f in
              let t = Option.value (List.assoc_opt "TITLE" n) ~default:"" in
              if t <> "" then Util.escape_html t
              else if s = "" then Adef.escaped ""
              else
                let open Def in
                "<em>"
                ^<^ (begin_text_without_html_tags 50 s |> Util.escape_html)
                ^>^ "</em>"
            in
            let c =
              let f = Notes.file_path conf base (Notes.path_of_fnotes f) in
              if Sys.file_exists f then "" else " style=\"color:red\""
            in
            Output.print_sstring conf {|<li class="file"><tt>[<a href="|};
            Output.print_string conf (Util.commd conf);
            Output.print_sstring conf {|m=NOTES&f=|};
            Output.print_string conf (Mutil.encode f);
            Output.print_sstring conf {|"|};
            Output.print_sstring conf c;
            Output.print_sstring conf ">";
            Output.print_string conf (Util.escape_html r);
            Output.print_sstring conf "</a>]</tt>";
            if (txt :> string) <> "" then (
              Output.print_sstring conf (Util.transl conf ":");
              Output.print_sstring conf " ";
              Output.print_string conf txt);
            Output.print_sstring conf "</li>"
        | r, None ->
            Output.print_sstring conf {|<li class="folder"><tt><a href="|};
            Output.print_string conf (Util.commd conf);
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
  let case_sens = Util.p_getenv conf.Config.env "c" = Some "on" in
  let db =
    let db = Notes.notes_links_db conf base true in
    let db = "" :: List.map fst db in
    match Util.p_getenv conf.Config.env "z" with
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
          let nenv, nt = Notes.read_notes base fnotes in
          let tit = Option.value (List.assoc_opt "TITLE" nenv) ~default:"" in
          if Util.in_text case_sens s tit || Util.in_text case_sens s nt then
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
  match List.assoc_opt "s" conf.Config.env with
  | Some s -> search_text conf base (Mutil.gen_decode false s)
  | None -> print_misc_notes conf base
