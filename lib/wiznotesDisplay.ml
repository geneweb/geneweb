(* Copyright (c) 1998-2007 INRIA *)

let dir conf base =
  Filename.concat
    (GWPARAM.bpath (conf.Config.bname ^ ".gwb"))
    (Gwdb.base_wiznotes_dir base)

let wzfile wddir wz = Filename.concat wddir (wz ^ ".txt")

let read_auth_file fname =
  let data = Util.read_gen_auth_file fname in
  List.map
    (fun au ->
      let wizname =
        Option.fold
          (String.index_opt au.Util.au_info ':')
          ~some:(String.sub au.Util.au_info 0)
          ~none:au.Util.au_user
      in
      let wizname, wizorder, islash =
        Option.fold
          (String.index_opt wizname '/')
          ~some:(fun i ->
            let w1 = String.sub wizname 0 i in
            let l = String.length wizname in
            let w2 = String.sub wizname (i + 1) (l - i - 1) in
            (w1 ^ w2, w2 ^ w1, i))
          ~none:(wizname, wizname, 0)
      in
      (* wizname may contain key: wizname|fn.occ.sn *)
      let wizname =
        match String.index_opt wizname '|' with
        | None -> wizname
        | Some i -> String.sub wizname 0 i
      in
      (au.Util.au_user, (wizname, (wizorder, islash))))
    data

let read_wizard_notes ?(limit = true) fname =
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
  | None -> ("", 0.)
  | Some ic ->
      let date, len =
        try
          let line = input_line ic in
          if line = "WIZNOTES" then
            let line = input_line ic in
            (float_of_string line, 0)
          else
            let s = Unix.stat fname in
            (s.Unix.st_mtime, Buff.store (Buff.mstore 0 line) '\n')
        with End_of_file | Failure _ -> (0., 0)
      in
      let rec loop len =
        match try Some (input_char ic) with End_of_file -> None with
        | Some c -> loop (Buff.store len c)
        | None ->
            close_in ic;
            len
      in
      let len = loop len in
      ( (if limit then Notes.limit_display_length else Fun.id) @@ Buff.get len,
        date )

let write_wizard_notes fname nn =
  if nn = "" then Files.rm fname
  else
    match try Some (Secure.open_out fname) with Sys_error _ -> None with
    | None -> ()
    | Some oc ->
        Printf.fprintf oc "WIZNOTES\n%.0f\n" (Unix.time ());
        output_string oc nn;
        output_string oc "\n";
        close_out oc

let wiznote_date wfile =
  match try Some (Secure.open_in wfile) with Sys_error _ -> None with
  | None -> ("", 0.)
  | Some ic ->
      let date =
        try
          let line = input_line ic in
          if line = "WIZNOTES" then float_of_string (input_line ic)
          else raise Exit
        with End_of_file | Failure _ | Exit ->
          let s = Unix.stat wfile in
          s.Unix.st_mtime
      in
      close_in ic;
      (wfile, date)

let print_wizards_by_alphabetic_order conf list =
  let wprint_elem (wz, (wname, (_, islash)), wfile, stm) =
    let tm = Unix.localtime stm in
    let wlink =
      (conf.Config.wizard && conf.Config.user = wz)
      || wfile <> "" || conf.Config.manitou
    in
    if wlink then (
      Output.print_sstring conf {|<a href="|};
      Output.print_string conf (Util.commd conf);
      Output.print_sstring conf "m=WIZNOTES&f=";
      Output.print_string conf (Mutil.encode wz);
      Output.printf conf "&d=%d-%02d-%02d,%02d:%02d:%02d"
        (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec;
      Output.print_sstring conf {|">|});
    if islash > 0 then (
      let s1 =
        let islash = if wname.[islash - 1] = ' ' then islash - 1 else islash in
        String.sub wname 0 islash
      in
      let s2 = String.sub wname islash (String.length wname - islash) in
      Output.print_string conf (Util.escape_html s2);
      Output.print_sstring conf " (";
      Output.print_string conf (Util.escape_html s1);
      Output.print_sstring conf ")")
    else Output.print_string conf (Util.escape_html wname);
    if wlink then Output.print_sstring conf "</a>"
  in
  let order (_, (_, (ord, _)), _, _) = ord in
  Util.wprint_in_columns conf order wprint_elem list

let print_wizards_by_date conf list =
  let sep_period_list =
    [
      ( (fun tm -> tm.Unix.tm_mon),
        fun (tm : Unix.tm) ->
          let dmy =
            Date.
              {
                year = tm.tm_year + 1900;
                month = tm.tm_mon + 1;
                day = tm.tm_mday;
                prec = Sure;
                delta = 0;
              }
          in
          Date.Dgreg (dmy, Dgregorian)
          |> (DateDisplay.string_of_ondate conf :> Date.date -> string)
          |> Utf8.capitalize_fst |> Output.print_sstring conf );
      ( (fun tm -> tm.Unix.tm_year),
        fun tm ->
          Output.print_sstring conf (string_of_int @@ (tm.Unix.tm_year + 1900))
      );
    ]
  in
  let list =
    List.sort (fun (_, _, _, mtm1) (_, _, _, mtm2) -> compare mtm2 mtm1) list
  in
  Output.print_sstring conf "<dl><dt>";
  ignore
  @@ List.fold_left
       (fun (spl, prev) (wz, (wname, _), wfile, stm) ->
         let tm = Unix.localtime stm in
         let new_item, spl =
           match prev with
           | Some prev_tm ->
               let sep_period, _ = List.hd spl in
               if sep_period tm <> sep_period prev_tm then (
                 Output.print_sstring conf "</dd><dt>";
                 let spl =
                   match spl with
                   | _ :: (next_sp, _) :: _ ->
                       if next_sp tm <> next_sp prev_tm then List.tl spl
                       else spl
                   | _ -> spl
                 in
                 (true, spl))
               else (false, spl)
           | None -> (true, spl)
         in
         (if new_item then
          if stm = 0.0 then Output.print_sstring conf "....."
          else
            match spl with
            | (_, disp_sep_period) :: _ -> disp_sep_period tm
            | [] -> ());
         if new_item then Output.print_sstring conf "</dt><dd>";
         let wname = if wname = "" then wz else wname in
         if not (prev = None || new_item) then Output.print_sstring conf ", ";
         if (conf.Config.wizard && conf.Config.user = wz) || wfile <> "" then (
           Output.print_sstring conf {|<a href="|};
           Output.print_string conf (Util.commd conf);
           Output.print_sstring conf {|m=WIZNOTES&f=|};
           Output.print_string conf (Mutil.encode wz);
           Output.print_sstring conf
             (Printf.sprintf "&d=%d-%02d-%02d,%02d:%02d:%02d"
                (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
                tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec);
           Output.print_sstring conf {|">|};
           Output.print_string conf (Util.escape_html wname);
           Output.print_sstring conf {|</a>|})
         else Output.print_string conf (Util.escape_html wname);
         (spl, Some tm))
       (sep_period_list, None) list;
  Output.print_sstring conf "</dd></dl>"

let print_old_wizards conf list =
  if list <> [] then (
    Output.print_sstring conf {|<dl><dd style="list-style-type:circle">|};
    Output.print_sstring conf (Util.transl_nth conf "and" 0);
    Output.print_sstring conf "...";
    Output.print_sstring conf "<dl><dd>";
    Ext_list.iter_first
      (fun first wz ->
        if not first then Output.print_sstring conf ", ";
        Output.print_sstring conf {|<a href="|};
        Output.print_string conf (Util.commd conf);
        Output.print_sstring conf {|m=WIZNOTES&f=|};
        Output.print_string conf (Mutil.encode wz);
        Output.print_sstring conf {|">|};
        for i = 0 to String.length wz - 1 do
          if wz.[i] = ' ' then Output.print_sstring conf "&nbsp;"
          else Output.print_sstring conf (String.make 1 wz.[i])
        done;
        Output.print_sstring conf "</a>")
      list;
    Output.print_sstring conf " </dd></dl></dd></dl>")

let wizard_list_from_dir conf base =
  match try Some (Sys.readdir (dir conf base)) with Sys_error _ -> None with
  | Some arr ->
      List.fold_left
        (fun list fname ->
          if Filename.check_suffix fname ".txt" then
            let n = Filename.chop_extension fname in
            n :: list
          else list)
        [] (Array.to_list arr)
  | None -> []

let print_search_form conf from_wiz =
  Output.print_sstring conf {|<table><tr><td align="|};
  Output.print_sstring conf conf.Config.right;
  Output.print_sstring conf {|"><form method="GET" action="|};
  Output.print_sstring conf conf.Config.command;
  Output.print_sstring conf {|"><p>|};
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "WIZNOTES_SEARCH");
  Output.print_sstring conf {|<input name="s" size="30" maxlength="40" value="|};
  (match Util.p_getenv conf.Config.env "s" with
  | Some s -> Output.print_string conf (Util.escape_html s)
  | None -> ());
  Output.print_sstring conf {|">|};
  if from_wiz <> "" then Util.hidden_input conf "z" (Mutil.encode from_wiz);
  Output.print_sstring conf
    {|<br><label><input type="checkbox" name="c" value="on"|};
  (match Util.p_getenv conf.Config.env "c" with
  | Some "on" -> Output.print_sstring conf " checked=\"checked\""
  | Some _ | None -> ());
  Output.print_sstring conf {|>|};
  Output.print_sstring conf (Util.transl_nth conf "search/case sensitive" 1);
  Output.print_sstring conf {| |};
  Output.print_sstring conf {|</label><input type="submit" value="|};
  Util.transl_nth conf "search/case sensitive" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf {|"></p></form></td></tr></table>|}

let print_main conf base auth_file =
  let wiztxt =
    Util.translate_eval
      (Util.transl_nth conf "wizard/wizards/friend/friends/exterior" 1)
  in
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst wiztxt);
    Output.print_sstring conf " - ";
    Output.print_sstring conf
      (Util.translate_eval (Util.transl_nth conf "note/notes" 1))
  in
  let by_alphab_order = Util.p_getenv conf.Config.env "o" <> Some "H" in
  let wizdata =
    let list = read_auth_file auth_file in
    if by_alphab_order then
      List.sort
        (fun (_, (_, (o1, _))) (_, (_, (o2, _))) -> Utf8.alphabetic_order o1 o2)
        list
    else list
  in
  let wddir = dir conf base in
  Hutil.header_no_page_title conf title;
  (* mouais... *)
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<h1>";
  title false;
  Output.print_sstring conf "</h1>";
  let list =
    List.map
      (fun (wz, wname) ->
        let wfile, wnote = wiznote_date (wzfile wddir wz) in
        (wz, wname, wfile, wnote))
      wizdata
  in
  let old_list =
    let list = wizard_list_from_dir conf base in
    List.filter (fun n -> not (List.mem_assoc n wizdata)) list
  in
  if by_alphab_order then (
    Output.print_sstring conf "<p>";
    Output.print_sstring conf (string_of_int @@ List.length wizdata);
    Output.print_sstring conf " ";
    Output.print_string conf (Util.safe_html wiztxt);
    Output.print_sstring conf "<br>";
    Output.print_sstring conf {|<em style="font-size:80%">|};
    Output.print_sstring conf (Utf8.capitalize_fst (Util.transl conf "click"));
    Output.print_sstring conf " ";
    Output.print_sstring conf {|<a href="|};
    Output.print_string conf (Util.commd conf);
    Output.print_sstring conf {|m=WIZNOTES&o=H">|};
    Output.print_sstring conf (Util.transl conf "here");
    Output.print_sstring conf "</a> ";
    Output.print_sstring conf
      (Util.transl conf
         "for the list ordered by the date of the last modification");
    Output.print_sstring conf ".</em></p>";
    print_wizards_by_alphabetic_order conf list)
  else (
    Output.print_sstring conf "<p>";
    Output.print_sstring conf (string_of_int @@ List.length wizdata);
    Output.print_sstring conf " ";
    Output.print_string conf (Util.safe_html wiztxt);
    Output.print_sstring conf "</p>";
    print_wizards_by_date conf list);
  if by_alphab_order then (
    print_old_wizards conf old_list;
    print_search_form conf "");
  Hutil.trailer conf

let wizard_page_title conf wizname _ = Output.print_string conf wizname

let print_whole_wiznote conf base auth_file wz wfile (s, date) ho =
  let wizname =
    let wizdata = read_auth_file auth_file in
    match List.assoc_opt wz wizdata with Some v -> fst v | None -> wz
  in
  let edit_opt =
    Some
      ( (conf.Config.wizard && conf.Config.user = wz) || conf.Config.manitou,
        "WIZNOTES",
        wz )
  in
  let title, s =
    try
      let i = Str.search_forward (Str.regexp "TITLE=") s 0 in
      Option.fold (String.index_opt s '\n')
        ~some:(fun j ->
          ( String.sub s (i + 6) (j - i - 6),
            String.sub s 0 i ^ String.sub s j (String.length s - j - 1) ))
        ~none:("", s)
    with Not_found -> ("", s)
  in
  let title =
    if title = "" then wizard_page_title conf @@ Util.escape_html wizname
    else wizard_page_title conf @@ Util.escape_html title
  in
  Hutil.header_no_page_title conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<h1>";
  title false;
  Output.print_sstring conf "</h1>";
  Util.include_template conf [] "summary" (fun () -> ());
  Output.print_sstring conf {|<table border="0" width="100%"><tr><td>|};
  let s = Util.string_with_macros ~conf ~env:[] s in
  let s =
    let wi =
      {
        Wiki.wi_mode = "NOTES";
        Wiki.wi_file_path = Notes.file_path conf base;
        Wiki.wi_person_exists = Util.person_exists conf base;
        Wiki.wi_always_show_link = conf.Config.wizard || conf.Config.friend;
      }
    in
    Wiki.html_with_summary_of_tlsw conf base wi edit_opt s
  in
  let s =
    match ho with
    | Some (case_sens, h) -> Util.html_highlight case_sens h s
    | None -> s
  in
  Output.print_string conf (Util.safe_html s);
  Output.print_sstring conf "</td></tr></table>";
  if Sys.file_exists wfile then (
    let tm = Unix.localtime date in
    let dmy =
      Date.
        {
          day = tm.Unix.tm_mday;
          month = tm.Unix.tm_mon + 1;
          year = 1900 + tm.Unix.tm_year;
          prec = Sure;
          delta = 0;
        }
    in
    Output.print_sstring conf "<p><tt>(";
    Output.print_string conf
      (DateDisplay.string_of_ondate conf (Dgreg (dmy, Dgregorian)));
    Output.print_sstring conf " ";
    Output.print_sstring conf (Printf.sprintf "%02d" tm.Unix.tm_hour);
    Output.print_sstring conf ":";
    Output.print_sstring conf (Printf.sprintf "%02d" tm.Unix.tm_min);
    Output.print_sstring conf ")</tt></p>");
  (match Util.p_getenv conf.Config.env "m" with
  | Some "WIZNOTES_SEARCH" -> print_search_form conf wz
  | Some _ | None -> ());
  Hutil.trailer conf

let print_part_wiznote conf base wz s cnt0 =
  let title = Util.escape_html wz in
  Hutil.header_no_page_title conf (fun _ -> Output.print_string conf title);
  let s = Util.safe_html @@ Util.string_with_macros ~conf ~env:[] s in
  let lines = Wiki.extract_sub_part (s : Adef.safe_string :> string) cnt0 in
  let lines =
    if cnt0 = 0 then (title :> string) :: "<br><br>" :: lines else lines
  in
  let file_path = Notes.file_path conf base in
  let can_edit =
    (conf.Config.wizard && conf.Config.user = wz) || conf.Config.manitou
  in
  let wi =
    {
      Wiki.wi_mode = "NOTES";
      Wiki.wi_file_path = file_path;
      Wiki.wi_person_exists = Util.person_exists conf base;
      Wiki.wi_always_show_link = conf.Config.wizard || conf.Config.friend;
    }
  in
  Wiki.print_sub_part conf base wi can_edit "WIZNOTES" wz cnt0 lines;
  Hutil.trailer conf

let wizard_auth_file_name conf =
  match
    ( List.assoc_opt "wizard_descr_file" conf.Config.base_env,
      List.assoc_opt "wizard_passwd_file" conf.Config.base_env )
  with
  | (Some "" | None), (Some "" | None) -> ""
  | Some auth_file, _ | _, Some auth_file -> auth_file

let print conf base =
  let auth_file = wizard_auth_file_name conf in
  if auth_file = "" then Hutil.incorrect_request conf
  else
    let f =
      (* backward compatibility *)
      match Util.p_getenv conf.Config.env "f" with
      | None -> Util.p_getenv conf.Config.env "v"
      | x -> x
    in
    match f with
    | Some wz -> (
        let wz = Filename.basename wz in
        let wfile = wzfile (dir conf base) wz in
        let s, date = read_wizard_notes wfile in
        match Util.p_getint conf.Config.env "v" with
        | Some cnt0 -> print_part_wiznote conf base wz s cnt0
        | None ->
            print_whole_wiznote conf base auth_file wz wfile (s, date) None)
    | None -> print_main conf base auth_file

let print_aux conf fn =
  let auth_file = wizard_auth_file_name conf in
  if auth_file = "" then Hutil.incorrect_request conf
  else
    match Util.p_getenv conf.Config.env "f" with
    | Some wz -> fn wz
    | None -> Hutil.incorrect_request conf

let print_mod conf base =
  print_aux conf (fun wz ->
      let wz = Filename.basename wz in
      let can_edit =
        (conf.Config.wizard && conf.Config.user = wz) || conf.Config.manitou
      in
      if can_edit then
        let title = wizard_page_title conf (Util.escape_html wz) in
        let wfile = wzfile (dir conf base) wz in
        let s, _ = read_wizard_notes ~limit:false wfile in
        Wiki.print_mod_view_page conf true (Adef.encoded "WIZNOTES") wz title []
          s
      else Hutil.incorrect_request conf)

let print_view conf base =
  print_aux conf (fun wz ->
      let wz = Filename.basename wz in
      let title = wizard_page_title conf (Util.escape_html wz) in
      let wfile = wzfile (dir conf base) wz in
      let s, _ = read_wizard_notes wfile in
      Wiki.print_mod_view_page conf false (Adef.encoded "WIZNOTES") wz title []
        s)

let commit_wiznotes conf base wz s =
  let wddir = dir conf base in
  let fname = wzfile wddir wz in
  (try Unix.mkdir wddir 0o755 with Unix.Unix_error (_, _, _) -> ());
  write_wizard_notes fname s;
  let pg = Def.NLDB.PgWizard wz in
  Notes.update_notes_links_db base pg s

let print_mod_ok conf base =
  let auth_file = wizard_auth_file_name conf in
  if auth_file = "" then Hutil.incorrect_request conf
  else
    let fname = function Some f -> f | None -> "nobody" in
    let edit_mode wz =
      if (conf.Config.wizard && conf.Config.user = wz) || conf.Config.manitou
      then Some "WIZNOTES"
      else None
    in
    let mode = "NOTES" in
    let read_string wz =
      ([], fst (read_wizard_notes ~limit:false (wzfile (dir conf base) wz)))
    in
    let commit = commit_wiznotes conf base in
    let string_filter s = Util.string_with_macros ~conf ~env:[] s in
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
      string_filter false

let wizard_denying wddir =
  let fname = Filename.concat wddir "connected.deny" in
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
  | Some ic ->
      let rec loop list =
        match try Some (input_line ic) with End_of_file -> None with
        | Some wname -> loop (wname :: list)
        | None ->
            close_in ic;
            List.rev list
      in
      loop []
  | None -> []

let print_connected_wizard conf first wddir wz tm_user =
  let wfile, stm = wiznote_date (wzfile wddir wz) in
  let tm = Unix.localtime stm in
  if wfile <> "" then (
    Output.print_sstring conf "<a href=\"";
    Output.print_string conf (Util.commd conf);
    Output.print_sstring conf "m=WIZNOTES&f=";
    Output.print_string conf (Mutil.encode wz);
    Output.print_sstring conf
      (Printf.sprintf "&d=%d-%02d-%02d,%02d:%02d:%02d" (tm.Unix.tm_year + 1900)
         (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
         tm.Unix.tm_sec);
    Output.print_string conf (Util.escape_html wz);
    Output.print_sstring conf "</a>")
  else Output.print_string conf (Util.escape_html wz);
  Output.print_sstring conf " <a href=\"";
  Output.print_string conf (Util.commd conf);
  Output.print_sstring conf "m=HIST&k=20&wiz=";
  Output.print_string conf (Mutil.encode wz);
  Output.print_sstring conf {|" style="text-decoration:none">(*)</a>|};
  let d = conf.Config.ctime -. tm_user in
  if d <> 0.0 then (
    Output.printf conf " - %.0f s" d;
    if first then (
      Output.print_sstring conf {| <span style="font-size:80%">(|};
      Output.print_sstring conf (Util.transl conf "since the last click");
      Output.print_sstring conf ")</span>"))

let do_connected_wizards conf base (_, _, _, wl) =
  let title _ =
    Util.transl_nth conf "wizard/wizards/friend/friends/exterior" 1
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  let wddir = dir conf base in
  let denying = wizard_denying wddir in
  let wl =
    if not (List.mem_assoc conf.Config.user wl) then
      (conf.Config.user, conf.Config.ctime) :: wl
    else wl
  in
  let wl = List.sort (fun (_, tm1) (_, tm2) -> compare tm1 tm2) wl in
  let is_visible = not (List.mem conf.Config.user denying) in
  Output.print_sstring conf "<ul>";
  let not_everybody, _ =
    List.fold_left
      (fun (not_everybody, first) (wz, tm_user) ->
        if
          wz <> conf.Config.user && List.mem wz denying
          && not conf.Config.manitou
        then (true, first)
        else (
          Output.print_sstring conf {|<li style="list-style-type:|};
          if
            (wz = conf.Config.user && not is_visible)
            || (conf.Config.manitou && List.mem wz denying)
          then Output.print_sstring conf "circle"
          else Output.print_sstring conf "disc";
          Output.print_sstring conf {|">|};
          print_connected_wizard conf first wddir wz tm_user;
          if wz = conf.Config.user then (
            Output.print_sstring conf (Util.transl conf ":");
            Output.print_sstring conf " :";
            Output.print_sstring conf
              (Util.transl_nth conf "you are visible/you are not visible"
                 (if is_visible then 0 else 1));
            Output.print_sstring conf " ; ";
            Output.print_sstring conf (Util.transl conf "click");
            Output.print_sstring conf " <a href=\"";
            Output.print_string conf (Util.commd conf);
            Output.print_sstring conf "m=CHANGE_WIZ_VIS&v=";
            Output.print_sstring conf
              (string_of_int @@ if is_visible then 0 else 1);
            Output.print_sstring conf "\">";
            Output.print_sstring conf (Util.transl conf "here");
            Output.print_sstring conf "</a> ";
            Output.print_sstring conf (Util.transl conf "to change");
            Output.print_sstring conf ".");
          Output.print_sstring conf "</li>";
          (not_everybody, false)))
      (false, true) wl
  in
  if not_everybody then Output.print_sstring conf "<li>...</li>";
  Output.print_sstring conf "</ul>";
  Hutil.trailer conf

let connected_wizards conf base =
  match conf.Config.n_connect with
  | Some x -> do_connected_wizards conf base x
  | None -> Hutil.incorrect_request conf

let do_change_wizard_visibility conf base x set_vis =
  let wddir = dir conf base in
  if not @@ Sys.file_exists wddir then Unix.mkdir wddir 0o755;
  let denying = wizard_denying wddir in
  let is_visible = not (List.mem conf.Config.user denying) in
  (if ((not set_vis) && not is_visible) || (set_vis && is_visible) then ()
  else
    let tmp_file = Filename.concat wddir "1connected.deny" in
    let oc = Secure.open_out tmp_file in
    let found =
      List.fold_left
        (fun found wz ->
          if wz = conf.Config.user && set_vis then true
          else (
            Printf.fprintf oc "%s\n" wz;
            found))
        false denying
    in
    if (not found) && not set_vis then Printf.fprintf oc "%s\n" conf.Config.user;
    close_out oc;
    let file = Filename.concat wddir "connected.deny" in
    Files.rm file;
    Sys.rename tmp_file file);
  do_connected_wizards conf base x

let change_wizard_visibility conf base =
  match (conf.Config.n_connect, Util.p_getint conf.Config.env "v") with
  | Some x, Some vis -> do_change_wizard_visibility conf base x (vis <> 0)
  | _ -> Hutil.incorrect_request conf

(* searching *)

let search_text conf base s =
  let s = if s = "" then " " else s in
  let case_sens = Util.p_getenv conf.Config.env "c" = Some "on" in
  let list =
    let list = wizard_list_from_dir conf base in
    let list = List.sort compare list in
    match Util.p_getenv conf.Config.env "z" with
    | Some "" | None -> list
    | Some wz ->
        let rec loop = function
          | wz1 :: list -> if wz = wz1 then list else loop list
          | [] -> []
        in
        loop list
  in
  let wizo =
    let rec loop = function
      | [] -> None
      | wz :: list ->
          let wz = Filename.basename wz in
          let wfile = wzfile (dir conf base) wz in
          let nt, dt = read_wizard_notes wfile in
          if Util.in_text case_sens s nt then Some (wz, wfile, nt, dt)
          else loop list
    in
    loop list
  in
  match wizo with
  | Some (wz, wf, nt, dt) ->
      let auth_file = wizard_auth_file_name conf in
      print_whole_wiznote conf base auth_file wz wf (nt, dt)
        (Some (case_sens, s))
  | None -> print conf base

let print_search conf base =
  match List.assoc_opt "s" conf.Config.env with
  | Some s -> search_text conf base (Mutil.gen_decode false s)
  | None -> print conf base
