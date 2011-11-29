(* camlp5r ./pa_html.cmo *)
(* $Id: wiznotes.ml,v 5.54 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Hutil;
open Util;

value dir conf base =
  Filename.concat (Util.base_path [] (conf.bname ^ ".gwb"))
    (Gwdb.base_wiznotes_dir base)
;

value wzfile wddir wz = Filename.concat wddir (wz ^ ".txt");

value read_auth_file fname =
  let data = read_gen_auth_file fname in
  List.map
    (fun au ->
       let wizname =
         try
           let k = String.index au.au_info ':' in
           String.sub au.au_info 0 k
         with
         [ Not_found -> au.au_user ]
       in
       let (wizname, wizorder, islash) =
         try
           let i = String.index wizname '/' in
           let w1 = String.sub wizname 0 i in
           let l = String.length wizname in
           let w2 = String.sub wizname (i + 1) (l - i - 1) in
           (w1 ^ w2, w2 ^ w1, i)
         with
         [ Not_found -> (wizname, wizname, 0) ]
       in
       (au.au_user, (wizname, (wizorder, islash))))
    data
;

value read_wizard_notes fname =
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let (date, len) =
        try
          let line = input_line ic in
          if line = "WIZNOTES" then
            let line = input_line ic in
            (float_of_string line, 0)
          else
            let s = Unix.stat fname in
            (s.Unix.st_mtime, Buff.store (Buff.mstore 0 line) '\n')
        with
        [ End_of_file | Failure _ -> (0., 0) ]
      in
      let rec loop len =
        match try Some (input_char ic) with [ End_of_file -> None ] with
        [ Some c -> loop (Buff.store len c)
        | None -> do { close_in ic; len } ]
      in
      let len = loop len in
      (Buff.get len, date)
  | None -> ("", 0.) ]
;

value write_wizard_notes fname nn =
  if nn = "" then try Sys.remove fname with [ Sys_error _ -> () ]
  else
    match try Some (Secure.open_out fname) with [ Sys_error _ -> None ] with
    [ Some oc ->
        do {
          Printf.fprintf oc "WIZNOTES\n%.0f\n" (Unix.time ());
          output_string oc nn;
          output_string oc "\n";
          close_out oc
        }
    | None -> () ]
;

value wiznote_date wfile =
  match try Some (Secure.open_in wfile) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let date =
        try
          let line = input_line ic in
          if line = "WIZNOTES" then float_of_string (input_line ic)
          else raise Exit
        with
        [ End_of_file | Failure _ | Exit ->
            let s = Unix.stat wfile in
            s.Unix.st_mtime ]
      in
      do { close_in ic; (wfile, date) }
  | None -> ("", 0.) ]
;

value print_wizards_by_alphabetic_order conf list =
  let wprint_elem (wz, (wname, (_, islash)), wfile, stm) = do {
    let tm = Unix.localtime stm in
    let with_link =
      conf.wizard && conf.user = wz || wfile <> "" ||
      conf.manitou
    in
    if with_link then
      Wserver.wprint
        "<a href=\"%sm=WIZNOTES;f=%s%t\">"
        (commd conf) (Util.code_varenv wz)
        (fun _ ->
           Printf.sprintf
            ";d=%d-%02d-%02d,%02d:%02d:%02d"
            (tm.Unix.tm_year + 1900)
            (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
            tm.Unix.tm_hour tm.Unix.tm_min
            tm.Unix.tm_sec)
    else ();
    if islash > 0 then
      let s1 =
        let islash =
          if wname.[islash-1] = ' ' then
            islash - 1
          else islash
        in
        String.sub wname 0 islash
      in
      let s2 =
        String.sub wname islash
          (String.length wname - islash)
      in
      Wserver.wprint "%s (%s)" s2 s1
    else
      Wserver.wprint "%s" wname;
    if with_link then Wserver.wprint "</a>"
    else ()
  }
  in
  let order (_, (_, (ord, _)), _, _) = ord in
  wprint_in_columns conf order wprint_elem list
;

value print_wizards_by_date conf list = do {
  let sep_period_list =
    [(fun tm -> tm.Unix.tm_mon,
      fun tm ->
        let dmy =
          {year = tm.Unix.tm_year + 1900; month = tm.Unix.tm_mon + 1;
           day = 0; prec = Sure; delta = 0}
        in
        Wserver.wprint "%s"
          (capitale (Date.string_of_ondate conf (Dgreg dmy Dgregorian))));
     (fun tm -> tm.Unix.tm_year,
      fun tm -> Wserver.wprint "%d" (tm.Unix.tm_year + 1900))]
  in
  let list =
    List.sort (fun (_, _, _, mtm1) (_, _, _, mtm2) -> compare mtm2 mtm1)
      list
  in
  Wserver.wprint "<dl>\n<dt>";
  let _ =
    List.fold_left
      (fun (spl, prev) (wz, (wname, _), wfile, stm) -> do {
         let tm = Unix.localtime stm in
         let (new_item, spl) =
           match prev with
           [ Some prev_tm ->
               let (sep_period, _) =
                 match spl with
                 [ [sp :: _] -> sp
                 | [] -> assert False ]
               in
               if sep_period tm <> sep_period prev_tm then do {
                 Wserver.wprint "</dd>\n<dt>";
                 let spl =
                   match spl with
                   [ [_; (next_sp, _) :: _] ->
                       if next_sp tm <> next_sp prev_tm then List.tl spl
                       else spl
                   | _ -> spl ]
                 in
                 (True, spl)
               }
               else (False, spl)
           | None -> (True, spl) ]
         in
         if new_item then
           if stm = 0.0 then Wserver.wprint "....."
           else
             match spl with
             [ [(_, disp_sep_period) :: _] -> disp_sep_period tm
             | [] -> () ]
         else ();
         if new_item then Wserver.wprint "</dt>\n<dd>\n" else ();
         let wname = if wname = "" then wz else wname in
         Wserver.wprint "%s%t"
           (if prev = None || new_item then "" else ",\n")
           (fun _ ->
              if conf.wizard && conf.user = wz || wfile <> "" then
                Printf.sprintf "<a href=\"%sm=WIZNOTES;f=%s%t\">%s</a>"
                  (commd conf) (Util.code_varenv wz)
                  (fun _ ->
                     Printf.sprintf ";d=%d-%02d-%02d,%02d:%02d:%02d"
                       (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
                       tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
                       tm.Unix.tm_sec)
                  wname
              else wname);
         (spl, Some tm)
       })
      (sep_period_list, None) list
  in
  ();
  Wserver.wprint "</dd></dl>\n"
};

value print_old_wizards conf list =
  if list = [] then ()
  else do {
    tag "dl" begin
      tag "dd" "style=\"list-style-type:circle\"" begin
        Wserver.wprint "%s..." (transl_nth conf "and" 0);
        tag "dl" begin
          tag "dd" begin
            Mutil.list_iter_first
              (fun first wz -> do {
                 if not first then Wserver.wprint ",\n" else ();
                 stag "a" "href=\"%sm=WIZNOTES;f=%s\"" (commd conf)
                   (Util.code_varenv wz)
                 begin
                   for i = 0 to String.length wz - 1 do {
                     if wz.[i] = ' ' then Wserver.wprint "&nbsp;"
                     else Wserver.wprint "%c" wz.[i];
                   };
                 end
               })
              list;
            Wserver.wprint "\n";
          end;
        end;
      end;
    end;
  }
;

value wizard_list_from_dir conf base =
  match
    try Some (Sys.readdir (dir conf base)) with [ Sys_error _ -> None ]
  with
  [ Some arr ->
      List.fold_left
        (fun list fname ->
           if Filename.check_suffix fname ".txt" then
             let n = Filename.chop_extension fname in
             [n :: list]
           else list)
        [] (Array.to_list arr)
  | None -> [] ]
;

value print_search_form conf from_wiz =
  tag "table" begin
    tag "tr" begin
      tag "td" "align=\"%s\"" conf.right begin
        tag "form" "method=\"get\" action=\"%s\"" conf.command begin
          tag "p" begin
            hidden_env conf;
            xtag "input"
              "type=\"hidden\" name=\"m\" value=\"WIZNOTES_SEARCH\"";
            xtag "input"
              "name=\"s\" size=\"30\" maxlength=\"40\" value=\"%s\""
              (match p_getenv conf.env "s" with
               [ Some s -> quote_escaped s
               | None -> "" ]);
            if from_wiz <> "" then
              xtag "input" "type=\"hidden\" name=\"z\" value=\"%s\"" from_wiz
            else ();
            xtag "br";
            tag "label" begin
              xtag "input" "type=\"checkbox\" name=\"c\" value=\"on\"%s"
                (match p_getenv conf.env "c" with
                 [ Some "on" -> " checked=\"checked\""
                 | Some _ | None -> "" ]);
              Wserver.wprint "%s\n"
                (transl_nth conf "search/case sensitive" 1);
            end;
            xtag "input" "type=\"submit\" value=\"%s\""
              (capitale (transl_nth conf "search/case sensitive" 0));
          end;
        end;
      end;
    end;
  end
;

value print_main conf base auth_file =
  let wiztxt =
    Util.translate_eval
      (transl_nth conf "wizard/wizards/friend/friends/exterior" 1)
  in
  let title _ =
    Wserver.wprint "%s - %s" (capitale wiztxt)
      (Util.translate_eval (transl_nth conf "note/notes" 1))
  in
  let by_alphab_order = p_getenv conf.env "o" <> Some "H" in
  let wizdata =
    let list = read_auth_file auth_file in
    if by_alphab_order then
      List.sort
        (fun (_, (_, (o1, _))) (_, (_, (o2, _))) ->
           Gutil.alphabetic_order o1 o2)
        list
    else list
  in
  let wddir = dir conf base in
  do {
    Hutil.header_no_page_title conf title; (* mouais... *)
    print_link_to_welcome conf True;
    Wserver.wprint "<h1>";
    title False;
    Wserver.wprint "</h1>\n";
    let list =
      List.map
        (fun (wz, wname) ->
           let (wfile, wnote) = wiznote_date (wzfile wddir wz) in
           (wz, wname, wfile, wnote))
        wizdata
    in
    let old_list =
      let list = wizard_list_from_dir conf base in
      List.filter (fun n -> not (List.mem_assoc n wizdata)) list
    in
    if by_alphab_order then do {
      tag "p" begin
        Wserver.wprint "%d %s<br%s>\n" (List.length wizdata) wiztxt conf.xhs;
        Wserver.wprint "<em style=\"font-size:80%%\">\n";
        Wserver.wprint "%s " (capitale (transl conf "click"));
        Wserver.wprint "<a href=\"%sm=WIZNOTES;o=H\">%s</a>\n" (commd conf)
          (transl conf "here");
        Wserver.wprint "%s"
          (transl conf
             "for the list ordered by the date of the last modification");
        Wserver.wprint ".</em>\n";
      end;
      print_wizards_by_alphabetic_order conf list;
    }
    else do {
      tag "p" begin
        Wserver.wprint "%d %s\n" (List.length wizdata) wiztxt;
      end;
      print_wizards_by_date conf list;
    };
    if by_alphab_order then do {
      print_old_wizards conf old_list;
      print_search_form conf "";
    }
    else ();
    trailer conf
  }
;

value wizard_page_title conf wz wizname h =
  Wserver.wprint "%s" wizname
;

value print_whole_wiznote conf base auth_file wz wfile (s, date) ho = do {
  let wizname =
    let wizdata = read_auth_file auth_file in
    try fst (List.assoc wz wizdata) with
    [ Not_found -> wz ]
  in
  let edit_opt =
    let can_edit = conf.wizard && conf.user = wz || conf.manitou in
    Some (can_edit, "WIZNOTES", code_varenv wz)
  in
  let title = wizard_page_title conf wz wizname in
  header_no_page_title conf title;
  print_link_to_welcome conf True;
  Wserver.wprint "<h1>";
  title False;
  Wserver.wprint "</h1>\n";
  match Util.open_etc_file "summary" with
  [ Some ic -> Templ.copy_from_templ conf [] ic
  | None -> () ];
  tag "table" "border=\"0\" width=\"100%%\"" begin
    tag "tr" begin
      tag "td" begin
        let s = string_with_macros conf [] s in
        let s =
          let wi =
            {Wiki.wi_mode = "NOTES";
             Wiki.wi_cancel_links = conf.cancel_links;
             Wiki.wi_file_path = Notes.file_path conf base;
             Wiki.wi_person_exists = person_exists conf base;
             Wiki.wi_always_show_link = conf.wizard || conf.friend}
          in
          Wiki.html_with_summary_of_tlsw conf wi edit_opt s
        in
        let s =
          match ho with
          [ Some (case_sens, h) -> html_highlight case_sens h s
          | None -> s ]
        in
        Wserver.wprint "%s\n"
          (if conf.pure_xhtml then Util.check_xhtml s else s);
      end;
    end;
  end;
  if Sys.file_exists wfile then do {
    let tm = Unix.localtime date in
    let dmy =
      {day = tm.Unix.tm_mday; month = tm.Unix.tm_mon + 1;
       year = 1900 + tm.Unix.tm_year; prec = Sure; delta = 0}
    in
    tag "p" begin
      Wserver.wprint "<tt>(%s %02d:%02d)</tt>\n"
        (Date.string_of_ondate conf (Dgreg dmy Dgregorian))
        tm.Unix.tm_hour tm.Unix.tm_min;
    end
  }
  else ();
  match p_getenv conf.env "m" with
  [ Some "WIZNOTES_SEARCH" -> print_search_form conf wz
  | Some _ | None -> () ];
  trailer conf
};

value print_part_wiznote conf base wz s cnt0 =
  let title = wz in
  do {
    Hutil.header_no_page_title conf (fun _ -> Wserver.wprint "%s" title);
    let s = string_with_macros conf [] s in
    let lines = Wiki.extract_sub_part s cnt0 in
    let lines = if cnt0 = 0 then [title; "<br /><br />" :: lines] else lines in
    let file_path = Notes.file_path conf base in
    let can_edit = conf.wizard && conf.user = wz || conf.manitou in
    let wi =
      {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
       Wiki.wi_file_path = file_path;
       Wiki.wi_person_exists = person_exists conf base;
       Wiki.wi_always_show_link = conf.wizard || conf.friend}
    in
    Wiki.print_sub_part conf wi can_edit "WIZNOTES" (code_varenv wz) cnt0
      lines;
    Hutil.trailer conf;
  }
;

value wizard_auth_file_name conf =
  match
    (p_getenv conf.base_env "wizard_descr_file",
     p_getenv conf.base_env "wizard_passwd_file")
  with
  [ (Some "" | None, Some "" | None) -> ""
  | (Some auth_file, _) -> auth_file
  | (_, Some auth_file) -> auth_file ]
;

value print conf base =
  let auth_file = wizard_auth_file_name conf in
  if auth_file = "" then incorrect_request conf
  else
    let f =
      (* backward compatibility *)
      match p_getenv conf.env "f" with
      [ None -> p_getenv conf.env "v"
      | x -> x ]
    in
    match f with
    [ Some wz ->
        let wz = Filename.basename wz in
        let wfile = wzfile (dir conf base) wz in
        let (s, date) = read_wizard_notes wfile in
        match p_getint conf.env "v" with
        [ Some cnt0 -> print_part_wiznote conf base wz s cnt0
        | None ->
            print_whole_wiznote conf base auth_file wz wfile (s, date) None ]
    | None -> print_main conf base auth_file ]
;

value print_mod conf base =
  let auth_file =
    match
      (p_getenv conf.base_env "wizard_descr_file",
       p_getenv conf.base_env "wizard_passwd_file")
    with
    [ (Some "" | None, Some "" | None) -> ""
    | (Some auth_file, _) -> auth_file
    | (_, Some auth_file) -> auth_file ]
  in
  if auth_file = "" then incorrect_request conf
  else
    match p_getenv conf.env "f" with
    [ Some wz ->
        let wz = Filename.basename wz in
        let can_edit = conf.wizard && conf.user = wz || conf.manitou in
        if can_edit then
          let title = wizard_page_title conf wz wz in
          let wfile = wzfile (dir conf base) wz in
          let (s, _) = read_wizard_notes wfile in
          Wiki.print_mod_view_page conf True "WIZNOTES" wz title [] s
        else incorrect_request conf
    | None -> incorrect_request conf ]
;

value print_view conf base =
  let auth_file =
    match
      (p_getenv conf.base_env "wizard_descr_file",
       p_getenv conf.base_env "wizard_passwd_file")
    with
    [ (Some "" | None, Some "" | None) -> ""
    | (Some auth_file, _) -> auth_file
    | (_, Some auth_file) -> auth_file ]
  in
  if auth_file = "" then incorrect_request conf
  else
    match p_getenv conf.env "f" with
    [ Some wz ->
        let wz = Filename.basename wz in
        let title = wizard_page_title conf wz wz in
        let wfile = wzfile (dir conf base) wz in
        let (s, _) = read_wizard_notes wfile in
        Wiki.print_mod_view_page conf False "WIZNOTES" wz title [] s
    | None -> incorrect_request conf ]
;

value commit_wiznotes conf base wz s =
  let wddir = dir conf base in
  let fname = wzfile wddir wz in
  do {
    try Unix.mkdir wddir 0o755 with [ Unix.Unix_error _ _ _ -> () ];
    write_wizard_notes fname s;
    let pg = NotesLinks.PgWizard wz in
    Notes.update_notes_links_db conf pg s;
  }
;

value print_mod_ok conf base =
  let auth_file =
    match
      (p_getenv conf.base_env "wizard_descr_file",
       p_getenv conf.base_env "wizard_passwd_file")
    with
    [ (Some "" | None, Some "" | None) -> ""
    | (Some auth_file, _) -> auth_file
    | (_, Some auth_file) -> auth_file ]
  in
  if auth_file = "" then incorrect_request conf
  else
    let fname =
      fun
      [ Some f -> f
      | None -> "nobody" ]
    in
    let edit_mode wz =
      if conf.wizard && conf.user = wz || conf.manitou then Some "WIZNOTES"
      else None
    in
    let mode = "NOTES" in
    let read_string wz =
      ([], fst (read_wizard_notes (wzfile (dir conf base) wz)))
    in
    let commit = commit_wiznotes conf base in
    let string_filter = string_with_macros conf [] in
    let file_path = Notes.file_path conf base in
    let wi =
      {Wiki.wi_mode = mode; Wiki.wi_cancel_links = conf.cancel_links;
       Wiki.wi_file_path = file_path;
       Wiki.wi_person_exists = person_exists conf base;
       Wiki.wi_always_show_link = conf.wizard || conf.friend}
    in
    Wiki.print_mod_ok conf wi edit_mode fname read_string commit string_filter
      False
;

value wizard_denying wddir =
  let fname = Filename.concat wddir "connected.deny" in
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop [] where rec loop list =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some wname -> loop [wname :: list]
        | None -> do { close_in ic; List.rev list } ]
  | None -> [] ]
;

value print_connected_wizard conf first wddir wz tm_user = do {
  let (wfile, stm) = wiznote_date (wzfile wddir wz) in
  let tm = Unix.localtime stm in
  if wfile <> "" then
    stag "a" "href=\"%sm=WIZNOTES;f=%s%t\""
      (commd conf) (Util.code_varenv wz)
      (fun _ ->
         Printf.sprintf ";d=%d-%02d-%02d,%02d:%02d:%02d"
           (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
           tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
           tm.Unix.tm_sec)
    begin
      Wserver.wprint "%s" wz;
    end
  else Wserver.wprint "%s" wz;
  Wserver.wprint " ";
  stag "a" "href=\"%sm=HIST;k=20;wiz=%s\" style=\"text-decoration:none\""
    (commd conf) (Util.code_varenv wz)
  begin
    Wserver.wprint "(*)";
  end;
  let d = conf.ctime -. tm_user in
  if d = 0.0 then ()
  else do {
    Wserver.wprint " - %.0f s" d;
    if first then do {
      Wserver.wprint " ";
      stag "span" "style=\"font-size:80%%\"" begin
        Wserver.wprint "(%s)" (transl conf "since the last click");
      end;
    }
    else ();
  };
};

value do_connected_wizards conf base (_, _, _, wl) = do {
  let title _ =
    Wserver.wprint "%s"
      (capitale (transl_nth conf "wizard/wizards/friend/friends/exterior" 1))
  in
  header conf title;
  print_link_to_welcome conf True;
  let wddir = dir conf base in
  let denying = wizard_denying wddir in
  let wl =
    if not (List.mem_assoc conf.user wl) then [(conf.user, conf.ctime) :: wl]
    else wl
  in
  let wl = List.sort (fun (_, tm1) (_, tm2) -> compare tm1 tm2) wl in
  let is_visible = not (List.mem conf.user denying) in
  tag "ul" begin
    let (not_everybody, _) =
      List.fold_left
        (fun (not_everybody, first) (wz, tm_user) ->
           if wz <> conf.user && List.mem wz denying && not conf.manitou
           then (True, first)
           else do {
             tag "li" "style=\"list-style-type:%s\""
               (if wz = conf.user && not is_visible ||
                   conf.manitou && List.mem wz denying
                then "circle"
                else "disc")
             begin
               print_connected_wizard conf first wddir wz tm_user;
               if wz = conf.user then do {
                 Wserver.wprint " :\n%s;"
                   (transl_nth conf "you are visible/you are not visible"
                      (if is_visible then 0 else 1));
                 Wserver.wprint " %s %s%s%s %s" (transl conf "click")
                   (Printf.sprintf "<a href=\"%sm=CHANGE_WIZ_VIS;v=%d\">"
                      (commd conf) (if is_visible then 0 else 1))
                   (transl conf "here") "</a>" (transl conf "to change");
                 Wserver.wprint ".";
               }
               else ();
               Wserver.wprint "\n";
             end;
             (not_everybody, False)
           })
        (False, True) wl
    in
    if not_everybody then tag "li" begin Wserver.wprint "..."; end else ();
  end;
  trailer conf;
};

value connected_wizards conf base =
  match conf.n_connect with
  [ Some x -> do_connected_wizards conf base x
  | None -> incorrect_request conf ]
;

value do_change_wizard_visibility conf base x set_vis = do {
  let wddir = dir conf base in
  let denying = wizard_denying wddir in
  let is_visible = not (List.mem conf.user denying) in
  if not set_vis && not is_visible || set_vis && is_visible then ()
  else do {
    let tmp_file = Filename.concat wddir "1connected.deny" in
    let oc = Secure.open_out tmp_file in
    let found =
      List.fold_left
        (fun found wz ->
           if wz = conf.user && set_vis then True
           else do {
             Printf.fprintf oc "%s\n" wz;
             found
           })
        False denying
    in
    if not found && not set_vis then Printf.fprintf oc "%s\n" conf.user
    else ();
    close_out oc;
    let file = Filename.concat wddir "connected.deny" in
    Mutil.remove_file file;
    Sys.rename tmp_file file;
  };
  do_connected_wizards conf base x
};

value change_wizard_visibility conf base =
  match (conf.n_connect, p_getint conf.env "v") with
  [ (Some x, Some vis) -> do_change_wizard_visibility conf base x (vis <> 0)
  | _ -> incorrect_request conf ]
;

(* searching *)

value search_text conf base s =
  let s = if s = "" then " " else s in
  let case_sens = p_getenv conf.env "c" = Some "on" in
  let list =
    let list = wizard_list_from_dir conf base in
    let list = List.sort compare list in
    match p_getenv conf.env "z" with
    [ Some "" | None -> list
    | Some wz ->
        loop list where rec loop =
          fun
          [ [wz1 :: list] -> if wz = wz1 then list else loop list
          | [] -> [] ] ]
  in
  let wizo =
    loop list where rec loop =
      fun
      [ [] -> None
      | [wz :: list] ->
          let wz = Filename.basename wz in
          let wfile = wzfile (dir conf base) wz in
          let (nt, dt) = read_wizard_notes wfile in
          if in_text case_sens s nt then Some (wz, wfile, nt, dt)
          else loop list ]
  in
  match wizo with
  [ Some (wz, wf, nt, dt) ->
      let auth_file = wizard_auth_file_name conf in
      print_whole_wiznote conf base auth_file wz wf (nt, dt)
         (Some (case_sens, s))
  | None -> print conf base ]
;

value print_search conf base =
  match try Some (List.assoc "s" conf.env) with [ Not_found -> None ] with
  [ Some s -> search_text conf base (Wserver.gen_decode False s)
  | None -> print conf base ]
;
