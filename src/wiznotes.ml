(* camlp4r ./pa_html.cmo *)
(* $Id: wiznotes.ml,v 4.38 2005-08-19 01:39:30 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Util;
open Def;

value dir conf =
  Filename.concat (Util.base_path [] (conf.bname ^ ".gwb")) "wiznotes"
;

value wzfile wddir wz = Filename.concat wddir (wz ^ ".txt");

value read_auth_file fname =
  let fname = Util.base_path [] fname in
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let rec loop data =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some line ->
            let data =
              try
                let i = String.index line ':' in
                let wizname =
                  try
                    let j = String.index_from line (i + 1) ':' in
                    let k = String.index_from line (j + 1) ':' in
                    String.sub line (j + 1) (k - j - 1)
                  with
                  [ Not_found -> "" ]
                in
                [(String.sub line 0 i, wizname) :: data]
              with
              [ Not_found -> data ]
            in
            loop data
        | None -> do { close_in ic; List.rev data } ]
      in
      loop []
  | None -> [] ]
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
        | None -> do { close_in ic; (Buff.get (max 0 (len - 1)), date) } ]
      in
      loop len
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

value print_main conf base auth_file =
  let wiztxt =
    Gutil.nominative (transl_nth conf "wizard/wizards/friend/friends" 1)
  in
  let title _ =
    Wserver.wprint "%s - %s" (capitale wiztxt)
      (Gutil.nominative (transl_nth conf "note/notes" 1))
  in
  let wizdata = read_auth_file auth_file in
  let wddir = dir conf in
  let by_alphab_order = p_getenv conf.env "o" <> Some "H" in
  do {
    header conf title;
    print_link_to_welcome conf True;
    let list =
      List.map
        (fun (wz, wname) ->
           let (wfile, wnote) = wiznote_date (wzfile wddir wz) in
           (wz, wname, wfile, wnote))
        wizdata
    in
    if by_alphab_order then
      tag "p" begin
        let _ =
          List.fold_left
            (fun prev (wz, wname, wfile, stm) ->
               let tm = Unix.localtime stm in
               let wname = if wname = "" then wz else wname in
               do {
                 Wserver.wprint "%s%t"
                   (if prev = None then "" else ",\n")
                   (fun _ ->
                      if conf.wizard && conf.user = wz || wfile <> "" then
                        Wserver.wprint "<a href=\"%sm=WIZNOTES;f=%s%t\">%s</a>"
                          (commd conf) (Util.code_varenv wz)
                          (fun _ ->
                             Wserver.wprint ";d=%d-%02d-%02d,%02d:%02d:%02d"
                               (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
                               tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
                               tm.Unix.tm_sec)
                          wname
                      else Wserver.wprint "%s" wname);
                 Some tm
               })
            None list
        in
        Wserver.wprint "\n";
      end
    else do {
      let sep_period_list =
        [(fun tm -> tm.Unix.tm_mon,
          fun tm ->
            Wserver.wprint "%s"
              (capitale (Date.code_dmy conf
                 {year = tm.Unix.tm_year + 1900; month = tm.Unix.tm_mon + 1;
                  day = 0; prec = Sure; delta = 0})));
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
          (fun (spl, prev) (wz, wname, wfile, stm) ->
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
             do {
               if new_item then
                 if stm = 0.0 then Wserver.wprint "....."
                 else
                   match spl with
                   [ [(_, disp_sep_period) :: _] -> disp_sep_period tm
                   | [] -> () ]
               else ();
               if new_item then Wserver.wprint "</dt>\n<dd>\n" else ();
                let () =
                 let wname = if wname = "" then wz else wname in
                 Wserver.wprint "%s%t"
                   (if prev = None || new_item then "" else ",\n")
                   (fun _ ->
                      if conf.wizard && conf.user = wz || wfile <> "" then
                        Wserver.wprint "<a href=\"%sm=WIZNOTES;f=%s%t\">%s</a>"
                          (commd conf) (Util.code_varenv wz)
                          (fun _ ->
                             Wserver.wprint ";d=%d-%02d-%02d,%02d:%02d:%02d"
                               (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
                               tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
                               tm.Unix.tm_sec)
                          wname
                      else Wserver.wprint "%s" wname)
               in
               (spl, Some tm)
             })
          (sep_period_list, None) list
      in
      ();
      Wserver.wprint "</dd></dl>\n"
    };
    tag "p" begin
      Wserver.wprint "%d %s\n" (List.length wizdata) wiztxt;
    end;
    if by_alphab_order then
      Wserver.wprint "<p>\n<a href=\"%sm=WIZNOTES;o=H\">%s</a>\n</p>\n"
        (commd conf) (transl conf "history of updates")
    else ();
    trailer conf
  }
;

value wizard_page_title wz wizname h =
  Wserver.wprint "%s%s" wizname
    (if wz <> wizname && not h then
       "<br><font size=\"-1\">(" ^ wz ^ ")</font>"
     else "")
;

value print_whole_wiznote conf auth_file edit_opt wz wfile (s, date) =
  let wizname =
    let wizdata = read_auth_file auth_file in
    try List.assoc wz wizdata with
    [ Not_found -> wz ]
  in
  let title = wizard_page_title wz wizname in
  do {
    header_no_page_title conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<h1 style=\"text-align:center\" class=\"highlight\">";
    title False;
    Wserver.wprint "</h1>\n";
    match Util.open_etc_file "summary" with
    [ Some ic -> Templ.copy_from_templ conf [] ic
    | None -> () ];
    html_p conf;
    tag "table" "border=\"0\" width=\"100%%\"" begin
      tag "tr" begin
        tag "td" begin
          let s = string_with_macros conf [] s in
          let s =
            Wiki.html_with_summary_of_tlsw conf "NOTES" (Notes.file_path conf)
              edit_opt s
          in
          Wserver.wprint "%s\n" s;
        end;
      end;
    end;
    if Sys.file_exists wfile then do {
      html_p conf;
      let tm = Unix.localtime date in
      let dmy =
        {day = tm.Unix.tm_mday; month = tm.Unix.tm_mon + 1;
         year = 1900 + tm.Unix.tm_year; prec = Sure; delta = 0}
      in
      Wserver.wprint "<tt>(%s %02d:%02d)</tt>\n" (Date.code_dmy conf dmy)
        tm.Unix.tm_hour tm.Unix.tm_min
    }
    else ();
    trailer conf
  }
;

value print_part_wiznote conf wz s cnt0 =
  let title = wz in
  do {
    Util.header_no_page_title conf (fun _ -> Wserver.wprint "%s" title);
    let s = string_with_macros conf [] s in
    let lines = Wiki.extract_sub_part s cnt0 in
    let lines = if cnt0 = 0 then [title; "<br /><br />" :: lines] else lines in
    let file_path = Notes.file_path conf in
    let can_edit = conf.wizard && conf.user = wz in
    Wiki.print_sub_part conf can_edit file_path "NOTES" "WIZNOTES"
      (code_varenv wz) cnt0 lines;
    Util.trailer conf;
  }
;

value print conf base =
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
    let f =
      (* backward compatibility *)
      match p_getenv conf.env "f" with
      [ None -> p_getenv conf.env "v"
      | x -> x ]
    in
    match f with
    [ Some wz ->
        let wz = Filename.basename wz in
        let wfile = wzfile (dir conf) wz in
        let (s, date) = read_wizard_notes wfile in
        let edit_opt =
          if conf.wizard && conf.user = wz then Some ("WIZNOTES", code_varenv wz)
          else None
        in
        match p_getint conf.env "v" with
        [ Some cnt0 -> print_part_wiznote conf wz s cnt0
        | None ->
            print_whole_wiznote conf auth_file edit_opt wz wfile (s, date) ]
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
        let can_edit = conf.wizard && conf.user = wz in
        if can_edit then
          let title = wizard_page_title wz wz in
          let wfile = wzfile (dir conf) wz in
          let (s, _) = read_wizard_notes wfile in
          Wiki.print_mod_page conf "WIZNOTES" wz title ("", []) s
        else incorrect_request conf
    | None -> incorrect_request conf ]
;

value commit_wiznotes conf wz s =
  let wddir = dir conf in
  let fname = wzfile wddir wz in
  do {
    try Unix.mkdir wddir 0o755 with [ Unix.Unix_error _ _ _ -> () ];
    write_wizard_notes fname s;
    let pg = NotesLinks.PgWizard wz in
    Notes.update_notes_links_db conf pg s True;
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
      if conf.wizard && conf.user = wz then Some "WIZNOTES" else None
    in
    let mode = "NOTES" in
    let read_string wz =
      ("", [], fst (read_wizard_notes (wzfile (dir conf) wz)))
    in
    let commit = commit_wiznotes in
    let string_filter = string_with_macros conf [] in
    let file_path = Notes.file_path conf in
    Wiki.print_mod_ok conf edit_mode mode fname read_string commit string_filter
      file_path False
;
