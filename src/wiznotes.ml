(* camlp4r ./pa_html.cmo *)
(* $Id: wiznotes.ml,v 4.16 2004-06-16 11:25:48 ddr Exp $ *)
(* Copyright (c) 2002 INRIA *)

open Config;
open Util;
open Def;

value dir conf =
  Filename.concat (Util.base_path [] (conf.bname ^ ".gwb")) "wiznotes"
;

value wzfile wddir wz = Filename.concat wddir (wz ^ ".txt");

value read_wizfile fname =
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

value print_main conf base wizfile =
  let wiztxt =
    Gutil.nominative (transl_nth conf "wizard/wizards/friend/friends" 1)
  in
  let title _ =
    Wserver.wprint "%s - %s" (capitale wiztxt)
      (Gutil.nominative (transl_nth conf "note/notes" 1))
  in
  let wizdata = read_wizfile wizfile in
  let wddir = dir conf in
  let by_alphab_order = p_getenv conf.env "o" <> Some "H" in
  do {
    header conf title;
    print_link_to_welcome conf False;
    html_p conf;
    let list =
      List.map
        (fun (wz, wname) ->
           let (wfile, wnote) = wiznote_date (wzfile wddir wz) in
           (wz, wname, wfile, wnote))
        wizdata
    in
    let list =
      if by_alphab_order then list
      else
        List.sort (fun (_, _, _, mtm1) (_, _, _, mtm2) -> compare mtm2 mtm1)
          list
    in
    let sep_period_list =
      [(fun tm -> tm.Unix.tm_mon,
        fun tm ->
          Wserver.wprint "%s"
            (Date.code_dmy conf
               {year = tm.Unix.tm_year + 1900; month = tm.Unix.tm_mon + 1;
                day = 0; prec = Sure; delta = 0}));
       (fun tm -> tm.Unix.tm_year,
        fun tm ->
          Wserver.wprint "%s"
            (Date.code_dmy conf
               {year = tm.Unix.tm_year + 1900; month = 0; day = 0;
                prec = Sure; delta = 0}))]
    in
    if by_alphab_order then () else Wserver.wprint "<dl>\n<dt>";
    let _ =
      List.fold_left
        (fun (spl, prev) (wz, wname, wfile, stm) ->
           let tm = Unix.localtime stm in
           let (new_item, spl) =
             if by_alphab_order then (False, spl)
             else
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
             if by_alphab_order then ()
             else do {
               if new_item then
                 if stm = 0.0 then Wserver.wprint "....."
                 else
                   match spl with
                   [ [(_, disp_sep_period) :: _] -> disp_sep_period tm
                   | [] -> () ]
               else ();
               if new_item then Wserver.wprint "</dt>\n<dd>\n" else ()
             };
             let () =
               let wname = if wname = "" then wz else wname in
               Wserver.wprint "%s%t"
                 (if prev = None || new_item then "" else ",\n")
                 (fun _ ->
                    if conf.wizard && conf.user = wz || wfile <> "" then
                      Wserver.wprint "<a href=\"%sm=WIZNOTES;v=%s%t\">%s</a>"
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
    if by_alphab_order then () else Wserver.wprint "</dd></dl>\n";
    html_p conf;
    Wserver.wprint "%d %s\n" (List.length wizdata) wiztxt;
    if by_alphab_order then
      Wserver.wprint "<p>\n<a href=\"%sm=WIZNOTES;o=H\">%s</a>" (commd conf)
        (transl conf "history of updates")
    else ();
    trailer conf
  }
;

value print_wizard conf base wizfile wz =
  let wizname =
    let wizdata = read_wizfile wizfile in
    try List.assoc wz wizdata with
    [ Not_found -> wz ]
  in
  let title h =
    Wserver.wprint "%s%s" wizname
      (if wz <> wizname && not h then
         "<br><font size=\"-1\">(" ^ wz ^ ")</font>"
       else "")
  in
  let wfile = wzfile (dir conf) wz in
  let (s, date) = read_wizard_notes wfile in
  do {
    header conf title;
    print_link_to_welcome conf False;
    html_p conf;
    tag "table" "border=0" begin
      tag "tr" begin
        tag "td" begin
          Wserver.wprint "%s\n" (string_with_macros conf False [] s);
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
    if conf.wizard && conf.user = wz then do {
      html_p conf;
      tag "form" "method=POST action=\"%s\"" conf.command begin
        Util.hidden_env conf;
        Wserver.wprint "<input type=hidden name=m value=WIZNOTES>\n";
        Wserver.wprint "<input type=hidden name=v value=\"%s\">\n" wz;
        let digest = Iovalue.digest s in
        Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n"
          digest;
        stag "textarea" "name=notes rows=30 cols=70 wrap=soft" begin
          if s <> "" then Wserver.wprint "%s" (quote_escaped s) else ();
        end;
        Wserver.wprint "\n";
        html_p conf;
        Wserver.wprint "<input type=submit value=Ok>\n";
      end
    }
    else ();
    trailer conf
  }
;

value print_wizard_mod conf base wizfile wz nn =
  let wddir = dir conf in
  let fname = wzfile wddir wz in
  let (on, _) = read_wizard_notes fname in
  let nn = Gutil.strip_all_trailing_spaces nn in
  let digest =
    match p_getenv conf.env "digest" with
    [ Some s -> s
    | None -> "" ]
  in
  if digest = Iovalue.digest on then do {
    if nn <> on then do {
      try Unix.mkdir wddir 0o755 with [ Unix.Unix_error _ _ _ -> () ];
      write_wizard_notes fname nn
    }
    else ();
    print_main conf base wizfile
  }
  else try Update.error_digest conf base with [ Update.ModErr -> () ]
;

value print conf base =
  let wizfile =
    match
      (p_getenv conf.base_env "wizard_descr_file",
       p_getenv conf.base_env "wizard_passwd_file")
    with
    [ (Some "" | None, Some "" | None) -> ""
    | (Some wizfile, _) -> wizfile
    | (_, Some wizfile) -> wizfile ]
  in
  if wizfile = "" then incorrect_request conf
  else
    match p_getenv conf.env "v" with
    [ Some wz ->
        let wz = Filename.basename wz in
        match p_getenv conf.env "notes" with
        [ Some nn ->
            if conf.wizard && conf.user = wz then
              print_wizard_mod conf base wizfile wz nn
            else incorrect_request conf
        | None -> print_wizard conf base wizfile wz ]
    | None -> print_main conf base wizfile ]
;
