(* camlp4r ./pa_html.cmo *)
(* $Id: wiznotes.ml,v 4.7 2002-12-30 18:40:06 ddr Exp $ *)
(* Copyright (c) 2002 INRIA *)

open Config;
open Util;

value dir conf =
  Filename.concat (Util.base_path [] (conf.bname ^ ".gwb")) "wiznotes"
;

value wzfile wddir wz =
  Filename.concat wddir (wz ^ ".txt")
;

value read_wizfile fname =
  let fname = Util.base_path [] fname in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop [] where rec loop data =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some line ->
            let data =
              try
                let i = String.index line ':' in
                [String.sub line 0 i :: data]
              with
              [ Not_found -> data ]
            in
            loop data
        | None -> do { close_in ic; List.rev data } ]
  | None -> [] ]
;

value read_wizard_notes fname =
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop 0 where rec loop len =
        match try Some (input_char ic) with [ End_of_file -> None ] with
        [ Some c -> loop (Buff.store len c)
        | None -> do { close_in ic; Buff.get (max 0 (len - 1)) } ]
  | None -> "" ]
;

value write_wizard_notes fname nn =
  if nn = "" then
    try Sys.remove fname with [ Sys_error _ -> () ]
  else
    match try Some (open_out fname) with [ Sys_error _ -> None ] with
    [ Some oc ->
        do {
          output_string oc nn;
          output_string oc "\n";
          close_out oc;
        }
    | None -> () ]
;

value print_main conf base wizfile =
  let wiztxt = Gutil.nominative (transl_nth conf "wizard/wizards/friend" 1) in
  let title _ =
    Wserver.wprint "%s - %s" (capitale wiztxt)
      (Gutil.nominative (transl_nth conf "note/notes" 1))
  in
  let wizdata = read_wizfile wizfile in
  let wddir = dir conf in
  do {
    header conf title;
    print_link_to_welcome conf False;
    html_p conf;
    Gutil.list_iter_first
     (fun first wz ->
        Wserver.wprint "%s%t" (if first then "" else ",\n")
          (fun _ ->
             if (conf.wizard && conf.user = wz) ||
                 Sys.file_exists (wzfile wddir wz)
             then
               Wserver.wprint "<a href=\"%sm=WIZNOTES;v=%s%t\">%s</a>"
                 (commd conf) (Util.code_varenv wz)
                 (fun _ ->
                    try
                      let s = Unix.stat (wzfile wddir wz) in
                      let tm = Unix.localtime s.Unix.st_mtime in
                      Wserver.wprint ";d=%d-%02d-%02d,%02d:%02d:%02d"
                        (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
                        tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
                        tm.Unix.tm_sec
                    with
                    [ Unix.Unix_error _ _ _ -> () ])
                 wz
             else
               Wserver.wprint "%s" wz))
     wizdata;
    html_p conf;
    Wserver.wprint "%d %s\n" (List.length wizdata) wiztxt;
    trailer conf;
  }
;

value print_wizard conf base wz =
  let title _ = Wserver.wprint "%s" wz in
  let s = read_wizard_notes (wzfile (dir conf) wz) in
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
    if conf.wizard && conf.user = wz then
      do {
        html_p conf;
        tag "form" "method=POST action=\"%s\"" conf.command begin
          Util.hidden_env conf;
          Wserver.wprint "<input type=hidden name=m value=WIZNOTES>\n";
          Wserver.wprint "<input type=hidden name=v value=\"%s\">\n" wz;
          let digest = Iovalue.digest s in
          Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n"
            digest;
          stag "textarea" "name=notes rows=30 cols=70 wrap=virtual" begin
            if s <> "" then Wserver.wprint "%s" (quote_escaped s) else ();
          end;
          Wserver.wprint "\n";
          html_p conf;
          Wserver.wprint "<input type=submit value=Ok>\n";
        end;
      }
    else ();
    trailer conf;
  }
;

value print_wizard_mod conf base wizfile wz nn =
  let wddir = dir conf in
  let fname = wzfile wddir wz in
  let on = read_wizard_notes fname in
  let nn = Gutil.strip_all_trailing_spaces nn in
  let digest =
    match p_getenv conf.env "digest" with
    [ Some s -> s
    | None -> "" ]
  in
  if digest = Iovalue.digest on then
    do {
      if nn <> on then
        do {
          try Unix.mkdir wddir 0o755 with
          [ Unix.Unix_error _ _ _ -> () ];
          write_wizard_notes fname nn;
        }
      else ();
      print_main conf base wizfile;
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
    | (Some wizfile, _)  -> wizfile
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
        | None -> print_wizard conf base wz ]
    | None -> print_main conf base wizfile ]
;
