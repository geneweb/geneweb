(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: forum.ml,v 4.8 2001-04-22 10:35:20 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Util;
open Config;
open Def;

value forum_file conf =
  Filename.concat (base_path [] (conf.bname ^ ".gwb")) "forum"
;

(* Print headers *)

value get_var ic lab s =
  let len = String.length lab in
  if String.length s >= len && String.sub s 0 len = lab then
    let start =
      if String.length s > len && s.[len] = ' ' then len + 1 else len
    in
    (String.sub s start (String.length s - start), input_line ic)
  else ("", s)
;

value sp2nbsp lim s =
  loop 0 0 where rec loop i len =
    if i >= String.length s then Buff.get len
    else if i > lim && String.length s > lim + 3 then Buff.get len ^ "..."
    else
      let len =
        match s.[i] with
        [ ' ' -> Buff.mstore len "&nbsp;"
        | x -> Buff.store len x ]
      in
      loop (i + 1) len
;

value print_one_header conf prec_date pos h =
  let (date, hour, ident, subject, beg_mess) = h in
  do {
    if date <> prec_date then do {
      let d =
        try
          let y = int_of_string (String.sub date 0 4) in
          let m = int_of_string (String.sub date 5 2) in
          let d = int_of_string (String.sub date 8 2) in
          Dgreg {year = y; month = m; day = d; prec = Sure; delta = 0}
            Dgregorian
        with
        [ Failure _ -> Dtext date ]
      in
      if prec_date = "" ||
         String.length prec_date > 7 && String.length date > 7 &&
         String.sub prec_date 5 2 <> String.sub date 5 2 then
         do {
        if prec_date <> "" then Wserver.wprint "</table>\n<p>\n" else ();
        Wserver.wprint "<table border=%d>\n" conf.border;
      }
      else ();
      tag "tr" begin
        tag "td" "colspan=4 align=left" begin
          Wserver.wprint "%s" (Date.string_of_date conf d);
        end;
      end;
    }
    else ();
    tag "tr" begin
      tag "td" begin Wserver.wprint "<tt>&nbsp;&nbsp;&nbsp;</tt>"; end;
      tag "td" begin Wserver.wprint "<tt>%s</tt>" hour; end;
      tag "td" begin
        Wserver.wprint "<a href=\"%sm=FORUM;p=%d\"><b>%s</b></a>" (commd conf)
          pos (secure (sp2nbsp 23 ident));
      end;
      tag "td" begin
        if subject = "" || subject = "-" then
          Wserver.wprint "<em>... %s</em>" (secure (sp2nbsp 30 beg_mess))
        else Wserver.wprint "%s" (secure (sp2nbsp 30 subject));
      end;
    end;
  }
;

value print_headers conf =
  let fname = forum_file conf in
  match try Some (open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let ic_len = in_channel_length ic in
      let last_date =
        loop "" where rec loop prec_date =
          let pos = ic_len - pos_in ic in
          match try Some (input_line ic) with [ End_of_file -> None ] with
          [ Some s ->
              let (time, s) = get_var ic "Time:" s in
              let (ident, s) = get_var ic "Ident:" s in
              let (_, s) = get_var ic "Email:" s in
              let (subject, s) = get_var ic "Subject:" s in
              let (_, s) = get_var ic "Text:" s in
              let (text, s) =
                if subject = "" || subject = "-" then
                  let rec get_mess sl s =
                    if String.length s >= 2 && s.[0] = ' ' && s.[1] = ' ' then
                      let s = String.sub s 2 (String.length s - 2) in
                      get_mess [s :: sl] (input_line ic)
                    else (List.rev sl, s)
                  in
                  get_mess [] s
                else
                  let rec skip_mess s =
                    if String.length s >= 2 && s.[0] = ' ' && s.[1] = ' ' then
                      skip_mess (input_line ic)
                    else ([], s)
                  in
                  skip_mess s
              in
              let (date, hour) =
                try
                  let i = String.index time ' ' in
                  (String.sub time 0 i,
                   String.sub time (i + 1) (String.length time - i - 1))
                with
                [ Not_found -> ("", time) ]
              in
              let beg_mess =
                match text with
                [ [x :: _] -> x
                | _ -> "" ]
              in
              do {
                if ident <> "" then
                  let h = (date, hour, ident, subject, beg_mess) in
                  print_one_header conf prec_date pos h
                else ();
                loop date
              }
          | None -> do { close_in ic; prec_date } ]
      in
      if last_date = "" then () else Wserver.wprint "</table>\n"
  | None -> () ]
;

value message_txt conf i =
  transl_nth conf "message/previous message/next message" i
;

value header_txt conf i = transl_nth conf "ident/email/subject" i;

value print_add_message conf =
  tag "form" "method=get action=\"%s\"" conf.command begin
    Util.hidden_env conf;
    Wserver.wprint "<input type=hidden name=m value=FORUM_ADD>\n";
    Wserver.wprint "<input type=submit value=\"%s\">\n"
      (capitale (transl_decline conf "add" (message_txt conf 0)));
  end
;

value print_forum_headers conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "data base forum"))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    print_add_message conf;
    Wserver.wprint "\n";
    print_headers conf;
    trailer conf;
  }
;

(* Print a message *)

value get_message conf pos =
  let fname = forum_file conf in
  match try Some (open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let ic_len = in_channel_length ic in
      let r =
        try
          do {
            seek_in ic (ic_len - pos);
            let s = input_line ic in
            let (time, s) = get_var ic "Time:" s in
            let (ident, s) = get_var ic "Ident:" s in
            let (email, s) = get_var ic "Email:" s in
            let (subject, s) = get_var ic "Subject:" s in
            let (_, s) = get_var ic "Text:" s in
            let (mess, s) =
              get_mess 0 s where rec get_mess len s =
                if String.length s >= 2 && s.[0] = ' ' && s.[1] = ' ' then
                  let s = String.sub s 2 (String.length s - 2) in
                  let len = if len = 0 then len else Buff.store len '\n' in
                  get_mess (Buff.mstore len s) (input_line ic)
                else (Buff.get len, s)
            in
            if ident <> "" then
              Some
                (time, ident, email, subject, mess, ic_len - pos_in ic,
                 ic_len)
            else None
          }
        with
        [ End_of_file -> None ]
      in
      do { close_in ic; r }
  | None -> None ]
;

value backward conf pos =
  let fname = forum_file conf in
  match try Some (open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let ic_len = in_channel_length ic in
      let sync_txt = "\nTime: " in
      let sync_txt_last = String.length sync_txt - 1 in
      let new_pos =
        loop (ic_len - pos - 1) sync_txt_last where rec loop new_pos i =
          if new_pos = 0 && i = 1 then ic_len
          else if new_pos > 0 then do {
            seek_in ic new_pos;
            let c = input_char ic in
            if c = sync_txt.[i] then
              if i = 0 then ic_len - new_pos - 1
              else loop (new_pos - 1) (i - 1)
            else loop (new_pos - 1) sync_txt_last
          }
          else pos
      in
      do { close_in ic; new_pos }
  | None -> pos ]
;

value print_forum_message conf base pos =
  let pos =
    match p_getenv conf.env "t" with
    [ Some "back" -> backward conf pos
    | _ -> pos ]
  in
  match get_message conf pos with
  [ Some (time, ident, email, subject, mess, next_pos, forum_length) ->
      let title _ =
        let subject =
          if subject = "" || subject = "-" then
            capitale (transl conf "data base forum")
          else secure subject
        in
        Wserver.wprint "%s" subject
      in
      do {
        header_no_page_title conf title;
        print_link_to_welcome conf True;
        tag "ul" begin
          Wserver.wprint "<li><a href=\"%sm=FORUM\">%s</a>\n" (commd conf)
            (capitale (transl conf "data base forum"));
          Wserver.wprint "<li>";
          if pos = forum_length then Wserver.wprint "&nbsp;"
          else
            Wserver.wprint "<a href=\"%sm=FORUM;p=%d;t=back\">%s</a>\n"
              (commd conf) pos (capitale (message_txt conf 2));
          Wserver.wprint "<li>";
          if next_pos > 0 then
            Wserver.wprint "<a href=\"%sm=FORUM;p=%d\">%s</a>\n" (commd conf)
              next_pos (capitale (message_txt conf 1))
          else Wserver.wprint "&nbsp;";
        end;
        Wserver.wprint "<p>\n";
        Wserver.wprint "<strong>%s</strong>\n" (secure ident);
        if email <> "" then
          let email = secure email in
          Wserver.wprint " <a href=\"mailto:%s\">%s</a>\n" email email
        else ();
        Wserver.wprint "<br>\n";
        if subject <> "" then
          Wserver.wprint "<b>%s: %s</b>\n<br>\n"
            (capitale (header_txt conf 2)) (secure subject)
        else ();
        Wserver.wprint "<em>%s</em>\n" time;
        Wserver.wprint "<dl><dt><dd>\n";
        if browser_doesnt_have_tables conf then ()
        else Wserver.wprint "<table cellspacing=0 cellpadding=0><tr><td>\n";
        let mess =
          loop True 0 0 where rec loop last_was_eoln len i =
            if i = String.length mess then Buff.get len
            else if mess.[i] = '\n' && last_was_eoln then
              loop False (Buff.mstore len "<p>\n") (i + 1)
            else loop (mess.[i] = '\n') (Buff.store len mess.[i]) (i + 1)
        in
        copy_string_with_macros conf [] mess;
        Wserver.wprint "\n";
        if browser_doesnt_have_tables conf then ()
        else Wserver.wprint "</table>";
        Wserver.wprint "</dl>\n";
        trailer conf;
      }
  | None -> print_forum_headers conf base ]
;

(* Print headers or message *)

value print conf base =
  match p_getint conf.env "p" with
  [ Some pos -> print_forum_message conf base pos
  | None -> print_forum_headers conf base ]
;

(* Send a message *)

value print_var conf var name opt def_value =
  tag "tr" begin
    stag "td" begin
      Wserver.wprint "%s" name;
      if opt then Wserver.wprint " (%s)" (transl conf "optional") else ();
    end;
    Wserver.wprint "\n";
    stag "td" begin
      Wserver.wprint "<input name=%s size=40 maxlength=200 value=\"%s\">\n"
        var def_value;
    end;
    Wserver.wprint "\n";
  end
;

value print_add conf base =
  let title _ =
    Wserver.wprint "%s"
      (capitale (transl_decline conf "add" (message_txt conf 0)))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    tag "form" "method=post action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      Wserver.wprint "<input type=hidden name=m value=FORUM_ADD_OK>\n";
      tag "table" "border=%d" conf.border begin
        print_var conf "Ident" (capitale (header_txt conf 0)) False conf.user;
        print_var conf "Email" (capitale (header_txt conf 1)) True "";
        print_var conf "Subject" (capitale (header_txt conf 2)) False "";
      end;
      html_p conf;
      Wserver.wprint "%s<br>\n"
        (capitale (Gutil.nominative (message_txt conf 0)));
      stag "textarea" "name=Text rows=15 cols=70 wrap=virtual" begin end;
      Wserver.wprint "\n<br>\n";
      Wserver.wprint "<input type=submit value=Ok>\n";
    end;
    trailer conf;
  }
;

value get conf key =
  match p_getenv conf.env key with
  [ Some v -> v
  | None -> failwith (key ^ " unbound") ]
;

value forum_add conf base ident comm =
  let email = String.lowercase (Gutil.strip_spaces (get conf "Email")) in
  let subject = Gutil.strip_spaces (get conf "Subject") in
  let bfile = base_path [] (conf.bname ^ ".gwb") in
  if ident <> "" && comm <> "" then
    lock (Iobase.lock_file bfile) with
    [ Accept ->
        let fname = forum_file conf in
        let tmp_fname = fname ^ "~" in
        let oc = open_out tmp_fname in
        try
          let (hh, mm, ss) = conf.time in
          do {
            Printf.fprintf oc "Time: %04d-%02d-%02d %02d:%02d:%02d\n"
              conf.today.year conf.today.month conf.today.day hh mm ss;
            Printf.fprintf oc "Ident: %s\n" ident;
            if email <> "" then Printf.fprintf oc "Email: %s\n" email else ();
            let subject = if subject = "" then "-" else subject in
            Printf.fprintf oc "Subject: %s\n" subject;
            Printf.fprintf oc "Text:\n";
            let rec loop i bol =
              if i == String.length comm then ()
              else do {
                if bol then Printf.fprintf oc "  " else ();
                if comm.[i] <> '\r' then output_char oc comm.[i] else ();
                loop (i + 1) (comm.[i] = '\n')
              }
            in
            loop 0 True;
            Printf.fprintf oc "\n\n";
            match
              try Some (open_in_bin fname) with [ Sys_error _ -> None ]
            with
            [ Some ic ->
                do {
                  try while True do { output_char oc (input_char ic) } with
                  [ End_of_file -> () ];
                  close_in ic;
                }
            | _ -> () ];
            close_out oc;
            try Sys.remove fname with [ Sys_error _ -> () ];
            Sys.rename tmp_fname fname;
          }
        with e ->
          do {
            try close_out oc with _ -> ();
            try Sys.remove tmp_fname with [ Sys_error _ -> () ];
            raise e
          }
    | Refuse -> do { Update.error_locked conf base; raise Update.ModErr } ]
  else ()
;

value print_add_ok conf base =
  let ident = Gutil.strip_spaces (get conf "Ident") in
  let comm = Gutil.strip_spaces (get conf "Text") in
  if ident = "" || comm = "" then print conf base
  else
    let title _ =
      Wserver.wprint "%s" (capitale (transl conf "message added"))
    in
    try
      do {
        forum_add conf base ident comm;
        header conf title;
        print_link_to_welcome conf True;
        Wserver.wprint "<a href=\"%sm=FORUM\">%s</a>\n" (commd conf)
          (capitale (transl conf "data base forum"));
        trailer conf;
      }
    with
    [ Update.ModErr -> () ]
;
