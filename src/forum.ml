(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: forum.ml,v 4.28 2003-07-07 06:30:56 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Util;
open Config;
open Def;
open Printf;

type message =
  { m_time : string;
    m_ident : string;
    m_wizard : string;
    m_email : string;
    m_access : string;
    m_subject : string;
    m_mess : string }
;

value forum_file conf =
  Filename.concat (base_path [] (conf.bname ^ ".gwb")) "forum"
;

(* Black list *)

value match_strings regexp s =
  loop 0 0 where rec loop i j =
    if i == String.length regexp && j == String.length s then True
    else if i == String.length regexp then False
    else if j == String.length s then False
    else if regexp.[i] = s.[j] then loop (i + 1) (j + 1)
    else if regexp.[i] = '*' then
      if i + 1 == String.length regexp then True
      else if regexp.[i + 1] = s.[j] then loop (i + 2) (j + 1)
      else loop i (j + 1)
    else False
;

value can_post conf =
  try
    let fname = List.assoc "forum_exclude_file" conf.base_env in
    let fname = Util.base_path [] fname in
    let ic = open_in fname in
    let rec loop () =
      match try Some (input_line ic) with [ End_of_file -> None ] with
      [ Some line ->
          if match_strings line conf.from then do { close_in ic; False }
          else loop ()
      | None -> do { close_in ic; True } ]
    in
    loop ()
  with
  [ Not_found | Sys_error _ -> True ]
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

value ndisp_items = 2;
value change_item pd d = d.month <> pd.month;

value print_one_header conf prec_date ndisp pos h =
  let (date, hour, ident, access, subject, beg_mess) = h in
  let ndisp =
    if date <> prec_date then do {
      let ndisp =
        match (prec_date, date) with
        [ (Dgreg pd _, Dgreg d _) ->
            if change_item d pd then
              if ndisp > 1 then do {
                if d.month <> pd.month then
                  Wserver.wprint
                    "<tr align=left><td colspan=4>&nbsp;</td></tr>\n"
                else ();
                ndisp - 1
              }
              else do {
                Wserver.wprint "</table>\n";
                if d.month <> pd.month then Wserver.wprint "<p>\n" else ();
                Wserver.wprint "<table border=%d>\n" conf.border;
                ndisp_items
              }
            else ndisp
        | _ ->
            do { Wserver.wprint "<table border=%d>\n" conf.border; ndisp } ]
      in
      tag "tr" "align=left" begin
        tag "td" "colspan=4 align=left" begin
          Wserver.wprint "%s" (Date.string_of_date conf date);
        end;
      end;
      ndisp
    }
    else ndisp
  in
  do {
    tag "tr" "align=left" begin
      tag "td" begin
        Wserver.wprint "<tt>&nbsp;%s&nbsp;</tt>"
          (if access = "priv" then "*" else "&nbsp;");
      end;
      tag "td" begin Wserver.wprint "<tt>%s</tt>" hour; end;
      tag "td" begin
        Wserver.wprint "<a href=\"%sm=FORUM;p=%d\"><b>%s</b></a>" (commd conf)
          pos (secure (sp2nbsp 23 ident));
      end;
      tag "td" begin
        if subject = "" || subject = "-" then
          Wserver.wprint "<em>...&nbsp;%s</em>" (secure (sp2nbsp 50 beg_mess))
        else Wserver.wprint "%s" (secure (sp2nbsp 50 subject));
      end;
    end;
    ndisp
  }
;

value message_txt conf i =
  transl_nth conf "message/previous message/previous messages/next message" i
;

value print_headers conf =
  let fname = forum_file conf in
  match
    try Some (Secure.open_in_bin fname) with [ Sys_error _ -> None ]
  with
  [ Some ic ->
      let ic_len = in_channel_length ic in
      let max_header_mess =
        match p_getint conf.env "len" with
        [ Some x -> x
        | None -> 100 ]
      in
      let _ =
        match p_getint conf.env "from" with
        [ Some x -> try seek_in ic x with [ Sys_error _ -> () ]
        | None -> () ]
      in
      let last_date =
        loop 0 (Dtext "", ndisp_items)
        where rec loop nmess (prec_date, ndisp) =
          let pos = ic_len - pos_in ic in
          match try Some (input_line ic) with [ End_of_file -> None ] with
          [ Some s ->
              if nmess > max_header_mess then do {
                 Wserver.wprint
                   "<tr align=left><td colspan=4>&nbsp;</td></tr>\n";
                 tag "tr" "align=left" begin
                   tag "td" "colspan=4" begin
                     Wserver.wprint
                       "<a href=\"%sm=FORUM;len=%d;from=%d\">%s</a>\n"
                          (commd conf) max_header_mess (ic_len - pos)
                          (message_txt conf 2);
                   end;
                end;
                close_in ic;
                prec_date
              }
              else
                let (time, s) = get_var ic "Time:" s in
                let (_, s) = get_var ic "From:" s in
                let (ident, s) = get_var ic "Ident:" s in
                let (wizard, s) = get_var ic "Wizard:" s in
                let (_, s) = get_var ic "Email:" s in
                let (access, s) = get_var ic "Access:" s in
                let (subject, s) = get_var ic "Subject:" s in
                let (_, s) = get_var ic "Text:" s in
                let (text, s) =
                  if subject = "" || subject = "-" then
                    let rec get_mess sl s =
                      if String.length s >= 2 && s.[0] = ' ' && s.[1] = ' '
                      then
                        let s = String.sub s 2 (String.length s - 2) in
                        get_mess [s :: sl] (input_line ic)
                      else (List.rev sl, s)
                    in
                    get_mess [] s
                  else
                    let rec skip_mess s =
                      if String.length s >= 2 && s.[0] = ' ' && s.[1] = ' '
                      then
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
                let date =
                  try
                    let y = int_of_string (String.sub date 0 4) in
                    let m = int_of_string (String.sub date 5 2) in
                    let d = int_of_string (String.sub date 8 2) in
                    Dgreg
                      {year = y; month = m; day = d; prec = Sure; delta = 0}
                      Dgregorian
                  with
                  [ Failure _ | Invalid_argument _ -> Dtext date ]
                in
                let (nmess, date, ndisp) =
                  if access = "priv" && not conf.wizard && not conf.friend then
                    (nmess, prec_date, ndisp)
                  else
                    let ndisp =
                      if ident <> "" then
                        let h =
                          (date, hour, ident, access, subject, beg_mess)
                        in
                        print_one_header conf prec_date ndisp pos h
                      else ndisp
                    in
                    (nmess + 1, date, ndisp)
                in
                loop nmess (date, ndisp)
          | None -> do { close_in ic; prec_date } ]
      in
      if last_date = Dtext "" then () else Wserver.wprint "</table>\n"
  | None -> () ]
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
    Wserver.wprint "%s" (capitale (transl conf "database forum"))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    if can_post conf then print_add_message conf else ();
    Wserver.wprint "\n";
    print_headers conf;
    trailer conf;
  }
;

(* Print a message *)

value get_message conf pos =
  let fname = forum_file conf in
  match
    try Some (Secure.open_in_bin fname) with [ Sys_error _ -> None ]
  with
  [ Some ic ->
      let ic_len = in_channel_length ic in
      let r =
        try
          do {
            seek_in ic (ic_len - pos);
            let s = input_line ic in
            let (time, s) = get_var ic "Time:" s in
            let (_, s) = get_var ic "From:" s in
            let (ident, s) = get_var ic "Ident:" s in
            let (wizard, s) = get_var ic "Wizard:" s in
            let (email, s) = get_var ic "Email:" s in
            let (access, s) = get_var ic "Access:" s in
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
              let m =
                {m_time = time; m_ident = ident; m_wizard = wizard;
                 m_email = email; m_access = access; m_subject = subject;
                 m_mess = mess}
              in
              Some (m, ic_len - pos_in ic, ic_len)
            else None
          }
        with
        [ End_of_file -> None ]
      in
      do { close_in ic; r }
  | None -> None ]
;

value backward_pos conf pos =
  let fname = forum_file conf in
  match
    try Some (Secure.open_in_bin fname) with [ Sys_error _ -> None ]
  with
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

value passwd_in_file conf =
  match p_getenv conf.base_env "wizard_passwd_file" with
  [ Some "" | None -> False
  | Some _ -> True ]
;

value print_one_forum_message conf m pos next_pos forum_length =
  let title _ =
    let subject =
      if m.m_subject = "" || m.m_subject = "-" then
        capitale (transl conf "database forum")
      else secure m.m_subject
    in
    Wserver.wprint "%s" subject
  in
  do {
    header_no_page_title conf title;
    print_link_to_welcome conf True;
    tag "ul" begin
      Wserver.wprint "<li><a href=\"%sm=FORUM\">%s</a>\n" (commd conf)
        (capitale (transl conf "database forum"));
      Wserver.wprint "<li>";
      loop pos where rec loop pos =
        if pos = forum_length then Wserver.wprint "&nbsp;"
        else
          let back_pos = backward_pos conf pos in
          match get_message conf back_pos with
          [ Some (m, _, _) ->
              if m.m_access = "priv" && not conf.wizard && not conf.friend then
                loop back_pos
              else
                Wserver.wprint "<a href=\"%sm=FORUM;p=%d\">%s</a>\n"
                  (commd conf) back_pos (capitale (message_txt conf 3))
          | None -> Wserver.wprint "&nbsp;" ];
      Wserver.wprint "<li>";
      loop next_pos where rec loop next_pos =
        if next_pos > 0 then
          match get_message conf next_pos with
          [ Some (m, next_next_pos, _) ->
              if m.m_access = "priv" && not conf.wizard && not conf.friend then
                loop next_next_pos
              else
                Wserver.wprint "<a href=\"%sm=FORUM;p=%d\">%s</a>\n"
                  (commd conf) next_pos (capitale (message_txt conf 1))
          | None -> Wserver.wprint "&nbsp;" ]
        else Wserver.wprint "&nbsp;";
    end;
    Wserver.wprint "<p>\n";
    Wserver.wprint "<strong>%s</strong>\n" (secure m.m_ident);
    if m.m_email <> "" then
      let email = secure m.m_email in
      Wserver.wprint " <a href=\"mailto:%s\">%s</a>\n" email email
    else ();
    Wserver.wprint "<br>\n";
    if m.m_subject <> "" then
      Wserver.wprint "<b>%s: %s</b>\n<br>\n"
        (capitale (header_txt conf 2)) (secure m.m_subject)
    else ();
    if m.m_access = "priv" then
      Wserver.wprint "<b>%s: %s</b>\n<br>\n"
        (capitale (transl conf "access")) (transl conf "private")
    else ();
    Wserver.wprint "<em>%s</em>\n" m.m_time;
    Wserver.wprint "<dl><dt><dd>\n";
    if browser_doesnt_have_tables conf then ()
    else
      Wserver.wprint
        "<table cellspacing=0 cellpadding=0><tr align=left><td>\n";
    let mess =
      loop True 0 0 where rec loop last_was_eoln len i =
        if i = String.length m.m_mess then Buff.get len
        else if m.m_mess.[i] = '\n' && last_was_eoln then
          loop False (Buff.mstore len "<p>\n") (i + 1)
        else
          loop (m.m_mess.[i] = '\n') (Buff.store len m.m_mess.[i]) (i + 1)
    in
    Wserver.wprint "%s\n" (string_with_macros conf True [] mess);
    if browser_doesnt_have_tables conf then ()
    else Wserver.wprint "</table>";
    Wserver.wprint "</dl>\n";
    if m.m_wizard <> "" && conf.wizard && conf.user = m.m_wizard &&
      passwd_in_file conf
    then
      let s = message_txt conf 0 in
      do {
        Wserver.wprint "<p>\n";
        tag "form" "method=post action=\"%s\"" conf.command begin
          Util.hidden_env conf;
          Wserver.wprint "<input type=hidden name=m value=FORUM_DEL>\n";
          Wserver.wprint "<input type=hidden name=p value=%d>\n" pos;
          Wserver.wprint "<input type=submit value=\"%s\">\n"
            (capitale
               (transl_decline conf "delete" (message_txt conf 0)));
        end;
      }
    else ();
    trailer conf;
  }
;

value print_forum_message conf base pos =
  match get_message conf pos with
  [ Some (m, next_pos, forum_length) ->
      if m.m_access = "priv" && not conf.wizard && not conf.friend then
        print_forum_headers conf base
      else
        print_one_forum_message conf m pos next_pos forum_length
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
  tag "tr" "align=left" begin
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
  if can_post conf then do {
    header conf title;
    print_link_to_welcome conf True;
    tag "form" "method=post action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      Wserver.wprint "<input type=hidden name=m value=FORUM_ADD_OK>\n";
      tag "table" "border=%d" conf.border begin
        print_var conf "Ident" (capitale (header_txt conf 0)) False conf.user;
        print_var conf "Email" (capitale (header_txt conf 1)) True "";
        print_var conf "Subject" (capitale (header_txt conf 2)) False "";
        if conf.wizard || conf.friend then
          tag "tr" "align=left colspan=2" begin
            stag "td" begin
              Wserver.wprint
                "<input type=radio name=Access value=publ checked>%s\n"
                (transl conf "public");
              Wserver.wprint "<input type=radio name=Access value=priv>%s\n"
                (transl conf "private");
            end;
            Wserver.wprint "\n";
          end
        else ();
      end;
      html_p conf;
      Wserver.wprint "%s<br>\n"
        (capitale (Gutil.nominative (message_txt conf 0)));
      stag "textarea" "name=Text rows=15 cols=70 wrap=soft" begin end;
      Wserver.wprint "\n<br>\n";
      Wserver.wprint "<input type=submit value=Ok>\n";
    end;
    trailer conf;
  }
  else incorrect_request conf
;

value get conf key =
  match p_getenv conf.env key with
  [ Some v -> v
  | None -> failwith (key ^ " unbound") ]
;

value forum_add conf base ident comm =
  let email = String.lowercase (Gutil.strip_spaces (get conf "Email")) in
  let subject = Gutil.strip_spaces (get conf "Subject") in
  let access =
    match p_getenv conf.env "Access" with
    [ Some v -> v
    | None -> "publ" ]
  in
  let bfile = base_path [] (conf.bname ^ ".gwb") in
  if ident <> "" && comm <> "" then
    lock (Iobase.lock_file bfile) with
    [ Accept ->
        let fname = forum_file conf in
        let tmp_fname = fname ^ "~" in
        let oc = Secure.open_out tmp_fname in
        try
          let (hh, mm, ss) = conf.time in
          do {
            fprintf oc "Time: %04d-%02d-%02d %02d:%02d:%02d\n"
              conf.today.year conf.today.month conf.today.day hh mm ss;
            fprintf oc "From: %s\n" conf.from;
            fprintf oc "Ident: %s\n" ident;
            if (conf.wizard || conf.just_friend_wizard) && conf.user <> ""
            then
              fprintf oc "Wizard: %s\n" conf.user
            else ();
            if email <> "" then fprintf oc "Email: %s\n" email else ();
            fprintf oc "Access: %s\n" access;
            let subject = if subject = "" then "-" else subject in
            fprintf oc "Subject: %s\n" subject;
            fprintf oc "Text:\n";
            let rec loop i bol =
              if i == String.length comm then ()
              else do {
                if bol then fprintf oc "  " else ();
                if comm.[i] <> '\r' then output_char oc comm.[i] else ();
                loop (i + 1) (comm.[i] = '\n')
              }
            in
            loop 0 True;
            fprintf oc "\n\n";
            match
              try
                Some (Secure.open_in_bin fname) with
                [ Sys_error _ -> None ]
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
  if not (can_post conf) then incorrect_request conf
  else if ident = "" || comm = "" then print conf base
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
          (capitale (transl conf "database forum"));
        trailer conf;
      }
    with
    [ Update.ModErr -> () ]
;

(* Deleting a message *)

value internal_error conf base =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Wserver.wprint "<em>internal error</em>\n";
    trailer conf;
    raise Update.ModErr
  }
;

value forum_del conf base pos next_pos =
  let fname = forum_file conf in
  let tmp_fname = fname ^ "~" in
  match
     try Some (Secure.open_in_bin fname) with [ Sys_error _ -> None ]
  with
  [ Some ic ->
      let oc = Secure.open_out tmp_fname in
      let len = in_channel_length ic in
      let pos = len - pos in
      let next_pos = len - next_pos in
      do {
        loop 0 where rec loop i =
          if i = len then ()
          else
            let c = input_char ic in
            do {
              if i < pos || i >= next_pos then output_char oc c
              else ();
              loop (i + 1);
            };
        close_in ic;
        close_out oc;
        try Sys.remove fname with [ Sys_error _ -> () ];
        Sys.rename tmp_fname fname;
      }
  | None -> internal_error conf base ]
;

value print_del_ok conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "message deleted"))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<a href=\"%sm=FORUM\">%s</a>\n" (commd conf)
      (capitale (transl conf "database forum"));
    trailer conf;
  }
;

value delete_forum_message conf base pos =
  let bfile = base_path [] (conf.bname ^ ".gwb") in
  lock (Iobase.lock_file bfile) with
  [ Accept ->
      match get_message conf pos with
      [ Some (m, next_pos, forum_length) ->
          if conf.wizard && conf.user <> "" && m.m_wizard = conf.user &&
            passwd_in_file conf
          then
            try
              do {
                forum_del conf base pos next_pos;
                print_del_ok conf base;
              }
            with
            [ Update.ModErr -> () ]
          else print_forum_headers conf base
      | None -> print_forum_headers conf base ]
  | Refuse -> Update.error_locked conf base ]
;

value print_del conf base =
  match p_getint conf.env "p" with
  [ Some pos -> delete_forum_message conf base pos
  | None -> print_forum_headers conf base ]
;
