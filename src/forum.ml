(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: forum.ml,v 3.5 2000-06-03 21:08:03 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Util;
open Config;
open Def;

value forum_file conf =
  List.fold_right Filename.concat [Util.base_dir.val; conf.bname ^ ".gwb"]
    "forum"
;

value get_var ic lab s =
  let len = String.length lab in
  if String.length s >= len && String.sub s 0 len = lab then
    let start =
      if String.length s > len && s.[len] = ' ' then len + 1 else len
    in
    (String.sub s start (String.length s - start), input_line ic)
  else ("", s)
;

value print_forum conf base =
  let fname = forum_file conf in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      try
        loop (input_line ic) where rec loop s =
          let (time, s) = get_var ic "Time:" s in
          let (ident, s) = get_var ic "Ident:" s in
          let (email, s) = get_var ic "Email:" s in
          let (_, s) = get_var ic "Text:" s in
          let (mess, s) =
            get_mess [] s where rec get_mess sl s =
              if String.length s >= 2 && s.[0] = ' ' && s.[1] = ' ' then
                let s = String.sub s 2 (String.length s - 2) in
                get_mess [s :: sl] (input_line ic)
              else (List.rev sl, s)
          in
          do if ident <> "" then
               do Wserver.wprint "<p>\n";
                  Wserver.wprint "<strong>%s</strong>\n" (secure ident);
                  if email <> "" then
                    let email = secure email in
                    Wserver.wprint " <a href=\"mailto:%s\">%s</a>\n"
                      email email
                  else ();
                  Wserver.wprint "<br>\n";
                  Wserver.wprint "<em>%s</em>\n" time;
                  Wserver.wprint "<dl><dt><dd>\n";
                  List.iter
                    (fun s ->
                       do if s = "" then Wserver.wprint "<p>"
                          else copy_string_with_macros conf s;
                          Wserver.wprint "\n";
                       return ())
                    mess;
                  Wserver.wprint "</dl>\n";
               return ()
             else ();
          return loop (input_line ic)
      with
      [ End_of_file -> close_in ic ]
  | None -> () ]
;

value print conf base =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "forum")) in
  do header conf title;
     print_link_to_welcome conf True;
     tag "form" "method=get action=\"%s\"" conf.command begin
       Util.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=FORUM_ADD>\n";
       Wserver.wprint "<input type=submit value=\"%s\">\n"
         (capitale (transl_decline conf "add" (transl conf "comment")));
     end;
     Wserver.wprint "\n";
     print_forum conf base;
     trailer conf;
  return ()
;

value print_var conf name opt def_value =
  tag "tr" begin
    stag "td" begin
      Wserver.wprint "%s" name;
      if opt then Wserver.wprint " (%s)" (transl conf "optional") else ();
    end;
    Wserver.wprint "\n";
    stag "td" begin
      Wserver.wprint "<input name=%s size=40 maxlength=200 value=\"%s\">\n"
        name def_value;
    end;
    Wserver.wprint "\n";
  end
;

value print_add conf base =
  let title _ =
    Wserver.wprint "%s"
      (capitale (transl_decline conf "add" (transl conf "comment")))
  in
  do header conf title;
     print_link_to_welcome conf True;
     tag "form" "method=post action=\"%s\"" conf.command begin
       Util.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=FORUM_ADD_OK>\n";
       tag "table" "border=%d" conf.border begin
         print_var conf "Ident" False conf.user;
         print_var conf "Email" True "";
       end;
       html_p conf;
       Wserver.wprint "%s<br>\n" (capitale (transl conf "comment"));
       stag "textarea" "name=Text rows=15 cols=70 wrap=virtual" begin end;
       Wserver.wprint "\n";
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     trailer conf;
  return ()
;

value get conf key =
  match p_getenv conf.env key with
  [ Some v -> v
  | None -> failwith (key ^ " unbound") ]
;

value forum_add conf base ident comm =
  let email = String.lowercase (Gutil.strip_spaces (get conf "Email")) in
  let bfile = Filename.concat Util.base_dir.val conf.bname in
  if ident <> "" && comm <> "" then
    lock (Iobase.lock_file bfile) with
    [ Accept ->
        let fname = forum_file conf in
        let tmp_fname = fname ^ "~" in
        let oc = open_out tmp_fname in
        try
          let (hh, mm, ss) = conf.time in
          do Printf.fprintf oc "Time: %04d-%02d-%02d %02d:%02d:%02d\n"
	       conf.today.year conf.today.month conf.today.day hh mm ss;
             Printf.fprintf oc "Ident: %s\n" ident;
             if email <> "" then Printf.fprintf oc "Email: %s\n" email else ();
             Printf.fprintf oc "Text:\n";
             loop 0 True where rec loop i bol =
               if i == String.length comm then ()
               else
                 do if bol then Printf.fprintf oc "  " else ();
                    if comm.[i] <> '\r' then output_char oc comm.[i]
                    else ();
                 return loop (i + 1) (comm.[i] = '\n');
             Printf.fprintf oc "\n\n";
             match try Some (open_in fname) with [ Sys_error _ -> None ] with
             [ Some ic ->
                 do try
                      while True do output_char oc (input_char ic); done
                    with [ End_of_file -> () ];
                    close_in ic;
                 return ()
             | _ -> () ];
             close_out oc;
             try Sys.remove fname with [ Sys_error _ -> () ];
             Sys.rename tmp_fname fname;
          return ()
        with e ->
          do try close_out oc with _ -> ();
             try Sys.remove tmp_fname with [ Sys_error _ -> () ];
          return raise e
    | Refuse -> do Update.error_locked conf base; return raise Update.ModErr ]
  else ()
;

value print_add_ok conf base =
  let ident = Gutil.strip_spaces (get conf "Ident") in
  let comm = Gutil.strip_spaces (get conf "Text") in
  if ident = "" || comm = "" then print conf base
  else
    let title _ =
      Wserver.wprint "%s" (capitale (transl conf "comment added"))
    in
    try
      do forum_add conf base ident comm;
         header conf title;
         print_link_to_welcome conf True;
         Wserver.wprint "<a href=\"%sm=FORUM\">%s</a>\n"
           (commd conf) (capitale (transl conf "forum"));
         trailer conf;
      return ()
    with
    [ Update.ModErr -> () ]
;
