(* camlp4r ./pa_html.cmo *)
(* $Id: forum.ml,v 2.1 1999-10-08 07:15:48 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Util;
open Config;

value get_var ic lab s =
  let len = String.length lab in
  if String.length s >= len && String.sub s 0 len = lab then
    let start =
      if String.length s > len && s.[len] = ' ' then len + 1 else len
    in
    (String.sub s start (String.length s - start), input_line ic)
  else ("", s)
;

value print_forum conf base label text =
  let fname =
    List.fold_right Filename.concat [Util.base_dir.val; conf.bname ^ ".gwb"]
      (label ^ "_forum")
  in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      try
        loop (input_line ic) where rec loop s =
          let (time, s) = get_var ic "Time:" s in
          let (ident, s) = get_var ic "Ident:" s in
          let (email, s) = get_var ic "Email:" s in
          let (web, s) = get_var ic "Web:" s in
          let (_, s) = get_var ic "Text:" s in
          let (mess, s) =
            get_mess [] s where rec get_mess sl s =
              if String.length s > 2 && s.[0] = ' ' && s.[1] = ' ' then
                let s = String.sub s 2 (String.length s - 2) in
                get_mess [s :: sl] (input_line ic)
              else (List.rev sl, s)
          in
          do Wserver.wprint "<p><hr>\n";
             Wserver.wprint "<tt>%s</tt><br>\n" time;
             if ident <> "" then Wserver.wprint "%s<br>\n" ident
             else ();
             if email <> "" then
               Wserver.wprint "<a href=\"mailto:%s\">%s</a><br>\n"
                 email email
             else ();
             if web <> "" then
               Wserver.wprint "< href=\"%s\">%s</a><br>\n" web web
             else ();
             Wserver.wprint "<dl><dt><dd>\n";
             List.iter (fun s -> Wserver.wprint "%s\n" s) mess;
             Wserver.wprint "</dl>\n";
          return loop (input_line ic)
      with
      [ End_of_file -> close_in ic ]
  | None -> () ]
;

value print conf base label text =
  let title h =
    Wserver.wprint "%s - %s%s%s" (capitale (transl conf "forum"))
      (if h then "" else "<em>") text (if h then "" else "</em>")
  in
  do header conf title;
     print_link_to_welcome conf True;
     stag "a" "href=\"%sm=FORUM_ADD;k=%s\"" (commd conf) label begin
       Wserver.wprint "%s\n"
         (capitale (transl_decline conf "add" (transl conf "comment")));
     end;
     Wserver.wprint "\n";
     print_forum conf base label text;
     trailer conf;
  return ()
;

value print_wizard conf base = print conf base "w" (transl conf "wizard");
value print_friend conf base = print conf base "f" (transl conf "friend");

value print_add conf base label text = ();

value print_add_wizard conf base =
  print_add conf base "w" (transl conf "wizard")
;
value print_add_friend conf base =
  print conf base "f" (transl conf "friend")
;
