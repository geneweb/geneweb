(* camlp4r ./pa_html.cmo *)
(* $Id: notes.ml,v 3.3 2001-03-08 14:13:24 ddr Exp $ *)

open Config;
open Def;
open Gutil;
open Util;

value print conf base =
  let title _ =
    Wserver.wprint "%s - %s"
      (capitale (nominative (transl_nth conf "note/notes" 1)))
      conf.bname
  in
  let s = base.data.bnotes.nread 0 in
  do header_no_page_title conf title;
     print_link_to_welcome conf False;
     html_p conf;
     copy_string_with_macros conf [] s;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_mod conf base =
  let title _ =
    let s = transl_nth conf "note/notes" 1 in
    Wserver.wprint "%s - %s" (capitale (transl_decline conf "modify" s))
      conf.bname
  in
  let s = base.data.bnotes.nread 0 in
  do header conf title;
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Util.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=MOD_NOTES_OK>\n";
       stag "textarea" "name=notes rows=30 cols=70 wrap=virtual" begin
         if s <> "" then
           Wserver.wprint "%s" (quote_escaped s)
         else ();
       end;
       Wserver.wprint "\n";
       html_p conf;
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     trailer conf;
  return ()
;

value print_mod_ok conf base =
  let s =
    match p_getenv conf.env "notes" with
    [ Some v -> strip_spaces (strip_controls_m v)
    | None -> failwith "notes unbound" ]
  in
  do base.func.commit_notes s; return
  print conf base
;
