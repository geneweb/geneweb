(* camlp4r ./pa_html.cmo *)
(* $Id: notes.ml,v 4.3 2002-11-14 04:15:34 ddr Exp $ *)

open Config;
open Def;
open Gutil;
open Util;

value print conf base =
  let title _ =
    Wserver.wprint "%s - %s"
      (capitale (nominative (transl_nth conf "note/notes" 1))) conf.bname
  in
  let s = base.data.bnotes.nread 0 in
  do {
    header_no_page_title conf title;
    print_link_to_welcome conf False;
    html_p conf;
    Wserver.wprint "%s\n" (string_with_macros conf [] s);
    trailer conf;
  }
;

value print_mod conf base =
  let title _ =
    let s = transl_nth conf "note/notes" 1 in
    Wserver.wprint "%s - %s" (capitale (transl_decline conf "modify" s))
      conf.bname
  in
  let s = base.data.bnotes.nread 0 in
  do {
    header conf title;
    tag "form" "method=POST action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      Wserver.wprint "<input type=hidden name=m value=MOD_NOTES_OK>\n";
      let digest = Iovalue.digest s in
      Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n" digest;
      stag "textarea" "name=notes rows=30 cols=70 wrap=virtual" begin
        if s <> "" then Wserver.wprint "%s" (quote_escaped s) else ();
      end;
      Wserver.wprint "\n";
      html_p conf;
      Wserver.wprint "<input type=submit value=Ok>\n";
    end;
    trailer conf;
  }
;

value print_mod_ok conf base =
  let s =
    match p_getenv conf.env "notes" with
    [ Some v -> strip_all_trailing_spaces v
    | None -> failwith "notes unbound" ]
  in
  let digest =
    match p_getenv conf.env "digest" with
    [ Some s -> s
    | None -> "" ]
  in
  let old_notes = base.data.bnotes.nread 0 in
  try
    if digest <> Iovalue.digest old_notes then Update.error_digest conf base
    else do { base.func.commit_notes s; print conf base }
  with
  [ Update.ModErr -> () ]
;
