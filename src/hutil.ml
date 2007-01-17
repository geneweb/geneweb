(* camlp4r ./pa_html.cmo *)
(* $Id: hutil.ml,v 5.5 2007-01-17 14:46:50 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config;

value header_without_http conf title = do {
  Wserver.wprint "%s\n" (Util.doctype conf);
  Wserver.wprint "<html>\n<head>\n";
  Wserver.wprint "  <title>";
  title True;
  Wserver.wprint "</title>\n";
  Wserver.wprint "  <meta name=\"robots\" content=\"none\"%s>\n" conf.xhs;
  Wserver.wprint "  <meta http-equiv=\"Content-Type\" \
                    content=\"text/html; charset=%s\"%s>\n"
    conf.charset conf.xhs;
  Wserver.wprint
    "  <meta http-equiv=\"Content-Style-Type\" content=\"text/css\"%s>\n"
    conf.xhs;
  Wserver.wprint "  \
  <style type=\"text/css\"><!--
    .highlight { color: %s; font-weight: bold }
    .found { color: black; background-color: #afa;font-weight:bold }
    hr { border: 0; border-bottom: 1px solid }
    a.date { text-decoration: none; color: black }
    div.summary ul { padding-left: 0; list-style-type: none }
    div.summary ul ul { padding-left: 1.618em }
  --></style>\n" conf.highlight;
  Util.include_hed_trl conf None ".hed";
  Wserver.wprint "</head>\n";
  let s =
    try " dir=\"" ^ Hashtbl.find conf.lexicon " !dir" ^ "\"" with
    [ Not_found -> "" ]
  in
  let s = s ^ Util.body_prop conf in Wserver.wprint "<body%s>" s;
  Wserver.wprint "\n";
  Util.message_to_wizard conf;
};

value header_without_page_title conf title = do {
  Util.html conf;
  Util.nl ();
  header_without_http conf title;
};

value header_link_welcome conf title = do {
  header_without_page_title conf title;
  Util.print_link_to_welcome conf True;
  Wserver.wprint "<h1 style=\"text-align:center\" class=\"highlight\">";
  title False;
  Wserver.wprint "</h1>\n";
};

value header_no_page_title conf title = do {
  header_without_page_title conf title;
  match Util.p_getenv conf.env "title" with
  [ None | Some "" -> ()
  | Some x -> do {
      Wserver.wprint "<h1 align=\"center\"><font color=%s>" conf.highlight;
      Wserver.wprint "%s" x;
      Wserver.wprint "</font></h1>\n"
    } ];
};

value header conf title = do {
  header_without_page_title conf title;
  Wserver.wprint "<h1 style=\"text-align:center\" class=\"highlight\">";
  title False;
  Wserver.wprint "</h1>\n";
};

value red_color = "red";

value rheader conf title = do {
  header_without_page_title conf title;
  Wserver.wprint "<center><h1><font color=%s>" red_color;
  title False;
  Wserver.wprint "</font></h1></center>\n";
};

value gen_trailer with_logo conf = do {
  if not with_logo then ()
  else
    Wserver.wprint "\
<div>
<a href=\"%s\"><img src=\"%s/gwlogo.png\"
 alt=\"...\" width=\"64\" height=\"72\" style=\"border:0;float:%s\"%s></a>
<br%s>
</div>
" (Util.commd conf) (Util.image_prefix conf) conf.right
    conf.xhs conf.xhs;
  Templ.print_copyright conf;
  Util.include_hed_trl conf None ".trl";
  Wserver.wprint "</body>\n</html>\n";
};

value trailer = gen_trailer True;

value incorrect_request conf = do {
  let title _ =
    Wserver.wprint "%s" (Util.capitale (Util.transl conf "incorrect request"))
  in
  header conf title;
  Wserver.wprint "<p>\n";
  Util.print_link_to_welcome conf False;
  Wserver.wprint "</p>\n";
  trailer conf
};

value error_cannot_access conf fname = do {
  let title _ = Wserver.wprint "Error" in
  header conf title;
  tag "ul" begin
    tag "li" begin
      Wserver.wprint "Cannot access file \"%s.txt\".\n"
        fname;
    end;
  end;
  trailer conf;
};

value interp conf base fname ifun env ep = do {
  let v = Templ.template_file.val in
  Templ.template_file.val := fname;
  try
    match Templ.input_templ conf fname with
    [ Some astl -> do {
        Util.html conf;
        Util.nl ();
        Templ.interp_ast conf (Some base) ifun env ep astl
      }
    | None ->
        error_cannot_access conf fname ]
  with e ->
    do { Templ.template_file.val := v; raise e };
  Templ.template_file.val := v;
};
