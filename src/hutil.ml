(* camlp5r ./pa_html.cmo *)
(* $Id: hutil.ml,v 5.11 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config;
open Printf;

value up_fname conf = "up.jpg";

value commd_no_params conf =
  conf.command ^ "?" ^
    List.fold_left
      (fun c (k, v) ->
         c ^ (if c = "" then "" else ";") ^ k ^
           (if v = "" then "" else "=" ^ v))
      "" conf.henv
;

value link_to_referer conf =
  let referer = Wserver.extract_param "referer: " '\n' conf.request in
  if referer <> "" then
    let fname = "left.jpg" in
    let wid_hei =
      match Util.image_size (Util.image_file_name fname) with
      [ Some (wid, hei) ->
          " width=\"" ^ string_of_int wid ^ "\" height=\"" ^
          string_of_int hei ^ "\""
      | None -> "" ]
    in
    sprintf "<a href=\"%s\"><img src=\"%s/%s\"%s style=\"border: 0\" title=\"&lt;&lt;\"%s></a>\n"
      referer (Util.image_prefix conf) fname wid_hei conf.xhs
  else ""
;

value gen_print_link_to_welcome f conf right_aligned =
  if conf.cancel_links then ()
  else do {
    let fname = up_fname conf in
    let wid_hei =
      match Util.image_size (Util.image_file_name fname) with
      [ Some (wid, hei) ->
          " width=\"" ^ string_of_int wid ^ "\" height=\"" ^
          string_of_int hei ^ "\""
      | None -> "" ]
    in
    if right_aligned then
      Wserver.wprint "<div style=\"float:%s\">\n" conf.right
    else Wserver.wprint "<p>\n";
    f ();
    let str = link_to_referer conf in
    if str = "" then () else Wserver.wprint "%s" str;
    Wserver.wprint "<a href=\"%s\">" (commd_no_params conf);
    Wserver.wprint "<img src=\"%s/%s\"%s style=\"border: 0\" title=\"^^\"%s>"
      (Util.image_prefix conf) fname wid_hei conf.xhs;
    Wserver.wprint "</a>\n";
    if right_aligned then Wserver.wprint "</div>\n"
    else Wserver.wprint "</p>\n"
  }
;

value print_link_to_welcome = gen_print_link_to_welcome (fun () -> ());

value header_without_http conf title = do {
  Wserver.wprint "%s\n" (Util.doctype conf);
  Wserver.wprint "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n";
  Wserver.wprint "<head>\n";
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
  Wserver.wprint 
    "  <link rel=\"shortcut icon\" href=\"images/favicon_gwd.png\" />\n" ;
  Wserver.wprint 
    "  <link rel=\"stylesheet\" type=\"text/css\" href=\"css/%s\" />\n"
    (Util.css_prop conf);
  Templ.include_hed_trl conf None "hed";
  Wserver.wprint "</head>\n";
  Wserver.wprint "<body>";
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
  print_link_to_welcome conf True;
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
  Wserver.wprint "<center><h1 class=\"error\">";
  title False;
  Wserver.wprint "</h1></center>\n";
};

value gen_trailer with_logo conf = do {
  Wserver.wprint "<br />\n";
  Wserver.wprint "<div id=\"footer\">\n" ;
  Wserver.wprint "<hr />\n";
  Templ.print_copyright conf;
  Wserver.wprint "</div>\n" ;
  Templ.include_hed_trl conf None "trl";
  Wserver.wprint "</body>\n</html>\n";
};

value trailer = gen_trailer True;

value incorrect_request conf = do {
  let title _ =
    Wserver.wprint "%s" (Util.capitale (Util.transl conf "incorrect request"))
  in
  header conf title;
  Wserver.wprint "<p>\n";
  print_link_to_welcome conf False;
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
