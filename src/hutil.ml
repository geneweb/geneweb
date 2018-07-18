(* camlp5r ./pa_html.cmo *)
(* $Id: hutil.ml,v 5.11 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config;
open Printf;

value commd_no_params conf =
  conf.command ^ "?" ^
    List.fold_left
      (fun c (k, v) ->
         c ^ (if c = "" then "" else ";") ^ k ^
           (if v = "" then "" else "=" ^ v))
      "" conf.henv
;

value link_to_referer conf =
  let referer = Util.get_referer conf in
  let back = Util.capitale (Util.transl conf "back") in
  if referer <> "" then
    sprintf
      "<a href=\"%s\">\
         <span class=\"fa fa-arrow-left fa-lg\" title=\"%s\"></span>\
       </a>\n"
      referer back
  else ""
;

value gen_print_link_to_welcome f conf right_aligned =
  if conf.cancel_links then ()
  else do {
    if right_aligned then
      Wserver.printf "<div class=\"btn-group float-%s mt-2\">\n" conf.right
    else Wserver.printf "<p>\n";
    f ();
    let str = link_to_referer conf in
    if str = "" then () else Wserver.printf "%s" str;
    Wserver.printf
      "<a href=\"%s\">\
         <span class=\"fa fa-home fa-lg ml-1 px-0\" title=\"%s\"></span>\
       </a>\n"
      (commd_no_params conf)
      (Util.capitale (Util.transl conf "home"));
    if right_aligned then Wserver.printf "</div>\n"
    else Wserver.printf "</p>\n"
  }
;

value print_link_to_welcome = gen_print_link_to_welcome (fun () -> ());

value header_without_http conf title = do {
  Wserver.printf "<!DOCTYPE html>\n";
  Wserver.printf "<head>\n";
  Wserver.printf "  <title>";
  title True;
  Wserver.printf "</title>\n";
  Wserver.printf "  <meta name=\"robots\" content=\"none\">\n";
  Wserver.printf "  <meta charset=\"%s\">\n" conf.charset;
  Wserver.printf
    "  <link rel=\"shortcut icon\" href=\"%s/favicon_gwd.png\">\n"
    (Util.image_prefix conf);
  Wserver.printf
    "  <link rel=\"apple-touch-icon\" href=\"%s/favicon_gwd.png\">\n"
    (Util.image_prefix conf);
  Wserver.printf "  <meta name=\"viewport\" content=\"width=device-width, \
                    initial-scale=1, shrink-to-fit=no\">\n";
  match Util.open_templ conf "css" with
  [ Some ic -> Templ.copy_from_templ conf [] ic
  | None -> () ];
  Templ.include_hed_trl conf "hed";
  Wserver.printf "\n</head>\n";
  let s =
    try " dir=\"" ^ Hashtbl.find conf.lexicon " !dir" ^ "\"" with
    [ Not_found -> "" ]
  in
  let s = s ^ Util.body_prop conf in Wserver.printf "<body%s>\n" s;
  Util.message_to_wizard conf;
};

value header_without_page_title conf title = do {
  Util.html conf;
  header_without_http conf title;
  (* balancing </div> in gen_trailer *)
  Wserver.printf "<div class=\"container\">";
};

value header_link_welcome conf title = do {
  header_without_page_title conf title;
  print_link_to_welcome conf True;
  Wserver.printf "<h1>";
  title False;
  Wserver.printf "</h1>\n";
};

value header_no_page_title conf title = do {
  header_without_page_title conf title;
  match Util.p_getenv conf.env "title" with
  [ None | Some "" -> ()
  | Some x -> Wserver.printf "<h1>%s</h1>\n" x ];
};

value header conf title = do {
  header_without_page_title conf title;
  Wserver.printf "\n<h1>";
  title False;
  Wserver.printf "</h1>\n";
};

value header_fluid conf title = do {
  header_without_http conf title;
  (* balancing </div> in gen_trailer *)
  Wserver.printf "<div class=\"container-fluid\">";
  Wserver.printf "\n<h1>";
  title False;
  Wserver.printf "</h1>\n";
};

value red_color = "red";

value rheader conf title = do {
  header_without_page_title conf title;
  Wserver.printf "<h1 class=\"error\">";
  title False;
  Wserver.printf "</h1>\n";
};

value gen_trailer with_logo conf = do {
  let conf = {(conf) with is_printed_by_template = False} in
  Templ.include_hed_trl conf "trl";
  if with_logo then Templ.print_copyright_with_logo conf
  else Templ.print_copyright conf;
  Wserver.printf "</div>\n"; (* balances header_without_http *)
  match Util.open_templ conf "js" with
  [ Some ic -> Templ.copy_from_templ conf [] ic
  | None -> () ];
  Wserver.printf "</body>\n</html>\n";
};

value trailer = gen_trailer True;

value incorrect_request conf = do {
  let title _ =
    Wserver.printf "%s" (Util.capitale (Util.transl conf "incorrect request"))
  in
  Wserver.http HttpStatus.Bad_Request;
  header conf title;
  Wserver.printf "<p>\n";
  print_link_to_welcome conf False;
  Wserver.printf "</p>\n";
  trailer conf
};

value error_cannot_access conf fname = do {
  let title _ = Wserver.printf "Error" in
  header conf title;
  tag "ul" begin
    tag "li" begin
      Wserver.printf "Cannot access file \"%s.txt\".\n"
        fname;
    end;
  end;
  trailer conf;
};

value gen_interp header conf fname ifun env ep = do {
  let v = Templ.template_file.val in
  Templ.template_file.val := fname;
  try
    match Templ.input_templ conf fname with
    [ Some astl -> do {
        if header then do {
          Util.html conf;
        }
        else ();
        Templ.interp_ast conf ifun env ep astl
      }
    | None ->
        error_cannot_access conf fname ]
  with e ->
    do { Templ.template_file.val := v; raise e };
  Templ.template_file.val := v;
};

value interp_no_header conf fname ifun env ep =
  gen_interp False conf fname ifun env ep;

value interp conf fname ifun env ep =
  gen_interp True conf fname ifun env ep;

