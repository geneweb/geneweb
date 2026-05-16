(* Copyright (c) 2007 INRIA *)

open Config
open Def
module Code = Geneweb_http.Code

let get_vother x = x
let set_vother x = Some x

let incorrect_request ?(comment = "") conf =
  GWPARAM.output_error conf Code.Bad_Request ~content:(Adef.safe comment)

let error_cannot_access conf fname =
  GWPARAM.output_error conf Code.Not_Found
    ~content:
      ("Cannot access file \""
      ^<^ (Util.escape_html fname : Adef.escaped_string :> Adef.safe_string)
      ^>^ ".txt\".")

let include_home_template conf =
  let ifun =
    Templ.
      {
        eval_var = (fun _ -> raise Not_found);
        eval_transl = (fun _ -> Templ.eval_transl conf);
        eval_predefined_apply = (fun _ -> raise Not_found);
        get_vother;
        set_vother;
        print_foreach = (fun _ -> raise Not_found);
      }
  in
  try Templ.output conf ifun Templ.Env.empty () "home"
  with Sys_error _ | Not_found -> error_cannot_access conf "home"

let link_to_referer conf =
  let referer = Util.get_referer conf in
  let back = Utf8.capitalize_fst (Util.transl conf "back") in
  (* Validate the scheme to block javascript: URIs that survive escape_html
     and could be used as XSS vectors in an href attribute. *)
  let referer_s = (referer :> string) in
  let safe_scheme =
    Util.starts_with referer_s "http://"
    || Util.starts_with referer_s "https://"
  in
  if referer_s <> "" && safe_scheme then
    ({|<a href="|} ^<^ referer
     ^>^ {|" class="btn btn-sm btn-link p-0 border-0" title="|} ^ back
     ^ {|"><i class="fa fa-arrow-left-long fa-fw fa-sm"></i></a>|}
      :> Adef.safe_string)
  else Adef.safe ""

let header_without_http_nor_home conf title =
  let robot = List.assoc_opt "robot_index" conf.base_env = Some "yes" in
  (* conf.lang comes from the URL; escape it to prevent attribute injection
     (e.g. lang="fr" onload="..."). In practice lang is validated as a known
     language code, but the injection site itself should be safe regardless. *)
  let str1 =
    Printf.sprintf {|<!DOCTYPE html>
<html lang="%s">
<head>
<title>|}
      (Util.escape_html conf.lang :> string)
  in
  let str2 =
    Printf.sprintf
      {|</title>
%s
<meta charset="%s">
<meta name="viewport" content="width=device-width, initial-scale=1">
<link rel="shortcut icon" href="%s/favicon_gwd.png">
<link rel="apple-touch-icon" href="%s/favicon_gwd.png">
|}
      (if robot then {|<meta name="robots" content="index,follow">|}
       else {|<meta name="robots" content="none">|})
      conf.charset
      (Util.images_prefix conf :> string)
      (Util.images_prefix conf :> string)
  in
  Output.print_sstring conf str1;
  title true;
  Output.print_sstring conf str2;
  Output.print_string conf (Permalink.script conf (Permalink.query_aux conf));
  Templ.output_simple conf Templ.Env.empty "css";
  Output.print_sstring conf "</head>\n";
  let s =
    try " dir=\"" ^ Hashtbl.find conf.lexicon "!dir" ^ "\""
    with Not_found -> ""
  in
  let s = s ^ Util.body_prop conf in
  Output.printf conf "<body%s>\n" s;
  Templ.output_simple conf Templ.Env.empty "hed";
  Util.message_to_wizard conf

let is_fluid conf =
  (try List.assoc "wide" conf.env = Adef.encoded "on" with Not_found -> false)
  || try List.assoc "wide" conf.base_env = "on" with Not_found -> false

let header_without_title conf =
  let fluid = is_fluid conf in
  Util.html conf;
  header_without_http_nor_home conf (fun _ -> ());
  include_home_template conf;
  Output.print_sstring conf
    (if fluid then "<div class=\"container-fluid mx-3\">\n"
     else "<div class=\"container\">\n")

let header_with_title ?(error = false) ?(fluid = false) conf title =
  let fluid = fluid || is_fluid conf in
  Util.html conf;
  header_without_http_nor_home conf title;
  include_home_template conf;
  Output.print_sstring conf
    (if fluid then "<div class=\"container-fluid mx-3\">\n"
     else "<div class=\"container\">\n");
  Output.print_sstring conf (if error then "<h1 class=\"error\">" else "<h1>");
  title false;
  Output.print_sstring conf "</h1>\n"

let header_with_adaptive_title ?(fluid = false) conf title_content =
  (* Note: title_content is injected directly into HTML. Callers are
     responsible for ensuring it is already safe (escaped or trusted). Unlike
     other header functions that receive a printing callback, this one accepts
     a pre-rendered string, so there is no type-level guarantee of safety. *)
  let fluid = fluid || is_fluid conf in
  Util.html conf;
  header_without_http_nor_home conf (fun _ -> ());
  include_home_template conf;
  Output.print_sstring conf
    (if fluid then "<div class=\"container-fluid mx-3\">\n"
     else "<div class=\"container\">\n");
  let title_length = String.length title_content in
  let header_class =
    if title_length > 2000 then "h5"
    else if title_length > 1000 then "h4"
    else if title_length > 500 then "h3"
    else ""
  in
  if header_class = "" then Output.printf conf "<h1>%s</h1>" title_content
  else Output.printf conf {|<h1 class="%s">%s</h1>|} header_class title_content

let header_fluid conf title = header_with_title ~fluid:true conf title

let header_without_home conf title =
  let fluid = is_fluid conf in
  Util.html conf;
  header_without_http_nor_home conf title;
  Output.print_sstring conf
    (if fluid then "<div class=\"container-fluid mx-3\">\n"
     else "<div class=\"container\">\n");
  Output.print_sstring conf "<h1>";
  title false;
  Output.print_sstring conf "</h1>\n"

let header_with_conf_title conf _title =
  let title _ =
    match Util.p_getenv conf.env "p_title" with
    | None | Some "" -> ()
    | Some s -> Output.printf conf "<h1>%s</h1>\n" s
  in
  header_with_title conf title

let header ?(error = false) ?(fluid = false) conf title =
  header_with_title ~error ~fluid conf title

let rheader conf title = header_with_title ~error:true conf title

let trailer conf =
  let conf = { conf with is_printed_by_template = false } in
  Templ.output_simple conf Templ.Env.empty "trl";
  Templ.output_simple conf Templ.Env.empty "copyr";
  Output.print_sstring conf "</div>\n";
  Templ.output_simple conf Templ.Env.empty "js";
  let query_time = Unix.gettimeofday () -. conf.query_start in
  Util.time_debug conf query_time !GWPARAM.nb_errors !GWPARAM.errors_undef
    !GWPARAM.errors_other !GWPARAM.set_vars;
  Output.print_sstring conf "</body>\n</html>\n"

let trailer_with_extra_js conf extra_js_sources =
  let conf = { conf with is_printed_by_template = false } in
  Templ.output_simple conf Templ.Env.empty "trl";
  Templ.output_simple conf Templ.Env.empty "copyr";
  Output.print_sstring conf "</div>\n";
  (if extra_js_sources <> "" then
     let sources = String.split_on_char '|' extra_js_sources in
     List.iter
       (fun source ->
         let source = String.trim source in
         if source <> "" then
           if Filename.check_suffix source ".js" then
             let js_path =
               Filename.concat conf.etc_prefix (Filename.concat "js" source)
             in
             match Util.open_etc_file conf js_path with
             | Some (ic, fname) -> (
                 close_in ic;
                 match Util.hash_file_cached fname with
                 | Some hash ->
                     Output.printf conf
                       {|<script src="js/%s?hash=%s"></script>|} source hash
                 | None ->
                     let prefix =
                       let p = conf.etc_prefix in
                       if p = "" || p.[String.length p - 1] = '/' then p
                       else p ^ "/"
                     in
                     Output.printf conf {|<script src="%sjs/%s"></script>|}
                       prefix source)
             | None -> ()
           else
             (* First check the template is accessible, then emit the <script>
                wrapper. This avoids an unclosed <script> tag when the template
                is missing, which would cause the browser to interpret the rest
                of the HTML page as script content. *)
             match Util.open_etc_file conf source with
             | None -> ()
             | Some (ic, _) ->
                 close_in ic;
                 Output.print_sstring conf "<script>\n";
                 (try Templ.output_simple conf Templ.Env.empty source
                  with Sys_error _ | Not_found -> ());
                 Output.print_sstring conf "\n</script>\n")
       sources);
  Templ.output_simple conf Templ.Env.empty "js";
  let query_time = Unix.gettimeofday () -. conf.query_start in
  Util.time_debug conf query_time !GWPARAM.nb_errors !GWPARAM.errors_undef
    !GWPARAM.errors_other !GWPARAM.set_vars;
  Output.print_sstring conf "</body>\n</html>\n"
