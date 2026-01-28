(* Copyright (c) 2007 INRIA *)

type meta = { name : string; content : string }

let link_to_referer conf =
  let referer = Util.get_referer conf in
  let back = Utf8.capitalize_fst (Util.transl conf "back") in
  match referer with
  | Some referer ->
      let open Def in
      ({|<a href="|} ^<^ referer
       ^>^ {|"><span class="fa fa-arrow-left fa-lg" title="|} ^ back
       ^ {|"></span></a>|}
        :> Adef.safe_string)
  | None -> Adef.safe ""

let gen_print_link_to_welcome f conf right_aligned =
  if right_aligned then
    Output.printf conf "<div style=\"float:%s\">\n" conf.right
  else Output.print_sstring conf "<p>\n";
  f ();
  let str = link_to_referer conf in
  if (str :> string) <> "" then Output.print_string conf str;
  Output.print_sstring conf {|<a href="|};
  Output.print_string conf (Util.commd ~senv:false conf);
  Output.print_sstring conf {|"><img src="|};
  Output.print_string conf (Image.prefix conf);
  Output.print_sstring conf {|/up.png" alt="^^" title="^^"></a>|};
  if right_aligned then Output.print_sstring conf "</div>"
  else Output.print_sstring conf "</p>"

let print_link_to_welcome = gen_print_link_to_welcome (fun () -> ())

(* S: use Util.include_template for "hed"? *)
let header_without_http conf title ?meta () =
  Output.print_sstring conf "<!DOCTYPE html><head><title>";
  title true;
  Output.print_sstring conf "</title><meta name=\"robots\" content=\"none\">";
  Output.print_sstring conf "<meta charset=\"";
  Output.print_sstring conf conf.charset;
  Output.print_sstring conf "\">";
  Output.print_sstring conf {|<link rel="shortcut icon" href="|};
  Output.print_string conf (Image.prefix conf);
  Output.print_sstring conf {|/favicon_gwd.png">|};
  Output.print_sstring conf {|<link rel="apple-touch-icon" href="|};
  Output.print_string conf (Image.prefix conf);
  Output.print_sstring conf {|/favicon_gwd.png">|};
  Output.print_sstring conf
    {|<meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">|};
  Option.iter
    (List.iter (fun { name; content } ->
         Output.printf conf "<meta name=\"%s\" content=\"%s\">" name content))
    meta;
  Util.include_template conf [] "css" (fun () -> ());
  (match Util.open_etc_file "hed" with
  | Some (ic, _) -> Templ.copy_from_templ conf [] ic
  | None -> ());
  Output.print_sstring conf "\n</head>\n";
  let s =
    Option.fold
      (Hashtbl.find_opt conf.lexicon "!dir")
      ~some:(fun d -> " dir=\"" ^ d ^ "\"")
      ~none:""
  in
  let s = s ^ Util.body_prop conf in
  Output.printf conf "<body%s>\n" s;
  Util.message_to_wizard conf

let header_without_page_title conf title ?(meta = []) () =
  Util.html conf;
  header_without_http conf title ~meta ()

let header_link_welcome conf title =
  header_without_page_title conf title ();
  print_link_to_welcome conf true;
  Output.print_sstring conf "<h1>";
  title false;
  Output.print_sstring conf "</h1>\n"

let header_no_page_title conf title =
  header_without_page_title conf title ();
  match Util.p_getenv conf.env "title" with
  | None | Some "" -> ()
  | Some x -> Output.printf conf "<h1>%s</h1>\n" x

let header conf title ?(meta = []) () =
  header_without_page_title conf title ~meta ();
  Output.print_sstring conf "\n<h1>";
  title false;
  Output.print_sstring conf "</h1>\n"

let header_with_meta conf title meta = header conf title ~meta ()
let header conf title = header conf title ()

let header_fluid conf title =
  header_without_http conf title ();
  Output.print_sstring conf "<h1>";
  title false;
  Output.print_sstring conf "</h1>\n"

let rheader conf title =
  header_without_page_title conf title ();
  Output.print_sstring conf "<h1 class=\"error\">";
  title false;
  Output.print_sstring conf "</h1>\n"

let trailer conf =
  let conf = { conf with Config.is_printed_by_template = false } in
  Output.print_sstring conf "</body>\n</html>\n"

let () =
  GWPARAM.set_wrap_output (fun conf title content ->
      header conf (fun _ -> Output.print_string conf title);
      content ();
      trailer conf)

let incorrect_request conf = GWPARAM.output_error conf Def.Bad_Request

let error_cannot_access conf fname =
  let open Def in
  GWPARAM.output_error conf Def.Not_Found
    ~content:
      ("Cannot access file \""
      ^<^ (Util.escape_html fname : Adef.escaped_string :> Adef.safe_string)
      ^>^ ".txt\".")

let gen_interp header conf fname ifun env ep =
  Templ_parser.wrap fname (fun () ->
      match Templ.input_templ conf fname with
      | Some astl ->
          if header then Util.html conf;
          let full_name = Util.etc_file_name fname in
          Templ.interp_ast conf ifun env ep [ Ainclude (full_name, astl) ]
      | None -> error_cannot_access conf fname)

let interp_no_header conf fname ifun env ep =
  gen_interp false conf fname ifun env ep

let interp conf fname ifun env ep = gen_interp true conf fname ifun env ep
