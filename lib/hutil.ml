(* Copyright (c) 2007 INRIA *)

open Config
open Def

type 'a value = Vint of int | Vother of 'a | Vnone

let get_env v env = try Templ.Env.find v env with Not_found -> Vnone
let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x

let incorrect_request ?(comment = "") conf =
  GWPARAM.output_error conf Def.Bad_Request ~content:(Adef.safe comment)

let error_cannot_access conf fname =
  GWPARAM.output_error conf Def.Not_Found
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
  with _ -> error_cannot_access conf "home"

let link_to_referer conf =
  let referer = Util.get_referer conf in
  let back = Utf8.capitalize_fst (Util.transl conf "back") in
  if (referer :> string) <> "" then
    ({|<a href="|} ^<^ referer
     ^>^ {|" class="btn btn-sm btn-link p-0 border-0" title="|} ^ back
     ^ {|"><i class="fa fa-arrow-left-long fa-fw fa-sm"></i></a>|}
      :> Adef.safe_string)
  else Adef.safe ""

(* S: use Util.include_template for "hed"? *)

let header_without_http_nor_home conf title =
  let robot = List.assoc_opt "robot_index" conf.base_env = Some "yes" in
  let str1 =
    Printf.sprintf {|<!DOCTYPE html>
<html lang="%s">
<head>
<title>|} conf.lang
  in
  let str2 =
    Printf.sprintf
      {|</title>
%s
<meta charset="%s">
<meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
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
    (if fluid then "<div class=\"container-fluid\">"
     else "<div class=\"container\">")

let header_with_title ?(error = false) ?(fluid = false) conf title =
  let fluid = fluid || is_fluid conf in
  Util.html conf;
  header_without_http_nor_home conf title;
  include_home_template conf;
  (* balancing </div> in gen_trailer *)
  Output.print_sstring conf
    (if fluid then "<div class=\"container-fluid\">"
     else "<div class=\"container\">");
  Output.print_sstring conf (if error then "<h1 class = \"error\">" else "<h1>");
  title false;
  Output.print_sstring conf "</h1>\n"

let header_fluid conf title = header_with_title ~fluid:true conf title

(* when the use of home.txt is not available *)
let header_without_home conf title =
  let fluid = is_fluid conf in
  Util.html conf;
  header_without_http_nor_home conf title;
  (* balancing </div> in gen_trailer *)
  Output.print_sstring conf
    (if fluid then "<div class=\"container-fluid\">"
     else "<div class=\"container\">");
  Output.print_sstring conf "<h1>";
  title false;
  Output.print_sstring conf "</h1>\n"

let header_with_conf_title conf _title =
  (* title is supplied bt conf.env *)
  let title _ =
    match Util.p_getenv conf.env "p_title" with
    | None | Some "" -> ()
    | Some s -> Output.printf conf "<h1>%s</h1>\n" s
  in
  header_with_title conf title

let header ?(error = false) ?(fluid = false) conf title =
  header_with_title ~error ~fluid conf title

(* TODO replace rheader by header ~error:true *)
let rheader conf title = header_with_title ~error:true conf title

let trailer conf =
  let conf = { conf with is_printed_by_template = false } in
  Templ.output_simple conf Templ.Env.empty "trl";
  Templ.output_simple conf Templ.Env.empty "copyr";
  Templ.output_simple conf Templ.Env.empty "js";
  let query_time = Unix.gettimeofday () -. conf.query_start in
  Util.time_debug conf query_time !GWPARAM.nb_errors !GWPARAM.errors_undef
    !GWPARAM.errors_other !GWPARAM.set_vars;
  Output.print_sstring conf "</body>\n</html>\n"

(* Calendar request *)

let eval_julian_day conf =
  let open Adef in
  let getint v = match Util.p_getint conf.env v with Some x -> x | _ -> 0 in
  List.fold_left
    (fun d (var, cal, conv, max_month) ->
      let yy =
        match Util.p_getenv conf.env ("y" ^ var) with
        | Some v -> (
            try
              let len = String.length v in
              if cal = Djulian && len > 2 && v.[len - 2] = '/' then
                int_of_string (String.sub v 0 (len - 2)) + 1
              else int_of_string v
            with Failure _ -> 0)
        | None -> 0
      in
      let mm = getint ("m" ^ var) in
      let dd = getint ("d" ^ var) in
      let dt = { day = dd; month = mm; year = yy; prec = Sure; delta = 0 } in
      match Util.p_getenv conf.env ("t" ^ var) with
      | Some _ -> conv dt
      | None -> (
          match
            ( Util.p_getenv conf.env ("y" ^ var ^ "1"),
              Util.p_getenv conf.env ("y" ^ var ^ "2"),
              Util.p_getenv conf.env ("m" ^ var ^ "1"),
              Util.p_getenv conf.env ("m" ^ var ^ "2"),
              Util.p_getenv conf.env ("d" ^ var ^ "1"),
              Util.p_getenv conf.env ("d" ^ var ^ "2") )
          with
          | Some _, _, _, _, _, _ -> conv { dt with year = yy - 1 }
          | _, Some _, _, _, _, _ -> conv { dt with year = yy + 1 }
          | _, _, Some _, _, _, _ ->
              let yy, mm =
                if mm = 1 then (yy - 1, max_month) else (yy, mm - 1)
              in
              conv { dt with year = yy; month = mm }
          | _, _, _, Some _, _, _ ->
              let yy, mm =
                if mm = max_month then (yy + 1, 1) else (yy, mm + 1)
              in
              let r = conv { dt with year = yy; month = mm } in
              if r = conv dt then
                let yy, mm =
                  if mm = max_month then (yy + 1, 1) else (yy, mm + 1)
                in
                conv { dt with year = yy; month = mm }
              else r
          | _, _, _, _, Some _, _ -> conv { dt with day = dd - 1 }
          | _, _, _, _, _, Some _ -> conv { dt with day = dd + 1 }
          | _ -> d))
    (Calendar.sdn_of_gregorian conf.today)
    [
      ("g", Dgregorian, Calendar.sdn_of_gregorian, 12);
      ("j", Djulian, Calendar.sdn_of_julian, 12);
      ("f", Dfrench, Calendar.sdn_of_french, 13);
      ("h", Dhebrew, Calendar.sdn_of_hebrew, 13);
    ]

(* *)

let eval_var conf env jd _loc =
  let module Ast = Geneweb_templ.Ast in
  function
  | [ "integer" ] -> (
      match get_env "integer" env with
      | Vint i -> Templ.VVstring (string_of_int i)
      | _ -> raise Not_found)
  | "date" :: sl -> Templ.eval_date_var conf jd sl
  | "today" :: sl ->
      Templ.eval_date_var conf (Calendar.sdn_of_gregorian conf.today) sl
  | _ -> raise Not_found

let print_foreach print_ast eval_expr =
  let eval_int_expr env jd e =
    let s = eval_expr env jd e in
    try int_of_string s with Failure _ -> raise Not_found
  in
  let rec print_foreach env jd _loc s sl el al =
    match (s, sl) with
    | "integer_range", [] -> print_integer_range env jd el al
    | _ -> raise Not_found
  and print_integer_range env jd el al =
    let i1, i2 =
      match el with
      | [ [ e1 ]; [ e2 ] ] -> (eval_int_expr env jd e1, eval_int_expr env jd e2)
      | _ -> raise Not_found
    in
    for i = i1 to i2 do
      let env = Templ.Env.add "integer" (Vint i) env in
      List.iter (print_ast env jd) al
    done
  in
  print_foreach

let print_calendar conf _base =
  let ifun =
    Templ.
      {
        eval_var = eval_var conf;
        eval_transl = (fun _ -> Templ.eval_transl conf);
        eval_predefined_apply = (fun _ -> raise Not_found);
        get_vother;
        set_vother;
        print_foreach;
      }
  in
  Templ.output conf ifun Templ.Env.empty (eval_julian_day conf) "calendar"
