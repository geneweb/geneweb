open Config
open Util

let esc s = (Util.escape_html s :> string)
let enc s = (Mutil.encode s :> string)
let page_sizes = [ 25; 50; 100; 200; 500; 1000 ]

let page_size ?(gwf_key = "terms_page_size") conf =
  let ok n = n >= 1 && n <= 5000 in
  match p_getint conf.env "n" with
  | Some n when ok n -> n
  | Some _ | None -> (
      match
        Option.bind (List.assoc_opt gwf_key conf.base_env) int_of_string_opt
      with
      | Some n when ok n -> n
      | Some _ | None -> 100)

let slice ?gwf_key conf total =
  let from =
    match p_getint conf.env "from" with
    | Some f when f >= 1 && f <= total -> f
    | Some _ | None -> 1
  in
  (from, min total (from + page_size ?gwf_key conf - 1))

let take (from, upto) l = List.filteri (fun i _ -> i >= from - 1 && i < upto) l

(* not [b], the name of the base in some URLs *)
let begins_with conf =
  match p_getenv_notrim conf.env "bw" with
  | Some b when b <> "" -> Some b
  | Some _ | None -> None

(* [s] lowercased and unaccented, its spaces and punctuation kept (unlike
   [Name.lower]): "33 " and "33," are different beginnings *)
let fold_case s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec loop i =
    if i < n then (
      if Char.code s.[i] < 0x80 then (
        Buffer.add_char b (Char.lowercase_ascii s.[i]);
        loop (i + 1))
      else
        let t, j = Name.unaccent_utf_8 true s i in
        Buffer.add_string b t;
        loop j)
  in
  loop 0;
  Buffer.contents b

let begins_ok conf =
  match begins_with conf with
  | None -> fun _ -> true
  | Some b ->
      let b = fold_case b in
      fun s -> String.starts_with ~prefix:b (fold_case s)

let print_list_bar ?gwf_key ?(begins = true) conf ~href ~hidden counts =
  let n = page_size ?gwf_key conf in
  let bw = begins_with conf in
  let url ?(bw = bw) from =
    Printf.sprintf "%s%s&n=%d&from=%d" href
      (match bw with Some b -> "&bw=" ^ enc b | None -> "")
      n from
  in
  let button label href =
    Printf.sprintf
      "<a class=\"btn btn-sm btn-outline-primary\" href=\"%s\">%s</a>" href
      label
  in
  Output.printf conf
    "<form method=\"get\" action=\"%s\" class=\"d-flex flex-wrap \
     align-items-center gap-2 my-2\">\n"
    conf.command;
  Util.hidden_env conf;
  hidden ();
  if begins then
    Output.printf conf
      "<label for=\"bw\" class=\"text-nowrap\">%s</label>\n\
       <input type=\"text\" class=\"form-control form-control-sm w-auto\" \
       id=\"bw\" name=\"bw\" value=\"%s\" size=\"20\">\n\
       <button type=\"submit\" class=\"btn btn-sm btn-outline-secondary\"><i \
       class=\"fa fa-magnifying-glass\"></i></button>\n"
      (esc (Utf8.capitalize_fst (transl conf "starting with")))
      (esc (Option.value ~default:"" bw));
  if begins && bw <> None then
    Output.print_sstring conf
      (button "<i class=\"fa fa-xmark\"></i>" (url ~bw:None 1));
  match counts with
  | None ->
      Output.print_sstring conf
        (button
           (esc (Utf8.capitalize_fst (transl conf "all")))
           (Printf.sprintf "%s&all=on&n=%d" href n));
      Output.print_sstring conf "</form>\n"
  | Some (total, (from, upto)) ->
      Output.printf conf
        "<select class=\"form-select form-select-sm w-auto ms-3 auto-submit\" \
         name=\"n\" aria-label=\"%s\" title=\"%s\">\n"
        (esc (Utf8.capitalize_fst (transl conf "rows per page")))
        (esc (Utf8.capitalize_fst (transl conf "rows per page")));
      List.iter
        (fun k ->
          Output.printf conf "<option value=\"%d\"%s>%d</option>\n" k
            (if k = n then " selected" else "")
            k)
        (List.sort_uniq compare (n :: page_sizes));
      Output.print_sstring conf "</select>\n";
      if from > 1 then
        Output.print_sstring conf
          (button
             (esc (Utf8.capitalize_fst (transl conf "previous")))
             (url (max 1 (from - n))));
      Output.printf conf "<span class=\"text-nowrap\">%s</span>\n"
        (if total = 0 then "0" else Printf.sprintf "%d–%d / %d" from upto total);
      if upto < total then
        Output.print_sstring conf
          (button
             (esc (Utf8.capitalize_fst (transl conf "next")))
             (url (upto + 1)));
      Output.print_sstring conf "</form>\n"
