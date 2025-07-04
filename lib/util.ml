(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
module Logs = Geneweb_logs.Logs
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let is_welcome = ref false
let p_getenv env label = Option.map Mutil.decode (List.assoc_opt label env)

let print_default_gwf_file bname =
  let gwf =
    [
      "access_by_key=yes";
      "disable_forum=yes";
      "hide_private_names=no";
      "use_restrict=no";
      "show_consang=yes";
      "display_sosa=yes";
      "place_surname_link_to_ind=yes";
      "max_anc_level=8";
      "max_anc_tree=7";
      "max_desc_level=12";
      "max_desc_tree=4";
      "max_cousins=2000";
      "max_cousins_level=5";
      "latest_event=20";
      "template=*";
      "long_date=no";
      "counter=no";
      "full_siblings=yes";
      "hide_advanced_request=no";
      "p_mod=";
    ]
  in
  let config_d = !GWPARAM.config_d bname in
  let fname = !GWPARAM.config bname in
  if not (Sys.file_exists fname) then
    try
      if not (Sys.file_exists config_d) then Unix.mkdir config_d 0o755;
      if bname = "" || Sys.file_exists fname then ()
      else
        let oc = open_out fname in
        List.iter (fun s -> Printf.fprintf oc "%s\n" s) gwf;
        close_out oc
    with Unix.Unix_error (_, _, _) ->
      Logs.syslog `LOG_WARNING
        (Printf.sprintf "Error while creating %s or %s\n" config_d fname)

let rec cut_at_equal i s =
  if i = String.length s then (s, "")
  else if s.[i] = '=' then
    (String.sub s 0 i, String.sub s (succ i) (String.length s - succ i))
  else cut_at_equal (succ i) s

let read_base_env bname gw_prefix debug =
  let load_file fname =
    try
      let ic = Secure.open_in fname in
      let env =
        let rec loop env =
          match input_line ic with
          | s ->
              let s = Mutil.strip_all_trailing_spaces s in
              if s = "" || s.[0] = '#' then loop env
              else loop (cut_at_equal 0 s :: env)
          | exception End_of_file -> env
        in
        loop []
      in
      close_in ic;
      List.rev env
    with Sys_error error ->
      Logs.syslog `LOG_WARNING
        (Printf.sprintf "Error %s while loading %s, using empty config\n%!"
           error fname);
      []
  in
  let fname1 = !GWPARAM.config bname in
  if Sys.file_exists fname1 then load_file fname1
  else
    let fname2 = Filename.concat gw_prefix "a.gwf" in
    if Sys.file_exists fname2 then (
      if debug then
        Logs.syslog `LOG_WARNING
          (Printf.sprintf "Using configuration from %s\n%!" fname2);
      load_file fname2)
    else (
      if debug then
        Logs.syslog `LOG_WARNING
          (Printf.sprintf "No config file found in either %s or %s\n%!" fname1
             fname2);
      [])

let time_debug conf query_time nb_errors errors_undef errors_other set_vars =
  (*Printf.eprintf "Errors set_vars:\n";
    List.iter (fun e -> Printf.eprintf "%s\n" e) set_vars;*)
  let errors_undef = List.sort_uniq compare errors_undef in
  let errors_undef =
    List.filter
      (fun e -> not (List.exists (fun s -> Mutil.contains e s) set_vars))
      errors_undef
  in
  let nb_errors =
    nb_errors + List.length errors_undef + List.length errors_other
  in
  let err_list1 = String.concat "," errors_undef in
  let err_list2 = String.concat "," errors_other in
  match
    (List.assoc_opt "hide_querytime_bugs" conf.base_env, conf.predictable_mode)
  with
  | _, true | Some "yes", _ -> ()
  | _, _ ->
      Output.print_sstring conf
        (Printf.sprintf
           {|<script>
  var q_time = %.3f;
  var nb_errors = %d;
  var errors_list = "\u{000A}%s%s";
  var home_time = document.getElementById('q_time');
  var home_errors = document.getElementById('nb_errors');
  if (home_time != null) {
    home_time.title = "Query treated in " + q_time + " s";
    if (q_time < 3) {
      home_time.classList.add("text-success");
    } else if (q_time < 8) {
      home_time.classList.add("text-warning");
     } else {
       home_time.classList.add("text-danger");
    }
  }
  if (home_errors != null) {
    if (nb_errors > 0) {
      home_errors.title = nb_errors +" error(s)!";
      home_errors.classList.remove("d-none");
    }
    if (errors_list != "\u{000A}") {
      home_errors.title = home_errors.title + errors_list + ".";
    }
  }
</script>|}
           query_time nb_errors
           (if errors_undef <> [] then
              Printf.sprintf "Unbound variable(s): %s. " err_list1
            else "")
           err_list2)

let escape_aux count blit str =
  let strlen = String.length str in
  let rec loop acc i =
    if i < strlen then loop (acc + count (String.unsafe_get str i)) (i + 1)
    else if acc = strlen then str
    else
      let buf = Bytes.create acc in
      let rec loop istr ibuf =
        if istr = strlen then Bytes.unsafe_to_string buf
        else blit buf ibuf istr loop (String.unsafe_get str istr)
      in
      loop 0 0
  in
  loop 0 0

(** [escape_html str] replaces '&', '"', '<' and '>' with their corresponding
    character entities (using entity number) *)
let escape_html s : Adef.escaped_string =
  escape_aux
    (function '&' | '"' | '\'' | '<' | '>' -> 5 (* "&#xx;" *) | _ -> 1)
    (fun buf ibuf istr loop -> function
      | '&' ->
          Bytes.blit_string "&#38;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | '"' ->
          Bytes.blit_string "&#34;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | '\'' ->
          Bytes.blit_string "&#39;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | '<' ->
          Bytes.blit_string "&#60;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | '>' ->
          Bytes.blit_string "&#62;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | c ->
          Bytes.unsafe_set buf ibuf c;
          loop (istr + 1) (ibuf + 1))
    s
  |> Adef.escaped

let esc x = (escape_html x :> Adef.safe_string)

(** [escape_attribute str] only escapes double quote and ampersand. Since we
    will return normalized HTML, ['"' and '\''] should be the only dangerous
    character here. *)
let escape_attribute =
  escape_aux
    (function '&' | '"' | '\'' -> 5 (* "&#xx;" *) | _ -> 1)
    (fun buf ibuf istr loop -> function
      | '&' ->
          Bytes.blit_string "&#38;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | '"' ->
          Bytes.blit_string "&#34;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | '\'' ->
          Bytes.blit_string "&#39;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | c ->
          Bytes.unsafe_set buf ibuf c;
          loop (istr + 1) (ibuf + 1))

let is_hide_names conf p =
  if conf.hide_names || Driver.get_access p = Private then true else false

let search_in_path p s =
  let rec loop = function
    | d :: dl ->
        let f = Filename.concat d s in
        if Sys.file_exists f then f else loop dl
    | [] -> s
  in
  loop (p ())

let search_in_assets = search_in_path Secure.assets

let hash_file file =
  try Some (Digest.to_hex (Digest.file file)) with Sys_error _ -> None

let hash_cache = Hashtbl.create 100

let hash_file_cached file =
  try
    let stats = Unix.stat file in
    let mtime = stats.st_mtime in
    match Hashtbl.find hash_cache file with
    | cached_mtime, hash when Float.equal cached_mtime mtime -> Some hash
    | _ -> (
        match hash_file file with
        | Some hash ->
            Hashtbl.replace hash_cache file (mtime, hash);
            Some hash
        | None -> None)
    | exception Not_found -> (
        match hash_file file with
        | Some hash ->
            Hashtbl.replace hash_cache file (mtime, hash);
            Some hash
        | None -> None)
  with Unix.Unix_error _ -> None

(* Internationalization *)

let start_with s i p =
  i + String.length p <= String.length s
  && String.lowercase_ascii (String.sub s i (String.length p)) = p

let start_with_vowel conf s =
  if String.length s > 0 then
    let s, _ = Name.unaccent_utf_8 true s 0 in
    List.mem s conf.vowels
  else false

type ('a, 'b) format2 = ('a, unit, string, 'b) format4

let fcapitale (a : ('a, 'b, 'c, 'd) format4) : ('a, 'b, 'c, 'd) format4 =
  Scanf.format_from_string (Utf8.capitalize_fst (string_of_format a)) a

let nth_field_abs w n =
  let rec start i n =
    if n = 0 then i
    else if i < String.length w then
      match w.[i] with
      | '<' -> start (i + 2) n
      | '/' -> start (i + 1) (n - 1)
      | _ -> start (i + 1) n
    else i
  in
  let rec stop i =
    if i < String.length w then
      match w.[i] with '<' -> stop (i + 2) | '/' -> i | _ -> stop (i + 1)
    else i
  in
  let i1 = start 0 n in
  let i2 = stop i1 in
  (i1, i2)

let nth_field w n =
  let i1, i2 = nth_field_abs w n in
  let i1, i2 = if i2 = i1 then nth_field_abs w 0 else (i1, i2) in
  String.sub w i1 (i2 - i1)

let tnf s = "[" ^ s ^ "]"
let transl conf w = try Hashtbl.find conf.lexicon w with Not_found -> tnf w

let transl_nth conf w n =
  let len = String.length w in
  let w =
    if len > 3 && w.[len - 1] = ':' && w.[len - 2] = ':' && w.[len - 3] = ':'
    then String.sub w 0 (len - 3)
    else w
  in
  try nth_field (Hashtbl.find conf.lexicon w) n
  with Not_found -> tnf (nth_field w n)

let gen_decline_basic wt s =
  let s1 = if s = "" then "" else if wt = "" then s else " " ^ s in
  let len = String.length wt in
  if len >= 3 && wt.[len - 3] = ':' && wt.[len - 1] = ':' then
    let start = String.sub wt 0 (len - 3) in
    start ^ Mutil.decline wt.[len - 2] s
  else
    match String.rindex_opt wt '+' with
    | Some i ->
        if
          i > 0
          && wt.[i - 1] = ' '
          && String.length wt - i = 7
          && String.get wt (i + 1) = 'b'
          && String.get wt (i + 2) = 'e'
          && String.get wt (i + 3) = 'f'
          && String.get wt (i + 4) = 'o'
          && String.get wt (i + 5) = 'r'
          && String.get wt (i + 6) = 'e'
        then
          let start = String.sub wt 0 (i - 1) in
          if s = "" then start else Mutil.decline 'n' s ^ " " ^ start
        else wt ^ Mutil.decline 'n' s1
    | None -> wt ^ Mutil.decline 'n' s1

let transl_decline conf w s =
  Translate.eval (gen_decline_basic (transl conf w) s)

(* in string s, handle xxx[aa|bb]Xcc according to X status (vowel) *)
let simple_decline conf wt =
  let len = String.length wt in
  let rec loop i =
    if i >= len then ""
    else
      let s, i =
        match wt.[i] with
        | '[' -> (
            try
              let j = String.index_from wt i ']' in
              let k = String.index_from wt i '|' in
              if k < j && j + 2 < len then
                let s2 = String.sub wt (j + 1) 1 in
                let s =
                  if start_with_vowel conf s2 then
                    String.sub wt (k + 1) (j - k - 1)
                  else String.sub wt (i + 1) (k - i - 1)
                  (*    [aa|bb]  *)
                in
                (s, j)
              else raise Not_found
            with Not_found -> (String.sub wt i (len - i), len))
        | c -> (String.make 1 c, i)
      in
      s ^ loop (i + 1)
  in
  loop 0

let gen_decline conf wt s1 s2 s2_raw =
  let string_of = function '1' -> Some s1 | '2' -> Some s2 | _ -> None in
  let len = String.length wt in
  let rec loop i =
    if i = len then ""
    else
      let s, i =
        match wt.[i] with
        | '%' when i + 1 < len -> (
            match string_of wt.[i + 1] with
            | Some s -> (s, i + 1)
            | None -> ("%", i))
        | ':' when i + 4 < len && wt.[i + 2] = ':' && wt.[i + 3] = '%' -> (
            let c = wt.[i + 1] in
            match string_of wt.[i + 4] with
            | Some s -> (Mutil.decline c s, i + 4)
            | None -> (":", i))
        | '[' -> (
            try
              let j = String.index_from wt i ']' in
              let k = String.index_from wt i '|' in
              if k < j && j + 2 < len && wt.[j + 1] = '%' then
                match string_of wt.[j + 2] with
                | Some s ->
                    let s =
                      if start_with_vowel conf s2_raw then
                        String.sub wt (k + 1) (j - k - 1) ^ s (*    [aa|bb]  *)
                      else String.sub wt (i + 1) (k - i - 1) ^ s (* i  k  j  *)
                    in
                    (s, j + 2)
                | None -> raise Not_found
              else raise Not_found
            with Not_found -> ("[", i))
        | c -> (String.make 1 c, i)
      in
      s ^ loop (i + 1)
  in
  loop 0

let transl_a_of_b conf x y1 y2 =
  gen_decline conf (transl_nth conf "%1 of %2" 0) x y1 y2

let transl_a_of_gr_eq_gen_lev conf x y1 y2 =
  gen_decline conf (transl_nth conf "%1 of %2" 1) x y1 y2

let check_format ini_fmt (r : string) =
  let s = string_of_format ini_fmt in
  let rec loop i j =
    if i < String.length s - 1 && j < String.length r - 1 then
      match (s.[i], s.[i + 1], r.[j], r.[j + 1]) with
      | '%', x, '%', y -> if x = y then loop (i + 2) (j + 2) else None
      | '%', _, _, _ -> loop i (j + 1)
      | _, _, '%', _ -> loop (i + 1) j
      | _ -> loop (i + 1) (j + 1)
    else if i < String.length s - 1 then
      if s.[i] = '%' then None else loop (i + 1) j
    else if j < String.length r - 1 then
      if r.[j] = '%' then None else loop i (j + 1)
    else Some (Scanf.format_from_string r ini_fmt)
  in
  loop 0 0

let valid_format ini_fmt r =
  match check_format ini_fmt r with
  | Some fmt -> fmt
  | None -> Scanf.format_from_string (tnf r) ini_fmt

let cftransl conf fmt =
  let fmt = transl conf fmt in
  let rec loop i = function
    | [] -> String.sub fmt i (String.length fmt - i)
    | a :: al as gal ->
        if
          i + 4 < String.length fmt
          && fmt.[i] = ':'
          && fmt.[i + 2] = ':'
          && fmt.[i + 3] = '%'
          && fmt.[i + 4] = 's'
        then Mutil.decline fmt.[i + 1] a ^ loop (i + 5) al
        else if i + 1 < String.length fmt && fmt.[i] = '%' && fmt.[i + 1] = 's'
        then Mutil.nominative a ^ loop (i + 2) al
        else if i < String.length fmt then
          String.make 1 fmt.[i] ^ loop (i + 1) gal
        else ""
  in
  loop 0

let ftransl conf s = valid_format s (transl conf (string_of_format s))

let ftransl_nth conf s p =
  valid_format s (transl_nth conf (string_of_format s) p)

let fdecline w s = valid_format w (gen_decline_basic (string_of_format w) s)
let translate_eval s = Translate.eval (Mutil.nominative s)

(* *)

let get_referer conf =
  let referer = Mutil.extract_param "referer: " '\n' conf.request in
  escape_html referer

let begin_centered conf =
  Output.printf conf
    "<table border=\"%d\" width=\"100%%\"><tr><td align=\"center\">\n"
    conf.border

let end_centered conf = Output.print_sstring conf "</td></tr></table>\n"

let week_day_txt =
  let txt = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  fun i ->
    let i = if i < 0 || i >= Array.length txt then 0 else i in
    txt.(i)

let month_txt =
  let txt =
    [|
      "Jan";
      "Feb";
      "Mar";
      "Apr";
      "May";
      "Jun";
      "Jul";
      "Aug";
      "Sep";
      "Oct";
      "Nov";
      "Dec";
    |]
  in
  fun i ->
    let i = if i < 0 || i >= Array.length txt then 0 else i in
    txt.(i)

let string_of_ctime conf =
  let lt = Unix.gmtime conf.ctime in
  Printf.sprintf "%s, %d %s %d %02d:%02d:%02d GMT"
    (week_day_txt lt.Unix.tm_wday)
    lt.Unix.tm_mday (month_txt lt.Unix.tm_mon) (1900 + lt.Unix.tm_year)
    lt.Unix.tm_hour lt.Unix.tm_min lt.Unix.tm_sec

let html ?(content_type = "text/html") conf =
  let charset = if conf.charset = "" then "utf-8" else conf.charset in
  Output.header conf "Content-type: %s; charset=%s" content_type charset;
  if not conf.cgi then Output.header conf "Server: GeneWeb/%s" Version.ver;
  Output.header conf "Date: %s" (string_of_ctime conf);
  Output.header conf "Connection: close"

let unauthorized conf auth_type =
  Output.status conf Def.Unauthorized;
  if not conf.cgi then
    Output.header conf "WWW-Authenticate: Basic realm=\"%s\"" auth_type;
  Output.header conf "Content-type: text/html; charset=%s" conf.charset;
  Output.print_sstring conf "<head><title>Access failed</title></head>\n";
  Output.print_sstring conf "<body><h1>Access failed</h1>\n";
  Output.printf conf "<ul><li>%s</ul>\n" auth_type;
  Output.print_sstring conf "</body>\n</html>\n"

let commd ?(excl = []) ?(trim = true) ?(pwd = true) ?(henv = true)
    ?(senv = true) conf : Adef.escaped_string =
  let aux =
    List.fold_left (fun c (k, (v : Adef.encoded_string)) ->
        if
          List.mem k excl
          || (trim && (k = "oc" || k = "ocz") && (v :> string) = "0")
          || (v :> string) = ""
          || k = "b"
          || (k = "lang" && conf.default_lang = (v :> string))
        then c
        else c ^^^ k ^<^ "=" ^<^ (v :> Adef.escaped_string) ^>^ "&")
  in
  let commd = conf.command in
  (* in CGI mode, b=bname is part of env *)
  (* in daemon mode, commd contains bname *)
  let commd =
    if pwd then commd
    else
      match String.split_on_char '_' commd with
      | b :: _p -> b
      | [] ->
          Logs.syslog `LOG_ERR
            (Format.sprintf "Poorly formatted command: %s" commd);
          commd
  in
  let s =
    Adef.escaped
    @@
    if conf.cgi then
      if conf.cgi_passwd = "" then commd ^ "?b=" ^ conf.bname ^ "&"
      else commd ^ "?b=" ^ conf.bname ^ "_" ^ conf.cgi_passwd ^ "&"
    else commd ^ "?"
  in
  let s = if henv then aux s conf.henv else s in
  let s = if senv then aux s conf.senv else s in
  s

let prefix_base conf =
  let cmmd = conf.command in
  Adef.escaped
  @@
  if conf.cgi then cmmd ^ "?b=" ^ conf.bname ^ "&"
  else
    let cmmd =
      match String.index_opt cmmd '_' with
      | Some i -> String.sub cmmd 0 i
      | None -> cmmd
    in
    cmmd ^ "?"

let prefix_base_password conf =
  Adef.escaped
  @@
  if conf.cgi then
    if conf.cgi_passwd = "" then conf.command ^ "?b=" ^ conf.bname ^ "&"
    else conf.command ^ "?b=" ^ conf.bname ^ "_" ^ conf.cgi_passwd ^ "&"
  else conf.command ^ "?"

let allowed_tags_file = ref ""

let default_safe_html_allowed_tags =
  [
    ("http://www.w3.org/1999/xhtml", "a");
    ("http://www.w3.org/1999/xhtml", "area");
    ("http://www.w3.org/1999/xhtml", "b");
    ("http://www.w3.org/1999/xhtml", "blockquote");
    ("http://www.w3.org/1999/xhtml", "br");
    ("http://www.w3.org/1999/xhtml", "center");
    ("http://www.w3.org/1999/xhtml", "cite");
    ("http://www.w3.org/1999/xhtml", "dd");
    ("http://www.w3.org/1999/xhtml", "dir");
    ("http://www.w3.org/1999/xhtml", "div");
    ("http://www.w3.org/1999/xhtml", "dl");
    ("http://www.w3.org/1999/xhtml", "dt");
    ("http://www.w3.org/1999/xhtml", "em");
    ("http://www.w3.org/1999/xhtml", "embed");
    ("http://www.w3.org/1999/xhtml", "font");
    ("http://www.w3.org/1999/xhtml", "h1");
    ("http://www.w3.org/1999/xhtml", "h2");
    ("http://www.w3.org/1999/xhtml", "h3");
    ("http://www.w3.org/1999/xhtml", "h4");
    ("http://www.w3.org/1999/xhtml", "h5");
    ("http://www.w3.org/1999/xhtml", "h6");
    ("http://www.w3.org/1999/xhtml", "hr");
    ("http://www.w3.org/1999/xhtml", "i");
    ("http://www.w3.org/1999/xhtml", "img");
    ("http://www.w3.org/1999/xhtml", "li");
    ("http://www.w3.org/1999/xhtml", "map");
    ("http://www.w3.org/1999/xhtml", "object");
    ("http://www.w3.org/1999/xhtml", "ol");
    ("http://www.w3.org/1999/xhtml", "ol");
    ("http://www.w3.org/1999/xhtml", "p");
    ("http://www.w3.org/1999/xhtml", "param");
    ("http://www.w3.org/1999/xhtml", "pre");
    ("http://www.w3.org/1999/xhtml", "s");
    ("http://www.w3.org/1999/xhtml", "small");
    ("http://www.w3.org/1999/xhtml", "span");
    ("http://www.w3.org/1999/xhtml", "strike");
    ("http://www.w3.org/1999/xhtml", "strong");
    ("http://www.w3.org/1999/xhtml", "sub");
    ("http://www.w3.org/1999/xhtml", "sup");
    ("http://www.w3.org/1999/xhtml", "table");
    ("http://www.w3.org/1999/xhtml", "tbody");
    ("http://www.w3.org/1999/xhtml", "td");
    ("http://www.w3.org/1999/xhtml", "tfoot");
    ("http://www.w3.org/1999/xhtml", "th");
    ("http://www.w3.org/1999/xhtml", "thead");
    ("http://www.w3.org/1999/xhtml", "tr");
    ("http://www.w3.org/1999/xhtml", "tt");
    ("http://www.w3.org/1999/xhtml", "u");
    ("http://www.w3.org/1999/xhtml", "ul");
    ("http://www.w3.org/1999/xhtml", "nav");
    ("http://www.w3.org/1999/xhtml", "section");
  ]

let safe_html_allowed_tags =
  lazy
    (if !allowed_tags_file = "" then default_safe_html_allowed_tags
     else if Sys.file_exists !allowed_tags_file then
       let ic = open_in !allowed_tags_file in
       let rec loop tags =
         match input_line ic with
         | tag ->
             let ns, tag =
               match String.split_on_char ' ' tag with
               | [ ns; tag ] -> (ns, tag)
               | [ tag ] -> ("http://www.w3.org/1999/xhtml", tag)
               | _ -> assert false
             in
             loop ((ns, String.lowercase_ascii tag) :: tags)
         | exception End_of_file ->
             close_in ic;
             tags
       in
       loop []
     else
       let str =
         Printf.sprintf "Requested allowed_tags file (%s) absent"
           !allowed_tags_file
       in
       Logs.syslog `LOG_WARNING str;
       default_safe_html_allowed_tags)

(* Few notes:

   According to https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/Data_URIs,
   > Data URLs are treated as unique opaque origins by modern browsers,
   > rather than inheriting the origin of the settings object responsible for the navigation.
   We do not need to filter this attribute.

   We filter out all attributes starting with ["on"] (prevent javascript from being executed).

   Remove any attribute when the value start with ["javascript"].

   Text is escaped using [escape_html].

   Replace tags not authorized with empty comments.

   Markup.ml automatically return tags names in lowercase.
*)
let safe_html_aux escape_text escape_attribute s =
  let open Markup in
  let stack = ref [] in
  let make_safe = function
    | `Start_element (name, attrs) ->
        if not @@ List.mem name @@ Lazy.force safe_html_allowed_tags then (
          stack := `KO :: !stack;
          `Comment "")
        else
          let attrs =
            List.filter
              (function
                | (_, k), v ->
                    (String.length k <= 2
                    || String.get k 0 <> 'o'
                    || String.get k 1 <> 'n')
                    && not
                         (Mutil.contains (String.lowercase_ascii v) "javascript"))
              attrs
          in
          stack := `OK :: !stack;
          `Start_element (name, attrs)
    | `End_element -> (
        match !stack with
        | `KO :: tl ->
            stack := tl;
            `Comment ""
        | `OK :: tl ->
            stack := tl;
            `End_element
        | _ -> failwith (__FILE__ ^ " " ^ string_of_int __LINE__))
    | e -> e
  in
  string s
  |> parse_html ~context:(`Fragment "body")
  |> signals |> map make_safe
  |> write_html ~escape_text ~escape_attribute
  |> to_string

let safe_html s =
  Adef.safe
    (safe_html_aux (fun s -> (escape_html s :> string)) escape_attribute s)

(* Clean HTML tags from a string. Block tags are replaced by a space,
   and inline tags are replaced by an empty string. *)
let clean_html_tags s =
  let open Str in
  let tag_pattern tag = Printf.sprintf "</?%s */?>" tag in
  let rep_block_tag s tag = global_replace (regexp (tag_pattern tag)) " " s in
  let rep_inline_tag s tag = global_replace (regexp (tag_pattern tag)) "" s in
  let block_tags = [ "br"; "div"; "h\\d"; "p"; "pre"; "ol"; "li"; "ul" ] in
  let inline_tags = [ "a"; "em"; "span"; "strong"; "sub"; "sup" ] in
  let s = List.fold_left rep_block_tag s block_tags in
  let s = List.fold_left rep_inline_tag s inline_tags in
  let s = global_replace (regexp " +") " " s in
  s

let clean_comment_tags s = Str.global_replace (Str.regexp "<!--.*-->") "" s
let uri_encode s = Uri.pct_encode ~component:`Query s
let uri_decode s = try Uri.pct_decode s with _ -> s

let hidden_textarea conf k v =
  Output.print_sstring conf {|<textarea style="display:none;" name="|};
  Output.print_string conf (escape_html k);
  Output.print_sstring conf {|">|};
  Output.print_string conf (escape_html (Mutil.decode v));
  Output.print_sstring conf "</textarea>\n"

let aux_input_s conf t k v =
  Output.print_sstring conf {|<input type="|};
  Output.print_string conf t;
  Output.print_sstring conf {|" name="|};
  Output.print_string conf (escape_html k);
  Output.print_sstring conf {|" value="|};
  Output.print_string conf (escape_html v);
  Output.print_sstring conf "\">\n"

let hidden_input_s conf k v = aux_input_s conf (Adef.encoded "hidden") k v
let hidden_input conf k v = hidden_input_s conf k (Mutil.decode v)
let hidden_env_aux conf = List.iter (fun (k, v) -> hidden_input conf k v)

let hidden_env conf =
  hidden_env_aux conf conf.henv;
  hidden_env_aux conf conf.senv

let submit_input conf k v =
  aux_input_s conf (Adef.encoded "submit") k (Mutil.decode v)

let p_getint env label =
  try Option.map (fun s -> int_of_string (String.trim s)) (p_getenv env label)
  with Failure _ -> None

let nobtit conf base p =
  Driver.nobtitles base conf.allowed_titles conf.denied_titles p

let strictly_after_private_years a lim =
  if a.year > lim then true
  else if a.year < lim then false
  else a.month > 0 || a.day > 0

let is_old_person conf p =
  match
    ( Date.cdate_to_dmy_opt p.birth,
      Date.cdate_to_dmy_opt p.baptism,
      p.death,
      Date.dmy_of_death p.death )
  with
  | _, _, NotDead, _ when conf.private_years > 0 -> false
  | Some d, _, _, _ ->
      let a = Date.time_elapsed d conf.today in
      strictly_after_private_years a conf.private_years
  | _, Some d, _, _ ->
      let a = Date.time_elapsed d conf.today in
      strictly_after_private_years a conf.private_years
  | _, _, _, Some d ->
      let a = Date.time_elapsed d conf.today in
      strictly_after_private_years a conf.private_years_death
  | None, None, DontKnowIfDead, None ->
      p.access <> Private && conf.public_if_no_date
  | _ -> false

let authorized_age conf base p = GWPARAM.p_auth conf base p

let is_restricted (conf : config) base (ip : Driver.iper) =
  let fct p =
    (not (Driver.Istr.is_quest (Driver.get_surname p)))
    && (not (Driver.Istr.is_quest (Driver.get_first_name p)))
    && not (authorized_age conf base p)
  in
  if conf.use_restrict then Driver.base_visible_get base fct ip else false

let pget_opt conf base ip =
  if is_restricted conf base ip then None else Some (Driver.poi base ip)

let pget conf base ip =
  if is_restricted conf base ip then Driver.empty_person base ip
  else Driver.poi base ip

let string_gen_person base p =
  Futil.map_person_ps (fun p -> p) (Driver.sou base) p

let string_gen_family base fam =
  Futil.map_family_ps (fun p -> p) (fun f -> f) (Driver.sou base) fam

let is_hidden p = Driver.Istr.is_empty (Driver.get_surname p)

let is_empty_name p =
  Driver.Istr.is_quest (Driver.get_surname p)
  && Driver.Istr.is_quest (Driver.get_first_name p)

let is_public conf base p =
  Driver.get_access p = Public
  || conf.public_if_titles
     && Driver.get_access p = IfTitles
     && nobtit conf base p <> []
  || is_old_person conf (Driver.gen_person_of_person p)

(* ********************************************************************** *)
(* [Fonc] accessible_by_key :
            config -> base -> person -> string -> string -> bool *)

(* ********************************************************************** *)

(** [Description] : Vrai si la personne est accessible par sa clé, Faux sinon.
    [Args] :
    - conf : configuration de la base
    - base : base de donnée
    - p : person
    - fn : prénom de la personne
    - sn : patronyme de la personne [Retour] :
    - bool : Vrai si la personne est accessible par sa clé, faux sinon. [Rem] :
      Exporté en clair hors de ce module. *)
let accessible_by_key conf base p fn sn =
  conf.access_by_key
  && (not (fn = "?" || sn = "?"))
  && ((not (is_hide_names conf p))
     || is_public conf base p || conf.friend || conf.wizard)

(* ********************************************************************** *)
(*  [Fonc] acces_n : config -> base -> string -> person -> string         *)

(* ********************************************************************** *)

let access_status p =
  match Driver.get_access p with
  | Private -> "Private"
  | SemiPublic -> "SemiPublic"
  | Public -> "Public"
  | IfTitles -> "IfTitles"

(** [Description] : Renvoie les paramètres URL pour l'accès à la nième personne.
    [Args] :
    - conf : configuration de la base
    - base : base de donnée
    - n : la nième personne (e.g. : calcul de parenté entre p1 et p2)
    - p : person [Retour] : string [Rem] : Exporté en clair hors de ce module.
*)
let acces_n conf base n x : Adef.escaped_string =
  let first_name = Driver.p_first_name base x in
  let surname = Driver.p_surname base x in
  if surname = "" then Adef.escaped ""
  else if accessible_by_key conf base x first_name surname then
    "p" ^<^ n ^^^ "="
    ^<^ (Mutil.encode (Name.lower first_name) :> Adef.escaped_string)
    ^^^ "&n" ^<^ n ^^^ "="
    ^<^ (Mutil.encode (Name.lower surname) :> Adef.escaped_string)
    ^^^
    if Driver.get_occ x <> 0 then
      "&oc" ^<^ n ^>^ "=" ^ string_of_int (Driver.get_occ x)
    else Adef.escaped ""
  else
    "i" ^<^ n ^^^ "="
    ^<^ Driver.Iper.to_string (Driver.get_iper x)
    ^<^
    if conf.wizard && Driver.get_occ x <> 0 then
      "&oc" ^<^ n ^>^ "=" ^ string_of_int (Driver.get_occ x)
    else Adef.escaped ""

(* ********************************************************************** *)
(*  [Fonc] acces : config -> base -> person -> string                     *)

(* ********************************************************************** *)

(** [Description] : Renvoie les paramètres URL pour l'accès à la personne.
    [Args] :
    - conf : configuration de la base
    - base : base de donnée
    - p : person [Retour] : string [Rem] : Exporté en clair hors de ce module.
*)
let acces conf base x = acces_n conf base (Adef.escaped "") x

(**/**)

let restricted_txt = Adef.safe "....."

let private_txt conf k =
  match k with
  | "p" -> transl conf "hidden first_name"
  | "n" -> transl conf "hidden surname"
  | _ -> transl conf "hidden person"

let gen_person_text ?(escape = true) ?(html = true) ?(sn = true)
    ?(p_first_name = Driver.p_first_name) ?(p_surname = Driver.p_surname) conf
    base p =
  let esc = if escape then esc else Adef.safe in
  if is_hidden p then restricted_txt
  else if GWPARAM.p_auth_sp conf base p then
    let beg =
      match
        (Driver.sou base (Driver.get_public_name p), Driver.get_qualifiers p)
      with
      | "", nn :: _ ->
          esc (p_first_name base p)
          ^^^ (if html then " <em>" else " ")
          ^<^ esc (Driver.sou base nn)
          ^>^ if html then "</em>" else ""
      | "", [] -> esc (p_first_name base p)
      | n, nn :: _ -> esc n ^^^ " <em>" ^<^ esc (Driver.sou base nn) ^>^ "</em>"
      | n, [] -> esc n
    in
    if sn then
      match p_surname base p with "" -> beg | sn -> beg ^^^ " " ^<^ esc sn
    else beg
  else Adef.safe (private_txt conf "")

let main_title conf base p =
  let titles = nobtit conf base p in
  match List.find_opt (fun x -> x.t_name = Tmain) titles with
  | None -> ( match titles with x :: _ -> Some x | _ -> None)
  | x -> x

let titled_person_text conf base p t : Adef.safe_string =
  if List.assoc_opt "print_advanced_title" conf.base_env = Some "yes" then
    let estate = Driver.sou base t.t_place in
    let surname = Driver.p_surname base p in
    (* Si le nom de l'individu est le même que son domaine, on renvoie : *)
    (*   - le nom du titre                                               *)
    (*   - le nom du titre et le premier sobriquet                       *)
    (*   - le nom de la personne (donné par son nom de domaine) en       *)
    (*     fonction du nom public et sobriquet                           *)
    if Name.strip_lower estate = Name.strip_lower surname then
      match (t.t_name, Driver.get_qualifiers p) with
      | Tname n, [] -> (esc (Driver.sou base n) :> Adef.safe_string)
      | Tname n, nn :: _ ->
          (esc (Driver.sou base n) :> Adef.safe_string)
          ^^^ " <em>"
          ^<^ (esc (Driver.sou base nn) :> Adef.safe_string)
          ^>^ "</em>"
      | _ -> gen_person_text ~sn:false conf base p
    else
      let elen = String.length estate in
      let slen = String.length surname in
      if elen < slen && String.sub surname (slen - elen) elen = estate then
        match (t.t_name, Driver.get_qualifiers p) with
        | Tname n, [] -> esc (Driver.sou base n)
        | Tname n, nn :: _ ->
            esc (Driver.sou base n)
            ^^^ " <em>"
            ^<^ esc (Driver.sou base nn)
            ^>^ "</em>"
        | _ ->
            gen_person_text
              ~p_surname:(fun _ _ ->
                String.trim (String.sub surname 0 (slen - elen)))
              conf base p
      else
        match t.t_name with
        | Tname s -> (
            let s = esc (Driver.sou base s) in
            match Driver.get_qualifiers p with
            | [] -> s
            | nn :: _ -> s ^^^ " <em>" ^<^ esc (Driver.sou base nn) ^>^ "</em>")
        | _ -> gen_person_text conf base p
  else gen_person_text conf base p

(* *********************************************************************** *)
(*  [Fonc] one_title_text : base -> istr gen_title     *)

(* *********************************************************************** *)

(** [Description] : Renvoie la chaîne de caractère du titre ainsi que le
    domaine. [Args] :
    - base : base de donnée
    - t : le titre de noblesse que l'on veut afficher [Retour] : string [Rem] :
      Non exporté en clair hors de ce module. *)
let one_title_text base t : Adef.safe_string =
  let place = Driver.sou base t.t_place in
  let s = Driver.sou base t.t_ident in
  let s = if place = "" then s else s ^ " " ^ place in
  " <em>" ^<^ (esc s :> Adef.safe_string) ^>^ "</em>"

let geneweb_link conf (href : Adef.escaped_string) (s : Adef.safe_string) =
  let cancel_links = p_getenv conf.env "cgl" = Some "on" in
  if cancel_links then s
  else
    "<a href=\""
    ^<^ (commd conf ^^^ href :> Adef.safe_string)
    ^^^ "\">" ^<^ s ^>^ "</a>"

let wprint_geneweb_link conf href s =
  Output.print_string conf (geneweb_link conf href s)

let mod_ind_link conf p (s : Adef.safe_string) =
  let cgl =
    match p_getenv conf.env "cgl" with Some "on" -> true | _ -> false
  in
  if is_hidden p || cgl || not conf.wizard then s
  else
    let s = (s :> string) in
    let href = "m=MOD_IND&i=" ^ Driver.Iper.to_string (Driver.get_iper p) in
    let txt =
      if s = "" then {|<i class="fa fa-wrench fa-xs ml-1" alt=" (edit)"></i>|}
      else s
    in
    Format.sprintf {|<a href="%s%s">%s</a>|} (commd conf :> string) href txt
    |> Adef.safe

let reference_flags with_id conf base p (s : Adef.safe_string) =
  let cgl =
    match p_getenv conf.env "cgl" with Some "on" -> true | _ -> false
  in
  let iper = Driver.get_iper p in
  (* let is_hidden = is_empty_string (get_surname p) !! *)
  if (not (GWPARAM.p_auth conf base p)) || cgl then s
  else
    "<a href=\""
    ^<^ (commd conf ^^^ acces conf base p :> Adef.safe_string)
    ^^^ (if with_id then "\" id=\"i" else "")
    ^<^ (if with_id then Driver.Iper.to_string iper else "")
    ^<^ "\">" ^<^ s ^>^ "</a>"

let reference = reference_flags true
let reference_noid = reference_flags false

(* ************************************************************************* *)
(*  [Fonc] update_family_loop : config -> base -> person -> string -> string *)

(* ************************************************************************* *)

(** [Description] : Essaie de déterminer dans quelle famille il peut y avoir une
    boucle. Si il n'y a pas d'ambiguité, alors on renvoie un lien vers la
    famille à modifier, sinon, on renvoie un lien vers le menu général de mise à
    jour. [Args] :
    - conf : configuration
    - base : base
    - p : person
    - s : la clé de la personne sous forme de string [Retour] :
    - string : retourne un lien de mise à jour soit vers la famille contenant la
      boucle, soit vers le menu de mise à jour. [Rem] : Exporté en clair hors de
      ce module. *)
let update_family_loop conf base p s =
  if is_hidden p then s
  else
    let iper = Driver.get_iper p in
    let list = Driver.get_family p in
    let list =
      Array.map
        (fun ifam -> (ifam, Driver.get_children (Driver.foi base ifam)))
        list
    in
    let res =
      Array.fold_left
        (fun acc (ifam, children) ->
          if Array.mem iper children then ifam :: acc else acc)
        [] list
    in
    if conf.wizard then
      match res with
      | [ res ] ->
          let iper = Driver.Iper.to_string iper in
          let ifam = Driver.Ifam.to_string res in
          "<a href=\""
          ^<^ (commd conf :> Adef.safe_string)
          ^^^ "m=MOD_FAM&i=" ^<^ ifam ^<^ "&ip=" ^<^ iper ^<^ "\">" ^<^ s
          ^>^ "</a>"
      | _ ->
          let iper = Driver.Iper.to_string iper in
          "<a href=\""
          ^<^ (commd conf :> Adef.safe_string)
          ^^^ "m=U&i=" ^<^ iper ^<^ "\">" ^<^ s ^>^ "</a>"
    else s

let no_reference _conf _base _p s = s

let gen_person_title_text reference conf base p =
  if authorized_age conf base p then
    match main_title conf base p with
    | Some t ->
        reference conf base p (titled_person_text conf base p t)
        ^^^ ", " ^<^ one_title_text base t
    | None -> reference conf base p (gen_person_text conf base p)
  else reference conf base p (gen_person_text conf base p)

let referenced_person_title_text = gen_person_title_text reference
let person_title_text = gen_person_title_text no_reference

let referenced_person_text conf base p =
  reference conf base p (gen_person_text conf base p)

let referenced_person_text_without_surname conf base p =
  reference conf base p (gen_person_text ~sn:false conf base p)

let person_text_without_title conf base p =
  match main_title conf base p with
  | Some t -> (
      if Driver.Istr.equal t.t_place (Driver.get_surname p) then
        gen_person_text ~sn:false conf base p
      else
        match (t.t_name, Driver.get_qualifiers p) with
        | Tname s, nn :: _ ->
            esc (Driver.sou base s)
            ^^^ " <em>"
            ^<^ esc (Driver.sou base nn)
            ^>^ "</em>"
        | Tname s, _ -> esc (Driver.sou base s)
        | _ -> gen_person_text conf base p)
  | None -> gen_person_text conf base p

let person_title conf base p =
  if authorized_age conf base p then
    match main_title conf base p with
    | Some t -> one_title_text base t
    | None -> Adef.safe ""
  else Adef.safe ""

let make_key base p =
  ( Name.lower (Driver.sou base p.first_name),
    Name.lower (Driver.sou base p.surname),
    p.occ )

let name_key base s =
  let part = Mutil.get_particle (Driver.base_particles base) s in
  if part = "" then s
  else
    let i = String.length part in
    String.sub s i (String.length s - i) ^ " " ^ String.sub s 0 i

let surname_particle base s =
  let part = Mutil.get_particle (Driver.base_particles base) s in
  let len = String.length part in
  if len = 0 then ""
  else if part.[len - 1] = ' ' then " (" ^ String.sub part 0 (len - 1) ^ ")"
  else " (" ^ part ^ ")"

let surname_without_particle base s =
  let part_len =
    String.length (Mutil.get_particle (Geneweb_db.Driver.base_particles base) s)
  in
  String.sub s part_len (String.length s - part_len)

let rec skip_spaces s i =
  if i < String.length s && s.[i] = ' ' then skip_spaces s (i + 1) else i

let create_env s =
  let s = (s : Adef.encoded_string :> string) in
  let use_amp = not (Mutil.contains s "content-disposition") in
  let rec get_assoc beg i =
    if i = String.length s then
      if i = beg then [] else [ String.sub s beg (i - beg) ]
    else if s.[i] = ';' || (s.[i] = '&' && use_amp) then
      let next_i = skip_spaces s (succ i) in
      String.sub s beg (i - beg) :: get_assoc next_i next_i
    else get_assoc beg (succ i)
  in
  let rec separate i s =
    if i = String.length s then (s, Adef.encoded "")
    else if s.[i] = '=' then
      ( String.sub s 0 i,
        Adef.encoded (String.sub s (succ i) (String.length s - succ i)) )
    else separate (succ i) s
  in
  List.map (separate 0) (get_assoc 0 0)

let std_color conf (s : Adef.safe_string) =
  "<span style=\"color:" ^<^ conf.highlight ^<^ "\">" ^<^ s ^>^ "</span>"

let index_of_sex = function Male -> 0 | Female -> 1 | Neuter -> 2

let string_of_pevent_name conf base epers_name =
  match epers_name with
  | Epers_Birth -> Adef.safe @@ transl conf "birth"
  | Epers_Baptism -> Adef.safe @@ transl conf "baptism"
  | Epers_Death -> Adef.safe @@ transl conf "death"
  | Epers_Burial -> Adef.safe @@ transl conf "burial"
  | Epers_Cremation -> Adef.safe @@ transl conf "cremation"
  | Epers_Accomplishment -> Adef.safe @@ transl conf "accomplishment"
  | Epers_Acquisition -> Adef.safe @@ transl conf "acquisition"
  | Epers_Adhesion -> Adef.safe @@ transl conf "adhesion"
  | Epers_BaptismLDS -> Adef.safe @@ transl conf "baptismLDS"
  | Epers_BarMitzvah -> Adef.safe @@ transl conf "bar mitzvah"
  | Epers_BatMitzvah -> Adef.safe @@ transl conf "bat mitzvah"
  | Epers_Benediction -> Adef.safe @@ transl conf "benediction"
  | Epers_ChangeName -> Adef.safe @@ transl conf "change name"
  | Epers_Circumcision -> Adef.safe @@ transl conf "circumcision"
  | Epers_Confirmation -> Adef.safe @@ transl conf "confirmation"
  | Epers_ConfirmationLDS -> Adef.safe @@ transl conf "confirmation LDS"
  | Epers_Decoration -> Adef.safe @@ transl conf "decoration"
  | Epers_DemobilisationMilitaire ->
      Adef.safe @@ transl conf "demobilisationMilitaire"
  | Epers_Diploma -> Adef.safe @@ transl conf "diploma"
  | Epers_Distinction -> Adef.safe @@ transl conf "distinction"
  | Epers_Dotation -> Adef.safe @@ transl conf "dotation"
  | Epers_DotationLDS -> Adef.safe @@ transl conf "dotationLDS"
  | Epers_Education -> Adef.safe @@ transl conf "education"
  | Epers_Election -> Adef.safe @@ transl conf "election"
  | Epers_Emigration -> Adef.safe @@ transl conf "emigration"
  | Epers_Excommunication -> Adef.safe @@ transl conf "excommunication"
  | Epers_FamilyLinkLDS -> Adef.safe @@ transl conf "familyLinkLDS"
  | Epers_FirstCommunion -> Adef.safe @@ transl conf "firstCommunion"
  | Epers_Funeral -> Adef.safe @@ transl conf "funeral"
  | Epers_Graduate -> Adef.safe @@ transl conf "graduate"
  | Epers_Hospitalisation -> Adef.safe @@ transl conf "hospitalisation"
  | Epers_Illness -> Adef.safe @@ transl conf "illness"
  | Epers_Immigration -> Adef.safe @@ transl conf "immigration"
  | Epers_ListePassenger -> Adef.safe @@ transl conf "listePassenger"
  | Epers_MilitaryDistinction -> Adef.safe @@ transl conf "militaryDistinction"
  | Epers_MilitaryPromotion -> Adef.safe @@ transl conf "militaryPromotion"
  | Epers_MilitaryService -> Adef.safe @@ transl conf "militaryService"
  | Epers_MobilisationMilitaire ->
      Adef.safe @@ transl conf "mobilisationMilitaire"
  | Epers_Naturalisation -> Adef.safe @@ transl conf "naturalisation"
  | Epers_Occupation -> Adef.safe @@ transl_nth conf "occupation/occupations" 0
  | Epers_Ordination -> Adef.safe @@ transl conf "ordination"
  | Epers_Property -> Adef.safe @@ transl conf "property"
  | Epers_Recensement -> Adef.safe @@ transl conf "recensement"
  | Epers_Residence -> Adef.safe @@ transl conf "residence"
  | Epers_Retired -> Adef.safe @@ transl conf "retired"
  | Epers_ScellentChildLDS -> Adef.safe @@ transl conf "scellentChildLDS"
  | Epers_ScellentParentLDS -> Adef.safe @@ transl conf "scellentParentLDS"
  | Epers_ScellentSpouseLDS -> Adef.safe @@ transl conf "scellentSpouseLDS"
  | Epers_VenteBien -> Adef.safe @@ transl conf "venteBien"
  | Epers_Will -> Adef.safe @@ transl conf "will"
  | Epers_Name n -> (escape_html (Driver.sou base n) :> Adef.safe_string)

let string_of_fevent_name conf base = function
  | Efam_Marriage -> Adef.safe @@ transl conf "marriage event"
  | Efam_NoMarriage -> Adef.safe @@ transl conf "no marriage event"
  | Efam_NoMention -> Adef.safe @@ transl conf "no mention"
  | Efam_Engage -> Adef.safe @@ transl conf "engage event"
  | Efam_Divorce -> Adef.safe @@ transl conf "divorce event"
  | Efam_Separated -> Adef.safe @@ transl conf "separate event"
  | Efam_Annulation -> Adef.safe @@ transl conf "annulation"
  | Efam_MarriageBann -> Adef.safe @@ transl conf "marriage bann"
  | Efam_MarriageContract -> Adef.safe @@ transl conf "marriage contract"
  | Efam_MarriageLicense -> Adef.safe @@ transl conf "marriage licence"
  | Efam_PACS -> Adef.safe @@ transl conf "PACS"
  | Efam_Residence -> Adef.safe @@ transl conf "residence"
  | Efam_Name n -> (escape_html (Driver.sou base n) :> Adef.safe_string)

let string_of_witness_kind conf sex witness_kind =
  let n = if witness_kind = Witness then 0 else index_of_sex sex in
  let s =
    match witness_kind with
    | Witness -> "witness/witness/witnesses"
    | Witness_CivilOfficer -> "civil registrar/civil registrar/civil registrar"
    | Witness_GodParent -> "godfather/godmother/godparents"
    | Witness_ReligiousOfficer ->
        "parrish registrar/parrish registrar/parrish registrar"
    | Witness_Informant -> "informant/informant/informant"
    | Witness_Attending -> "present/present/present"
    | Witness_Mentioned -> "mentioned/mentioned/mentioned"
    | Witness_Other -> "other/other/other"
  in
  Adef.safe @@ transl_nth conf s n

let string_of_witness_kind_raw witness_kind =
  let s =
    match witness_kind with
    | Witness -> ""
    | Witness_CivilOfficer -> "offi"
    | Witness_GodParent -> "godp"
    | Witness_ReligiousOfficer -> "reli"
    | Witness_Informant -> "info"
    | Witness_Attending -> "atte"
    | Witness_Mentioned -> "ment"
    | Witness_Other -> "othe"
  in
  Adef.safe s

let bpath bname = !GWPARAM.bpath bname

let find_file_in_directories directories filename =
  let rec search = function
    | [] -> None
    | dir :: remaining ->
        let full_path = Filename.concat dir filename in
        if Sys.file_exists full_path then Some full_path else search remaining
  in
  search directories

(* ************************************************************************ *)
(*  [Func] generate_search_directories : config -> string list              *)
(* ************************************************************************ *)

(** Generates the ordered list of search directories for template files.

    The search is done in this order:
    - bases/etc/mybase/templx/ (template [templx] in mybase)
    - bases/etc/mybase/ (default template in mybase)
    - gw/etc/templx/ (template [templx] in etc)
    - gw/etc/ (default template in etc)

    The template configuration variable can contain:
    - template=templ1,templ2: allows only these templates
    - template=*: allows all templates

    @param conf base configuration
    @return ordered list of directories to traverse *)

let generate_search_directories conf =
  let base_etc = !GWPARAM.etc_d conf.bname in
  let asset_dirs = Secure.assets () in
  let configured_templates, allow_all =
    try
      let templates =
        List.assoc "template" conf.base_env
        |> String.split_on_char ',' |> List.map String.trim
        |> List.filter (( <> ) "")
      in
      let allow_all = List.mem "*" templates in
      let explicit_templates = List.filter (( <> ) "*") templates in
      (explicit_templates, allow_all)
    with Not_found -> ([ conf.bname ], true)
  in
  let current_template =
    match p_getenv conf.env "templ" with
    | Some t when allow_all || List.mem t configured_templates -> Some t
    | _ -> List.nth_opt configured_templates 0
  in
  let template_dirs =
    match current_template with
    | Some t -> [ Filename.concat base_etc t; base_etc ]
    | None -> [ base_etc ]
  in
  let asset_template_dirs =
    List.concat
      (List.map
         (fun asset_dir ->
           let etc_dir = Filename.concat asset_dir "etc" in
           match current_template with
           | Some t -> [ Filename.concat etc_dir t; etc_dir ]
           | None -> [ etc_dir ])
         asset_dirs)
  in
  template_dirs @ asset_template_dirs

(* ************************************************************************ *)
(*  [Func] find_template_file : config -> string -> bool -> string          *)
(* ************************************************************************ *)

(** Generic function to find a file in the template hierarchy.
    @param conf base configuration
    @param fname file name
    @param auto_txt
      if [true], automatically adds [.txt] extension for compatibility
    @return full path to the found file*)

let find_template_file conf fname auto_txt =
  let normalized_fname =
    List.fold_left Filename.concat "" (String.split_on_char '/' fname)
  in
  let final_fname =
    if auto_txt && not (Filename.check_suffix normalized_fname ".txt") then
      normalized_fname ^ ".txt"
    else normalized_fname
  in
  let search_dirs = generate_search_directories conf in
  match find_file_in_directories search_dirs final_fname with
  | Some path -> path
  | None -> search_in_assets (Filename.concat "etc" final_fname)

(* ************************************************************************ *)
(*  [Fonc] etc_file_name : config -> string -> string                       *)
(* ************************************************************************ *)

(** Find an HTML template file with automatic. For compatibility, it
    automatically adds the [.txt] extension to the filename.

    @param conf base configuration
    @param fname file name (without [.txt] extension)
    @return full path to the found file *)

let etc_file_name conf fname = find_template_file conf fname true

(* ************************************************************************ *)
(*  [Fonc] resolve_asset_file : config -> string -> string                  *)
(* ************************************************************************ *)

(** Resolves asset files (CSS/JS/fonts) while respecting the template hierarchy.
    Does not add [.txt] extension.

    @param conf base configuration
    @param fname asset file name with its extension
    @return full path to the asset *)

let resolve_asset_file conf fname = find_template_file conf fname false

let open_etc_file conf fname =
  let fname = etc_file_name conf fname in
  try Some (Secure.open_in fname, fname)
  with Sys_error e ->
    Logs.syslog `LOG_ERR
      (Format.sprintf "Error opening file %s in open_etc_file: %s" fname e);
    None

(* Detect if a template file is a full HTML page *)
let is_full_html_template conf fname =
  match open_etc_file conf fname with
  | None -> false
  | Some (ic, _) ->
      let rec check_lines n =
        if n <= 0 then false
        else
          try
            let line = input_line ic in
            let normalized = String.trim line |> String.lowercase_ascii in
            if normalized = "<!doctype html>" then true else check_lines (n - 1)
          with End_of_file -> false
      in
      let result = check_lines 3 in
      close_in ic;
      result

let body_prop conf =
  try match List.assoc "body_prop" conf.base_env with "" -> "" | s -> " " ^ s
  with Not_found -> ""

let get_server_string conf =
  if not conf.cgi then Mutil.extract_param "host: " '\r' conf.request
  else
    let server_name = try Sys.getenv "SERVER_NAME" with Not_found -> "" in
    let server_port =
      try Sys.getenv "SERVER_PORT" with Not_found | Failure _ -> "80"
    in
    if server_port = "80" then server_name else server_name ^ ":" ^ server_port

let get_request_string conf =
  if not conf.cgi then Mutil.extract_param "GET " ' ' conf.request
  else
    let script_name = try Sys.getenv "SCRIPT_NAME" with Not_found -> "" in
    let query_string = try Sys.getenv "QUERY_STRING" with Not_found -> "" in
    script_name ^ "?" ^ query_string

let message_to_wizard conf =
  if conf.wizard || conf.just_friend_wizard then (
    let print_file fname =
      let fname =
        Filename.concat (!GWPARAM.etc_d conf.bname) (fname ^ ".txt")
      in
      try
        let ic = Secure.open_in fname in
        try
          while true do
            Output.printf conf "%c" (input_char ic)
          done
        with End_of_file -> close_in ic
      with Sys_error _ -> ()
    in
    print_file "mess_wizard";
    if conf.user <> "" then print_file ("mess_wizard_" ^ conf.user))

let doctype = Adef.safe "<!DOCTYPE html>"

let http_string s i =
  let http = "http://" in
  let https = "https://" in
  let http, start_with_http =
    if start_with s i http then (http, true) else (https, start_with s i https)
  in
  if start_with_http then
    let j, par =
      let rec loop j par =
        if j < String.length s then
          match s.[j] with
          | 'a' .. 'z'
          | 'A' .. 'Z'
          | '\128' .. '\255'
          | '0' .. '9'
          | '!' | '#' | '$' | '%' | '&' | '(' | ')' | '*' | '+' | ',' | '-'
          | '.' | '/' | ':' | ';' | '=' | '?' | '@' | '\\' | '_' | '~' ->
              if s.[j] = '(' then loop (j + 1) (par + 1)
              else if s.[j] = ')' then loop (j + 1) (par - 1)
              else loop (j + 1) par
          | '[' | '^' | '{' | '|' -> (j + 1, par)
          | ']' | '}' -> (j, par)
          | _ -> (j, par)
        else (j, par)
      in
      loop (i + String.length http) 0
    in
    let j =
      let rec loop j =
        match s.[j - 1] with
        | ')' | ',' | '.' | ':' | ';' ->
            if s.[j - 1] = ')' && par = 0 then j
            else if s.[j - 1] = ')' && par < 0 then j - 1
            else loop (j - 1)
        | _ -> j
      in
      loop j
    in
    let s = String.sub s i (j - i) in
    Some (s, j)
  else None

let rec followed_by_ident_semi s i =
  if i = String.length s then false
  else
    match s.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' -> followed_by_ident_semi s (i + 1)
    | '#' | '0' .. '9' -> followed_by_ident_semi s (i + 1)
    | ';' -> true
    | _ -> false

let expand_ampersand buff s =
  let rec loop i =
    if i = String.length s then ()
    else (
      if s.[i] = '&' then Buffer.add_string buff "&amp;"
      else Buffer.add_char buff s.[i];
      loop (i + 1))
  in
  loop 0

let email_addr s i =
  let rec before_at empty i =
    if i = String.length s then None
    else
      match s.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' ->
          before_at false (i + 1)
      | '@' -> if empty then None else after_at true (i + 1)
      | _ -> None
  and after_at empty i =
    if i = String.length s then None
    else
      match s.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' ->
          after_at false (i + 1)
      | '.' -> if empty then None else after_dot 0 (i + 1)
      | _ -> None
  and after_dot len i =
    if i = String.length s then Some (len, i)
    else
      match s.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' ->
          after_dot (len + 1) (i + 1)
      | _ -> Some (len, i)
  in
  match before_at true i with
  | Some (len, i) ->
      let len, i =
        if len > 0 && s.[i - 1] = '.' then (len - 1, i - 1) else (len, i)
      in
      if len = 0 then None else Some i
  | None -> None

let get_variable s i =
  let rec loop len i =
    if i = String.length s then (Buff.get len, [], i)
    else
      match s.[i] with
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c ->
          loop (Buff.store len c) (i + 1)
      | ':' ->
          let v = Buff.get len in
          let rec loop vl len i =
            if i = String.length s then (v, List.rev (Buff.get len :: vl), i)
            else
              match s.[i] with
              | ':' -> loop (Buff.get len :: vl) 0 (i + 1)
              | ';' -> (v, List.rev (Buff.get len :: vl), i + 1)
              | c -> loop vl (Buff.store len c) (i + 1)
          in
          loop [] 0 (i + 1)
      | ';' -> (Buff.get len, [], i + 1)
      | _ -> (Buff.get len, [], i)
  in
  loop 0 i

type tag_type = In_a_href | In_norm | Out

let expand_env =
  let buff = Buffer.create 30 in
  fun conf s ->
    match List.assoc_opt "expand_env" conf.base_env with
    | Some "yes" ->
        let _ = (Buffer.clear buff : unit) in
        let rec loop i =
          if i = String.length s then Buffer.contents buff
          else if i + 1 < String.length s && s.[i] = '$' && s.[i + 1] = '{' then (
            try
              let j = String.index_from s (i + 1) '}' in
              let v = Sys.getenv (String.sub s (i + 2) (j - i - 2)) in
              Buffer.add_string buff v;
              loop (j + 1)
            with Not_found ->
              Buffer.add_char buff s.[i];
              loop (i + 1))
          else (
            Buffer.add_char buff s.[i];
            loop (i + 1))
        in
        loop 0
    | _ -> s

(* in srcfileDisplay, there is a macro function with many more macros! *)
(* not necessarily easy to transpose in this context (base absent) *)
let string_with_macros conf env s =
  let start_with s i p =
    i + String.length p <= String.length s
    && String.lowercase_ascii (String.sub s i (String.length p)) = p
  in
  let buff = Buffer.create 1000 in
  let rec loop tt i =
    if i < String.length s then
      if i + 1 < String.length s && s.[i] = '%' then
        let i =
          try
            Buffer.add_string buff (List.assoc s.[i + 1] env ());
            i + 2
          with Not_found -> (
            match s.[i + 1] with
            | 's' ->
                Buffer.add_string buff (commd conf :> string);
                i + 2
            | 'v' ->
                let k, vl, j = get_variable s (i + 2) in
                let v, i =
                  let v =
                    try
                      let v = List.assoc ("var_" ^ k) conf.base_env in
                      Some (expand_env conf v)
                    with Not_found -> None
                  in
                  match v with
                  | Some s ->
                      let s =
                        let rec loop vl len i =
                          if i = String.length s then Buff.get len
                          else if
                            i + 1 < String.length s
                            && s.[i] = '%'
                            && s.[i + 1] = 's'
                          then
                            match vl with
                            | v :: vl -> loop vl (Buff.mstore len v) (i + 2)
                            | [] ->
                                Buff.get len
                                ^ String.sub s i (String.length s - i)
                          else loop vl (Buff.store len s.[i]) (i + 1)
                        in
                        loop vl 0 0
                      in
                      (s, j)
                  | None -> ("%", i + 1)
                in
                Buffer.add_string buff v;
                i
            | '%' ->
                Buffer.add_string buff "%";
                i + 2
            | _ ->
                Buffer.add_string buff "%";
                i + 1)
        in
        loop tt i
      else
        match tt with
        | In_a_href ->
            let tt = if start_with s i "</a>" then Out else In_a_href in
            Buffer.add_char buff s.[i];
            loop tt (i + 1)
        | In_norm ->
            let tt = if s.[i] = '>' then Out else In_norm in
            Buffer.add_char buff s.[i];
            loop tt (i + 1)
        | Out -> (
            match http_string s i with
            | Some (x, j) ->
                Printf.bprintf buff "<a href=\"%s\">" x;
                expand_ampersand buff x;
                Printf.bprintf buff "</a>";
                loop Out j
            | None -> (
                match email_addr s i with
                | Some j ->
                    let x = String.sub s i (j - i) in
                    Printf.bprintf buff "<a href=\"mailto:%s\">%s</a>" x x;
                    loop Out j
                | None ->
                    let tt =
                      if start_with s i "<a href=" || start_with s i "<a\nhref="
                      then In_a_href
                      else if s.[i] = '<' then In_norm
                      else Out
                    in
                    if s.[i] = '&' && not (followed_by_ident_semi s (i + 1))
                    then Buffer.add_string buff "&amp;"
                    else Buffer.add_char buff s.[i];
                    loop tt (i + 1)))
    else Buffer.contents buff
  in
  loop Out 0

let place_of_string conf place =
  match List.assoc_opt "place" conf.base_env with
  | Some gwf_place ->
      let list = String.split_on_char ',' gwf_place in
      let list = List.map String.trim list in
      let list_p = String.split_on_char ',' place in
      let list_p = List.map String.trim list_p in
      let place =
        {
          other = "";
          town = "";
          township = "";
          canton = "";
          district = "";
          county = "";
          region = "";
          country = "";
        }
      in
      let place =
        let rec loop list list_p place =
          match list_p with
          | [] -> place
          | x :: list_p -> (
              match list with
              | [] ->
                  let other = String.concat ", " (x :: list_p) in
                  let other = place.other ^ " " ^ other in
                  { place with other }
              | t :: list ->
                  let place =
                    match t with
                    | "town" -> { place with town = x }
                    | "township" -> { place with township = x }
                    | "canton" -> { place with canton = x }
                    | "district" -> { place with district = x }
                    | "county" -> { place with county = x }
                    | "region" -> { place with region = x }
                    | "country" -> { place with country = x }
                    | _ ->
                        let other = place.other ^ " " ^ x in
                        { place with other }
                  in
                  loop list list_p place)
        in
        loop list list_p place
      in
      Some place
  | None -> None

let raw_string_of_place _conf place =
  List.fold_left (fun s c -> Name.strip_c s c) place [ '['; ']' ]

let string_of_place _conf place = raw_string_of_place _conf place |> escape_html
let menu_threshold = 20
let is_number t = match t.[0] with '1' .. '9' -> true | _ -> false

let hexa_string s =
  let s' = Bytes.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    Bytes.set s' (2 * i) "0123456789ABCDEF".[Char.code s.[i] / 16];
    Bytes.set s' ((2 * i) + 1) "0123456789ABCDEF".[Char.code s.[i] mod 16]
  done;
  Bytes.unsafe_to_string s'

let print_alphab_list conf crit print_elem liste =
  let len = List.length liste in
  (* No work to do if list is empty *)
  if liste = [] then Output.print_sstring conf "<ul></ul>\n"
  else (
    (* Print alphabetical index links at the top if we have many items *)
    if len > menu_threshold then (
      Output.print_sstring conf "<p>\n";

      (* Create index links without duplicates *)
      let rec print_index seen = function
        | [] -> ()
        | e :: rest ->
            let t = crit e in
            if not (List.mem t seen) then
              Output.printf conf "<a href=\"#ai%s\">%s</a>\n" (hexa_string t) t;
            print_index (t :: seen) rest
      in
      print_index [] liste;
      Output.print_sstring conf "</p>\n";
      Output.print_sstring conf "<ul>\n")
    else Output.print_sstring conf "<ul>\n";
    (* Group items by their criteria and print each group *)
    let rec process_groups current_group current_index = function
      | [] when current_group != [] ->
          (* Print the last group *)
          print_group (List.rev current_group) current_index
      | [] ->
          (* Empty list, nothing to do *)
          ()
      | e :: rest ->
          let t = crit e in
          (* If we're using numerical indexes or have many items, group by criteria *)
          if len > menu_threshold || is_number t then
            if current_index = None || Some t <> current_index then
              (* New group - print previous group if any, then start new group *)
              let () =
                if current_group <> [] then
                  print_group (List.rev current_group) current_index
              in
              process_groups [ e ] (Some t) rest
            else
              (* Continue same group *)
              process_groups (e :: current_group) current_index rest
          else
            (* Not grouping - print all items together *)
            print_group (e :: rest) (Some "")
    (* Print a group of items with the same criteria *)
    and print_group items index_opt =
      let index = match index_opt with Some t -> t | None -> "" in
      (* If we're grouping items, create a container with an anchor *)
      if len > menu_threshold && index <> "" then (
        Output.print_sstring conf "<li>\n";
        Output.printf conf "<a id=\"ai%s\">%s</a>\n" (hexa_string index) index;
        Output.print_sstring conf "<ul>\n");
      (* Print each item in the group *)
      List.iter
        (fun e ->
          Output.print_sstring conf "<li>\n  ";
          print_elem e;
          Output.print_sstring conf "</li>\n")
        items;
      (* Close the container if we opened one *)
      if len > menu_threshold && index <> "" then (
        Output.print_sstring conf "</ul>\n";
        Output.print_sstring conf "</li>\n")
    in
    process_groups [] None liste;
    Output.print_sstring conf "</ul>\n")

let relation_txt conf sex fam =
  let is = index_of_sex sex in
  match Driver.get_relation fam with
  | NotMarried | NoSexesCheckNotMarried ->
      ftransl_nth conf "relationship%t to" is
  | MarriageContract -> ftransl_nth conf "marriage contract%t with" is
  | MarriageLicense | Married | NoSexesCheckMarried ->
      ftransl_nth conf "married%t to" is
  | Engaged -> ftransl_nth conf "engaged%t to" is
  | MarriageBann -> ftransl_nth conf "marriage banns%t to" is
  | Pacs -> ftransl_nth conf "pacsed%t to" is
  | Residence -> ftransl_nth conf "residence%t to" is
  | NoMention -> "%t" ^^ ftransl conf "with"

let relation_date conf fam : Adef.safe_string =
  Adef.safe
  @@
  match Date.cdate_to_dmy_opt (Driver.get_marriage fam) with
  | None -> ""
  | Some dmy -> " " ^ transl conf "in (year)" ^ " " ^ string_of_int dmy.year

let child_of_parent conf base p =
  (* Si le père a un nom de famille différent de la personne *)
  (* alors on l'affiche, sinon on n'affiche que le prénom.   *)
  let print_father fath =
    if not (Driver.Istr.equal (Driver.get_surname p) (Driver.get_surname fath))
    then gen_person_text conf base fath
    else gen_person_text ~sn:false conf base fath
  in
  let a = pget conf base (Driver.get_iper p) in
  let ifam =
    match Driver.get_parents a with
    | Some ifam ->
        let cpl = Driver.foi base ifam in
        let fath =
          let fath = pget conf base (Driver.get_father cpl) in
          if Driver.p_first_name base fath = "?" then None else Some fath
        in
        let moth =
          let moth = pget conf base (Driver.get_mother cpl) in
          if Driver.p_first_name base moth = "?" then None else Some moth
        in
        Some (fath, moth)
    | None -> None
  in
  match ifam with
  | Some (None, None) | None -> Adef.safe ""
  | Some (fath, moth) ->
      let s =
        match (fath, moth) with
        | Some fath, None -> print_father fath
        | None, Some moth -> gen_person_text conf base moth
        | Some fath, Some moth ->
            print_father fath ^^^ " " ^<^ transl_nth conf "and" 0 ^<^ " "
            ^<^ gen_person_text conf base moth
        | _ -> Adef.safe ""
      in
      let is = index_of_sex (Driver.get_sex p) in
      let s = (s :> string) in
      transl_a_of_gr_eq_gen_lev conf
        (transl_nth conf "son/daughter/child" is)
        s s
      |> translate_eval |> Adef.safe

let husband_wife conf base p all =
  let multiple =
    let rec loop i kind =
      if i < Array.length (Driver.get_family p) then
        let fam = Driver.foi base (Driver.get_family p).(i) in
        let cur_type = Driver.get_relation fam in
        if i = 0 then loop (i + 1) cur_type
        else if cur_type = kind then loop (i + 1) kind
        else -1
      else i
    in
    loop 0 NoMention
  in
  let relation =
    if Array.length (Driver.get_family p) > 0 then
      if multiple >= 0 then
        let fam = Driver.foi base (Driver.get_family p).(0) in
        Printf.sprintf (relation_txt conf (Driver.get_sex p) fam) (fun () -> "")
        |> translate_eval |> Adef.safe
      else transl conf "marriages with" |> Adef.safe
    else Adef.safe ""
  in
  let nb_fam = Array.length (Driver.get_family p) in
  let res =
    let rec loop i res =
      if i < nb_fam then
        let fam = Driver.foi base (Driver.get_family p).(i) in
        let conjoint = Gutil.spouse (Driver.get_iper p) fam in
        let conjoint = pget conf base conjoint in
        if not @@ is_empty_name conjoint then
          let res =
            res
            ^>^ translate_eval
                  ((if nb_fam > 1 then Format.sprintf " &%d " (i + 1) else " ")
                   ^<^ gen_person_text conf base conjoint
                   ^^^ relation_date conf fam
                    :> string)
            ^ ","
          in
          if all then loop (i + 1) res else res
        else loop (i + 1) res ^>^ " ? ?,"
      else res
    in
    loop 0 relation
  in
  let res = (res :> string) in
  (* suppress last , *)
  let res =
    if String.length res > 1 && res.[String.length res - 1] = ',' then
      String.sub res 0 (String.length res - 1)
    else res
  in
  Adef.safe res

let first_child conf base p =
  let is = index_of_sex (Driver.get_sex p) in
  let rec loop i =
    if i < Array.length (Driver.get_family p) then
      let fam = Driver.foi base (Driver.get_family p).(i) in
      let ct = Driver.get_children fam in
      if Array.length ct > 0 then
        let enfant = pget conf base ct.(0) in
        let child =
          if is_hide_names conf enfant && not (authorized_age conf base enfant)
          then Adef.safe "xx"
          else if
            not
              (Driver.Istr.equal (Driver.get_surname p)
                 (Driver.get_surname enfant))
          then gen_person_text conf base enfant
          else gen_person_text ~sn:false conf base enfant
        in
        let child = (child :> string) in
        transl_a_of_b conf (transl_nth conf "father/mother" is) child child
        |> translate_eval |> Adef.safe
      else loop (i + 1)
    else Adef.safe ""
  in
  loop 0

let specify_homonymous conf base p specify_public_name =
  match (Driver.get_public_name p, Driver.get_qualifiers p) with
  | n, nn :: _ when Driver.sou base n <> "" && specify_public_name ->
      Output.print_sstring conf " ";
      Output.print_string conf (esc @@ Driver.sou base n);
      Output.print_sstring conf " <em>";
      Output.print_string conf (esc @@ Driver.sou base nn);
      Output.print_sstring conf "</em>"
  | _, nn :: _ when specify_public_name ->
      Output.print_sstring conf " ";
      Output.print_string conf (esc @@ Driver.p_first_name base p);
      Output.print_sstring conf " <em>";
      Output.print_string conf (esc @@ Driver.sou base nn);
      Output.print_sstring conf "</em>"
  | n, [] when Driver.sou base n <> "" && specify_public_name ->
      Output.print_sstring conf " ";
      Output.print_string conf (esc @@ Driver.sou base n)
  | _, _ ->
      (* Le nom public et le qualificatif ne permettent pas de distinguer *)
      (* la personne, donc on affiche les informations sur les parents,   *)
      (* le mariage et/ou le premier enfant.                              *)
      let cop = child_of_parent conf base p in
      if (cop :> string) <> "" then (
        Output.print_sstring conf ", ";
        Output.print_string conf cop);
      let hw = husband_wife conf base p true in
      if (hw :> string) = "" then (
        let fc = first_child conf base p in
        if (fc :> string) <> "" then (
          Output.print_sstring conf ", ";
          Output.print_string conf fc))
      else (
        Output.print_sstring conf ", ";
        Output.print_string conf hw)

let get_approx_date_place d1 (p1 : Adef.safe_string) d2 (p2 : Adef.safe_string)
    =
  match (d1, (p1 :> string), d2, (p2 :> string)) with
  | Some d, "", None, _ -> (Some d, p2)
  | Some d, "", Some x, y ->
      if y = "" then (Some d, Adef.safe "") else (Some x, p2)
  | Some d, _, _, _ -> (Some d, p1)
  | None, "", None, _ -> (None, p1)
  | None, "", Some x, _ -> (Some x, p2)
  | None, _, None, _ -> (None, p1)
  | None, _, Some x, y -> if y = "" then (Some x, p1) else (Some x, p2)

let get_approx_birth_date_place conf base p =
  let birth = Date.od_of_cdate (Driver.get_birth p) in
  let birth_place =
    string_of_place conf (Driver.sou base (Driver.get_birth_place p))
  in
  let baptism = Date.od_of_cdate (Driver.get_baptism p) in
  let baptism_place =
    string_of_place conf (Driver.sou base (Driver.get_baptism_place p))
  in
  get_approx_date_place birth
    (birth_place :> Adef.safe_string)
    baptism
    (baptism_place :> Adef.safe_string)

let get_approx_death_date_place conf base p =
  let death = Date.date_of_death (Driver.get_death p) in
  let death_place =
    string_of_place conf (Driver.sou base (Driver.get_death_place p))
  in
  let buri =
    match Driver.get_burial p with
    | Buried cd | Cremated cd -> Date.od_of_cdate cd
    | UnknownBurial -> None
  in
  let buri_place =
    string_of_place conf (Driver.sou base (Driver.get_burial_place p))
  in
  get_approx_date_place death
    (death_place :> Adef.safe_string)
    buri
    (buri_place :> Adef.safe_string)

let string_of_decimal_num conf f =
  let s = string_of_float f in
  let b = Buffer.create 20 in
  let rec loop i =
    if i = String.length s then Buffer.contents b
    else (
      (match s.[i] with
      | '.' ->
          if i = String.length s - 1 then ()
          else Buffer.add_string b (transl conf "(decimal separator)")
      | x -> Buffer.add_char b x);
      loop (i + 1))
  in
  loop 0

let find_person_in_env_aux conf base env_i env_p env_n env_occ =
  match p_getenv conf.env env_i with
  | Some i when i <> "" ->
      let i = Geneweb_db.Driver.Iper.of_string i in
      if Geneweb_db.Driver.iper_exists base i then
        let p = pget conf base i in
        if is_hidden p then None else Some p
      else None
  | _ -> (
      match (p_getenv conf.env env_p, p_getenv conf.env env_n) with
      | None, Some n -> (
          match Gutil.person_of_string_key base n with
          | Some ip ->
              let p = pget conf base ip in
              if is_hidden p then None
              else if (not (is_hide_names conf p)) || authorized_age conf base p
              then Some p
              else None
          | None -> None)
      | Some p, Some n -> (
          let occ = Option.value ~default:0 (p_getint conf.env env_occ) in
          match Driver.person_of_key base p n occ with
          | Some ip ->
              let p = pget conf base ip in
              if is_hidden p then None
              else if (not (is_hide_names conf p)) || authorized_age conf base p
              then Some p
              else None
          | None -> None)
      | _ -> None)

let find_person_in_env conf base suff =
  find_person_in_env_aux conf base ("i" ^ suff) ("p" ^ suff) ("n" ^ suff)
    ("oc" ^ suff)

let find_person_in_env_pref conf base pref =
  find_person_in_env_aux conf base (pref ^ "i") (pref ^ "p") (pref ^ "n")
    (pref ^ "oc")

let person_exists conf base (fn, sn, oc) =
  let auth =
    match Driver.person_of_key base fn sn oc with
    | Some ip -> authorized_age conf base (pget conf base ip)
    | None -> false
  in
  match List.assoc_opt "red_if_not_exist" conf.base_env with
  | Some "off" -> true
  | Some _ | None -> auth

let mark_if_not_public conf base (fn, sn, oc) =
  match p_getenv conf.env "red_if_not_public" with
  | Some "on" -> (
      match Driver.person_of_key base fn sn oc with
      | Some ip -> Driver.get_access (Driver.poi base ip) <> Public
      | None -> false)
  | _ -> false

let default_sosa_ref conf base =
  match List.assoc_opt "default_sosa_ref" conf.base_env with
  | Some n -> (
      if n = "" then None
      else
        match Gutil.person_ht_find_all base n with
        | [ ip ] ->
            let p = pget conf base ip in
            if is_hidden p then None else Some p
        | _ -> None)
  | None -> None

let find_sosa_ref conf base =
  match find_person_in_env conf base "z" with
  | Some p -> Some p
  | None -> default_sosa_ref conf base

let write_default_sosa conf key =
  let gwf =
    List.fold_left
      (fun acc (k, v) ->
        if k = "default_sosa_ref" then ("default_sosa_ref", key) :: acc
        else (k, v) :: acc)
      [] (List.rev conf.base_env)
  in
  let fname = !GWPARAM.config conf.bname in
  let tmp_fname = fname ^ "2" in
  let oc =
    try Stdlib.open_out tmp_fname
    with Sys_error _ -> failwith "the gwf file is not writable"
  in
  List.iter (fun (k, v) -> Stdlib.output_string oc (k ^ "=" ^ v ^ "\n")) gwf;
  close_out oc;
  Mutil.rm (fname ^ "~");
  Sys.rename fname (fname ^ "~");
  try Sys.rename tmp_fname fname with Sys_error _ -> ()

let update_gwf_sosa conf base (ip, (fn, sn, occ)) =
  let sosa_ref_key =
    match snd conf.default_sosa_ref with
    | Some p ->
        Driver.p_first_name base p ^ "."
        ^ string_of_int (Driver.get_occ p)
        ^ " " ^ Driver.p_surname base p
    | None -> ""
  in
  let new_key = fn ^ "." ^ string_of_int occ ^ " " ^ sn in
  if ip = fst conf.default_sosa_ref && new_key != sosa_ref_key then
    write_default_sosa conf new_key

let create_topological_sort conf base =
  match p_getenv conf.env "opt" with
  | Some "no_tsfile" ->
      let () = Driver.load_ascends_array base in
      let () = Driver.load_couples_array base in
      Consang.topological_sort base (pget conf)
  | Some "no_tstab" -> Driver.iper_marker (Driver.ipers base) 0
  | _ ->
      let bfile = bpath (conf.bname ^ ".gwb") in
      let tstab_file =
        if conf.use_restrict && (not conf.wizard) && not conf.friend then
          Filename.concat bfile "tstab_visitor"
        else Filename.concat bfile "tstab"
      in
      Mutil.read_or_create_value ~magic:Mutil.executable_magic tstab_file
        (fun () ->
          Driver.load_ascends_array base;
          Driver.load_couples_array base;
          let tstab = Consang.topological_sort base (pget conf) in
          (* FIXME: we silently ignores error if we cannot lock the database. *)
          let on_exn _exn _bt = () in
          if conf.use_restrict && (not conf.wizard) && not conf.friend then
            Lock.control ~on_exn ~wait:false ~lock_file:(Mutil.lock_file bfile)
              (fun () -> Driver.base_visible_write base);
          tstab)

let p_of_sosa conf base sosa p0 =
  let path = Sosa.branches sosa in
  let rec aux acc = function
    | [] -> Some acc
    | hd :: tl -> (
        match Driver.get_parents acc with
        | Some ifam ->
            let cpl = Driver.foi base ifam in
            if hd = 0 then aux (pget conf base (Driver.get_father cpl)) tl
            else aux (pget conf base (Driver.get_mother cpl)) tl
        | None -> None)
  in
  aux p0 path

let branch_of_sosa conf base sosa p =
  if Sosa.eq sosa Sosa.zero then invalid_arg "branch_of_sosa";
  let rec expand bl sosa =
    if Sosa.eq sosa Sosa.one then bl
    else expand (Sosa.even sosa :: bl) (Sosa.half sosa)
  in
  let rec loop pl p = function
    | [] -> Some (p :: pl)
    | male :: tl -> (
        match Driver.get_parents p with
        | Some ifam ->
            let cpl = Driver.foi base ifam in
            if male then
              loop (p :: pl) (pget conf base @@ Driver.get_father cpl) tl
            else loop (p :: pl) (pget conf base @@ Driver.get_mother cpl) tl
        | _ -> None)
  in
  loop [] p (expand [] sosa)

let sosa_of_branch ipl =
  if ipl = [] then failwith "sosa_of_branch";
  let ipl = List.tl (List.rev ipl) in
  List.fold_left
    (fun b p ->
      let b = Sosa.twice b in
      match Driver.get_sex p with
      | Male -> b
      | Female -> Sosa.inc b 1
      | Neuter -> assert false)
    Sosa.one ipl

(* FIXME: remove this and use sosa_of_branch only *)
let old_sosa_of_branch conf base (ipl : (Driver.iper * sex) list) =
  sosa_of_branch (List.map (fun (ip, _) -> pget conf base ip) ipl)

(* FIXME: remove this and use branch_of_sosa only *)
let old_branch_of_sosa conf base ip sosa =
  branch_of_sosa conf base sosa (pget conf base ip)
  |> Option.map @@ List.map (fun p -> (Driver.get_iper p, Driver.get_sex p))

let gen_only_printable or_nl s =
  let s' =
    let conv_char i =
      if Char.code s.[i] > 127 then s.[i]
      else
        match s.[i] with
        | ' ' .. '~' | '\160' .. '\255' -> s.[i]
        | '\n' -> if or_nl then '\n' else ' '
        | _ -> ' '
    in
    String.init (String.length s) conv_char
  in
  String.trim s'

let only_printable_or_nl = gen_only_printable true
let only_printable = gen_only_printable false

let relation_type_text conf t n =
  match t with
  | Adoption ->
      transl_nth conf "adoptive father/adoptive mother/adoptive parents" n
      |> Adef.safe
  | Recognition ->
      transl_nth conf
        "recognizing father/recognizing mother/recognizing parents" n
      |> Adef.safe
  | CandidateParent ->
      transl_nth conf "candidate father/candidate mother/candidate parents" n
      |> Adef.safe
  | GodParent -> transl_nth conf "godfather/godmother/godparents" n |> Adef.safe
  | FosterParent ->
      transl_nth conf "foster father/foster mother/foster parents" n
      |> Adef.safe

let rchild_type_text conf t n =
  match t with
  | Adoption ->
      transl_nth conf "adoptive son/adoptive daughter/adoptive child" n
      |> Adef.safe
  | Recognition ->
      transl_nth conf "recognized son/recognized daughter/recognized child" n
      |> Adef.safe
  | CandidateParent ->
      transl_nth conf "candidate son/candidate daughter/candidate child" n
      |> Adef.safe
  | GodParent -> transl_nth conf "godson/goddaughter/godchild" n |> Adef.safe
  | FosterParent ->
      transl_nth conf "foster son/foster daughter/foster child" n |> Adef.safe

exception Ok

let has_nephews_or_nieces conf base p =
  try
    let a = p in
    match Driver.get_parents a with
    | Some ifam ->
        let fam = Driver.foi base ifam in
        Array.iter
          (fun ip ->
            if ip = Driver.get_iper p then ()
            else
              Array.iter
                (fun ifam ->
                  if
                    Array.length (Driver.get_children (Driver.foi base ifam))
                    > 0
                  then raise Ok)
                (Driver.get_family (pget conf base ip)))
          (Driver.get_children fam);
        false
    | _ -> false
  with Ok -> true

let h s = Digest.to_hex (Digest.string s)

let is_that_user_and_password auth_scheme user passwd =
  match auth_scheme with
  | NoAuth -> false
  | TokenAuth ts -> user = ts.ts_user && passwd = ts.ts_pass
  | HttpAuth (Basic bs) -> user = bs.bs_user && passwd = bs.bs_pass
  | HttpAuth (Digest ds) ->
      if user <> ds.ds_username then false
      else
        let that_response_would_be =
          let a1 = Printf.sprintf "%s:%s:%s" user ds.ds_realm passwd in
          let a2 = Printf.sprintf "%s:%s" ds.ds_meth ds.ds_uri in
          if ds.ds_qop = "auth" || ds.ds_qop = "auth-int" then
            h
              (h a1 ^ ":" ^ ds.ds_nonce ^ ":" ^ ds.ds_nc ^ ":" ^ ds.ds_cnonce
             ^ ":" ^ ds.ds_qop ^ ":" ^ h a2)
          else h (h a1 ^ ":" ^ ds.ds_nonce ^ ":" ^ h a2)
        in
        that_response_would_be = ds.ds_response

let browser_doesnt_have_tables conf =
  let user_agent = Mutil.extract_param "user-agent: " '/' conf.request in
  String.lowercase_ascii user_agent = "lynx"

let of_course_died conf p =
  match Date.cdate_to_dmy_opt (Driver.get_birth p) with
  | Some d ->
      (* TODO this value should be defined elsewhere *)
      conf.today.year - d.year > conf.private_years + 20
  | None -> false

let escache_value base =
  let t = Driver.date_of_last_change base in
  let v = int_of_float (mod_float t (float_of_int max_int)) in
  Adef.encoded (string_of_int v)

let sprintf_today conf =
  let hh, mm, ss = conf.time in
  let tm =
    Unix.
      {
        tm_year = conf.today.year - 1900;
        tm_mon = conf.today.month - 1;
        tm_mday = conf.today.day;
        tm_hour = hh;
        tm_min = mm;
        tm_sec = ss;
        tm_wday = -1;
        tm_yday = -1;
        tm_isdst = false;
      }
  in
  Mutil.sprintf_date tm

let read_wf_trace fname =
  try
    let ic = Secure.open_in fname in
    let rec loop acc =
      match input_line ic with
      | line -> loop (line :: acc)
      | exception End_of_file ->
          close_in ic;
          List.rev acc
    in
    loop []
  with Sys_error _ -> []

let write_wf_trace fname wt =
  let oc = Secure.open_out fname in
  List.iter (fun (dt, u) -> Printf.fprintf oc "%s %s\n" dt u) wt;
  close_out oc

let update_wf_trace conf fname =
  let dt = (sprintf_today conf :> string) in
  let wt =
    let r = read_wf_trace fname in
    let dtlen = String.length dt in
    let rec loop found r = function
      | x :: l ->
          if String.length x > dtlen + 2 then
            let u = String.sub x (dtlen + 1) (String.length x - dtlen - 1) in
            if u = conf.user then loop true ((dt, u) :: r) l
            else loop found ((String.sub x 0 dtlen, u) :: r) l
          else loop found r l
      | [] -> if found then r else (dt, conf.user) :: r
    in
    loop false [] r
  in
  write_wf_trace fname (List.sort (fun x y -> compare y x) wt)

let test_cnt_d conf =
  let config_d = !GWPARAM.config_d conf.bname in
  let cnt_d = !GWPARAM.cnt_d conf.bname in
  (if not (Sys.file_exists config_d) then
     try Unix.mkdir config_d 0o755
     with Unix.Unix_error (_, _, _) ->
       Logs.syslog `LOG_WARNING
         (Printf.sprintf "Failure when creating config_dir (util): %s" config_d));
  if not (Sys.file_exists cnt_d) then
    try Unix.mkdir cnt_d 0o755
    with Unix.Unix_error (_, _, _) ->
      Logs.syslog `LOG_WARNING
        (Printf.sprintf "Failure when creating cnt_dir (util): %s" cnt_d)
  else ();
  cnt_d

let commit_patches conf base =
  let _ = test_cnt_d in
  Driver.commit_patches base;
  conf.henv <-
    List.map
      (fun (k, v) -> if k = "escache" then (k, escache_value base) else (k, v))
      conf.henv;
  if conf.user <> "" then
    let wpf =
      try List.assoc "wizard_passwd_file" conf.base_env with Not_found -> ""
    in
    if wpf <> "" then
      let fname = !GWPARAM.adm_file (conf.bname ^ "_u.txt") in
      update_wf_trace conf fname

let short_f_month m =
  match m with
  | 1 -> "VD"
  | 2 -> "BR"
  | 3 -> "FM"
  | 4 -> "NI"
  | 5 -> "PL"
  | 6 -> "VT"
  | 7 -> "GE"
  | 8 -> "FL"
  | 9 -> "PR"
  | 10 -> "ME"
  | 11 -> "TH"
  | 12 -> "FT"
  | 13 -> "JC"
  | _ -> ""

(* reading password file *)

type auth_user = { au_user : string; au_passwd : string; au_info : string }

let read_gen_auth_file fname base_file =
  let fname =
    if GWPARAM.is_reorg_base base_file then
      Filename.concat (!GWPARAM.config_d base_file) fname
    else Filename.concat (Secure.base_dir ()) fname
  in
  try
    let ic = Secure.open_in fname in
    let rec loop data =
      match input_line ic with
      | line ->
          let len = String.length line in
          let data =
            match String.index_opt line ':' with
            | Some i ->
                let user = String.sub line 0 i in
                let j =
                  try String.index_from line (i + 1) ':' with Not_found -> len
                in
                let passwd = String.sub line (i + 1) (j - i - 1) in
                let rest =
                  if j = len then "" else String.sub line (j + 1) (len - j - 1)
                in
                let au =
                  { au_user = user; au_passwd = passwd; au_info = rest }
                in
                au :: data
            | None -> data
          in
          loop data
      | exception End_of_file ->
          close_in ic;
          List.rev data
    in
    loop []
  with Sys_error _ -> []

let start_equiv_with case_sens s m i =
  let rec test i j =
    if j = String.length s then Some i
    else if i = String.length m then None
    else if case_sens then if m.[i] = s.[j] then test (i + 1) (j + 1) else None
    else
      match Name.next_chars_if_equiv m i s j with
      | Some (i, j) -> test i j
      | None -> None
  in
  if case_sens then if m.[i] = s.[0] then test (i + 1) 1 else None
  else
    match Name.next_chars_if_equiv m i s 0 with
    | Some (i, j) -> test i j
    | None -> None

let rec in_text case_sens s m =
  let rec loop in_tag i =
    if i = String.length m then false
    else if in_tag then loop (m.[i] <> '>') (i + 1)
    else if m.[i] = '<' then loop true (i + 1)
    else if m.[i] = '[' && i + 1 < String.length m && m.[i + 1] = '[' then
      match NotesLinks.misc_notes_link m i with
      | NotesLinks.WLpage (j, _, _, _, text)
      | NotesLinks.WLperson (j, _, Some text, _)
      | NotesLinks.WLwizard (j, _, text) ->
          if in_text case_sens s text then true else loop false j
      | NotesLinks.WLperson (j, (fn, sn, _), None, _) ->
          if in_text case_sens s (fn ^ " " ^ sn) then true else loop false j
      | NotesLinks.WLnone (j, _) -> loop false j
    else
      match start_equiv_with case_sens s m i with
      | Some _ -> true
      | None -> loop false (i + 1)
  in
  loop false 0

let html_highlight case_sens h s =
  let ht i j = "<span class=\"found\">" ^ String.sub s i (j - i) ^ "</span>" in
  let rec loop in_tag i len =
    if i = String.length s then Buff.get len
    else if in_tag then loop (s.[i] <> '>') (i + 1) (Buff.store len s.[i])
    else if s.[i] = '<' then loop true (i + 1) (Buff.store len s.[i])
    else
      match start_equiv_with case_sens h s i with
      | Some j -> loop false j (Buff.mstore len (ht i j))
      | None -> loop false (i + 1) (Buff.store len s.[i])
  in
  loop false 0 0

(* Print list in columns with Gutil.alphabetic order *)

type elem_kind = HeadElem | ContElem | Elem

let kind_size = function HeadElem | ContElem -> 4 | Elem -> 1

let dispatch_in_columns ncol list order =
  let rlist =
    List.fold_left
      (fun rlist elem ->
        let ord = order elem in
        let kind =
          match rlist with
          | (_, prev_ord, _prev_elem) :: _ ->
              if
                ord = prev_ord
                || (ord <> "" && prev_ord <> "" && ord.[0] = prev_ord.[0])
              then Elem
              else HeadElem
          | [] -> HeadElem
        in
        (ref kind, ord, elem) :: rlist)
      [] list
  in
  let ini_list, ini_len =
    List.fold_left
      (fun (list, len) ((kind, _, _) as elem) ->
        (elem :: list, len + kind_size !kind))
      ([], 0) rlist
  in
  let len_list =
    let rec loop rlen_list cnt col accu len list =
      if col > ncol then List.rev rlen_list
      else
        let list, kind, is_last =
          match list with
          | (kind, _, _) :: list -> (list, kind, false)
          | [] -> ([], ref Elem, true)
        in
        let accu = accu + (ncol * kind_size !kind) in
        let cnt = cnt + 1 in
        if accu > len && (not is_last) && !kind = Elem then (
          (* put a new size and restart from zero *)
          kind := ContElem;
          loop [] 0 1 0 (len + kind_size ContElem - 1) ini_list)
        else
          let rlen_list, cnt, col, accu =
            if accu > len && cnt > 1 then
              ((cnt - 1) :: rlen_list, 1, col + 1, accu - len)
            else (rlen_list, cnt, col, accu)
          in
          loop rlen_list cnt col accu len list
    in
    loop [] 0 1 0 ini_len ini_list
  in
  (len_list, ini_list)

let print_in_columns conf ncols len_list list wprint_elem =
  begin_centered conf;
  Output.printf conf "<table width=\"95%%\" border=\"%d\">\n" conf.border;
  Output.printf conf "<tr align=\"%s\" valign=\"top\">\n" conf.left;
  (let _ =
     List.fold_left
       (fun (list, _first) len ->
         let rec loop n list =
           if n = 0 then (
             Output.print_sstring conf "</ul>\n</td>\n";
             (list, false))
           else
             match list with
             | (kind, ord, elem) :: list ->
                 if n = len then
                   Output.printf conf "<td width=\"%d\">\n" (100 / ncols)
                 else if !kind <> Elem then Output.print_sstring conf "</ul>\n";
                 if !kind <> Elem then (
                   Output.printf conf "<h3 class=\"subtitle mx-3\">%s%s</h3>\n"
                     (if ord = "" then "..." else String.make 1 ord.[0])
                     (if !kind = HeadElem then ""
                      else " (" ^ transl conf "continued" ^ ")");
                   Output.print_sstring conf "<ul>\n");
                 Output.print_sstring conf "<li>";
                 wprint_elem elem;
                 Output.print_sstring conf "</li>\n";
                 loop (n - 1) list
             | [] -> ([], false)
         in
         loop len list)
       (list, true) len_list
   in
   ());
  Output.print_sstring conf "</tr>\n";
  Output.print_sstring conf "</table>\n";
  end_centered conf

let wprint_in_columns conf order wprint_elem list =
  let ncols =
    match p_getint conf.env "ncols" with
    | Some n -> max 1 n
    | None ->
        let len_list = List.length list in
        if len_list < 10 then 1
        else if len_list < 100 then 2
        else if len_list < 200 then 3
        else 4
  in
  let len_list, list = dispatch_in_columns ncols list order in
  print_in_columns conf ncols len_list list wprint_elem

(* ********************************************************************** *)
(*  [Fonc] reduce_list : int -> list 'a -> list 'a                        *)

(* ********************************************************************** *)

(** [Description] : Retourne la sous liste de taille size composée des éléments
    0 à (size - 1) [Args] :
    - size : la taille de la nouvelle liste
    - list : la liste originiale [Retour] :
    - list : la nouvelle liste de taille size [Rem] : Exporté en clair hors de
      ce module. *)
let reduce_list size list =
  let rec loop size cnt reduced_list list =
    if cnt >= size then reduced_list
    else
      match list with
      | [] -> reduced_list
      | x :: l -> loop size (cnt + 1) (x :: reduced_list) l
  in
  let sublist = loop size 0 [] list in
  List.rev sublist

(* ********************************************************************** *)
(*  [Fonc] gen_print_tips : conf -> string -> unit                        *)

(* ********************************************************************** *)

(** [Description] : Affiche un tips. [Args] :
    - conf : configuration de la base
    - s : le contenu du tips [Retour] : Néant [Rem] : Non exporté en clair hors
      de ce module. *)
let gen_print_tips conf s =
  Output.print_sstring conf "<div class=\"tips alert alert-warning\"";
  Output.print_sstring conf " role=\"alert\">";
  Output.print_string conf s;
  Output.print_sstring conf "</div>"

let print_tips_relationship conf =
  if p_getenv conf.env "em" = Some "R" || p_getenv conf.env "m" = Some "C" then
    Utf8.capitalize_fst (transl conf "select person to compute relationship")
    |> Adef.safe |> gen_print_tips conf

let images_prefix conf =
  let s =
    if conf.cgi then Adef.escaped conf.images_prefix else Adef.escaped "images"
  in
  (s :> string)

(* ********************************************************************** *)
(*  [Fonc] display_options : config -> string                             *)

(* ********************************************************************** *)

let get_opt conf evar default =
  match evar with
  | "im" -> (
      match (p_getenv conf.env "im", p_getenv conf.env "image") with
      | Some ("off" | "0"), _ | _, Some "off" -> not default
      | _, _ -> default)
  | "sp" -> (
      match (p_getenv conf.env "sp", p_getenv conf.env "spouse") with
      | Some ("off" | "0"), _ | _, Some "off" -> not default
      | _, _ -> default)
  | "ma" -> (
      match (p_getenv conf.env "ma", p_getenv conf.env "marriage") with
      | Some ("off" | "0"), _ | _, Some "off" -> not default
      | _, _ -> default)
  | _ -> failwith "bad get_opt parameter"

(** [Description] : Recherche dans l'URL les options d'affichage qui sont
    données et renvoie la concaténation de ces options. [Args] :
    - conf : configuration de la base [Retour] : string [Rem] : Exporté en clair
      hors de ce module. *)
let display_options conf =
  let img = get_opt conf "im" true in
  let mar = get_opt conf "ma" true in
  let s = Adef.escaped @@ if img then "" else "&im=0" in
  let s = if mar then s else s ^>^ "&ma=0" in
  let s =
    match p_getenv conf.env "bd" with
    | Some i -> s ^^^ "&bd=" ^<^ (Mutil.encode i :> Adef.escaped_string)
    | None -> s
  in
  match p_getenv conf.env "color" with
  | Some c -> s ^^^ "&color=" ^<^ (Mutil.encode c :> Adef.escaped_string)
  | None -> s

(* Hashtbl qui associe un user à la liste des dernières personnes visitées. *)
(* On en profite aussi pour stocker la date de la dernière visite.          *)
type cache_visited_t = (string, (Driver.iper * string) list) Hashtbl.t

(* ************************************************************************ *)
(*  [Fonc] cache_visited : config -> string                                 *)

(* ************************************************************************ *)

(** [Description] : Renvoie le chemin du fichier de cache. [Args] :
    - config : configuration de la base [Retour] : unit [Rem] : Exporté en clair
      hors de ce module. *)
let cache_visited conf =
  let bname =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  Filename.concat (bpath bname) "cache_visited"

(* ************************************************************************ *)
(*  [Fonc] read_visited : string -> cache_visited_t                         *)

(* ************************************************************************ *)

(** [Description] : List le fichier de cache des dernières fiches visités.
    [Args] :
    - fname : le fichier de cache (qui se trouve dans base.gwb) [Retour] :
      Hashtbl des user => dernières visites [Rem] : Exporté en clair hors de ce
      module. *)
let read_visited conf =
  let fname = cache_visited conf in
  try
    let ic = Secure.open_in_bin fname in
    let ht : cache_visited_t = input_value ic in
    close_in ic;
    ht
  with Sys_error _ -> Hashtbl.create 0

(* ************************************************************************ *)
(*  [Fonc] write_visited : string -> Hashtbl.t string (list iper) -> unit   *)

(* ************************************************************************ *)

(** [Description] : Met à jour le fichier de cache des visites. [Args] :
    - fname : le fichier de cache (qui se trouve dans base.gwb)
    - ht : le compteur de visite [Retour] : unit [Rem] : Non exporté en clair
      hors de ce module. *)
let write_visited conf ht =
  let fname = cache_visited conf in
  try
    let oc = Secure.open_out_bin fname in
    output_value oc ht;
    close_out oc
  with Sys_error _ -> ()

(* ************************************************************************ *)
(*  [Fonc] record_visited : config -> iper -> unit                          *)

(* ************************************************************************ *)

(** [Description] : Vérifie si le user est ami ou magicien et met à jour le
    fichier de cache. [Args] :
    - conf : configuration de la base
    - ip : iper [Retour] : unit [Rem] : Exporté en clair hors de ce module. *)
let record_visited conf ip =
  if conf.friend || conf.wizard then
    let ht = read_visited conf in
    let time = (sprintf_today conf :> string) in
    let () =
      try
        let vl = Hashtbl.find ht conf.user in
        let vl = (ip, time) :: vl in
        (* On rend la liste unique sur les ip. *)
        let uniq = function
          | ([ _ ] | []) as l -> l
          | ((ip, _) as x) :: l ->
              let rec loop rl x = function
                | ((ip2, _) as y) :: l ->
                    if ip = ip2 then loop rl x l else loop (x :: rl) y l
                | [] -> List.rev (x :: rl)
              in
              loop [] x l
        in
        let vl = uniq vl in
        let vl = reduce_list 10 vl in
        Hashtbl.replace ht conf.user vl
      with Not_found -> Hashtbl.add ht conf.user [ (ip, time) ]
    in
    write_visited conf ht

(**/**)

(* TODO OCaml 4.13 : use Array.find_opt *)
let array_mem_witn conf base x a =
  let rec loop i =
    if i = Array.length a then None
    else if x = fst a.(i) then
      Some
        (string_of_witness_kind conf
           (Driver.get_sex @@ Driver.poi base x)
           (snd a.(i)))
    else loop (i + 1)
  in
  loop 0

let nb_char_occ c s =
  let cnt = ref 0 in
  String.iter (fun x -> if x = c then incr cnt) s;
  !cnt

let select_masc conf base ips =
  let poi = if conf.wizard || conf.friend then Driver.poi else pget conf in
  let fam = Hashtbl.create 1024 in
  let asc = Hashtbl.create 1024 in
  let add_asc gen i p =
    match Hashtbl.find_opt asc i with
    | Some (already, _) when already <= gen -> ()
    | _ -> Hashtbl.replace asc i (gen, p)
  in
  let select_masc max_gen =
    let rec loop = function
      | [] -> ()
      | (gen, ifam) :: tl -> (
          match Hashtbl.find_opt fam ifam with
          | Some already when already <= gen -> loop tl
          | _ ->
              Hashtbl.replace fam ifam gen;
              if gen = max_gen then (
                let cpl = Driver.foi base ifam in
                let fa = Driver.get_father cpl in
                let mo = Driver.get_mother cpl in
                add_asc gen fa (poi base fa);
                add_asc gen mo (poi base mo);
                loop tl)
              else
                let pgen = gen + 1 in
                let aux acc i =
                  let p = poi base i in
                  match Driver.get_parents p with
                  | None ->
                      add_asc gen i p;
                      acc
                  | Some pifam -> (
                      match Hashtbl.find_opt fam pifam with
                      | Some already when already <= pgen -> acc
                      | _ ->
                          Hashtbl.replace fam pifam (pgen + 1);
                          (pgen, pifam) :: acc)
                in
                let cpl = Driver.foi base ifam in
                let fa = Driver.get_father cpl in
                let mo = Driver.get_mother cpl in
                loop (aux (aux tl fa) mo))
    in
    loop
  in
  List.iter
    (fun (ip, max_gen) ->
      match Driver.get_parents @@ poi base ip with
      | Some ifam -> select_masc max_gen [ (1, ifam) ]
      | None -> ())
    ips;
  asc

let select_desc conf base gen_desc ips =
  let desc = Hashtbl.create 64 in
  let skip = Hashtbl.create 64 in
  let rec loop_desc gen ip =
    if not @@ Hashtbl.mem skip ip then (
      let p = pget conf base ip in
      Hashtbl.add skip ip true;
      Hashtbl.replace desc ip p;
      Array.iter
        (fun ifam ->
          let sp = Gutil.spouse ip (Driver.foi base ifam) in
          Hashtbl.replace desc sp (pget conf base sp))
        (Driver.get_family p);
      if gen > gen_desc then
        List.iter (loop_desc (gen - 1)) @@ Driver.children_of_p base p)
  in
  List.iter (fun (ip, gen) -> loop_desc gen ip) ips;
  desc

let select_mascdesc conf base ips gen_desc =
  let asc = select_masc conf base ips in
  let ips = Hashtbl.fold (fun ip (gen, _) acc -> (ip, gen) :: acc) asc [] in
  let r = select_desc conf base gen_desc ips in
  r

let auth_warning conf base w =
  let pauth p = authorized_age conf base p in
  let fauth ifam =
    let fam = Driver.foi base ifam in
    pauth (Driver.get_father fam |> Driver.poi base)
    && pauth (Driver.get_mother fam |> Driver.poi base)
  in
  match w with
  | BigAgeBetweenSpouses (p1, p2, _) -> pauth p1 && pauth p2
  | BirthAfterDeath p -> pauth p
  | ChildrenNotInOrder (ifam, _, elder, x) ->
      pauth elder && pauth x && fauth ifam
  | CloseChildren (ifam, c1, c2) -> pauth c1 && pauth c2 && fauth ifam
  | DeadOld (p, _) -> pauth p
  | DeadTooEarlyToBeFather (father, child) -> pauth father && pauth child
  | DistantChildren (ifam, p1, p2) -> pauth p1 && pauth p2 && fauth ifam
  | FEventOrder (p, _, _) -> pauth p
  | FWitnessEventAfterDeath (p, _, fam) -> pauth p && fauth fam
  | FWitnessEventBeforeBirth (p, _, fam) -> pauth p && fauth fam
  | IncoherentSex (p, _, _) -> pauth p
  | IncoherentAncestorDate (anc, p) -> pauth anc && pauth p
  | MarriageDateAfterDeath p -> pauth p
  | MarriageDateBeforeBirth p -> pauth p
  | MotherDeadBeforeChildBirth (mother, child) -> pauth mother && pauth child
  | ParentBornAfterChild (parent, child) -> pauth parent && pauth child
  | ParentTooOld (p, _, c) -> pauth p && pauth c
  | ParentTooYoung (p, _, c) -> pauth p && pauth c
  | PossibleDuplicateFam (f1, f2) -> fauth f1 && fauth f2
  | PossibleDuplicateFamHomonymous (f1, f2, p) ->
      fauth f1 && fauth f2 && pauth p
  | PEventOrder (p, _, _) -> pauth p
  | PWitnessEventAfterDeath (p, _, origin) -> pauth p && pauth origin
  | PWitnessEventBeforeBirth (p, _, origin) -> pauth p && pauth origin
  | TitleDatesError (p, _) -> pauth p
  | UndefinedSex p -> pauth p
  | YoungForMarriage (_, _, fam) -> fauth fam
  | OldForMarriage (_, _, fam) -> fauth fam
  | ChangedOrderOfChildren _ | ChangedOrderOfMarriages _
  | ChangedOrderOfFamilyEvents _ | ChangedOrderOfPersonEvents _ ->
      false

let name_with_roman_number str =
  let rec loop found len i =
    if i = String.length str then if found then Some (Buff.get len) else None
    else
      match str.[i] with
      | '0' .. '9' as c ->
          let n, i =
            let rec loop n i =
              if i = String.length str then (n, i)
              else
                match str.[i] with
                | '0' .. '9' as c ->
                    loop ((10 * n) + Char.code c - Char.code '0') (i + 1)
                | _ -> (n, i)
            in
            loop (Char.code c - Char.code '0') (i + 1)
          in
          loop true (Buff.mstore len (Mutil.roman_of_arabian n)) i
      | c -> loop found (Buff.store len c) (i + 1)
  in
  loop false 0 0

let cut_words str =
  let rec loop beg i =
    if i < String.length str then
      match str.[i] with
      | ' ' ->
          if beg = i then loop (succ beg) (succ i)
          else String.sub str beg (i - beg) :: loop (succ i) (succ i)
      | _ -> loop beg (succ i)
    else if beg = i then []
    else [ String.sub str beg (i - beg) ]
  in
  loop 0 0

let designation base p = Gutil.designation base p |> escape_html

let has_children base u =
  Array.exists
    (fun ifam ->
      let des = Driver.foi base ifam in
      Array.length (Driver.get_children des) > 0)
    (Driver.get_family u)

let get_bases_list ?(format_fun = fun x -> x) () =
  let list = ref [] in
  let dh = Unix.opendir (Secure.base_dir ()) in
  (try
     while true do
       let e = Unix.readdir dh in
       if Filename.check_suffix e ".gwb" then
         list := format_fun (Filename.chop_suffix e ".gwb") :: !list
     done
   with End_of_file -> ());
  Unix.closedir dh;
  list := List.sort compare !list;
  !list

let extract_value delimiter s =
  let len = String.length s in
  let rec loop equal_pos i =
    if i = len then equal_pos
    else
      let c = s.[i] in
      match equal_pos with
      | Some _ when c = delimiter ->
          (* The line contains several delimiters. *)
          None
      | None when c = delimiter ->
          (* We found the first delimiter. *)
          loop (Some i) (i + 1)
      | _ -> loop equal_pos (i + 1)
  in
  match loop None 0 with
  | Some i -> String.sub s (i + 1) (len - i - 1)
  | None -> raise Not_found

let sys_to_note_link p =
  let dir_sep = Filename.dir_sep.[0] in
  String.split_on_char dir_sep p
  |> String.concat (String.make 1 NotesLinks.char_dir_sep)

let note_link_to_sys p =
  String.split_on_char NotesLinks.char_dir_sep p
  |> String.concat Filename.dir_sep
