(* Copyright (c) 1998-2007 INRIA *)

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

(** [escape_html str] replaces '&', '"', '<' and '>'
    with their corresponding character entities (using entity number) *)
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

(** [escape_attribute str] only escapes double quote and ampersand.
    Since we will return normalized HTML, ['"'] should be the only
    dangerous character here. *)
let escape_attribute =
  escape_aux
    (function '&' | '"' -> 5 (* "&#xx;" *) | _ -> 1)
    (fun buf ibuf istr loop -> function
      | '&' ->
          Bytes.blit_string "&#38;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | '"' ->
          Bytes.blit_string "&#34;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | c ->
          Bytes.unsafe_set buf ibuf c;
          loop (istr + 1) (ibuf + 1))

(* aswer the question "should we show p's names and other private stuffs?";
   takes into account if you are a wizard or not *)
(* TODO ??
   - change conf.hide_names to not take in account wizard|friend;
   - take into account wizard|friend in this function
   - and authorized_age
   - rename to is_hidden
   ??
*)
(* it is always combined with a p_auth or authorized_age
   TODO : diff between p_auth and authorized_age *)
(* TODO why not conf.hide_names && not Util.is_public *)
(* bug: is_hide_names if true if person is Private but with age > private_year *)
let is_hide_names conf p =
  conf.Config.hide_private_names
  && not (conf.Config.wizard || conf.Config.friend)
  || Gwdb.get_access p = Def.Private

let cnt_dir = ref Filename.current_dir_name

let search_in_path p s =
  let rec loop = function
    | d :: dl ->
        let f = Filename.concat d s in
        if Sys.file_exists f then f else loop dl
    | [] -> s
  in
  loop (p ())

let search_in_assets = search_in_path Secure.assets

(* Internationalization *)

let start_with_vowel s =
  if String.length s > 0 then
    let s, _ = Utf8.unaccent true s 0 in
    match s.[0] with 'a' | 'e' | 'i' | 'o' | 'u' -> true | _ -> false
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

let transl conf w =
  try Hashtbl.find conf.Config.lexicon w with Not_found -> tnf w

let transl_nth conf w n =
  try nth_field (Hashtbl.find conf.Config.lexicon w) n
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

let gen_decline wt s1 s2 s2_raw =
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
                      if start_with_vowel s2_raw then
                        String.sub wt (k + 1) (j - k - 1) ^ s (* [aa|bb]  *)
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
  gen_decline (transl_nth conf "%1 of %2" 0) x y1 y2

let transl_a_of_gr_eq_gen_lev conf x y1 y2 =
  gen_decline (transl_nth conf "%1 of %2" 1) x y1 y2

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
  let referer = Mutil.extract_param "referer: " '\n' conf.Config.request in
  escape_html referer

let begin_centered conf =
  Output.printf conf
    "<table border=\"%d\" width=\"100%%\"><tr><td align=\"center\">\n"
    conf.Config.border

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
  let lt = Unix.gmtime conf.Config.ctime in
  Printf.sprintf "%s, %d %s %d %02d:%02d:%02d GMT"
    (week_day_txt lt.Unix.tm_wday)
    lt.Unix.tm_mday (month_txt lt.Unix.tm_mon) (1900 + lt.Unix.tm_year)
    lt.Unix.tm_hour lt.Unix.tm_min lt.Unix.tm_sec

let html ?(content_type = "text/html") conf =
  let charset =
    if conf.Config.charset = "" then "utf-8" else conf.Config.charset
  in
  if not conf.Config.cgi then
    Output.header conf "Server: GeneWeb/%s" Version.txt;
  Output.header conf "Date: %s" (string_of_ctime conf);
  Output.header conf "Connection: close";
  Output.header conf "Content-type: %s; charset=%s" content_type charset

let unauthorized conf auth_type =
  Output.status conf Def.Unauthorized;
  if not conf.Config.cgi then
    Output.header conf "WWW-Authenticate: Basic realm=\"%s\"" auth_type;
  Output.header conf "Content-type: text/html; charset=%s" conf.Config.charset;
  Output.print_sstring conf "<head><title>Access failed</title></head>\n";
  Output.print_sstring conf "<body><h1>Access failed</h1>\n";
  Output.printf conf "<ul><li>%s</ul>\n" auth_type;
  Output.print_sstring conf "</body>\n"

let commd ?(excl = []) ?(trim = true) ?(henv = true) ?(senv = true) conf :
    Adef.escaped_string =
  let aux =
    List.fold_left (fun c (k, (v : Adef.encoded_string)) ->
        if
          List.mem k excl
          || (trim && (k = "oc" || k = "ocz") && (v :> string) = "0")
          || (v :> string) = ""
        then c
        else
          let open Def in
          c ^^^ k ^<^ "=" ^<^ (v :> Adef.escaped_string) ^>^ "&")
  in
  let s = Adef.escaped @@ conf.Config.command ^ "?" in
  let s = if henv then aux s conf.Config.henv else s in
  let s = if senv then aux s conf.Config.senv else s in
  s

let prefix_base conf =
  let cmmd = conf.Config.command in
  Adef.escaped
  @@
  if conf.Config.cgi then cmmd ^ "?b=" ^ conf.Config.bname ^ "&"
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
  if conf.Config.cgi then
    if conf.Config.cgi_passwd = "" then
      conf.Config.command ^ "?b=" ^ conf.Config.bname ^ "&"
    else
      conf.Config.command ^ "?b=" ^ conf.Config.bname ^ "_"
      ^ conf.Config.cgi_passwd ^ "&"
  else conf.Config.command ^ "?"

let prefix_base_password_2 conf =
  Adef.escaped
  @@
  if conf.Config.cgi then
    if conf.Config.cgi_passwd = "" then
      conf.Config.command ^ "?b=" ^ conf.Config.bname
    else
      conf.Config.command ^ "?b=" ^ conf.Config.bname ^ "_"
      ^ conf.Config.cgi_passwd
  else conf.Config.command ^ "?"

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
    else
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
      loop [])

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
let safe_html_aux escape_text s =
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
                         (Ext_string.contains (String.lowercase_ascii v)
                            "javascript"))
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
  Adef.safe (safe_html_aux (fun s -> (escape_html s :> string)) s)

(* Version 1 => moche *)
let clean_html_tags s l =
  List.fold_left
    (fun s html_tag -> Str.global_replace (Str.regexp html_tag) "&nbsp;" s)
    s l

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
  hidden_env_aux conf conf.Config.henv;
  hidden_env_aux conf conf.Config.senv

let submit_input conf k v =
  aux_input_s conf (Adef.encoded "submit") k (Mutil.decode v)

let p_getenv env label = Option.map Mutil.decode (List.assoc_opt label env)

let p_getint env label =
  try Option.map (fun s -> int_of_string (String.trim s)) (p_getenv env label)
  with Failure _ -> None

let nobtit conf base p =
  Gwdb.nobtitles base conf.Config.allowed_titles conf.Config.denied_titles p

let strictly_after_private_years conf a =
  if a.Date.year > conf.Config.private_years then true
  else if a.year < conf.Config.private_years then false
  else
    (* TODO why true if a.year = conf.private_years and unknown day or month? *)
    a.month > 0 || a.day > 0

(* TODO why do we have both is_old_person and p_auth *)
let is_old_person conf p =
  match
    ( Date.cdate_to_dmy_opt p.Def.birth,
      Date.cdate_to_dmy_opt p.Def.baptism,
      p.Def.death,
      Date.dmy_of_death p.Def.death )
  with
  | _, _, Def.NotDead, _ when conf.Config.private_years > 0 -> false
  | Some d, _, _, _ ->
      let a = Date.time_elapsed d conf.Config.today in
      strictly_after_private_years conf a
  | _, Some d, _, _ ->
      let a = Date.time_elapsed d conf.Config.today in
      strictly_after_private_years conf a
  | _, _, _, Some d ->
      let a = Date.time_elapsed d conf.Config.today in
      strictly_after_private_years conf a
  | None, None, Def.DontKnowIfDead, None ->
      (* TODO is_old_person is supposed to check if p is older than conf.private_years;
         do not check access here *)
      p.Def.access <> Def.Private && conf.Config.public_if_no_date
  | _ -> false

let authorized_age conf base p = GWPARAM.p_auth conf base p

let is_restricted (conf : Config.config) base (ip : Gwdb.iper) =
  let fct p =
    (not (Gwdb.is_quest_string (Gwdb.get_surname p)))
    && (not (Gwdb.is_quest_string (Gwdb.get_first_name p)))
    && not (authorized_age conf base p)
  in
  if conf.Config.use_restrict then Gwdb.base_visible_get base fct ip else false

let pget_opt conf base ip =
  if is_restricted conf base ip then None else Some (Gwdb.poi base ip)

let pget conf base ip =
  Option.value ~default:(Gwdb.empty_person base ip) (pget_opt conf base ip)

let string_gen_person base p =
  Futil.map_person_ps (fun p -> p) (Gwdb.sou base) p

let string_gen_family base fam =
  Futil.map_family_ps (fun p -> p) (fun f -> f) (Gwdb.sou base) fam

(* TODO
   should it be is_empty_name instead? (deleted person have surname and first_name = "?")
   I don't think it is possible to have surname = empty_string *)
let is_empty_person p = Gwdb.is_empty_string (Gwdb.get_surname p)

let is_empty_name p =
  Gwdb.is_quest_string (Gwdb.get_surname p)
  && Gwdb.is_quest_string (Gwdb.get_first_name p)

let is_fully_visible_to_visitors conf base p =
  let conf = { conf with Config.wizard = false; friend = false } in
  authorized_age conf base p

(* TODO should probably not exists *)
let is_public conf base p =
  Gwdb.get_access p = Def.Public
  || conf.Config.public_if_titles
     && Gwdb.get_access p = Def.IfTitles
     && nobtit conf base p <> []
  || is_old_person conf (Gwdb.gen_person_of_person p)

(* ********************************************************************** *)
(* [Fonc] accessible_by_key :
            config -> base -> person -> string -> string -> bool *)

(* ********************************************************************** *)

(** [Description] : Vrai si la personne est accessible par sa clé,
                    Faux sinon.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
      - fn   : prénom de la personne
      - sn   : patronyme de la personne
    [Retour] :
      - bool : Vrai si la personne est accessible par sa clé, faux sinon.
    [Rem] : Exporté en clair hors de ce module.                           *)
let accessible_by_key conf base p fn sn =
  conf.Config.access_by_key
  && (not ((* should it be is_empty_person here? *) fn = "?" || sn = "?"))
  && ((not (is_hide_names conf p))
     || is_public conf base p || conf.Config.friend || conf.Config.wizard)

(* ********************************************************************** *)
(*  [Fonc] acces_n : config -> base -> string -> person -> string         *)

(* ********************************************************************** *)

(** [Description] : Renvoie les paramètres URL pour l'accès à la nième
                    personne.
    [Args] :
    - conf : configuration de la base
    - base : base de donnée
    - n    : la nième personne (e.g. : calcul de parenté entre p1 et p2)
    - p    : person
      [Retour] : string
      [Rem] : Exporté en clair hors de ce module.                           *)
let acces_n conf base n x : Adef.escaped_string =
  let first_name = Gwdb.p_first_name base x in
  let surname = Gwdb.p_surname base x in
  if surname = "" then Adef.escaped ""
  else if accessible_by_key conf base x first_name surname then
    let open Def in
    "p" ^<^ n ^^^ "="
    ^<^ (Mutil.encode (Name.lower first_name) :> Adef.escaped_string)
    ^^^ "&n" ^<^ n ^^^ "="
    ^<^ (Mutil.encode (Name.lower surname) :> Adef.escaped_string)
    ^^^
    if Gwdb.get_occ x <> 0 then
      "&oc" ^<^ n ^>^ "=" ^ string_of_int (Gwdb.get_occ x)
    else Adef.escaped ""
  else
    let open Def in
    "i" ^<^ n ^^^ "="
    ^<^ Gwdb.string_of_iper (Gwdb.get_iper x)
    ^<^
    if conf.Config.wizard && Gwdb.get_occ x <> 0 then
      "&oc" ^<^ n ^>^ "=" ^ string_of_int (Gwdb.get_occ x)
    else Adef.escaped ""

(* ********************************************************************** *)
(*  [Fonc] acces : config -> base -> person -> string                     *)

(* ********************************************************************** *)

(** [Description] : Renvoie les paramètres URL pour l'accès à la personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                           *)
let acces conf base x = acces_n conf base (Adef.escaped "") x

(**/**)

let restricted_txt = Adef.safe "....."
let x_x_txt = Adef.safe "x x"
let hidden_name_txt = Adef.safe "x"

let gen_person_text ?(escape = true) ?(html = true) ?(sn = true) ?(chk = true)
    ?(p_first_name = Gwdb.p_first_name) ?(p_surname = Gwdb.p_surname) conf base
    p =
  let esc = if escape then esc else Adef.safe in
  if is_empty_person p then restricted_txt
  else if chk && is_hide_names conf p && not (authorized_age conf base p) then
    x_x_txt
  else
    let beg =
      match (Gwdb.sou base (Gwdb.get_public_name p), Gwdb.get_qualifiers p) with
      | "", nn :: _ ->
          let open Def in
          esc (p_first_name base p)
          ^^^ (if html then " <em>" else " ")
          ^<^ esc (Gwdb.sou base nn)
          ^>^ if html then "</em>" else ""
      | "", [] -> esc (p_first_name base p)
      | n, nn :: _ ->
          let open Def in
          esc n ^^^ " <em>" ^<^ esc (Gwdb.sou base nn) ^>^ "</em>"
      | n, [] -> esc n
    in
    if sn then
      match p_surname base p with
      | "" -> beg
      | sn ->
          let open Def in
          beg ^^^ " " ^<^ esc sn
    else beg

let max_ancestor_level conf base ip max_lvl =
  let module Person_id_map = Map.Make (struct
    type t = Gwdb.iper

    let compare = Gwdb.compare_iper
  end) in
  let module Node = struct
    type kind = At_max_level | Internal | Leaf
    type t = { kind : kind; level : int }
  end in
  (* Loading ITL cache, up to 10 generations. *)
  let () = !GWPARAM_ITL.init_cache conf base ip 10 0 0 in
  let rec loop ~visited_nodes level ip =
    match Person_id_map.find_opt ip visited_nodes with
    | None -> visit ~visited_nodes level ip
    | Some node ->
        if level < node.Node.level then visit ~visited_nodes level ip
        else visited_nodes
  and visit ~visited_nodes level ip =
    if level = max_lvl then
      Person_id_map.add ip
        { Node.kind = Node.At_max_level; level = max_lvl }
        visited_nodes
    else
      match Gwdb.get_parents (pget conf base ip) with
      | Some ifam ->
          let cpl = Gwdb.foi base ifam in
          let visited_nodes =
            Person_id_map.add ip
              { Node.kind = Node.Internal; level }
              visited_nodes
          in
          loop
            ~visited_nodes:
              (loop ~visited_nodes (succ level) (Gwdb.get_father cpl))
            (succ level) (Gwdb.get_mother cpl)
      | None ->
          let update_node node =
            let level =
              match node with
              | Some node -> min level node.Node.level
              | None ->
                  !GWPARAM_ITL.max_ancestor_level
                    conf base ip conf.Config.bname max_lvl level
            in
            Some { Node.kind = Node.Leaf; level }
          in
          Person_id_map.update ip update_node visited_nodes
  in
  let max_level { Node.kind; level } current_max_level =
    match kind with
    | Node.Internal -> current_max_level
    | Node.Leaf | Node.At_max_level -> max current_max_level level
  in
  Person_id_map.fold (Fun.const max_level)
    (loop ~visited_nodes:Person_id_map.empty 0 ip)
    0

let main_title conf base p =
  let titles = nobtit conf base p in
  match List.find_opt (fun x -> x.Def.t_name = Def.Tmain) titles with
  | None -> ( match titles with x :: _ -> Some x | _ -> None)
  | x -> x

let titled_person_text conf base p t : Adef.safe_string =
  if List.assoc_opt "print_advanced_title" conf.Config.base_env = Some "yes"
  then
    let estate = Gwdb.sou base t.Def.t_place in
    let surname = Gwdb.p_surname base p in
    (* Si le nom de l'individu est le même que son domaine, on renvoie : *)
    (*   - le nom du titre                                               *)
    (*   - le nom du titre et le premier sobriquet                       *)
    (*   - le nom de la personne (donné par son nom de domaine) en       *)
    (*     fonction du nom public et sobriquet                           *)
    if Name.strip_lower estate = Name.strip_lower surname then
      match (t.Def.t_name, Gwdb.get_qualifiers p) with
      | Def.Tname n, [] -> (esc (Gwdb.sou base n) :> Adef.safe_string)
      | Def.Tname n, nn :: _ ->
          let open Def in
          (esc (Gwdb.sou base n) :> Adef.safe_string)
          ^^^ " <em>"
          ^<^ (esc (Gwdb.sou base nn) :> Adef.safe_string)
          ^>^ "</em>"
      | _ -> gen_person_text ~sn:false conf base p
    else
      let elen = String.length estate in
      let slen = String.length surname in
      if elen < slen && String.sub surname (slen - elen) elen = estate then
        match (t.Def.t_name, Gwdb.get_qualifiers p) with
        | Def.Tname n, [] -> esc (Gwdb.sou base n)
        | Def.Tname n, nn :: _ ->
            let open Def in
            esc (Gwdb.sou base n)
            ^^^ " <em>"
            ^<^ esc (Gwdb.sou base nn)
            ^>^ "</em>"
        | _ ->
            gen_person_text
              ~p_surname:(fun _ _ ->
                String.trim (String.sub surname 0 (slen - elen)))
              conf base p
      else
        match t.Def.t_name with
        | Def.Tname s -> (
            let s = esc (Gwdb.sou base s) in
            match Gwdb.get_qualifiers p with
            | [] -> s
            | nn :: _ ->
                let open Def in
                s ^^^ " <em>" ^<^ esc (Gwdb.sou base nn) ^>^ "</em>")
        | _ -> gen_person_text conf base p
  else gen_person_text conf base p

(* *********************************************************************** *)
(*  [Fonc] one_title_text : base -> istr gen_title     *)

(* *********************************************************************** *)

(** [Description] : Renvoie la chaîne de caractère du titre ainsi que le
                    domaine.
    [Args] :
      - base : base de donnée
      - t    : le titre de noblesse que l'on veut afficher
    [Retour] : string
    [Rem] : Non exporté en clair hors de ce module.                        *)
let one_title_text base t : Adef.safe_string =
  let place = Gwdb.sou base t.Def.t_place in
  let s = Gwdb.sou base t.Def.t_ident in
  let s = if place = "" then s else s ^ " " ^ place in
  let open Def in
  ", <em>" ^<^ (esc s :> Adef.safe_string) ^>^ "</em>"

let geneweb_link conf (href : Adef.escaped_string) (s : Adef.safe_string) =
  let open Def in
  "<a href=\""
  ^<^ (commd conf ^^^ href :> Adef.safe_string)
  ^^^ "\">" ^<^ s ^>^ "</a>"

let wprint_geneweb_link conf href s =
  Output.print_string conf (geneweb_link conf href s)

(* ************************************************************************* *)
(*  [Fonc] update_family_loop : config -> base -> person -> string -> string *)

(* ************************************************************************* *)

(** [Description] : Essaie de déterminer dans quelle famille il peut y avoir
                    une boucle. Si il n'y a pas d'ambiguité, alors on renvoie
                    un lien vers la famille à modifier, sinon, on renvoie un
                    lien vers le menu général de mise à jour.
    [Args] :
      - conf : configuration
      - base : base
      - p    : person
      - s    : la clé de la personne sous forme de string
    [Retour] :
      - string : retourne un lien de mise à jour soit vers la famille
                 contenant la boucle, soit vers le menu de mise à jour.
    [Rem] : Exporté en clair hors de ce module.                              *)
let update_family_loop conf base p s =
  if is_empty_person p then s
  else
    let iper = Gwdb.get_iper p in
    let list = Gwdb.get_family p in
    let list =
      Array.map
        (fun ifam -> (ifam, Gwdb.get_children (Gwdb.foi base ifam)))
        list
    in
    let res =
      Array.fold_left
        (fun acc (ifam, children) ->
          if Array.mem iper children then ifam :: acc else acc)
        [] list
    in
    if conf.Config.wizard then
      if List.length res = 1 then
        let iper = Gwdb.string_of_iper iper in
        let ifam = Gwdb.string_of_ifam (List.hd res) in
        let open Def in
        "<a href=\""
        ^<^ (commd conf :> Adef.safe_string)
        ^^^ "m=MOD_FAM&i=" ^<^ ifam ^<^ "&ip=" ^<^ iper ^<^ "\">" ^<^ s
        ^>^ "</a>"
      else
        let iper = Gwdb.string_of_iper iper in
        let open Def in
        "<a href=\""
        ^<^ (commd conf :> Adef.safe_string)
        ^^^ "m=U&i=" ^<^ iper ^<^ "\">" ^<^ s ^>^ "</a>"
    else s

let person_text_without_title conf base p =
  match main_title conf base p with
  | Some t -> (
      if Gwdb.eq_istr t.Def.t_place (Gwdb.get_surname p) then
        gen_person_text ~sn:false conf base p
      else
        match (t.Def.t_name, Gwdb.get_qualifiers p) with
        | Def.Tname s, nn :: _ ->
            let open Def in
            esc (Gwdb.sou base s)
            ^^^ " <em>"
            ^<^ esc (Gwdb.sou base nn)
            ^>^ "</em>"
        | Def.Tname s, _ -> esc (Gwdb.sou base s)
        | _ -> gen_person_text conf base p)
  | None -> gen_person_text conf base p

let person_title conf base p =
  if authorized_age conf base p then
    match main_title conf base p with
    | Some t -> one_title_text base t
    | None -> Adef.safe ""
  else Adef.safe ""

let name_key base s =
  let part = Mutil.get_particle (Gwdb.base_particles base) s in
  if part = "" then s
  else
    let i = String.length part in
    String.sub s i (String.length s - i) ^ " " ^ String.sub s 0 i

let surname_particle base s =
  let part = Mutil.get_particle (Gwdb.base_particles base) s in
  let len = String.length part in
  if len = 0 then ""
  else if part.[len - 1] = ' ' then " (" ^ String.sub part 0 (len - 1) ^ ")"
  else " (" ^ part ^ ")"

let surname_without_particle base s =
  let part_len =
    String.length (Mutil.get_particle (Gwdb.base_particles base) s)
  in
  String.sub s part_len (String.length s - part_len)

let rec skip_spaces s i =
  if i < String.length s && s.[i] = ' ' then skip_spaces s (i + 1) else i

let create_env s =
  let s = (s : Adef.encoded_string :> string) in
  let rec get_assoc beg i =
    if i = String.length s then
      if i = beg then [] else [ String.sub s beg (i - beg) ]
    else if s.[i] = ';' || s.[i] = '&' then
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
  let open Def in
  "<span style=\"color:" ^<^ conf.Config.highlight ^<^ "\">" ^<^ s ^>^ "</span>"

let index_of_sex = function Def.Male -> 0 | Def.Female -> 1 | Def.Neuter -> 2

let string_of_pevent_name_without_base conf = function
  | Def.Epers_Birth -> Adef.safe @@ transl conf "birth"
  | Def.Epers_Baptism -> Adef.safe @@ transl conf "baptism"
  | Def.Epers_Death -> Adef.safe @@ transl conf "death"
  | Def.Epers_Burial -> Adef.safe @@ transl conf "burial"
  | Def.Epers_Cremation -> Adef.safe @@ transl conf "cremation"
  | Def.Epers_Accomplishment -> Adef.safe @@ transl conf "accomplishment"
  | Def.Epers_Acquisition -> Adef.safe @@ transl conf "acquisition"
  | Def.Epers_Adhesion -> Adef.safe @@ transl conf "adhesion"
  | Def.Epers_BaptismLDS -> Adef.safe @@ transl conf "baptismLDS"
  | Def.Epers_BarMitzvah -> Adef.safe @@ transl conf "bar mitzvah"
  | Def.Epers_BatMitzvah -> Adef.safe @@ transl conf "bat mitzvah"
  | Def.Epers_Benediction -> Adef.safe @@ transl conf "benediction"
  | Def.Epers_ChangeName -> Adef.safe @@ transl conf "change name"
  | Def.Epers_Circumcision -> Adef.safe @@ transl conf "circumcision"
  | Def.Epers_Confirmation -> Adef.safe @@ transl conf "confirmation"
  | Def.Epers_ConfirmationLDS -> Adef.safe @@ transl conf "confirmation LDS"
  | Def.Epers_Decoration -> Adef.safe @@ transl conf "decoration"
  | Def.Epers_DemobilisationMilitaire ->
      Adef.safe @@ transl conf "demobilisationMilitaire"
  | Def.Epers_Diploma -> Adef.safe @@ transl conf "diploma"
  | Def.Epers_Distinction -> Adef.safe @@ transl conf "distinction"
  | Def.Epers_Dotation -> Adef.safe @@ transl conf "dotation"
  | Def.Epers_DotationLDS -> Adef.safe @@ transl conf "dotationLDS"
  | Def.Epers_Education -> Adef.safe @@ transl conf "education"
  | Def.Epers_Election -> Adef.safe @@ transl conf "election"
  | Def.Epers_Emigration -> Adef.safe @@ transl conf "emigration"
  | Def.Epers_Excommunication -> Adef.safe @@ transl conf "excommunication"
  | Def.Epers_FamilyLinkLDS -> Adef.safe @@ transl conf "familyLinkLDS"
  | Def.Epers_FirstCommunion -> Adef.safe @@ transl conf "firstCommunion"
  | Def.Epers_Funeral -> Adef.safe @@ transl conf "funeral"
  | Def.Epers_Graduate -> Adef.safe @@ transl conf "graduate"
  | Def.Epers_Hospitalisation -> Adef.safe @@ transl conf "hospitalisation"
  | Def.Epers_Illness -> Adef.safe @@ transl conf "illness"
  | Def.Epers_Immigration -> Adef.safe @@ transl conf "immigration"
  | Def.Epers_ListePassenger -> Adef.safe @@ transl conf "listePassenger"
  | Def.Epers_MilitaryDistinction ->
      Adef.safe @@ transl conf "militaryDistinction"
  | Def.Epers_MilitaryPromotion -> Adef.safe @@ transl conf "militaryPromotion"
  | Def.Epers_MilitaryService -> Adef.safe @@ transl conf "militaryService"
  | Def.Epers_MobilisationMilitaire ->
      Adef.safe @@ transl conf "mobilisationMilitaire"
  | Def.Epers_Naturalisation -> Adef.safe @@ transl conf "naturalisation"
  | Def.Epers_Occupation ->
      Adef.safe @@ transl_nth conf "occupation/occupations" 0
  | Def.Epers_Ordination -> Adef.safe @@ transl conf "ordination"
  | Def.Epers_Property -> Adef.safe @@ transl conf "property"
  | Def.Epers_Recensement -> Adef.safe @@ transl conf "recensement"
  | Def.Epers_Residence -> Adef.safe @@ transl conf "residence"
  | Def.Epers_Retired -> Adef.safe @@ transl conf "retired"
  | Def.Epers_ScellentChildLDS -> Adef.safe @@ transl conf "scellentChildLDS"
  | Def.Epers_ScellentParentLDS -> Adef.safe @@ transl conf "scellentParentLDS"
  | Def.Epers_ScellentSpouseLDS -> Adef.safe @@ transl conf "scellentSpouseLDS"
  | Def.Epers_VenteBien -> Adef.safe @@ transl conf "venteBien"
  | Def.Epers_Will -> Adef.safe @@ transl conf "will"
  | _ -> failwith "bad argument in Util.string_of_pevent_without_base"

let string_of_fevent_name_without_base conf = function
  | Def.Efam_Marriage -> Adef.safe @@ transl conf "marriage event"
  | Def.Efam_NoMarriage -> Adef.safe @@ transl conf "no marriage event"
  | Def.Efam_NoMention -> Adef.safe @@ transl conf "no mention"
  | Def.Efam_Engage -> Adef.safe @@ transl conf "engage event"
  | Def.Efam_Divorce -> Adef.safe @@ transl conf "divorce event"
  | Def.Efam_Separated -> Adef.safe @@ transl conf "separate event"
  | Def.Efam_Annulation -> Adef.safe @@ transl conf "annulation"
  | Def.Efam_MarriageBann -> Adef.safe @@ transl conf "marriage bann"
  | Def.Efam_MarriageContract -> Adef.safe @@ transl conf "marriage contract"
  | Def.Efam_MarriageLicense -> Adef.safe @@ transl conf "marriage licence"
  | Def.Efam_PACS -> Adef.safe @@ transl conf "PACS"
  | Def.Efam_Residence -> Adef.safe @@ transl conf "residence"
  | _ -> failwith "bad argument in Util.string_of_fevent_without_base"

let string_of_pevent_name conf base epers_name =
  match epers_name with
  | Def.Epers_Name n -> (escape_html (Gwdb.sou base n) :> Adef.safe_string)
  | _ -> string_of_pevent_name_without_base conf epers_name

let string_of_pevent_name' conf epers_name =
  match epers_name with
  | Def.Epers_Name n -> (escape_html n :> Adef.safe_string)
  | _ -> string_of_pevent_name_without_base conf epers_name

let string_of_fevent_name conf base efam_name =
  match efam_name with
  | Def.Efam_Name n -> (escape_html (Gwdb.sou base n) :> Adef.safe_string)
  | _ -> string_of_fevent_name_without_base conf efam_name

let string_of_fevent_name' conf efam_name =
  match efam_name with
  | Def.Efam_Name n -> (escape_html n :> Adef.safe_string)
  | _ -> string_of_fevent_name_without_base conf efam_name

let string_of_witness_kind conf sex witness_kind =
  let n = if witness_kind = Def.Witness then 0 else index_of_sex sex in
  let s =
    match witness_kind with
    | Def.Witness -> "witness/witness/witnesses"
    | Def.Witness_CivilOfficer -> "other/other/other"
    | Def.Witness_GodParent -> "godfather/godmother/godparents"
    | Def.Witness_ReligiousOfficer -> "other/other/other"
    | Def.Witness_Informant -> "informant/informant/informant"
    | Def.Witness_Attending -> "present/present/present"
    | Def.Witness_Mentioned -> "mentioned/mentioned/mentioned"
    | Def.Witness_Other -> "other/other/other"
  in
  Adef.safe @@ transl_nth conf s n

let string_of_access conf access =
  let n =
    match access with Def.IfTitles -> 0 | Def.Public -> 1 | Def.Private -> 2
  in
  Adef.safe @@ transl_nth conf "iftitles/public/private" n

let base_path pref bname = GWPARAM.base_path pref bname
let bpath bname = GWPARAM.bpath bname
let copy_from_templ_ref = ref (fun _ _ _ -> assert false)
let copy_from_templ conf env ic = !copy_from_templ_ref conf env ic

let include_begin_end_aux (k : Adef.safe_string) conf (fname : Adef.safe_string)
    =
  if conf.Config.debug then
    match Filename.extension (fname :> string) with
    | ".css" | ".js" ->
        Output.print_sstring conf "\n/* ";
        Output.print_string conf k;
        Output.print_sstring conf " ";
        Output.print_string conf fname;
        Output.print_sstring conf " */\n"
    | _ ->
        Output.print_sstring conf "\n<!-- ";
        Output.print_string conf k;
        Output.print_sstring conf " ";
        Output.print_string conf fname;
        Output.print_sstring conf " -->\n"

let include_begin = include_begin_end_aux (Adef.safe "begin")
let include_end = include_begin_end_aux (Adef.safe "end")

let etc_file_name fname =
  search_in_assets (Filename.concat "etc" (fname ^ ".txt"))

let open_etc_file fname =
  let fname = etc_file_name fname in
  try Some (Secure.open_in fname, fname) with Sys_error _ -> None

let include_template conf env fname failure =
  match open_etc_file fname with
  | Some (ic, fname) ->
      include_begin conf (esc fname);
      copy_from_templ conf env ic;
      include_end conf (esc fname)
  | None -> failure ()

let body_prop conf =
  try
    match List.assoc "body_prop" conf.Config.base_env with
    | "" -> ""
    | s -> " " ^ s
  with Not_found -> ""

let get_server_string conf =
  if not conf.Config.cgi then
    Mutil.extract_param "host: " '\r' conf.Config.request
  else
    let server_name = try Sys.getenv "SERVER_NAME" with Not_found -> "" in
    let server_port =
      try Sys.getenv "SERVER_PORT" with Not_found | Failure _ -> "80"
    in
    if server_port = "80" then server_name else server_name ^ ":" ^ server_port

let get_request_string conf =
  if not conf.Config.cgi then Mutil.extract_param "GET " ' ' conf.Config.request
  else
    let script_name = try Sys.getenv "SCRIPT_NAME" with Not_found -> "" in
    let query_string = try Sys.getenv "QUERY_STRING" with Not_found -> "" in
    script_name ^ "?" ^ query_string

let message_to_wizard conf =
  if conf.Config.wizard || conf.Config.just_friend_wizard then (
    let print_file fname =
      let fname = base_path [ "etc"; conf.Config.bname ] (fname ^ ".txt") in
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
    if conf.Config.user <> "" then print_file ("mess_wizard_" ^ conf.Config.user))

let doctype = Adef.safe "<!DOCTYPE html>"

let http_string s i =
  let start_with s i p =
    i + String.length p <= String.length s
    && String.lowercase_ascii (String.sub s i (String.length p)) = p
  in
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
    match List.assoc_opt "expand_env" conf.Config.base_env with
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
                      let v = List.assoc ("var_" ^ k) conf.Config.base_env in
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

let raw_string_of_place place =
  List.fold_left (fun s c -> Name.strip_c s c) place [ '['; ']' ]

let string_of_place place = raw_string_of_place place |> escape_html

let trimmed_string_of_place place =
  let field_separator = ',' in
  let trim field =
    let field = String.trim field in
    Ext_option.return_if (field <> "") @@ fun () -> field
  in
  place |> raw_string_of_place
  |> String.split_on_char field_separator
  |> List.filter_map trim
  |> String.concat (Printf.sprintf "%c " field_separator)
  |> escape_html

let menu_threshold = 20
let is_number t = match t.[0] with '1' .. '9' -> true | _ -> false

let print_alphab_list conf crit print_elem liste =
  let len = List.length liste in
  if len > menu_threshold then (
    Output.print_sstring conf "<p>\n";
    (let _ =
       List.fold_left
         (fun last e ->
           let t = crit e in
           let same_than_last =
             match last with Some t1 -> t = t1 | _ -> false
           in
           if not same_than_last then
             Output.printf conf "<a href=\"#ai%s\">%s</a>\n"
               (Ext_string.hexa_string t) t;
           Some t)
         None liste
     in
     ());
    Output.print_sstring conf "</p>\n");
  Output.print_sstring conf "<ul>\n";
  (let _ =
     List.fold_left
       (fun last e ->
         let t = crit e in
         let same_than_last =
           match last with Some t1 -> t = t1 | _ -> false
         in
         if len > menu_threshold || is_number t then (
           (match last with
           | Some _ ->
               if not same_than_last then
                 Output.print_sstring conf "</ul>\n</li>\n"
           | _ -> ());
           if not same_than_last then (
             Output.print_sstring conf "<li>\n";
             Output.printf conf "<a id=\"ai%s\">%s</a>\n"
               (Ext_string.hexa_string t) t;
             Output.print_sstring conf "<ul>\n"));
         Output.print_sstring conf "<li>\n  ";
         print_elem e;
         Output.print_sstring conf "</li>\n";
         Some t)
       None liste
   in
   ());
  if len > menu_threshold then Output.print_sstring conf "</ul>\n</li>\n";
  Output.print_sstring conf "</ul>\n"

let relation_txt conf sex fam =
  let is = index_of_sex sex in
  match Gwdb.get_relation fam with
  | Def.NotMarried | Def.NoSexesCheckNotMarried ->
      ftransl_nth conf "relationship%t to" is
  | Def.MarriageContract -> ftransl_nth conf "marriage contract%t with" is
  | Def.MarriageLicense | Def.Married | Def.NoSexesCheckMarried ->
      ftransl_nth conf "married%t to" is
  | Def.Engaged -> ftransl_nth conf "engaged%t to" is
  | Def.MarriageBann -> ftransl_nth conf "marriage banns%t to" is
  | Def.Pacs -> ftransl_nth conf "pacsed%t to" is
  | Def.Residence -> ftransl_nth conf "residence%t to" is
  | Def.NoMention -> "%t" ^^ ftransl conf "with"

let relation_date conf base fam : Adef.safe_string =
  let is_visible family =
    let is_visible person = authorized_age conf base (pget conf base person) in
    is_visible (Gwdb.get_father family) && is_visible (Gwdb.get_mother family)
  in
  Adef.safe
  @@
  if not @@ is_visible fam then ""
  else
    match Date.cdate_to_dmy_opt (Gwdb.get_marriage fam) with
    | None -> ""
    | Some dmy -> " " ^ transl conf "in (year)" ^ " " ^ string_of_int dmy.year

let child_of_parent conf base p =
  (* Si le père a un nom de famille différent de la personne *)
  (* alors on l'affiche, sinon on n'affiche que le prénom.   *)
  let print_father fath =
    if not (Gwdb.eq_istr (Gwdb.get_surname p) (Gwdb.get_surname fath)) then
      gen_person_text conf base fath
    else gen_person_text ~sn:false conf base fath
  in
  let a = pget conf base (Gwdb.get_iper p) in
  let ifam =
    match Gwdb.get_parents a with
    | Some ifam ->
        let cpl = Gwdb.foi base ifam in
        let fath =
          let fath = pget conf base (Gwdb.get_father cpl) in
          if Gwdb.p_first_name base fath = "?" then None else Some fath
        in
        let moth =
          let moth = pget conf base (Gwdb.get_mother cpl) in
          if Gwdb.p_first_name base moth = "?" then None else Some moth
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
            let open Def in
            print_father fath ^^^ " " ^<^ transl_nth conf "and" 0 ^<^ " "
            ^<^ gen_person_text conf base moth
        | _ -> Adef.safe ""
      in
      let is = index_of_sex (Gwdb.get_sex p) in
      let s = (s :> string) in
      transl_a_of_gr_eq_gen_lev conf
        (transl_nth conf "son/daughter/child" is)
        s s
      |> translate_eval |> Adef.safe

let husband_wife conf base p all =
  let relation =
    let rec loop i =
      if i < Array.length (Gwdb.get_family p) then
        let fam = Gwdb.foi base (Gwdb.get_family p).(i) in
        let conjoint = Gutil.spouse (Gwdb.get_iper p) fam in
        let conjoint = pget conf base conjoint in
        if not @@ is_empty_name conjoint then
          Printf.sprintf (relation_txt conf (Gwdb.get_sex p) fam) (fun () -> "")
          |> translate_eval |> Adef.safe
        else loop (i + 1)
      else Adef.safe ""
    in
    loop 0
  in
  let res =
    let rec loop i res =
      if i < Array.length (Gwdb.get_family p) then
        let fam = Gwdb.foi base (Gwdb.get_family p).(i) in
        let conjoint = Gutil.spouse (Gwdb.get_iper p) fam in
        let conjoint = pget conf base conjoint in
        if not @@ is_empty_name conjoint then
          let res =
            let open Def in
            res
            ^>^ translate_eval
                  (" "
                   ^<^ gen_person_text conf base conjoint
                   ^^^ relation_date conf base fam
                    :> string)
            ^ ","
          in
          if all then loop (i + 1) res else res
        else loop (i + 1) res
      else res
    in
    loop 0 relation
  in
  let res = (res :> string) in
  let res =
    if String.length res > 1 then String.sub res 0 (String.length res - 1)
    else res
  in
  Adef.safe res

let first_child conf base p =
  let is = index_of_sex (Gwdb.get_sex p) in
  let rec loop i =
    if i < Array.length (Gwdb.get_family p) then
      let fam = Gwdb.foi base (Gwdb.get_family p).(i) in
      let ct = Gwdb.get_children fam in
      if Array.length ct > 0 then
        let enfant = pget conf base ct.(0) in
        let child =
          if is_hide_names conf enfant && not (authorized_age conf base enfant)
          then Adef.safe "xx"
          else if
            not (Gwdb.eq_istr (Gwdb.get_surname p) (Gwdb.get_surname enfant))
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
  match (Gwdb.get_public_name p, Gwdb.get_qualifiers p) with
  | n, nn :: _ when Gwdb.sou base n <> "" && specify_public_name ->
      Output.print_sstring conf " ";
      Output.print_string conf (esc @@ Gwdb.sou base n);
      Output.print_sstring conf " <em>";
      Output.print_string conf (esc @@ Gwdb.sou base nn);
      Output.print_sstring conf "</em>"
  | _, nn :: _ when specify_public_name ->
      Output.print_sstring conf " ";
      Output.print_string conf (esc @@ Gwdb.p_first_name base p);
      Output.print_sstring conf " <em>";
      Output.print_string conf (esc @@ Gwdb.sou base nn);
      Output.print_sstring conf "</em>"
  | n, [] when Gwdb.sou base n <> "" && specify_public_name ->
      Output.print_sstring conf " ";
      Output.print_string conf (esc @@ Gwdb.sou base n)
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

let get_approx_birth_date_place base p =
  let birth = Date.od_of_cdate (Gwdb.get_birth p) in
  let birth_place =
    trimmed_string_of_place (Gwdb.sou base (Gwdb.get_birth_place p))
  in
  let baptism = Date.od_of_cdate (Gwdb.get_baptism p) in
  let baptism_place =
    trimmed_string_of_place (Gwdb.sou base (Gwdb.get_baptism_place p))
  in
  get_approx_date_place birth
    (birth_place :> Adef.safe_string)
    baptism
    (baptism_place :> Adef.safe_string)

let get_approx_death_date_place base p =
  let death = Date.date_of_death (Gwdb.get_death p) in
  let death_place =
    trimmed_string_of_place (Gwdb.sou base (Gwdb.get_death_place p))
  in
  let buri =
    match Gwdb.get_burial p with
    | Def.Buried cd | Def.Cremated cd -> Date.od_of_cdate cd
    | Def.UnknownBurial -> None
  in
  let buri_place =
    trimmed_string_of_place (Gwdb.sou base (Gwdb.get_burial_place p))
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
  match p_getenv conf.Config.env env_i with
  | Some i when i <> "" ->
      let i = Gwdb.iper_of_string i in
      if Gwdb.iper_exists base i then
        let p = pget conf base i in
        if is_empty_person p then None else Some p
      else None
  | _ -> (
      match
        (p_getenv conf.Config.env env_p, p_getenv conf.Config.env env_n)
      with
      | Some p, Some n -> (
          let occ =
            Option.value ~default:0 (p_getint conf.Config.env env_occ)
          in
          match Gwdb.person_of_key base p n occ with
          | Some ip ->
              let p = pget conf base ip in
              if is_empty_person p then None
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
  match List.assoc_opt "red_if_not_exist" conf.Config.base_env with
  | Some "off" -> true
  | Some _ | None -> (
      match Gwdb.person_of_key base fn sn oc with
      | Some ip -> authorized_age conf base (pget conf base ip)
      | None -> false)

let default_sosa_ref conf base =
  match List.assoc_opt "default_sosa_ref" conf.Config.base_env with
  | Some n -> (
      if n = "" then None
      else
        match Gutil.person_ht_find_all base n with
        | [ ip ] ->
            let p = pget conf base ip in
            if is_empty_person p then None else Some p
        | _ -> None)
  | None -> None

let find_sosa_ref conf base =
  match find_person_in_env conf base "z" with
  | Some p -> Some p
  | None -> default_sosa_ref conf base

let write_default_sosa conf key =
  let gwf = List.remove_assoc "default_sosa_ref" conf.Config.base_env in
  let gwf = List.rev (("default_sosa_ref", key) :: gwf) in
  let fname = bpath (conf.Config.bname ^ ".gwf") in
  let tmp_fname = fname ^ "2" in
  let oc =
    try Stdlib.open_out tmp_fname
    with Sys_error _ -> failwith "the gwf database is not writable"
  in
  List.iter (fun (k, v) -> Stdlib.output_string oc (k ^ "=" ^ v ^ "\n")) gwf;
  close_out oc;
  Files.rm (fname ^ "~");
  Sys.rename fname (fname ^ "~");
  try Sys.rename tmp_fname fname with Sys_error _ -> ()

let update_gwf_sosa conf base (ip, (fn, sn, occ)) =
  let sosa_ref_key =
    match snd conf.Config.default_sosa_ref with
    | Some p ->
        Gwdb.p_first_name base p ^ "."
        ^ string_of_int (Gwdb.get_occ p)
        ^ " " ^ Gwdb.p_surname base p
    | None -> ""
  in
  let new_key = fn ^ "." ^ string_of_int occ ^ " " ^ sn in
  if ip = fst conf.Config.default_sosa_ref && new_key != sosa_ref_key then
    write_default_sosa conf new_key

let create_topological_sort conf base =
  match p_getenv conf.Config.env "opt" with
  | Some "no_tsfile" ->
      let () = Gwdb.load_ascends_array base in
      let () = Gwdb.load_couples_array base in
      Consang.topological_sort base (pget conf)
  | Some "no_tstab" -> Gwdb.iper_marker (Gwdb.ipers base) 0
  | _ ->
      let bfile = bpath (conf.Config.bname ^ ".gwb") in
      let tstab_file =
        if
          conf.Config.use_restrict && (not conf.Config.wizard)
          && not conf.Config.friend
        then Filename.concat bfile "tstab_visitor"
        else Filename.concat bfile "tstab"
      in
      Files.read_or_create_value ~magic:Mutil.executable_magic tstab_file
        (fun () ->
          Lock.control (Files.lock_file bfile) false
            ~onerror:(fun () ->
              let () = Gwdb.load_ascends_array base in
              let () = Gwdb.load_couples_array base in
              Consang.topological_sort base (pget conf))
            (fun () ->
              let () = Gwdb.load_ascends_array base in
              let () = Gwdb.load_couples_array base in
              let tstab = Consang.topological_sort base (pget conf) in
              if
                conf.Config.use_restrict && (not conf.Config.wizard)
                && not conf.Config.friend
              then Gwdb.base_visible_write base;
              tstab))

let p_of_sosa conf base sosa p0 =
  let path = Sosa.branches sosa in
  let rec aux acc = function
    | [] -> Some acc
    | hd :: tl -> (
        match Gwdb.get_parents acc with
        | Some ifam ->
            let cpl = Gwdb.foi base ifam in
            if hd = 0 then aux (pget conf base (Gwdb.get_father cpl)) tl
            else aux (pget conf base (Gwdb.get_mother cpl)) tl
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
        match Gwdb.get_parents p with
        | Some ifam ->
            let cpl = Gwdb.foi base ifam in
            if male then
              loop (p :: pl) (pget conf base @@ Gwdb.get_father cpl) tl
            else loop (p :: pl) (pget conf base @@ Gwdb.get_mother cpl) tl
        | _ -> None)
  in
  loop [] p (expand [] sosa)

let sosa_of_branch ipl =
  if ipl = [] then failwith "sosa_of_branch";
  let ipl = List.tl (List.rev ipl) in
  List.fold_left
    (fun b p ->
      let b = Sosa.twice b in
      match Gwdb.get_sex p with
      | Def.Male -> b
      | Def.Female -> Sosa.inc b 1
      | Def.Neuter -> assert false)
    Sosa.one ipl

(* FIXME: remove this and use sosa_of_branch only *)
let old_sosa_of_branch conf base (ipl : (Gwdb.iper * Def.sex) list) =
  sosa_of_branch (List.map (fun (ip, _) -> pget conf base ip) ipl)

(* FIXME: remove this and use branch_of_sosa only *)
let old_branch_of_sosa conf base ip sosa =
  branch_of_sosa conf base sosa (pget conf base ip)
  |> Option.map @@ List.map (fun p -> (Gwdb.get_iper p, Gwdb.get_sex p))

let relation_type_text conf t sex =
  let s =
    match t with
    | Def.Adoption -> "adoptive father/adoptive mother/adoptive parents"
    | Def.Recognition ->
        "recognizing father/recognizing mother/recognizing parents"
    | Def.CandidateParent ->
        "candidate father/candidate mother/candidate parents"
    | Def.GodParent -> "godfather/godmother/godparents"
    | Def.FosterParent -> "foster father/foster mother/foster parents"
  in
  Adef.safe @@ transl_nth conf s (index_of_sex sex)

let rchild_type_text conf t sex =
  let s =
    match t with
    | Def.Adoption -> "adoptive son/adoptive daughter/adoptive child"
    | Def.Recognition -> "recognized son/recognized daughter/recognized child"
    | Def.CandidateParent -> "candidate son/candidate daughter/candidate child"
    | Def.GodParent -> "godson/goddaughter/godchild"
    | Def.FosterParent -> "foster son/foster daughter/foster child"
  in
  Adef.safe @@ transl_nth conf s (index_of_sex sex)

let has_nephews_or_nieces conf base p =
  let exception Ok in
  try
    let a = p in
    match Gwdb.get_parents a with
    | None -> false
    | Some ifam ->
        let fam = Gwdb.foi base ifam in
        Array.iter
          (fun ip ->
            if ip = Gwdb.get_iper p then ()
            else
              Array.iter
                (fun ifam ->
                  if Array.length (Gwdb.get_children (Gwdb.foi base ifam)) > 0
                  then raise Ok)
                (Gwdb.get_family (pget conf base ip)))
          (Gwdb.get_children fam);
        false
  with Ok -> true

let h s = Digest.to_hex (Digest.string s)

let is_that_user_and_password auth_scheme user passwd =
  match auth_scheme with
  | Config.NoAuth -> false
  | Config.TokenAuth ts ->
      user = ts.Config.ts_user && passwd = ts.Config.ts_pass
  | Config.HttpAuth (Config.Basic bs) ->
      user = bs.Config.bs_user && passwd = bs.Config.bs_pass
  | Config.HttpAuth (Config.Digest ds) ->
      if user <> ds.Config.ds_username then false
      else
        let that_response_would_be =
          let a1 = Printf.sprintf "%s:%s:%s" user ds.Config.ds_realm passwd in
          let a2 = Printf.sprintf "%s:%s" ds.Config.ds_meth ds.Config.ds_uri in
          if ds.Config.ds_qop = "auth" || ds.Config.ds_qop = "auth-int" then
            h
              (h a1 ^ ":" ^ ds.Config.ds_nonce ^ ":" ^ ds.Config.ds_nc ^ ":"
             ^ ds.Config.ds_cnonce ^ ":" ^ ds.Config.ds_qop ^ ":" ^ h a2)
          else h (h a1 ^ ":" ^ ds.Config.ds_nonce ^ ":" ^ h a2)
        in
        that_response_would_be = ds.Config.ds_response

let browser_doesnt_have_tables conf =
  let user_agent = Mutil.extract_param "user-agent: " '/' conf.Config.request in
  String.lowercase_ascii user_agent = "lynx"

let of_course_died conf p =
  match Date.cdate_to_dmy_opt (Gwdb.get_birth p) with
  | Some d ->
      (* TODO this value should be defined elsewhere *)
      conf.Config.today.year - d.year > 120
  | None -> false

let escache_value base =
  let t = Gwdb.date_of_last_change base in
  let v = int_of_float (mod_float t (float_of_int max_int)) in
  Adef.encoded (string_of_int v)

let adm_file f = List.fold_right Filename.concat [ !cnt_dir; "cnt" ] f

let sprintf_today conf =
  let hh, mm, ss = conf.Config.time in
  let tm =
    Unix.
      {
        tm_year = conf.Config.today.year - 1900;
        tm_mon = conf.Config.today.month - 1;
        tm_mday = conf.Config.today.day;
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
            if u = conf.Config.user then loop true ((dt, u) :: r) l
            else loop found ((String.sub x 0 dtlen, u) :: r) l
          else loop found r l
      | [] -> if found then r else (dt, conf.Config.user) :: r
    in
    loop false [] r
  in
  write_wf_trace fname (List.sort (fun x y -> compare y x) wt)

let commit_patches conf base =
  Gwdb.commit_patches base;
  conf.Config.henv <-
    List.map
      (fun (k, v) -> if k = "escache" then (k, escache_value base) else (k, v))
      conf.Config.henv;
  if conf.Config.user <> "" then
    let wpf =
      try List.assoc "wizard_passwd_file" conf.Config.base_env
      with Not_found -> ""
    in
    if wpf <> "" then
      let fname = adm_file (conf.Config.bname ^ "_u.txt") in
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

let read_gen_auth_file fname =
  let fname = bpath fname in
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
      | NotesLinks.WLperson (j, _, text, _)
      | NotesLinks.WLwizard (j, _, text) ->
          if in_text case_sens s text then true else loop false j
      | NotesLinks.WLnone -> loop false (i + 1)
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
  Output.printf conf "<table width=\"95%%\" border=\"%d\">\n" conf.Config.border;
  Output.printf conf "<tr align=\"%s\" valign=\"top\">\n" conf.Config.left;
  (let _ =
     List.fold_left
       (fun list len ->
         let rec loop n list =
           if n = 0 then (
             Output.print_sstring conf "</ul>\n</td>\n";
             list)
           else
             match list with
             | (kind, ord, elem) :: list ->
                 if n = len then
                   Output.printf conf "<td width=\"%d\">\n" (100 / ncols)
                 else if !kind <> Elem then Output.print_sstring conf "</ul>\n";
                 if !kind <> Elem then (
                   Output.printf conf "<h3 class=\"subtitle\">%s%s</h3>\n"
                     (if ord = "" then "..." else String.make 1 ord.[0])
                     (if !kind = HeadElem then ""
                     else " (" ^ transl conf "continued" ^ ")");
                   Output.print_sstring conf "<ul>\n");
                 Output.print_sstring conf "<li>";
                 wprint_elem elem;
                 Output.print_sstring conf "</li>\n";
                 loop (n - 1) list
             | [] -> []
         in
         loop len list)
       list len_list
   in
   ());
  Output.print_sstring conf "</tr>\n";
  Output.print_sstring conf "</table>\n";
  end_centered conf

let wprint_in_columns conf order wprint_elem list =
  let ncols =
    match p_getint conf.Config.env "ncols" with
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
(*  [Fonc] print_reference : config -> string -> int -> string -> unit    *)

(* ********************************************************************** *)

(** [Description] : Affiche la référence d'une personne
    [Args] :
      - conf : configuration de la base
      - fn   : first name
      - occ  : occ
      - sn   : surname
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                           *)
let print_reference conf fn occ sn =
  Output.print_sstring conf "<span class=\"reference\">";
  Output.printf conf " (%s %s.%d %s)"
    (transl conf "reference key")
    (Name.lower fn) occ (Name.lower sn);
  Output.print_sstring conf "</span>"

(* ********************************************************************** *)
(*  [Fonc] gen_print_tips : conf -> string -> unit                        *)

(* ********************************************************************** *)

(** [Description] : Affiche un tips.
    [Args] :
      - conf : configuration de la base
      - s    : le contenu du tips
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                       *)
let gen_print_tips conf s =
  Output.print_sstring conf "<div class=\"tips\">\n";
  Output.print_sstring conf "<table>\n";
  Output.print_sstring conf "<tr>\n";
  Output.print_sstring conf "<td>\n";
  Output.print_string conf s;
  Output.print_sstring conf "</td>\n";
  Output.print_sstring conf "</tr>\n";
  Output.print_sstring conf "</table>\n";
  Output.print_sstring conf "</div>\n";
  Output.print_sstring conf "<br>\n"

let print_tips_relationship conf =
  if
    p_getenv conf.Config.env "em" = Some "R"
    || p_getenv conf.Config.env "m" = Some "C"
  then
    Utf8.capitalize_fst (transl conf "select person to compute relationship")
    |> Adef.safe |> gen_print_tips conf

(* ********************************************************************** *)
(*  [Fonc] display_options : config -> string                             *)

(* ********************************************************************** *)

(** [Description] : Recherche dans l'URL les options d'affichage qui sont
                    données et renvoie la concaténation de ces options.
    [Args] :
      - conf : configuration de la base
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                           *)
let display_options conf =
  let s =
    Adef.escaped
    @@
    if p_getenv conf.Config.env "image" = Some "off" then "&image=off" else ""
  in
  let s =
    if p_getenv conf.Config.env "marriage" = Some "on" then
      let open Def in
      s ^>^ "&marriage=on"
    else s
  in
  let s =
    match p_getenv conf.Config.env "bd" with
    | Some i ->
        let open Def in
        s ^^^ "&bd=" ^<^ (Mutil.encode i :> Adef.escaped_string)
    | None -> s
  in
  match p_getenv conf.Config.env "color" with
  | Some c ->
      let open Def in
      s ^^^ "&color=" ^<^ (Mutil.encode c :> Adef.escaped_string)
  | None -> s

(* Hashtbl qui associe un user à la liste des dernières personnes visitées. *)
(* On en profite aussi pour stocker la date de la dernière visite.          *)
type cache_visited_t = (string, (Gwdb.iper * string) list) Hashtbl.t

(* ************************************************************************ *)
(*  [Fonc] cache_visited : config -> string                                 *)

(* ************************************************************************ *)

(** [Description] : Renvoie le chemin du fichier de cache.
    [Args] :
      - config : configuration de la base
    [Retour] : unit
    [Rem] : Exporté en clair hors de ce module.                             *)
let cache_visited conf =
  let bname =
    if Filename.check_suffix conf.Config.bname ".gwb" then conf.Config.bname
    else conf.Config.bname ^ ".gwb"
  in
  Filename.concat (bpath bname) "cache_visited"

(* ************************************************************************ *)
(*  [Fonc] read_visited : string -> cache_visited_t                         *)

(* ************************************************************************ *)

(** [Description] : List le fichier de cache des dernières fiches visités.
    [Args] :
      - fname : le fichier de cache (qui se trouve dans base.gwb)
    [Retour] : Hashtbl des user => dernières visites
    [Rem] : Exporté en clair hors de ce module.                             *)
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

(** [Description] : Met à jour le fichier de cache des visites.
    [Args] :
      - fname : le fichier de cache (qui se trouve dans base.gwb)
      - ht    : le compteur de visite
    [Retour] : unit
    [Rem] : Non exporté en clair hors de ce module.                         *)
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

(** [Description] : Vérifie si le user est ami ou magicien et met à jour
                    le fichier de cache.
    [Args] :
      - conf : configuration de la base
      - ip   : iper
    [Retour] : unit
    [Rem] : Exporté en clair hors de ce module.                             *)
let record_visited conf ip =
  if conf.Config.friend || conf.Config.wizard then
    let ht = read_visited conf in
    let time = (sprintf_today conf :> string) in
    let () =
      try
        let vl = Hashtbl.find ht conf.Config.user in
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
        let vl = Ext_list.take vl 10 in
        Hashtbl.replace ht conf.Config.user vl
      with Not_found -> Hashtbl.add ht conf.Config.user [ (ip, time) ]
    in
    write_visited conf ht

(**/**)

(* TODO OCaml 4.13 : use Array.find_opt *)
let array_mem_witn base ip witnesses wnotes =
  let get_note i =
    if i < Array.length wnotes then Gwdb.sou base wnotes.(i)
    else Gwdb.sou base Gwdb.empty_string
  in
  let rec loop i =
    if i = Array.length witnesses then None
    else if ip = fst witnesses.(i) then Some (snd witnesses.(i), get_note i)
    else loop (i + 1)
  in
  loop 0

module IperSet = Set.Make (struct
  type t = Gwdb.iper

  let compare = Stdlib.compare
end)

module IfamSet = Set.Make (struct
  type t = Gwdb.ifam

  let compare = Stdlib.compare
end)

let select_masc conf base ips =
  let poi =
    if conf.Config.wizard || conf.Config.friend then Gwdb.poi else pget conf
  in
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
                let cpl = Gwdb.foi base ifam in
                let fa = Gwdb.get_father cpl in
                let mo = Gwdb.get_mother cpl in
                add_asc gen fa (poi base fa);
                add_asc gen mo (poi base mo);
                loop tl)
              else
                let pgen = gen + 1 in
                let aux acc i =
                  let p = poi base i in
                  match Gwdb.get_parents p with
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
                let cpl = Gwdb.foi base ifam in
                let fa = Gwdb.get_father cpl in
                let mo = Gwdb.get_mother cpl in
                loop (aux (aux tl fa) mo))
    in
    loop
  in
  List.iter
    (fun (ip, max_gen) ->
      match Gwdb.get_parents @@ poi base ip with
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
          let sp = Gutil.spouse ip (Gwdb.foi base ifam) in
          Hashtbl.replace desc sp (pget conf base sp))
        (Gwdb.get_family p);
      if gen > gen_desc then
        List.iter (loop_desc (gen - 1)) @@ Gwdb.children_of_p base p)
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
    let fam = Gwdb.foi base ifam in
    pauth (Gwdb.get_father fam |> Gwdb.poi base)
    && pauth (Gwdb.get_mother fam |> Gwdb.poi base)
  in
  match w with
  | Warning.BigAgeBetweenSpouses (p1, p2, _) -> pauth p1 && pauth p2
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

let designation base p = Gutil.designation base p |> escape_html

let has_children base u =
  Array.exists
    (fun ifam ->
      let des = Gwdb.foi base ifam in
      Array.length (Gwdb.get_children des) > 0)
    (Gwdb.get_family u)
