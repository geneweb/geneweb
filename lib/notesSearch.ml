(* Text search helpers for misc notes: plain-text extraction, occurrence
   counting and highlighted snippets. Matching uses [Util.find_in_text], so it
   follows the same rules as [Util.in_text]. *)

let named_entities =
  [
    ("amp", "&");
    ("lt", "<");
    ("gt", ">");
    ("quot", "\"");
    ("apos", "'");
    ("nbsp", " ");
    ("hellip", "…");
    ("mdash", "—");
    ("ndash", "–");
    ("laquo", "«");
    ("raquo", "»");
    ("lsquo", "‘");
    ("rsquo", "’");
    ("ldquo", "“");
    ("rdquo", "”");
    ("euro", "€");
    ("copy", "©");
  ]

(* [decode_entity m i] decodes the character reference starting with '&' at
   [i]: a few named entities and numeric ones (&#38; &#x26;), which is what
   [Util.escape_html] produces. Returns the decoded UTF-8 text and the
   position after ';'. *)
let decode_entity m i =
  let len = String.length m in
  match String.index_from_opt m i ';' with
  | Some k when k - i <= 10 -> (
      let body = String.sub m (i + 1) (k - i - 1) in
      let numeric =
        if String.length body >= 2 && body.[0] = '#' then
          let digits = String.sub body 1 (String.length body - 1) in
          let digits =
            if digits.[0] = 'x' || digits.[0] = 'X' then
              "0x" ^ String.sub digits 1 (String.length digits - 1)
            else digits
          in
          match int_of_string_opt digits with
          | Some n when n > 0 && Uchar.is_valid n ->
              let b = Buffer.create 4 in
              Buffer.add_utf_8_uchar b (Uchar.of_int n);
              Some (Buffer.contents b)
          | Some _ | None -> None
        else None
      in
      match numeric with
      | Some c -> Some (c, k + 1)
      | None -> (
          match List.assoc_opt body named_entities with
          | Some c when k < len -> Some (c, k + 1)
          | Some _ | None -> None))
  | Some _ | None -> None

(* [true] if the tag whose name starts at [i] (after '<' or '</') separates
   words when rendered; inline tags such as <b> or <a> do not. *)
let is_block_tag m i =
  let len = String.length m in
  let i = if i < len && m.[i] = '/' then i + 1 else i in
  let rec name_end j =
    if j < len then
      match m.[j] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> name_end (j + 1)
      | _ -> j
    else j
  in
  let name = String.lowercase_ascii (String.sub m i (name_end i - i)) in
  List.mem name
    [
      "br"; "p"; "div"; "li"; "ul"; "ol"; "dl"; "dt"; "dd"; "tr"; "td"; "th";
      "table"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "hr"; "blockquote"; "pre";
      "section"; "article"; "figure"; "figcaption"; "img";
    ]

(* The text a reader sees in a note, close enough for searching and
   snippets: HTML tags are dropped, wiki links are replaced by their
   displayed text (as [Util.in_text] does), wiki markup characters
   ('' and =) are removed, common entities are decoded and white space
   is collapsed to single spaces. *)
let plain_text m =
  let len = String.length m in
  let b = Buffer.create len in
  let add_space () =
    let n = Buffer.length b in
    if n > 0 && Buffer.nth b (n - 1) <> ' ' then Buffer.add_char b ' '
  in
  let add_text s =
    String.iter
      (fun c ->
        match c with
        | ' ' | '\n' | '\r' | '\t' -> add_space ()
        | c -> Buffer.add_char b c)
      s
  in
  let rec loop in_tag i =
    if i >= len then ()
    else if in_tag then
      if m.[i] = '>' then loop false (i + 1) else loop true (i + 1)
    else
      match m.[i] with
      | '<' ->
          if is_block_tag m (i + 1) then add_space ();
          loop true (i + 1)
      | '[' when i + 1 < len && m.[i + 1] = '[' ->
          let j =
            match NotesLinks.misc_notes_link m i with
            | NotesLinks.WLpage (j, _, _, _, text)
            | NotesLinks.WLperson (j, _, Some text, _, _)
            | NotesLinks.WLwizard (j, _, text) ->
                add_text text;
                j
            | NotesLinks.WLperson (j, (fn, sn, _), None, _, _) ->
                add_text (fn ^ " " ^ sn);
                j
            | NotesLinks.WLimage (j, _, alt, _) ->
                add_text alt;
                j
            | NotesLinks.WLnone (j, _) -> j
          in
          (* guard against a parser that would not advance *)
          loop false (if j > i then j else i + 2)
      | '\'' when i + 1 < len && m.[i + 1] = '\'' ->
          let rec skip j = if j < len && m.[j] = '\'' then skip (j + 1) else j in
          loop false (skip i)
      | '=' -> loop false (i + 1)
      | ' ' | '\n' | '\r' | '\t' ->
          add_space ();
          loop false (i + 1)
      | '&' -> (
          match decode_entity m i with
          | Some (c, j) ->
              add_text c;
              loop false j
          | None ->
              Buffer.add_char b '&';
              loop false (i + 1))
      | c ->
          Buffer.add_char b c;
          loop false (i + 1)
  in
  loop false 0;
  String.trim (Buffer.contents b)

(* Split a query into terms: words separated by white space, or phrases
   between double quotes ("Saint Pair"). A term starting with '-' (-word,
   -"some phrase") is excluded; a '-' inside a word (Saint-Pair) is not
   special, and a lone '-' is ignored. Empty terms are dropped, and
   duplicates are removed (comparing as [Util.in_text] does). Returns the
   required terms and the excluded ones. *)
let query case_sens q =
  let len = String.length q in
  let is_sep c = match c with ' ' | '\n' | '\r' | '\t' -> true | _ -> false in
  let word neg i j acc =
    if j > i then (neg, String.sub q i (j - i)) :: acc else acc
  in
  (* term starting at [i], excluded if [neg] *)
  let term neg acc i =
    if i < len && q.[i] = '"' then
      match String.index_from_opt q (i + 1) '"' with
      | Some j -> (word neg (i + 1) j acc, j + 1)
      | None -> (word neg (i + 1) len acc, len)
    else
      let rec stop j =
        if j < len && not (is_sep q.[j] || q.[j] = '"') then stop (j + 1)
        else j
      in
      let j = stop i in
      (word neg i j acc, j)
  in
  let rec loop acc i =
    if i >= len then List.rev acc
    else if is_sep q.[i] then loop acc (i + 1)
    else if q.[i] = '-' then
      let acc, j = term true acc (i + 1) in
      loop acc (max j (i + 1))
    else
      let acc, j = term false acc i in
      loop acc j
  in
  let same t1 t2 =
    String.length t1 = String.length t2
    && Util.find_in_text case_sens t1 t2 0 = Some (0, String.length t2)
  in
  let add l t =
    let t = String.trim t in
    if t = "" || List.exists (same t) l then l else t :: l
  in
  let req, excl =
    List.fold_left
      (fun (req, excl) (neg, t) ->
        if neg then (req, add excl t) else (add req t, excl))
      ([], []) (loop [] 0)
  in
  (List.rev req, List.rev excl)

let terms case_sens q = fst (query case_sens q)

(* Earliest occurrence of any of [terms] in [p] at or after [i]; at equal
   start, the longest one, so that overlapping terms (henri, henriette)
   highlight the larger span. *)
let find_any case_sens terms p i =
  List.fold_left
    (fun best t ->
      match (Util.find_in_text case_sens t p i, best) with
      | None, _ -> best
      | Some m, None -> Some m
      | Some (x, y), Some (bx, by) ->
          if x < bx || (x = bx && y > by) then Some (x, y) else best)
    None terms

let mem_all case_sens terms p =
  List.for_all (fun t -> Util.find_in_text case_sens t p 0 <> None) terms

let count_occurrences case_sens terms p =
  let rec loop n i =
    match find_any case_sens terms p i with
    | Some (_, j) -> loop (n + 1) j
    | None -> n
  in
  loop 0 0

(* start of the UTF-8 character containing byte [i] *)
let utf8_start p i =
  let rec loop i =
    if i > 0 && Char.code p.[i] land 0xC0 = 0x80 then loop (i - 1) else i
  in
  loop i

(* Move [i] back by about [n] bytes, then forward to the start of a word. *)
let snippet_start p i n =
  if i <= n then 0
  else
    let k = utf8_start p (i - n) in
    match String.index_from_opt p k ' ' with
    | Some sp when sp < i -> sp + 1
    | Some _ | None -> k

(* Move [j] forward by about [n] bytes, then back to the end of a word. *)
let snippet_stop p j n =
  let len = String.length p in
  if j + n >= len then len
  else
    let k = utf8_start p (j + n) in
    match String.rindex_from_opt p k ' ' with
    | Some sp when sp > j -> sp
    | Some _ | None -> k

(* [p] restricted to [a, b) as HTML: escaped, every occurrence of [terms]
   wrapped in <span class="found">, with an ellipsis on each truncated
   side. *)
let highlighted_range case_sens terms p a b =
  let buf = Buffer.create (b - a + 64) in
  let add_escaped i j =
    if j > i then
      Buffer.add_string buf (Util.escape_html (String.sub p i (j - i)) :> string)
  in
  if a > 0 then Buffer.add_string buf "… ";
  let rec loop i =
    match find_any case_sens terms p i with
    | Some (x, y) when y <= b ->
        add_escaped i x;
        Buffer.add_string buf {|<span class="found">|};
        add_escaped x y;
        Buffer.add_string buf "</span>";
        loop y
    | Some _ | None -> add_escaped i b
  in
  loop a;
  if b < String.length p then Buffer.add_string buf " …";
  Buffer.contents buf

let snippet ?(context = 80) case_sens terms p =
  if p = "" then ""
  else
    match find_any case_sens terms p 0 with
    | Some (i, j) ->
        let a = snippet_start p i context in
        let b = snippet_stop p j context in
        highlighted_range case_sens terms p a b
    | None ->
        let b = snippet_stop p 0 (2 * context) in
        highlighted_range case_sens terms p 0 b

let highlight case_sens terms p =
  highlighted_range case_sens terms p 0 (String.length p)

let is_space c =
  match c with ' ' | '\n' | '\r' | '\t' -> true | _ -> false

(* Highlight occurrences in one HTML text run (no tag inside). Matching is
   done on the decoded text, where an entity is one character and a white
   space run is one space; each occurrence is wrapped around the original
   bytes it comes from, so the output stays valid, already escaped HTML. *)
let highlight_text_run case_sens terms first buf run =
  let len = String.length run in
  (* units: original [start, stop) and decoded text *)
  let units =
    let rec loop acc i =
      if i >= len then List.rev acc
      else if is_space run.[i] then
        let rec skip j = if j < len && is_space run.[j] then skip (j + 1) else j in
        let j = skip i in
        loop ((i, j, " ") :: acc) j
      else if run.[i] = '&' then
        match decode_entity run i with
        | Some (c, j) -> loop ((i, j, c) :: acc) j
        | None -> loop ((i, i + 1, "&") :: acc) (i + 1)
      else
        let j = min len (i + Utf8.nbc run.[i]) in
        loop ((i, j, String.sub run i (j - i)) :: acc) j
    in
    Array.of_list (loop [] 0)
  in
  let d = String.concat "" (Array.to_list (Array.map (fun (_, _, d) -> d) units)) in
  (* unit index of each decoded byte *)
  let owners = Array.make (String.length d) 0 in
  ignore
    (Array.fold_left
       (fun (k, pos) (_, _, u) ->
         Array.fill owners pos (String.length u) k;
         (k + 1, pos + String.length u))
       (0, 0) units);
  let rec loop pos i =
    match find_any case_sens terms d i with
    | Some (x, y) when y > x ->
        let a, _, _ = units.(owners.(x)) in
        let _, b, _ = units.(owners.(y - 1)) in
        Buffer.add_string buf (String.sub run pos (a - pos));
        if !first then (
          first := false;
          Buffer.add_string buf {|<span class="found" id="first-found">|})
        else Buffer.add_string buf {|<span class="found">|};
        Buffer.add_string buf (String.sub run a (b - a));
        Buffer.add_string buf "</span>";
        loop b y
    | Some _ | None -> Buffer.add_string buf (String.sub run pos (len - pos))
  in
  loop 0 0

let raw_text_elements = [ "script"; "style"; "textarea"; "title" ]

let tag_name m i =
  let len = String.length m in
  let rec stop j =
    if j < len then
      match m.[j] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> stop (j + 1)
      | _ -> j
    else j
  in
  String.lowercase_ascii (String.sub m i (stop i - i))

(* position after the '>' closing the tag opened at [i], skipping quoted
   attribute values *)
let tag_end m i =
  let len = String.length m in
  let rec loop q j =
    if j >= len then len
    else
      match (q, m.[j]) with
      | None, '>' -> j + 1
      | None, (('"' | '\'') as c) -> loop (Some c) (j + 1)
      | Some c, c' when c = c' -> loop None (j + 1)
      | (None | Some _), _ -> loop q (j + 1)
  in
  loop None (i + 1)

let find_from m i sub =
  let n = String.length sub and len = String.length m in
  let rec loop j =
    if j + n > len then None
    else if String.sub m j n = sub then Some j
    else loop (j + 1)
  in
  loop i

(* position of the "</name" closing the raw text element, case-insensitive *)
let find_closing m i name =
  let lm = String.lowercase_ascii m in
  match find_from lm i ("</" ^ name) with Some j -> j | None -> String.length m

let highlight_html case_sens terms html =
  if terms = [] then html
  else
    let len = String.length html in
    let buf = Buffer.create (len + 256) in
    let first = ref true in
    let rec loop i =
      if i >= len then ()
      else if html.[i] = '<' then
        if i + 3 < len && String.sub html i 4 = "<!--" then (
          let j =
            match find_from html (i + 4) "-->" with
            | Some j -> j + 3
            | None -> len
          in
          Buffer.add_string buf (String.sub html i (j - i));
          loop j)
        else
          let j = tag_end html i in
          Buffer.add_string buf (String.sub html i (j - i));
          let name = tag_name html (i + 1) in
          if List.mem name raw_text_elements then (
            let k = find_closing html j name in
            Buffer.add_string buf (String.sub html j (k - j));
            loop k)
          else loop j
      else
        let j =
          match String.index_from_opt html i '<' with Some j -> j | None -> len
        in
        highlight_text_run case_sens terms first buf (String.sub html i (j - i));
        loop j
    in
    loop 0;
    Buffer.contents buf
