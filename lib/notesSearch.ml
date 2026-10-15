(* Text search helpers for misc notes: plain-text extraction, occurrence
   counting and highlighted snippets. Matching uses [Util.find_in_text], so it
   follows the same rules as [Util.in_text]. *)

let decode_entity m i =
  let entities =
    [
      ("&amp;", "&");
      ("&lt;", "<");
      ("&gt;", ">");
      ("&quot;", "\"");
      ("&#39;", "'");
      ("&apos;", "'");
      ("&nbsp;", " ");
    ]
  in
  let len = String.length m in
  List.find_map
    (fun (e, c) ->
      let n = String.length e in
      if i + n <= len && String.sub m i n = e then Some (c, i + n) else None)
    entities

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

let count_occurrences case_sens s p =
  let rec loop n i =
    match Util.find_in_text case_sens s p i with
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

(* [p] restricted to [a, b) as HTML: escaped, every occurrence of [s]
   wrapped in <span class="found">, with an ellipsis on each truncated
   side. *)
let highlighted_range case_sens s p a b =
  let buf = Buffer.create (b - a + 64) in
  let add_escaped i j =
    if j > i then
      Buffer.add_string buf (Util.escape_html (String.sub p i (j - i)) :> string)
  in
  if a > 0 then Buffer.add_string buf "… ";
  let rec loop i =
    match Util.find_in_text case_sens s p i with
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

let snippet ?(context = 80) case_sens s p =
  if p = "" then ""
  else
    match Util.find_in_text case_sens s p 0 with
    | Some (i, j) ->
        let a = snippet_start p i context in
        let b = snippet_stop p j context in
        highlighted_range case_sens s p a b
    | None ->
        let b = snippet_stop p 0 (2 * context) in
        highlighted_range case_sens s p 0 b

let highlight case_sens s p = highlighted_range case_sens s p 0 (String.length p)
