open Def
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

(* Basic types *)
type dict_type =
  | Fnames
  | Snames
  | Places
  | PubNames
  | Qualifiers
  | Aliases
  | Occupation
  | Estates
  | Titles
  | Sources

type error_type =
  | InvisibleCharacters
  | BadCapitalization
  | MultipleSpaces
  | NonBreakingSpace

type highlight_style = {
  make_class : string -> string;
  make_title : ?code:string -> ?name:string -> Config.config -> string option;
}

let first_word s =
  try
    let i = String.index s ' ' in
    if i = String.length s then if i > 6 then String.sub s 0 6 else s
    else String.sub s 0 i
  with Not_found -> if String.length s > 6 then String.sub s 0 6 else s

(* Split a string into its words, filtering out empty strings *)
let split_words s =
  let words = String.split_on_char ' ' s in
  List.filter (fun w -> w <> "") words

(* Detect if a word is a Roman numeral with various notations:
   - Basic roman numerals: I, II, III, IV, V, etc.
   - French ordinals: Ier, Ire
   - English/German style: I., II., III., etc. *)
let is_roman_numeral s =
  let roman = Str.regexp "^[IVX]+[.]?$" in
  let first_ordinal = Str.regexp "^I\\(ᵉʳ\\|ʳᵉ\\|er\\|re\\)$" in
  try Str.string_match roman s 0 || Str.string_match first_ordinal s 0
  with _ -> false

(* Multiple Spaces functions *)
let has_multiple_spaces s =
  let rec aux i =
    if i >= String.length s - 1 then false
    else if s.[i] = ' ' && s.[i + 1] = ' ' then true
    else aux (i + 1)
  in
  aux 0

(* Check if the character following nbsp is part of a roman numeral *)
let has_roman_after_nbsp s i =
  if i + 2 >= String.length s then false
  else
    let rest = String.sub s (i + 2) (String.length s - (i + 2)) in
    let next_word =
      try String.sub rest 0 (String.index rest ' ') with Not_found -> rest
    in
    is_roman_numeral next_word

let find_multiple_spaces_positions s =
  let rec aux acc i =
    if i >= String.length s then List.rev acc
    else if s.[i] = ' ' then
      let rec count_spaces pos positions =
        if pos >= String.length s || s.[pos] <> ' ' then (pos, positions)
        else count_spaces (pos + 1) (pos :: positions)
      in
      let next_pos, spaces = count_spaces i [] in
      if List.length spaces > 1 then aux (spaces @ acc) next_pos
      else aux acc (i + 1)
    else aux acc (i + 1)
  in
  aux [] 0

let fix_multiple_spaces s =
  let rec aux acc i =
    if i >= String.length s then String.concat "" (List.rev acc)
    else if s.[i] = ' ' then
      let rec skip_spaces j =
        if j >= String.length s || s.[j] != ' ' then j else skip_spaces (j + 1)
      in
      let next = skip_spaces (i + 1) in
      aux (" " :: acc) next
    else aux (String.make 1 s.[i] :: acc) (i + 1)
  in
  aux [] 0

(* Non-breaking spaces functions *)
let has_regular_nbsp s i =
  i + 1 < String.length s && s.[i] = '\xC2' && s.[i + 1] = '\xA0'

let has_narrow_nbsp s i =
  i + 2 < String.length s
  && s.[i] = '\xE2'
  && s.[i + 1] = '\x80'
  && s.[i + 2] = '\xAF'

let has_non_breaking_space s =
  let rec aux i =
    if i >= String.length s - 1 then false
    else if has_regular_nbsp s i then
      if has_roman_after_nbsp s i then false else true
    else if has_narrow_nbsp s i then
      if has_roman_after_nbsp s i then false else true
    else aux (i + 1)
  in
  aux 0

let find_non_breaking_space_positions s =
  let rec aux acc i =
    if i >= String.length s - 1 then List.rev acc
    else if has_regular_nbsp s i then
      if has_roman_after_nbsp s i then aux acc (i + 2)
      else aux (i :: (i + 1) :: acc) (i + 2)
    else if has_narrow_nbsp s i then
      if has_roman_after_nbsp s i then aux acc (i + 3)
      else aux (i :: (i + 1) :: (i + 2) :: acc) (i + 3)
    else aux acc (i + 1)
  in
  aux [] 0

(* Detect if a word starts with Irish name prefixes Mac/Mc/Fitz *)
let is_irish_prefix s =
  let prefixes = [ "Mac"; "Mc"; "Fitz" ] in
  List.exists
    (fun prefix ->
      String.length s >= String.length prefix
      && String.sub s 0 (String.length prefix) = prefix)
    prefixes

(* Check if a word is either a Roman numeral or starts with an Irish prefix
   Examples of valid words:
   - Roman numerals: "XIV", "III", "IX"
   - Irish prefixes: "MacDonald", "McLeod", "FitzGerald" *)
let is_allowed_word s = is_roman_numeral s || is_irish_prefix s

(* Detect invalid capitalization patterns:
   - Multiple uppercase letters in sequence
   - Lowercase followed by uppercase
   - Uppercase, lowercase, uppercase sequence *)

(* Bad Capitalization functions *)
let has_bad_capitalization_pattern s =
  let re = Str.regexp "\\([A-Z]\\{2,\\}\\|[a-z][A-Z]\\|[A-Z][a-z][A-Z]\\)" in
  try
    let _ = Str.search_forward re s 0 in
    true
  with Not_found -> false

(* Capitalization check function
   Examines each word for valid patterns for some books
   For other types, just checks for invalid capitalization patterns *)
let has_legitimate_mixed_case dict s =
  let words = split_words s in
  match dict with
  | Fnames | Snames | PubNames | Aliases | Places ->
      (* For these types, we check if each word is either:
         - A Roman numeral (e.g. "MacDonald III")
         - An Irish prefix name (e.g. "MacArthur", "FitzGerald") *)
      let rec check_words = function
        | [] -> true (* All words are valid *)
        | word :: rest ->
            if has_bad_capitalization_pattern word && not (is_allowed_word word)
            then false
            else check_words rest
      in
      check_words words
  | _ -> false

let has_bad_capitalization dict s =
  match dict with
  | Sources -> false
  | _ ->
      has_bad_capitalization_pattern s && not (has_legitimate_mixed_case dict s)

let find_bad_capitalization_positions s =
  let re = Str.regexp "\\([A-Z]\\{2,\\}\\|[a-z][A-Z]\\|[A-Z][a-z][A-Z]\\)" in
  let rec aux acc pos =
    try
      let pos' = Str.search_forward re s pos in
      let len = String.length (Str.matched_string s) in
      let new_pos = pos' + len in
      let positions = List.init len (fun i -> pos' + i) in
      aux (positions @ acc) new_pos
    with Not_found -> List.rev acc
  in
  aux [] 0

(* Table des caractères invisibles indésirables
   Association code point héxadécimaux -> nom officiel Unicode *)
(* Table des caractères invisibles indésirables *)
let invisible_chars =
  [|
    ("00AD", "SOFT HYPHEN");
    ("034F", "COMBINING GRAPHEME JOINER");
    ("0600", "ARABIC NUMBER SIGN");
    ("0601", "ARABIC SIGN SANAH");
    ("0602", "ARABIC FOOTNOTE MARKER");
    ("0603", "ARABIC SIGN SAFHA");
    ("06DD", "ARABIC END OF AYAH");
    ("070F", "SYRIAC ABBREVIATION MARK");
    ("0F0C", "TIBETAN MARK DELIMITER");
    ("115F", "HANGUL CHOSEONG FILLER");
    ("1160", "HANGUL JUNGSEONG FILLER");
    ("1680", "OGHAM SPACE MARK");
    ("180E", "MONGOLIAN VOWEL SEPARATOR");
    ("2000", "EN QUAD");
    ("2001", "EM QUAD");
    ("2002", "EN SPACE");
    ("2003", "EM SPACE");
    ("2004", "THREE-PER-EM SPACE");
    ("2005", "FOUR-PER-EM SPACE");
    ("2006", "SIX-PER-EM SPACE");
    ("2007", "FIGURE SPACE");
    ("2008", "PUNCTUATION SPACE");
    ("2009", "THIN SPACE");
    ("200A", "HAIR SPACE");
    ("200B", "ZERO WIDTH SPACE");
    ("200C", "ZERO WIDTH NON-JOINER");
    ("200D", "ZERO WIDTH JOINER");
    ("200E", "LEFT-TO-RIGHT MARK");
    ("200F", "RIGHT-TO-LEFT MARK");
    ("205F", "MEDIUM MATHEMATICAL SPACE");
    ("2060", "WORD JOINER");
    ("2061", "FUNCTION APPLICATION");
    ("2062", "INVISIBLE TIMES");
    ("2063", "INVISIBLE SEPARATOR");
    ("2064", "INVISIBLE PLUS");
    ("206A", "INHIBIT SYMMETRIC SWAPPING");
    ("206B", "ACTIVATE SYMMETRIC SWAPPING");
    ("206C", "INHIBIT ARABIC FORM SHAPING");
    ("206D", "ACTIVATE ARABIC FORM SHAPING");
    ("206E", "NATIONAL DIGIT SHAPES");
    ("206F", "NOMINAL DIGIT SHAPES");
    ("3000", "IDEOGRAPHIC SPACE");
    ("FEFF", "ZERO WIDTH NO-BREAK SPACE");
  |]

let is_zero_width hex =
  match hex with "200B" | "200C" | "200D" | "FEFF" -> true | _ -> false

let get_unicode_info code =
  let hex = Printf.sprintf "%04X" code in
  let rec find_in_array i =
    if i >= Array.length invisible_chars then
      (hex, "UNKNOWN INVISIBLE CHARACTER")
    else
      let h, name = invisible_chars.(i) in
      if h = hex then (hex, name) else find_in_array (i + 1)
  in
  find_in_array 0

let get_unicode_point s i =
  let n = Char.code (String.get s i) in
  if n < 0x80 then (n, 1)
  else if n <= 0xdf && i + 1 < String.length s then
    (((n - 0xc0) lsl 6) lor (0x7f land Char.code (String.get s (i + 1))), 2)
  else if n <= 0xef && i + 2 < String.length s then
    let n' = n - 0xe0 in
    let m = Char.code (String.get s (i + 1)) in
    let n' = (n' lsl 6) lor (0x7f land m) in
    let m = Char.code (String.get s (i + 2)) in
    ((n' lsl 6) lor (0x7f land m), 3)
  else if i + 3 < String.length s then
    let n' = n - 0xf0 in
    let m = Char.code (String.get s (i + 1)) in
    let n' = (n' lsl 6) lor (0x7f land m) in
    let m = Char.code (String.get s (i + 2)) in
    let n' = (n' lsl 6) lor (0x7f land m) in
    let m = Char.code (String.get s (i + 3)) in
    ((n' lsl 6) lor (0x7f land m), 4)
  else (n, 1)

let hex_to_int hex = int_of_string ("0x" ^ hex)

let has_invisible_chars s =
  let len = String.length s in
  let rec aux i =
    if i >= len then false
    else
      let code, size = get_unicode_point s i in
      if Array.exists (fun (h, _) -> hex_to_int h = code) invisible_chars then
        true
      else aux (i + size)
  in
  aux 0

let find_invisible_positions s =
  let len = String.length s in
  let rec aux acc i =
    if i >= len then List.rev acc
    else
      let code, size = get_unicode_point s i in
      if Array.exists (fun (h, _) -> hex_to_int h = code) invisible_chars then
        aux (i :: acc) (i + size)
      else aux acc (i + size)
  in
  aux [] 0

let fix_invisible_chars s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec aux i =
    if i >= len then Buffer.contents buf
    else
      let code, size = get_unicode_point s i in
      if Array.exists (fun (h, _) -> hex_to_int h = code) invisible_chars then (
        Buffer.add_char buf ' ';
        aux (i + size))
      else (
        Buffer.add_char buf s.[i];
        aux (i + 1))
  in
  aux 0

(* Generic error handling *)
let find_error_positions error_type s =
  match error_type with
  | InvisibleCharacters -> find_invisible_positions s
  | BadCapitalization -> find_bad_capitalization_positions s
  | MultipleSpaces -> find_multiple_spaces_positions s
  | NonBreakingSpace -> find_non_breaking_space_positions s

let fix_error error_type s =
  match error_type with
  | InvisibleCharacters -> fix_invisible_chars s
  | BadCapitalization -> s (* Return original string *)
  | MultipleSpaces -> fix_multiple_spaces s
  | NonBreakingSpace -> s

(* Define style and tooltip of error types *)
let get_highlight_style error_type conf =
  match error_type with
  | NonBreakingSpace ->
      {
        make_class =
          (fun s ->
            let is_narrow =
              String.length s >= 3
              && s.[0] = '\xE2'
              && s.[1] = '\x80'
              && s.[2] = '\xAF'
            in
            if is_narrow then " narrow-nbsp" else "");
        make_title =
          (fun ?code ?name:_ _ ->
            match code with
            | Some "00A0" -> Some "U+00A0 NON-BREAKING SPACE"
            | Some "202F" -> Some "U+202F NARROW NON-BREAKING SPACE"
            | _ -> None);
      }
  | InvisibleCharacters ->
      {
        make_class =
          (fun hex -> if is_zero_width hex then " zero-width-char" else "");
        make_title =
          (fun ?code ?name _ ->
            let base =
              Printf.sprintf "U+%s %s"
                (Option.value code ~default:"")
                (Option.value name ~default:"")
            in
            let () =
              Printf.printf "code: %s\n" (Option.value code ~default:"")
            in
            if is_zero_width (Option.value code ~default:"") then
              Some
                (Printf.sprintf "%s (%s)" base
                   (Util.transl conf "zero-width character"))
            else Some base);
      }
  | _ ->
      {
        make_class = (fun _ -> "");
        make_title = (fun ?code:_ ?name:_ _ -> None);
      }

(* HTML Generation *)

let make_highlight_html s positions error_type conf =
  let buf = Buffer.create (String.length s * 2) in
  let style = get_highlight_style error_type conf in

  let rec process_char i in_span =
    if i >= String.length s then (
      if in_span then Buffer.add_string buf "</span>";
      Buffer.contents buf)
    else
      let is_highlight = List.mem i positions in
      if is_highlight then (
        let code, size = get_unicode_point s i in
        let hex, name = get_unicode_info code in
        let title_attr =
          match style.make_title ~code:hex ~name conf with
          | Some t -> Printf.sprintf " title=\"%s\"" t
          | None -> ""
        in
        let original = String.sub s i size in
        Printf.bprintf buf "<span class=\"hl-check-data%s\"%s>%s</span>"
          (style.make_class original)
          title_attr original;
        process_char (i + size) false)
      else (
        Buffer.add_char buf s.[i];
        process_char (i + 1) false)
  in
  process_char 0 false

let make_error_html conf data entry error_type =
  let s = first_word entry in
  let s1 = entry in
  let s2 = fix_error error_type entry in
  let positions = find_error_positions error_type entry in
  let highlighted = make_highlight_html entry positions error_type conf in

  let url =
    Printf.sprintf "%sm=MOD_DATA&data=%s&s=%s&s1=%s&s2=%s"
      (Util.commd conf :> string)
      data
      (Mutil.encode s :> string)
      (Mutil.encode s1 :> string)
      (Mutil.encode s2 :> string)
  in
  Printf.sprintf "<a href=\"%s\">%s</a>" url highlighted

(* Error collection *)
(* Event accessors *)
let get_epers_place evt = evt.epers_place
let get_epers_src evt = evt.epers_src
let get_efam_place evt = evt.efam_place
let get_efam_src evt = evt.efam_src

(* Basic getters returning lists *)
let get_birth_place_x p = [ Driver.get_birth_place p ]
let get_baptism_place_x p = [ Driver.get_baptism_place p ]
let get_death_place_x p = [ Driver.get_death_place p ]
let get_burial_place_x p = [ Driver.get_burial_place p ]
let get_birth_src_x p = [ Driver.get_birth_src p ]
let get_baptism_src_x p = [ Driver.get_baptism_src p ]
let get_death_src_x p = [ Driver.get_death_src p ]
let get_burial_src_x p = [ Driver.get_burial_src p ]
let get_psources_x p = [ Driver.get_psources p ]
let get_marriage_place_x fam = [ Driver.get_marriage_place fam ]
let get_marriage_src_x fam = [ Driver.get_marriage_src fam ]
let get_fsources_x fam = [ Driver.get_fsources fam ]

(* Generic collector for attributes - places or sources *)
let collect_attributes base p
    ~std_attrs (* list of standard attribute collectors *)
    ~epers_attr_get (* personal event attribute getter *)
    ~marriage_attr (* marriage attribute collector *)
    ~efam_attr_get (* family event attribute getter *) =
  let attrs = ref [] in
  List.iter
    (fun getter -> List.iter (fun x -> attrs := x :: !attrs) (getter p))
    std_attrs;
  List.iter
    (fun evt -> attrs := epers_attr_get evt :: !attrs)
    (Driver.get_pevents p);
  Array.iter
    (fun ifam ->
      let fam = Driver.foi base ifam in
      List.iter (fun x -> attrs := x :: !attrs) (marriage_attr fam);
      List.iter
        (fun evt -> attrs := efam_attr_get evt :: !attrs)
        (Driver.get_fevents fam))
    (Driver.get_family p);
  !attrs
  |> List.filter (fun i -> not (Driver.Istr.is_empty i))
  |> List.sort_uniq compare

let collect_places base p =
  collect_attributes base p
    ~std_attrs:
      [
        get_birth_place_x;
        get_baptism_place_x;
        get_death_place_x;
        get_burial_place_x;
      ]
    ~epers_attr_get:get_epers_place ~marriage_attr:get_marriage_place_x
    ~efam_attr_get:get_efam_place

let collect_sources base p =
  let sources =
    collect_attributes base p
      ~std_attrs:
        [
          get_birth_src_x;
          get_baptism_src_x;
          get_death_src_x;
          get_burial_src_x;
          get_psources_x;
        ]
      ~epers_attr_get:get_epers_src ~marriage_attr:get_marriage_src_x
      ~efam_attr_get:get_efam_src
  in
  let sources = ref sources in
  Array.iter
    (fun ifam ->
      let fam = Driver.foi base ifam in
      List.iter (fun x -> sources := x :: !sources) (get_fsources_x fam))
    (Driver.get_family p);
  List.sort_uniq compare !sources

let collect_dict_strings base = function
  | Fnames -> fun p -> [ Driver.get_first_name p ]
  | Snames -> fun p -> [ Driver.get_surname p ]
  | Places -> collect_places base
  | PubNames -> fun p -> [ Driver.get_public_name p ]
  | Qualifiers -> Driver.get_qualifiers
  | Aliases -> Driver.get_aliases
  | Occupation -> fun p -> [ Driver.get_occupation p ]
  | Titles -> fun p -> List.map (fun t -> t.t_ident) (Driver.get_titles p)
  | Estates -> fun p -> List.map (fun t -> t.t_place) (Driver.get_titles p)
  | Sources -> collect_sources base

let collect_all_errors base dict =
  let strings_with_errors = Hashtbl.create 1024 in
  let add_error s err =
    match Hashtbl.find_opt strings_with_errors s with
    | Some errs -> Hashtbl.replace strings_with_errors s (err :: errs)
    | None -> Hashtbl.add strings_with_errors s [ err ]
  in

  let check_string s =
    if has_invisible_chars s then add_error s InvisibleCharacters;
    if has_bad_capitalization dict s then add_error s BadCapitalization;
    if has_multiple_spaces s then add_error s MultipleSpaces;
    if has_non_breaking_space s then add_error s NonBreakingSpace
  in

  let collect_strings = collect_dict_strings base dict in

  Collection.iter
    (fun i ->
      let p = Driver.poi base i in
      List.iter
        (fun istr ->
          let s = Driver.sou base istr in
          if s <> "" then check_string s)
        (collect_strings p))
    (Driver.ipers base);

  let result = ref [] in
  Hashtbl.iter
    (fun s errs -> result := (s, errs) :: !result)
    strings_with_errors;
  !result
