open Def
open Config
module Driver = Geneweb_db.Driver

module IstrMap = Map.Make (struct
  type t = Driver.istr

  let compare = compare
end)

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

type checkdata_entry = Driver.istr * string

module ErrorSet = Set.Make (struct
  type t = error_type

  let compare = compare
end)

(* Pré-calcul des sets d'erreurs *)
let make_error_set = function
  | [] ->
      ErrorSet.of_list
        [
          InvisibleCharacters;
          BadCapitalization;
          MultipleSpaces;
          NonBreakingSpace;
        ]
  | lst -> ErrorSet.of_list lst

(* Split a string into its words, filtering out empty strings *)
let split_words s =
  let words = String.split_on_char ' ' s in
  List.filter (fun w -> w <> "") words

(* Detect if a word is a Roman numeral with various notations:
   - Basic roman numerals: I, II, III, IV, V, etc.
   - French ordinals: Ier, Ire
   - English/German style: I., II., III., etc. *)
let roman_re = lazy (Str.regexp "^[IVX]+[.]?$")
let first_ordinal_re = lazy (Str.regexp "^I\\(ᵉʳ\\|ʳᵉ\\|er\\|re\\)$")

let bad_cap_re =
  lazy (Str.regexp "\\([A-Z]\\{2,\\}\\|[a-z][A-Z]\\|[A-Z][a-z][A-Z]\\)")

let nbsp_re = lazy (Str.regexp "\xC2\xA0\\|\xE2\x80\xAF")

let is_roman_numeral s =
  try
    Str.string_match (Lazy.force roman_re) s 0
    || Str.string_match (Lazy.force first_ordinal_re) s 0
  with _ -> false

(* Multiple Spaces functions *)
(* Check if the character following nbsp is part of a roman numeral *)
let has_roman_after_nbsp s i =
  if i + 2 >= String.length s then false
  else
    let rest = String.sub s (i + 2) (String.length s - (i + 2)) in
    let next_word =
      try String.sub rest 0 (String.index rest ' ') with Not_found -> rest
    in
    is_roman_numeral next_word

let is_any_space s pos =
  if pos >= String.length s then false
  else if s.[pos] = ' ' then true
  else if pos + 1 < String.length s && s.[pos] = '\xC2' && s.[pos + 1] = '\xA0'
  then true
  else if
    pos + 2 < String.length s
    && s.[pos] = '\xE2'
    && s.[pos + 1] = '\x80'
    && s.[pos + 2] = '\xAF'
  then true
  else false

let has_multiple_spaces s =
  let len = String.length s in
  let rec find_spaces byte_pos =
    if byte_pos >= len then false
    else if is_any_space s byte_pos then
      let next_pos = Utf8.next s byte_pos in
      if next_pos < len && is_any_space s next_pos then true
      else find_spaces next_pos
    else find_spaces (Utf8.next s byte_pos)
  in
  find_spaces 0

let find_multiple_spaces_positions s =
  let positions = ref [] in
  let len = String.length s in
  let rec find_sequences byte_pos =
    if byte_pos >= len then List.rev !positions
    else if is_any_space s byte_pos then (
      let rec collect_sequence pos acc_positions =
        if pos >= len then (pos, List.rev acc_positions)
        else if is_any_space s pos then
          let next_pos = Utf8.next s pos in
          collect_sequence next_pos (pos :: acc_positions)
        else (pos, List.rev acc_positions)
      in
      let end_pos, sequence_positions = collect_sequence byte_pos [] in
      if List.length sequence_positions >= 2 then
        positions := sequence_positions @ !positions;
      find_sequences end_pos)
    else find_sequences (Utf8.next s byte_pos)
  in
  find_sequences 0

let fix_multiple_spaces =
  let buf = Buffer.create 256 in
  fun s ->
    Buffer.clear buf;
    let len = String.length s in
    let rec loop i in_space =
      if i >= len then Buffer.contents buf
      else
        let is_space = is_any_space s i in
        let char_size =
          if
            i + 2 < len
            && s.[i] = '\xE2'
            && s.[i + 1] = '\x80'
            && s.[i + 2] = '\xAF'
          then 3
          else if i + 1 < len && s.[i] = '\xC2' && s.[i + 1] = '\xA0' then 2
          else 1
        in
        if is_space && not in_space then Buffer.add_char buf ' '
        else if not is_space then Buffer.add_substring buf s i char_size;
        loop (i + char_size) is_space
    in
    loop 0 false

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
  let positions = ref [] in
  let rec find_all pos =
    try
      let pos = Str.search_forward (Lazy.force nbsp_re) s pos in
      positions := pos :: !positions;
      find_all (pos + 1)
    with Not_found -> ()
  in
  find_all 0;
  List.rev !positions

(* Detect if a word starts with Irish name prefixes Mac/Mc/Fitz *)
let is_irish_prefix s =
  let len = String.length s in
  (len >= 3 && String.sub s 0 3 = "Mac")
  || (len >= 2 && String.sub s 0 2 = "Mc")
  || (len >= 4 && String.sub s 0 4 = "Fitz")
(* Ocaml 4.13+
let is_irish_prefix s =
  String.starts_with ~prefix:"Mac" s ||
  String.starts_with ~prefix:"Mc" s ||
  String.starts_with ~prefix:"Fitz" s *)

(* Check if a word is either a Roman numeral or starts with an Irish prefix
   Examples of valid words:
   - Roman numerals: "XIV", "III", "IX"
   - Irish prefixes: "MacDonald", "McLeod", "FitzGerald" *)
let is_allowed_word s = is_roman_numeral s || is_irish_prefix s

(* Detect invalid capitalization patterns:
   - Multiple uppercase letters in sequence
   - Lowercase followed by uppercase
   - Uppercase, lowercase, uppercase sequence *)

(* Bad capitalization functions *)
let has_bad_capitalization_pattern s =
  try
    let _ = Str.search_forward (Lazy.force bad_cap_re) s 0 in
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
  match hex with
  | "00AD" | "034F" | "200B" | "200C" | "200D" | "200E" | "200F" | "2060"
  | "2061" | "2062" | "2063" | "2064" | "206A" | "206B" | "206C" | "206D"
  | "206E" | "206F" | "FEFF" ->
      true
  | _ -> false

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

let invisible_chars_tbl =
  let tbl = Hashtbl.create (Array.length invisible_chars) in
  Array.iter
    (fun (hex, name) ->
      let code = int_of_string ("0x" ^ hex) in
      Hashtbl.add tbl code name)
    invisible_chars;
  tbl

let is_invisible_char code = Hashtbl.mem invisible_chars_tbl code

let get_invisible_char_name code =
  try Some (Hashtbl.find invisible_chars_tbl code) with Not_found -> None

let has_invisible_chars s =
  let len = String.length s in
  let rec aux i =
    if i >= len then false
    else
      let code, size = get_unicode_point s i in
      if is_invisible_char code then true else aux (i + size)
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
  | BadCapitalization -> s
  | MultipleSpaces -> fix_multiple_spaces s
  | NonBreakingSpace -> s

(* Define style and tooltip of error types *)
let get_highlight_style error_type conf =
  match error_type with
  | NonBreakingSpace ->
      {
        make_class = (fun hex -> if hex = "202F" then "nnbsp" else "nbsp");
        make_title =
          (fun ?code ?name:_ _ ->
            match code with
            | Some "00A0" -> Some "U+00A0 NON-BREAKING SPACE"
            | Some "202F" -> Some "U+202F NARROW NON-BREAKING SPACE"
            | _ -> None);
      }
  | MultipleSpaces ->
      {
        make_class = (fun _ -> "ms");
        make_title = (fun ?code:_ ?name:_ _ -> None);
      }
  | InvisibleCharacters ->
      {
        make_class = (fun hex -> if is_zero_width hex then "zw" else "ic");
        make_title =
          (fun ?code ?name _ ->
            let base =
              Printf.sprintf "U+%s %s"
                (Option.value code ~default:"")
                (Option.value name ~default:"")
            in
            if is_zero_width (Option.value code ~default:"") then
              Some
                (Printf.sprintf "%s (%s)" base
                   (Util.transl conf "zero-width character"))
            else Some base);
      }
  | BadCapitalization ->
      {
        make_class = (fun _ -> "bc");
        make_title = (fun ?code:_ ?name:_ _ -> None);
      }

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
        let char_size = Utf8.nbc s.[i] in
        let code = Utf8.C.cp s i in
        let hex = Printf.sprintf "%04X" (Uchar.to_int code) in
        let name =
          match get_invisible_char_name (Uchar.to_int code) with
          | Some n -> n
          | None -> "UNICODE CHARACTER"
        in
        let title_attr =
          match style.make_title ~code:hex ~name conf with
          | Some t -> Printf.sprintf " title=\"%s\"" t
          | None -> ""
        in
        let original = String.sub s i char_size in
        let escaped_original = (Util.escape_html original :> string) in
        Printf.bprintf buf "<span class=\"%s\"%s>%s</span>"
          (style.make_class hex) title_attr escaped_original;
        process_char (i + char_size) false)
      else
        let char_size = Utf8.nbc s.[i] in
        let original = String.sub s i char_size in
        let escaped_char = (Util.escape_html original :> string) in
        Buffer.add_string buf escaped_char;
        process_char (i + char_size) false
  in
  process_char 0 false

let first_word s =
  try
    let i = String.index s ' ' in
    if i = String.length s then if i > 7 then String.sub s 0 7 else s
    else String.sub s 0 i
  with Not_found -> if String.length s > 7 then String.sub s 0 7 else s

let make_error_html conf base data istr entry error_type =
  let istr_ = Driver.Istr.to_string istr in
  let commd = (Util.commd conf :> string) in
  let entry_modified =
    match data with
    | "sn" | "domain" -> UpdateData.move_particle base entry
    | _ -> entry
  in
  let s' =
    if data = "place" then
      let main_place = Place.without_suburb entry in
      if String.length (first_word main_place) > 7 then
        String.sub main_place 0 7
      else first_word main_place
    else first_word entry_modified
  in
  let s = (Mutil.encode s' :> string) in
  let s_ori = (Mutil.encode entry :> string) in
  let entry_fixed = fix_error error_type entry in
  let entry_escaped = (Util.escape_html entry :> string) in
  let s2 = (Mutil.encode entry_fixed :> string) in
  let s2_auto = entry_fixed <> entry in
  let s2_p = if s2_auto then "&s2=" ^ s2 else "" in
  let pos = find_error_positions error_type entry in
  let hl = make_highlight_html entry pos error_type conf in
  let url_mod =
    Printf.sprintf "%sm=MOD_DATA&data=%s&key=%s&s=%s%s" commd data istr_ s s2_p
  in
  let url_chk =
    Printf.sprintf "%sm=CHK_DATA_OK&k=%s&s=%s&s2=%s" commd istr_ s_ori s2
  in
  (hl, url_mod, url_chk, entry_escaped, s2_auto)

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

exception Max_results_reached

(* Fonction commune pour analyser les erreurs dans une chaîne *)
let analyze_string_errors dict_type s =
  let add_if cond error acc = if cond then error :: acc else acc in
  []
  |> add_if (has_non_breaking_space s) NonBreakingSpace
  |> add_if (has_multiple_spaces s) MultipleSpaces
  |> add_if (has_invisible_chars s) InvisibleCharacters
  |> add_if (has_bad_capitalization dict_type s) BadCapitalization

let dict_to_cache_name dict_type =
  match dict_type with
  | Fnames -> "fnames"
  | Snames -> "snames"
  | Places -> "places"
  | PubNames -> "pub_names"
  | Qualifiers -> "qualifiers"
  | Aliases -> "aliases"
  | Occupation -> "occupations"
  | Titles -> "titles"
  | Estates -> "estates"
  | Sources -> "sources"

let cache_file_path conf dict_type =
  let bname = Filename.remove_extension conf.bname in
  let cache_dir =
    Filename.concat (Secure.base_dir ())
      (Filename.concat "etc" (Filename.concat bname "cache"))
  in
  let fname = dict_to_cache_name dict_type in
  Filename.concat cache_dir (bname ^ "_" ^ fname ^ "_checkdata.cache")

let cache_file_exists conf dict_type =
  let cache_file = cache_file_path conf dict_type in
  Sys.file_exists cache_file

let read_cache conf dict_type =
  let cache_file = cache_file_path conf dict_type in
  try
    let ic = Secure.open_in_bin cache_file in
    try
      let data = (Marshal.from_channel ic : checkdata_entry list) in
      close_in ic;
      data
    with e ->
      close_in ic;
      raise e
  with Sys_error _ -> []

(* Collecter les erreurs depuis le cache binaire checkdata *)
let collect_all_errors_from_cache conf dict_type max_results
    ?(sel_err_types = []) () =
  let entries = read_cache conf dict_type in
  let check_errors_set = make_error_set sel_err_types in

  let rec process_entries entries acc count =
    match (entries, max_results) with
    | [], _ -> acc
    | _, Some max when count >= max -> acc
    | (istr, s) :: rest, _ ->
        let errors = analyze_string_errors dict_type s in
        let filtered_errors =
          if ErrorSet.is_empty check_errors_set then errors
          else List.filter (fun e -> ErrorSet.mem e check_errors_set) errors
        in
        if filtered_errors = [] then process_entries rest acc count
        else process_entries rest ((istr, s, filtered_errors) :: acc) (count + 1)
  in
  List.rev (process_entries entries [] 0)

let collect_all_errors ?(max_results = None) ?(sel_err_types = []) base dict =
  let istr_errors = Hashtbl.create 1024 in
  let unique_istrs = ref 0 in
  let collect_strings = collect_dict_strings base dict in
  let check_error_types_set =
    ErrorSet.of_list
      (if sel_err_types = [] then
         [
           InvisibleCharacters;
           BadCapitalization;
           MultipleSpaces;
           NonBreakingSpace;
         ]
       else sel_err_types)
  in
  let add_error istr s err =
    if ErrorSet.mem err check_error_types_set then
      match Hashtbl.find_opt istr_errors istr with
      | Some (stored_s, errs) ->
          if not (List.mem err errs) then
            Hashtbl.replace istr_errors istr (stored_s, err :: errs)
      | None -> (
          Hashtbl.add istr_errors istr (s, [ err ]);
          incr unique_istrs;
          match max_results with
          | Some max when !unique_istrs >= max -> raise Max_results_reached
          | _ -> ())
  in
  (try
     Geneweb_db.Collection.iter
       (fun i ->
         let p = Driver.poi base i in
         let istrs = collect_strings p in
         List.iter
           (fun istr ->
             if not (Driver.Istr.is_empty istr) then
               let s = Driver.sou base istr in
               if s <> "" then (
                 if has_invisible_chars s then
                   add_error istr s InvisibleCharacters;
                 if has_bad_capitalization dict s then
                   add_error istr s BadCapitalization;
                 if has_multiple_spaces s then add_error istr s MultipleSpaces;
                 if has_non_breaking_space s then
                   add_error istr s NonBreakingSpace))
           istrs)
       (Driver.ipers base)
   with Max_results_reached -> ());
  let result = ref [] in
  Hashtbl.iter
    (fun istr (s, errs) -> result := (istr, s, errs) :: !result)
    istr_errors;
  !result

(* Main function *)
let collect_all_errors_with_cache ?(max_results = None) ?(sel_err_types = [])
    conf base dict =
  let use_cache =
    match Util.p_getenv conf.env "nocache" with Some "1" -> false | _ -> true
  in
  if use_cache then
    if cache_file_exists conf dict then
      collect_all_errors_from_cache conf dict max_results ~sel_err_types ()
    else []
  else
    let is_roglo =
      try List.assoc "roglo" conf.base_env = "yes" with Not_found -> false
    in
    if is_roglo then []
    else collect_all_errors ~max_results ~sel_err_types base dict
