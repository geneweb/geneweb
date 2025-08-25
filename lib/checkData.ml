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
  | Fnames_alias
  | Snames_alias
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
  | MiscTypographicErrors
  | MixedScripts

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
          MiscTypographicErrors;
          MixedScripts;
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
let roman_re =
  lazy
    (Re.compile
       (Re.seq [ Re.bos; Re.rep1 (Re.set "IVX"); Re.opt (Re.char '.'); Re.eos ]))

let first_ordinal_re =
  lazy
    (Re.compile
       (Re.seq
          [
            Re.bos;
            Re.char 'I';
            Re.alt [ Re.str "ᵉʳ"; Re.str "ʳᵉ"; Re.str "er"; Re.str "re" ];
            Re.eos;
          ]))

let bad_cap_re =
  lazy
    (Re.compile
       (Re.alt
          [
            Re.seq [ Re.rg 'A' 'Z'; Re.rg 'A' 'Z'; Re.rg 'a' 'z' ];
            Re.seq [ Re.rg 'a' 'z'; Re.rg 'A' 'Z' ];
          ]))

let nbsp_re =
  lazy
    (Re.compile
       (Re.alt
          [
            Re.str "\xC2\xA0";
            (* U+00A0 espace insécable *)
            Re.str "\xE2\x80\xAF";
            (* U+202F espace insécable fine *)
          ]))

let has_any_particle base s =
  let particle = Mutil.get_particle (Driver.base_particles base) s in
  if particle <> "" then true
  else
    let rec check_from pos =
      if pos >= String.length s then false
      else
        let remaining = String.sub s pos (String.length s - pos) in
        let particle =
          Mutil.get_particle (Driver.base_particles base) remaining
        in
        if particle <> "" then true
        else
          match String.index_from_opt s pos ' ' with
          | None -> false
          | Some space_pos -> check_from (space_pos + 1)
    in
    check_from 0

let is_roman_numeral s =
  try
    Re.execp (Lazy.force roman_re) s || Re.execp (Lazy.force first_ordinal_re) s
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

let fix_multiple_spaces s =
  let len = String.length s in
  let buf = Buffer.create len in
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
  let re = Lazy.force nbsp_re in
  Re.all re s
  |> List.filter_map (fun grp ->
         let pos = Re.Group.start grp 0 in
         if has_roman_after_nbsp s pos then None else Some pos)
  |> List.sort_uniq compare

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

let lowercase_allowed_words =
  [ "dit"; "dite"; "ou"; "et"; "genannt"; "gennant"; "vel"; "y"; "e" ]

let is_lowercase_allowed word =
  List.mem (String.lowercase_ascii word) lowercase_allowed_words

(* Detect invalid capitalization patterns:
   - Multiple uppercase letters in sequence
   - Lowercase followed by uppercase
   - Uppercase, lowercase, uppercase sequence *)
(* Bad capitalization functions *)
let has_bad_capitalization_pattern s =
  try Re.execp (Lazy.force bad_cap_re) s with _ -> false

(* Capitalization check function
   Examines each word for valid patterns for some books
   For other types, just checks for invalid capitalization patterns *)
let has_legitimate_mixed_case dict base s =
  match dict with
  | Fnames | Snames | Fnames_alias | Snames_alias | PubNames | Aliases | Places
    ->
      has_any_particle base s
      ||
      let words = split_words s in
      List.for_all
        (fun w -> (not (has_bad_capitalization_pattern w)) || is_allowed_word w)
        words
  | _ -> false

let has_bad_capitalization dict base s =
  match dict with
  | Sources -> false
  | _ ->
      let has_particles = has_any_particle base s in
      if has_particles then false
      else
        let has_general =
          has_bad_capitalization_pattern s
          && not (has_legitimate_mixed_case dict base s)
        in
        let has_lowercase =
          match dict with
          | Fnames | Snames | Fnames_alias | Snames_alias ->
              let words = split_words s in
              List.exists
                (fun w ->
                  String.length w > 0
                  && w.[0] >= 'a'
                  && w.[0] <= 'z'
                  && not (is_lowercase_allowed w))
                words
          | _ -> false
        in
        has_general || has_lowercase

let find_bad_capitalization_positions dict base s =
  let positions = ref [] in
  let has_particles = has_any_particle base s in
  if not has_particles then (
    let rec scan i =
      if i >= String.length s - 2 then ()
      else if
        s.[i] >= 'A'
        && s.[i] <= 'Z'
        && s.[i + 1] >= 'A'
        && s.[i + 1] <= 'Z'
        && i + 2 < String.length s
        && s.[i + 2] >= 'a'
        && s.[i + 2] <= 'z'
      then (
        positions := i :: (i + 1) :: !positions;
        scan (i + 1))
      else if
        i < String.length s - 1
        && s.[i] >= 'a'
        && s.[i] <= 'z'
        && s.[i + 1] >= 'A'
        && s.[i + 1] <= 'Z'
      then (
        positions := i :: (i + 1) :: !positions;
        scan (i + 1))
      else scan (i + 1)
    in
    scan 0;
    match dict with
    | Fnames | Snames ->
        let words = split_words s in
        let word_pos = ref 0 in
        List.iter
          (fun word ->
            if
              String.length word > 0
              && word.[0] >= 'a'
              && word.[0] <= 'z'
              && not (is_lowercase_allowed word)
            then positions := !word_pos :: !positions;
            word_pos := !word_pos + String.length word + 1)
          words
    | _ -> ());
  List.sort_uniq compare !positions

let is_zero_width hex =
  match hex with
  | "00AD" | "034F" | "200B" | "200C" | "200D" | "200E" | "200F" | "2060"
  | "2061" | "2062" | "2063" | "2064" | "206A" | "206B" | "206C" | "206D"
  | "206E" | "206F" | "FEFF" ->
      true
  | _ -> false

let _hex_to_int hex = int_of_string ("0x" ^ hex)

let invisible_chars_tbl =
  let codes =
    Util.get_problem_chars_codes `Invisible
    @ Util.get_problem_chars_codes `ZeroWidth
  in
  let tbl = Hashtbl.create (List.length codes) in
  List.iter (fun code -> Hashtbl.add tbl code true) codes;
  tbl

let is_invisible_char code = Hashtbl.mem invisible_chars_tbl code

let has_invisible_chars s =
  let len = String.length s in
  let rec aux i =
    if i >= len then false
    else
      let code, size = Util.get_unicode_point s i in
      if is_invisible_char code then true else aux (i + size)
  in
  aux 0

let find_invisible_positions s =
  let len = String.length s in
  let rec aux acc i =
    if i >= len then List.rev acc
    else
      let code, size = Util.get_unicode_point s i in
      if is_invisible_char code then aux (i :: acc) (i + size)
      else aux acc (i + size)
  in
  aux [] 0

let simple_replacements =
  [
    (Re.str "--", "-");
    (Re.str " )", ")");
    (Re.str "( ", "(");
    (Re.str "((", "(");
    (Re.str "))", ")");
    (Re.str " ,", ",");
    (Re.str ",,", ",");
    (Re.str " .", ".");
    (Re.str "....", "…");
    (Re.str "...", "…");
    (Re.str "..", ".");
    (Re.str "\"\"", "\"");
    (Re.str "’’", "’");
    (Re.str "`", "’");
    (Re.str "´", "’");
    (Re.str "‘", "’");
    (Re.str "ʹ", "’");
    (Re.str "ʻ", "’");
  ]

(* Replacements spécifiques par dictionnaire *)
let dict_specific_replacements =
  [
    (Re.str " -", "-", [ Occupation; Sources ]);
    (Re.str "- ", "-", [ Occupation; Sources ]);
  ]

let detect_only_patterns =
  [
    ( Re.seq [ Re.char '('; Re.rep1 (Re.char ' '); Re.char ')' ],
      "empty parenthesis" );
  ]

let char_before_parenthesis_pattern =
  Re.seq [ Re.group (Re.set "A-Za-z0-9"); Re.char '(' ]

let breton_trigram_pattern =
  Re.seq [ Re.group (Re.set "cC"); Re.set "’'"; Re.group (Re.set "hH") ]

let compiled_detect =
  lazy (List.map (fun (pat, msg) -> (Re.compile pat, msg)) detect_only_patterns)

let compiled_complex =
  lazy
    [
      Re.compile char_before_parenthesis_pattern;
      Re.compile breton_trigram_pattern;
    ]

let get_applicable_replacements dict_type =
  let base = simple_replacements in
  let specific =
    List.filter_map
      (fun (pat, repl, excluded) ->
        if List.mem dict_type excluded then None else Some (pat, repl))
      dict_specific_replacements
  in
  base @ specific

let has_misc_typographic_errors dict_type s =
  let replacements = get_applicable_replacements dict_type in
  let patterns =
    List.map fst replacements
    @ List.map fst detect_only_patterns
    @ [ char_before_parenthesis_pattern; breton_trigram_pattern ]
  in
  let re = Re.compile (Re.alt patterns) in
  try
    ignore (Re.exec re s);
    true
  with Not_found -> false

let find_misc_typographic_positions dict_type s =
  let replacements = get_applicable_replacements dict_type in
  let patterns =
    List.map fst replacements
    @ List.map fst detect_only_patterns
    @ [ char_before_parenthesis_pattern; breton_trigram_pattern ]
  in
  let re = Re.compile (Re.alt patterns) in
  let positions = ref [] in
  let pos = ref 0 in
  while !pos < String.length s do
    try
      let result = Re.exec ~pos:!pos re s in
      let start_pos = Re.Group.start result 0 in
      let end_pos = Re.Group.stop result 0 in
      for i = start_pos to end_pos - 1 do
        positions := i :: !positions
      done;
      pos := end_pos
    with Not_found -> pos := String.length s
  done;
  List.sort_uniq compare !positions

let fix_misc_typographic_errors dict_type s =
  let replacements = get_applicable_replacements dict_type in
  let s =
    List.fold_left
      (fun acc (pat, repl) -> Re.replace_string (Re.compile pat) ~by:repl acc)
      s replacements
  in
  let patterns = Lazy.force compiled_complex in
  s
  |> Re.replace (List.nth patterns 0) ~f:(fun groups ->
         Re.Group.get groups 1 ^ " (")
  |> Re.replace (List.nth patterns 1) ~f:(fun groups ->
         let c = Re.Group.get groups 1 in
         let h = Re.Group.get groups 2 in
         c ^ "ʼ" ^ h)

(* Détermine si un caractère est latin, grec ou cyrillique *)
let script_class_of_uchar u =
  match Uucp.Script.script u with
  | `Latn -> Some `Latin
  | `Grek -> Some `Greek
  | `Cyrl -> Some `Cyrillic
  | _ -> None

(* Vérifie si un mot unique contient des scripts mélangés *)
let has_mixed_scripts_in_word s =
  let has_latin = ref false in
  let has_greek = ref false in
  let has_cyrillic = ref false in
  let check_char () _ = function
    | `Uchar u -> (
        match script_class_of_uchar u with
        | Some `Latin ->
            has_latin := true;
            ()
        | Some `Greek ->
            has_greek := true;
            ()
        | Some `Cyrillic ->
            has_cyrillic := true;
            ()
        | _ -> ())
    | `Malformed _ -> ()
  in
  Uutf.String.fold_utf_8 check_char () s;
  !has_latin && (!has_greek || !has_cyrillic)

let has_mixed_scripts s =
  let words = split_words s in
  List.exists has_mixed_scripts_in_word words

(* Trouve les positions des caractères grecs et cyrilliques *)
let find_mixed_scripts_positions s =
  let positions = ref [] in
  let rec find_all_words pos acc =
    if pos >= String.length s then List.rev acc
    else if s.[pos] = ' ' || s.[pos] = '\t' || s.[pos] = '\n' || s.[pos] = '\r'
    then find_all_words (pos + 1) acc
    else
      let word_start = pos in
      let rec find_end p =
        if p >= String.length s then p
        else if s.[p] = ' ' || s.[p] = '\t' || s.[p] = '\n' || s.[p] = '\r' then
          p
        else find_end (p + 1)
      in
      let word_end = find_end pos in
      let word = String.sub s word_start (word_end - word_start) in
      find_all_words word_end ((word, word_start, word_end) :: acc)
  in
  let all_words = find_all_words 0 [] in
  List.iter
    (fun (word, start_pos, end_pos) ->
      if has_mixed_scripts_in_word word then
        (* Parcourir chaque octet du mot *)
        let pos = ref start_pos in
        while !pos < end_pos do
          let char_size = Utf8.nbc s.[!pos] in
          let char_str = String.sub s !pos (min char_size (end_pos - !pos)) in
          let mark_it = ref false in
          let check () _ = function
            | `Uchar u -> (
                match script_class_of_uchar u with
                | Some `Greek | Some `Cyrillic -> mark_it := true
                | _ -> ())
            | `Malformed _ -> ()
          in
          Uutf.String.fold_utf_8 check () char_str;
          if !mark_it then
            for i = 0 to char_size - 1 do
              positions := (!pos + i) :: !positions
            done;
          pos := !pos + char_size
        done)
    all_words;
  List.sort_uniq compare !positions

let _get_detect_only_message s =
  try
    List.find_map
      (fun (re, msg) -> if Re.execp re s then Some msg else None)
      (Lazy.force compiled_detect)
  with Not_found -> None

let data_to_dict_type data =
  match data with
  | "fn" -> Fnames
  | "sn" -> Snames
  | "place" -> Places
  | "pub_name" -> PubNames
  | "qualif" -> Qualifiers
  | "alias" -> Aliases
  | "occup" -> Occupation
  | "title" -> Titles
  | "estate" -> Estates
  | "src" -> Sources
  | _ -> Fnames

(* Generic error handling *)
let find_error_positions error_type data base s =
  let dict_type = data_to_dict_type data in
  match error_type with
  | InvisibleCharacters -> find_invisible_positions s
  | BadCapitalization ->
      let dict_type = data_to_dict_type data in
      find_bad_capitalization_positions dict_type base s
  | MultipleSpaces -> find_multiple_spaces_positions s
  | NonBreakingSpace -> find_non_breaking_space_positions s
  | MiscTypographicErrors -> find_misc_typographic_positions dict_type s
  | MixedScripts -> find_mixed_scripts_positions s

let fix_error error_type dict_type s =
  match error_type with
  | InvisibleCharacters -> Util.only_printable s
  | BadCapitalization -> s
  | MultipleSpaces -> fix_multiple_spaces s
  | NonBreakingSpace -> s
  | MiscTypographicErrors -> fix_misc_typographic_errors dict_type s
  | MixedScripts -> s

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
  | MiscTypographicErrors ->
      {
        make_class = (fun _ -> "mt");
        make_title =
          (fun ?code:_ ?name:_ _ ->
            Some (Util.transl conf "chk_data ponctuation error"));
      }
  | MixedScripts ->
      {
        make_class = (fun _ -> "mx");
        make_title =
          (fun ?code:_ ?name:_ _ ->
            Some (Util.transl conf "chk_data mixed alphabet"));
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
          match Util.get_problem_char_name (Uchar.to_int code) with
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
    if i = String.length s then if i > 8 then String.sub s 0 8 else s
    else String.sub s 0 i
  with Not_found -> if String.length s > 8 then String.sub s 0 8 else s

let simple_prefix s =
  let len = String.length s in
  if len <= 8 then s else String.sub s 0 8

let make_error_html conf base data istr entry error_type =
  let dict_type = data_to_dict_type data in
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
      if String.length (first_word main_place) > 8 then
        String.sub main_place 0 8
      else first_word main_place
    else simple_prefix entry_modified
  in
  let s = (Mutil.encode s' :> string) in
  let s_ori = (Mutil.encode entry :> string) in
  let entry_fixed = fix_error error_type dict_type entry in
  let entry_escaped = (Util.escape_html entry :> string) in
  let s2 = (Mutil.encode entry_fixed :> string) in
  let s2_auto = entry_fixed <> entry in
  let s2_p = if s2_auto then "&s2=" ^ s2 else "" in
  let positions = find_error_positions error_type data base entry in
  let hl = make_highlight_html entry positions error_type conf in
  let url_mod =
    match data with
    | "fna" | "sna" -> ""
    | _ ->
        Printf.sprintf "%sm=MOD_DATA&data=%s&key=%s&s=%s%s" commd data istr_ s
          s2_p
  in
  let url_chk =
    Printf.sprintf "%sm=CHK_DATA_OK&d=%s&k=%s&s=%s&s2=%s" commd data istr_ s_ori
      s2
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
  | Fnames_alias -> Driver.get_first_names_aliases
  | Snames_alias -> Driver.get_surnames_aliases
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
let analyze_string_errors dict_type base s =
  let add_if cond error acc = if cond then error :: acc else acc in
  []
  |> add_if (has_non_breaking_space s) NonBreakingSpace
  |> add_if (has_multiple_spaces s) MultipleSpaces
  |> add_if (has_invisible_chars s) InvisibleCharacters
  |> add_if (has_bad_capitalization dict_type base s) BadCapitalization
  |> add_if (has_misc_typographic_errors dict_type s) MiscTypographicErrors
  |> add_if (has_mixed_scripts s) MixedScripts

let dict_to_cache_name dict_type =
  match dict_type with
  | Fnames -> "fnames"
  | Snames -> "snames"
  | Fnames_alias -> "fnames_alias"
  | Snames_alias -> "snames_alias"
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

let update_cache_entry conf dict_type istr new_value =
  let cache_file = cache_file_path conf dict_type in
  if Sys.file_exists cache_file then
    try
      let entries = read_cache conf dict_type in
      let updated_entries =
        List.map
          (fun (i, s) -> if i = istr then (i, new_value) else (i, s))
          entries
      in
      let oc = Secure.open_out_bin cache_file in
      try
        Marshal.to_channel oc updated_entries [ Marshal.No_sharing ];
        close_out oc;
        true
      with e ->
        close_out oc;
        raise e
    with
    | Sys_error _ -> false
    | End_of_file -> false
    | Failure _ -> false
  else false

let find_dict_type_for_istr conf istr =
  let check_in_cache dict_type =
    if cache_file_exists conf dict_type then
      let entries = read_cache conf dict_type in
      List.exists (fun (i, _) -> i = istr) entries
    else false
  in
  List.find_opt check_in_cache
    [
      Fnames;
      Snames;
      Fnames_alias;
      Snames_alias;
      Places;
      PubNames;
      Qualifiers;
      Aliases;
      Occupation;
      Titles;
      Estates;
      Sources;
    ]

(* Collecter les erreurs depuis le cache binaire checkdata *)
let collect_all_errors_from_cache conf dict_type base max_results
    ?(sel_err_types = []) () =
  let entries = read_cache conf dict_type in
  let check_errors_set = make_error_set sel_err_types in

  let rec process_entries entries acc count =
    match (entries, max_results) with
    | [], _ -> acc
    | _, Some max when count >= max -> acc
    | (istr, s) :: rest, _ ->
        let errors = analyze_string_errors dict_type base s in
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
           MiscTypographicErrors;
           MixedScripts;
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
                 if has_bad_capitalization dict base s then
                   add_error istr s BadCapitalization;
                 if has_multiple_spaces s then add_error istr s MultipleSpaces;
                 if has_non_breaking_space s then
                   add_error istr s NonBreakingSpace;
                 if has_misc_typographic_errors dict s then
                   add_error istr s MiscTypographicErrors;
                 if has_mixed_scripts s then add_error istr s MixedScripts))
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
      collect_all_errors_from_cache conf dict base max_results ~sel_err_types ()
    else []
  else
    let is_roglo =
      try List.assoc "roglo" conf.base_env = "yes" with Not_found -> false
    in
    if is_roglo then []
    else collect_all_errors ~max_results ~sel_err_types base dict
