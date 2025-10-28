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
  make_title :
    ?code:string ->
    ?name:string ->
    ?misc_msg:string ->
    Config.config ->
    string option;
}

type checkdata_entry = Driver.istr * string
type misc_error_info = { pos : int; message : string }

type highlight_info =
  | SimplePositions of int list
  | WithMessages of misc_error_info list

module ErrorSet = Set.Make (struct
  type t = error_type

  let compare = compare
end)

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
  let rec collect_words acc start i =
    if i >= String.length s then
      if i > start then String.sub s start (i - start) :: acc else acc
    else if s.[i] = ' ' then
      if i > start then
        collect_words (String.sub s start (i - start) :: acc) (i + 1) (i + 1)
      else collect_words acc (i + 1) (i + 1)
    else collect_words acc start (i + 1)
  in
  List.rev (collect_words [] 0 0)

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
    else if s.[byte_pos] = ' ' then
      if byte_pos + 1 < len && s.[byte_pos + 1] = ' ' then true
      else find_spaces (byte_pos + 1)
    else if Char.code s.[byte_pos] < 0x80 then find_spaces (byte_pos + 1)
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
let has_non_breaking_space s =
  let len = String.length s in
  let rec aux i =
    if i >= len then false
    else if s.[i] = '\xC2' then
      (* Check for nbsp: 0xC2 0xA0 *)
      if i + 1 < len && s.[i + 1] = '\xA0' then
        if has_roman_after_nbsp s i then aux (i + 2) else true
      else aux (i + 2) (* Other 0xC2 char, skip *)
    else if s.[i] = '\xE2' then
      (* Check for narrow nbsp: 0xE2 0x80 0xAF *)
      if i + 2 < len && s.[i + 1] = '\x80' && s.[i + 2] = '\xAF' then
        if has_roman_after_nbsp s i then aux (i + 3) else true
      else aux (i + 3) (* Other 0xE2 char, skip 3 bytes *)
    else
      (* Everything else: skip based on UTF-8 byte structure *)
      let c = Char.code s.[i] in
      if c < 0x80 then aux (i + 1)
      else if c < 0xE0 then aux (i + 2) (* 2-byte char *)
      else if c < 0xF0 then aux (i + 3) (* 3-byte char *)
      else aux (i + 4)
    (* 4-byte char *)
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
  let len = String.length s in
  let rec scan i =
    if i >= len then false
    else
      (* Check AAz pattern: [A-Z][A-Z][a-z] *)
      let has_aaz =
        i + 2 < len
        && s.[i] >= 'A'
        && s.[i] <= 'Z'
        && s.[i + 1] >= 'A'
        && s.[i + 1] <= 'Z'
        && s.[i + 2] >= 'a'
        && s.[i + 2] <= 'z'
      in
      if has_aaz then true
      else
        (* Check aZ pattern: [a-z][A-Z] *)
        let has_az =
          i + 1 < len
          && s.[i] >= 'a'
          && s.[i] <= 'z'
          && s.[i + 1] >= 'A'
          && s.[i + 1] <= 'Z'
        in
        if has_az then true else scan (i + 1)
  in
  scan 0

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
    Util.get_problem_chars_codes `Control
    @ Util.get_problem_chars_codes `Invisible
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
      let c = Char.code s.[i] in
      if c < 0x20 then if c = 0x09 || c = 0x0A then aux (i + 1) else true
      else if c = 0x7F then true
      else if c < 0x80 then aux (i + 1)
      else if c = 0xC2 then
        if i + 1 < len then
          let c2 = Char.code s.[i + 1] in
          if (c2 >= 0x80 && c2 <= 0x9F) || c2 = 0xAD then true else aux (i + 2)
        else false
      else if c >= 0xC3 && c < 0xCC then aux (i + 2)
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

let char_before_parenthesis_pattern =
  Re.seq [ Re.group (Re.set "A-Za-z0-9"); Re.char '(' ]

let breton_trigram_pattern =
  Re.seq [ Re.group (Re.set "cC"); Re.set "’'"; Re.group (Re.set "hH") ]

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

let compiled_misc_errors_by_dict =
  lazy
    (let build_for_dict dict_type =
       let replacements = get_applicable_replacements dict_type in
       let patterns =
         List.map fst replacements
         @ [ char_before_parenthesis_pattern; breton_trigram_pattern ]
       in
       Re.compile (Re.alt patterns)
     in
     [
       (Fnames, build_for_dict Fnames);
       (Snames, build_for_dict Snames);
       (Fnames_alias, build_for_dict Fnames_alias);
       (Snames_alias, build_for_dict Snames_alias);
       (Places, build_for_dict Places);
       (PubNames, build_for_dict PubNames);
       (Qualifiers, build_for_dict Qualifiers);
       (Aliases, build_for_dict Aliases);
       (Occupation, build_for_dict Occupation);
       (Estates, build_for_dict Estates);
       (Titles, build_for_dict Titles);
       (Sources, build_for_dict Sources);
     ])

let has_misc_typographic_errors dict_type s =
  let re = List.assoc dict_type (Lazy.force compiled_misc_errors_by_dict) in
  try
    ignore (Re.exec re s);
    true
  with Not_found -> false

let find_misc_typographic_positions dict_type s conf =
  let replacements = get_applicable_replacements dict_type in
  let errors = ref [] in
  let pos = ref 0 in
  let len = String.length s in
  let try_pattern pattern message_or_key add_positions found_match =
    if !found_match = None then
      try
        let result = Re.exec ~pos:!pos pattern s in
        let start_pos = Re.Group.start result 0 in
        let end_pos = Re.Group.stop result 0 in
        if start_pos = !pos then (
          add_positions start_pos end_pos message_or_key;
          found_match := Some end_pos)
      with Not_found -> ()
  in
  while !pos < len do
    let found_match = ref None in
    List.iter
      (fun (pat, repl) ->
        let add_pos start_pos end_pos _ =
          let matched = String.sub s start_pos (end_pos - start_pos) in
          let msg = Util.transl conf "chk_data ponctuation error" in
          let col = Util.transl conf ":" in
          let message =
            Printf.sprintf "%s%s ‹%s› → ‹%s›" msg col matched repl
          in
          for i = start_pos to end_pos - 1 do
            errors := { pos = i; message } :: !errors
          done
        in
        try_pattern (Re.compile pat) "" add_pos found_match)
      replacements;
    let complex_patterns = Lazy.force compiled_complex in
    let add_single_pos start_pos _end_pos msg_key =
      let message = Util.transl conf msg_key in
      errors := { pos = start_pos; message } :: !errors
    in
    let add_range_pos start_pos end_pos msg_key =
      let message = Util.transl conf msg_key in
      for i = start_pos to end_pos - 1 do
        errors := { pos = i; message } :: !errors
      done
    in
    try_pattern
      (List.nth complex_patterns 0)
      "chk_data ponctuation error missing space" add_single_pos found_match;
    try_pattern
      (List.nth complex_patterns 1)
      "chk_data ponctuation error breton trigram help" add_range_pos found_match;
    pos := match !found_match with Some p -> p | None -> !pos + 1
  done;
  List.rev !errors

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
let script_cache = Hashtbl.create 256

let script_class_of_code code =
  try Hashtbl.find script_cache code
  with Not_found ->
    let result =
      if (code >= 0x0041 && code <= 0x005A) || (code >= 0x0061 && code <= 0x007A)
      then Some `Latin
      else
        let u = Uchar.of_int code in
        match Uucp.Script.script u with
        | `Latn -> Some `Latin
        | `Grek -> Some `Greek
        | `Cyrl -> Some `Cyrillic
        | _ -> None
    in
    Hashtbl.add script_cache code result;
    result

let has_mixed_scripts_in_word s =
  let has_latin = ref false in
  let has_greek = ref false in
  let has_cyrillic = ref false in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let c = Char.code s.[!i] in
    if c < 0x80 then (
      if (c >= 0x41 && c <= 0x5A) || (c >= 0x61 && c <= 0x7A) then
        has_latin := true;
      incr i)
    else
      let code, size = Util.get_unicode_point s !i in
      (match script_class_of_code code with
      | Some `Latin -> has_latin := true
      | Some `Greek -> has_greek := true
      | Some `Cyrillic -> has_cyrillic := true
      | None -> ());
      i := !i + size
  done;
  !has_latin && (!has_greek || !has_cyrillic)

let has_mixed_scripts s =
  let words = split_words s in
  List.exists has_mixed_scripts_in_word words

let find_mixed_scripts_positions s =
  let positions = ref [] in
  let words_with_pos = ref [] in
  let pos = ref 0 in
  let len = String.length s in
  while !pos < len do
    while !pos < len && s.[!pos] = ' ' do
      incr pos
    done;
    if !pos < len then (
      let start = !pos in
      while !pos < len && s.[!pos] <> ' ' do
        incr pos
      done;
      let word = String.sub s start (!pos - start) in
      words_with_pos := (word, start, !pos) :: !words_with_pos)
  done;
  List.iter
    (fun (word, start_pos, end_pos) ->
      if has_mixed_scripts_in_word word then
        let byte_pos = ref start_pos in
        while !byte_pos < end_pos do
          let c = Char.code s.[!byte_pos] in
          let char_size, code =
            if c < 0x80 then (1, c)
            else
              let code, size = Util.get_unicode_point s !byte_pos in
              (size, code)
          in
          (match script_class_of_code code with
          | Some `Greek | Some `Cyrillic ->
              for i = 0 to char_size - 1 do
                positions := (!byte_pos + i) :: !positions
              done
          | _ -> ());
          byte_pos := !byte_pos + char_size
        done)
    (List.rev !words_with_pos);
  List.sort_uniq compare !positions

(* Generic error handling *)
let find_error_positions error_type data base s =
  let dict_type = data_to_dict_type data in
  match error_type with
  | InvisibleCharacters -> find_invisible_positions s
  | BadCapitalization -> find_bad_capitalization_positions dict_type base s
  | MultipleSpaces -> find_multiple_spaces_positions s
  | NonBreakingSpace -> find_non_breaking_space_positions s
  | MiscTypographicErrors -> []
  | MixedScripts -> find_mixed_scripts_positions s

let find_error_highlight_info error_type data base s conf =
  let dict_type = data_to_dict_type data in
  match error_type with
  | MiscTypographicErrors ->
      WithMessages (find_misc_typographic_positions dict_type s conf)
  | _ -> SimplePositions (find_error_positions error_type data base s)

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
          (fun ?code ?name:_ ?misc_msg:_ _ ->
            match code with
            | Some "00A0" -> Some "U+00A0 NON-BREAKING SPACE"
            | Some "202F" -> Some "U+202F NARROW NON-BREAKING SPACE"
            | _ -> None);
      }
  | MultipleSpaces ->
      {
        make_class = (fun _ -> "ms");
        make_title = (fun ?code:_ ?name:_ ?misc_msg:_ _ -> None);
      }
  | InvisibleCharacters ->
      {
        make_class = (fun hex -> if is_zero_width hex then "zw" else "ic");
        make_title =
          (fun ?code ?name ?misc_msg:_ _ ->
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
        make_title = (fun ?code:_ ?name:_ ?misc_msg:_ _ -> None);
      }
  | MiscTypographicErrors ->
      {
        make_class = (fun _ -> "mt");
        make_title = (fun ?code:_ ?name:_ ?misc_msg _ -> misc_msg);
      }
  | MixedScripts ->
      {
        make_class = (fun _ -> "mx");
        make_title =
          (fun ?code:_ ?name:_ ?misc_msg:_ _ ->
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
let make_highlight_html s highlight_info error_type conf =
  let buf = Buffer.create (String.length s * 2) in
  let style = get_highlight_style error_type conf in
  let pos_to_message =
    match (error_type, highlight_info) with
    | MiscTypographicErrors, WithMessages infos ->
        let map = Hashtbl.create (List.length infos) in
        List.iter (fun info -> Hashtbl.replace map info.pos info.message) infos;
        Some map
    | _ -> None
  in
  let positions =
    match highlight_info with
    | SimplePositions pos_list -> pos_list
    | WithMessages infos -> List.map (fun info -> info.pos) infos
  in
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
        let misc_msg =
          match pos_to_message with
          | Some map -> Hashtbl.find_opt map i
          | None -> None
        in
        let title_attr =
          match style.make_title ~code:hex ~name ?misc_msg conf with
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
  let prefix = (Util.prefix_base_password conf :> string) in
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
  let highlight_info =
    find_error_highlight_info error_type data base entry conf
  in
  let hl = make_highlight_html entry highlight_info error_type conf in
  let url_mod =
    Printf.sprintf "%sm=MOD_DATA&data=%s&key=%s&s=%s%s" prefix data istr_ s s2_p
  in
  let url_chk =
    Printf.sprintf "%sm=CHK_DATA_OK&d=%s&k=%s&s=%s&s2=%s" prefix data istr_
      s_ori s2
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
let analyze_string_errors dict_type base s check_errors_set =
  let add_if test error acc =
    if ErrorSet.mem error check_errors_set && test () then error :: acc else acc
  in
  []
  |> add_if (fun () -> has_non_breaking_space s) NonBreakingSpace
  |> add_if (fun () -> has_multiple_spaces s) MultipleSpaces
  |> add_if (fun () -> has_invisible_chars s) InvisibleCharacters
  |> add_if
       (fun () -> has_bad_capitalization dict_type base s)
       BadCapitalization
  |> add_if
       (fun () -> has_misc_typographic_errors dict_type s)
       MiscTypographicErrors
  |> add_if (fun () -> has_mixed_scripts s) MixedScripts

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
        let errors = analyze_string_errors dict_type base s check_errors_set in
        let filtered_errors =
          if ErrorSet.is_empty check_errors_set then errors else errors
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
                 if
                   ErrorSet.mem InvisibleCharacters check_error_types_set
                   && has_invisible_chars s
                 then add_error istr s InvisibleCharacters;
                 if
                   ErrorSet.mem BadCapitalization check_error_types_set
                   && has_bad_capitalization dict base s
                 then add_error istr s BadCapitalization;
                 if
                   ErrorSet.mem MultipleSpaces check_error_types_set
                   && has_multiple_spaces s
                 then add_error istr s MultipleSpaces;
                 if
                   ErrorSet.mem NonBreakingSpace check_error_types_set
                   && has_non_breaking_space s
                 then add_error istr s NonBreakingSpace;
                 if
                   ErrorSet.mem MiscTypographicErrors check_error_types_set
                   && has_misc_typographic_errors dict s
                 then add_error istr s MiscTypographicErrors;
                 if
                   ErrorSet.mem MixedScripts check_error_types_set
                   && has_mixed_scripts s
                 then add_error istr s MixedScripts))
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
