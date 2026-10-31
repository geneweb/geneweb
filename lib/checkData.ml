open Def
open Config
module Driver = Geneweb_db.Driver

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

type misc_error_info = { pos : int; message : string }

type highlight_info =
  | SimplePositions of int list
  | WithMessages of misc_error_info list

module ErrorSet = Set.Make (struct
  type t = error_type

  let compare = compare
end)

let data_to_dict_type = function
  | "fn" -> Fnames
  | "sn" -> Snames
  | "fna" -> Fnames_alias
  | "sna" -> Snames_alias
  | "place" -> Places
  | "pubn" -> PubNames
  | "qual" -> Qualifiers
  | "alias" -> Aliases
  | "occu" -> Occupation
  | "title" -> Titles
  | "domain" -> Estates
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

let place_suburb_sep_range s =
  let len = String.length s in
  if len > 0 && s.[0] = '[' then
    match String.index_opt s ']' with
    | Some i ->
        let rec find_end j =
          if j >= len then j
          else
            let c = Char.code s.[j] in
            if c = 0x20 || c = 0x2D then find_end (j + 1)
            else if
              c = 0xE2
              && j + 2 < len
              && Char.code s.[j + 1] = 0x80
              && (Char.code s.[j + 2] = 0x93 || Char.code s.[j + 2] = 0x94)
            then find_end (j + 3)
            else j
        in
        Some (i, find_end (i + 1))
    | None -> None
  else None

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

let is_roman_numeral s =
  Re.execp (Lazy.force roman_re) s || Re.execp (Lazy.force first_ordinal_re) s

(* Multiple Spaces functions *)
(* Check if the character following nbsp is part of a roman numeral *)
let has_roman_after_nbsp s i =
  let w = if s.[i] = '\xC2' then 2 else 3 in
  if i + w >= String.length s then false
  else
    let rest = String.sub s (i + w) (String.length s - (i + w)) in
    let next_word =
      match String.index_opt rest ' ' with
      | Some j -> String.sub rest 0 j
      | None -> rest
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

let char_len s i =
  match Utf8.nbc s.[i] with
  | n -> min n (String.length s - i)
  | exception Invalid_argument _ -> 1

let has_multiple_spaces s =
  let len = String.length s in
  let rec loop i =
    if i >= len then false
    else if is_any_space s i then
      let j = i + char_len s i in
      (j < len && is_any_space s j) || loop j
    else loop (i + char_len s i)
  in
  loop 0

let find_multiple_spaces_positions s =
  let positions = ref [] in
  let len = String.length s in
  let rec find_sequences byte_pos =
    if byte_pos >= len then List.rev !positions
    else if is_any_space s byte_pos then (
      let rec collect_sequence pos acc_positions =
        if pos >= len then (pos, List.rev acc_positions)
        else if is_any_space s pos then
          let next_pos = pos + char_len s pos in
          collect_sequence next_pos (pos :: acc_positions)
        else (pos, List.rev acc_positions)
      in
      let end_pos, sequence_positions = collect_sequence byte_pos [] in
      if List.length sequence_positions >= 2 then
        positions := List.rev_append sequence_positions !positions;
      find_sequences end_pos)
    else find_sequences (byte_pos + char_len s byte_pos)
  in
  find_sequences 0

let fix_multiple_spaces s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec skip i = if is_any_space s i then skip (i + char_len s i) else i in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else
      let j = i + char_len s i in
      if is_any_space s i && is_any_space s j then (
        Buffer.add_char buf ' ';
        loop (skip j))
      else (
        Buffer.add_substring buf s i (j - i);
        loop j)
  in
  loop 0

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
  String.starts_with ~prefix:"Mac" s
  || String.starts_with ~prefix:"Mc" s
  || String.starts_with ~prefix:"Fitz" s

(* Check if a word is either a Roman numeral or starts with an Irish prefix
   Examples of valid words:
   - Roman numerals: "XIV", "III", "IX"
   - Irish prefixes: "MacDonald", "McLeod", "FitzGerald" *)
let is_allowed_word s = is_roman_numeral s || is_irish_prefix s

let lowercase_allowed_words =
  [ "dit"; "dite"; "ou"; "et"; "genannt"; "vel"; "y"; "e" ]

let is_lowercase_allowed word =
  List.mem (String.lowercase_ascii word) lowercase_allowed_words

(* Bad capitalization functions *)

let is_ascii_upper c = c >= 'A' && c <= 'Z'
let is_ascii_lower c = c >= 'a' && c <= 'z'

(* Byte spans [start, stop) of the space-separated words of [s]. *)
let word_spans s =
  let len = String.length s in
  let rec loop i acc =
    if i >= len then List.rev acc
    else if s.[i] = ' ' then loop (i + 1) acc
    else
      let j =
        match String.index_from_opt s i ' ' with Some j -> j | None -> len
      in
      loop j ((i, j) :: acc)
  in
  loop 0 []

(* Byte spans [start, stop) of [s] covered by a particle of the base
   particles file. Particles are looked for at the beginning of [s], after
   a space or a hyphen, and right after a previous particle, so that
   "de la ", "d'", "bin " are recognised wherever they appear. A segment
   followed by a hyphen is also a particle when it is one followed by a
   space: "al" in "Abd al-Rahman". *)
let particle_spans particles s =
  let len = String.length s in
  let is_sep c = c = ' ' || c = '-' in
  let rec next_sep j =
    if j >= len || is_sep s.[j] then j else next_sep (j + 1)
  in
  let rec scan pos acc =
    if pos >= len then List.rev acc
    else if is_sep s.[pos] then scan (pos + 1) acc
    else
      let p = Mutil.get_particle particles (String.sub s pos (len - pos)) in
      if p <> "" then
        let stop = pos + String.length p in
        scan stop ((pos, stop) :: acc)
      else
        let stop = next_sep pos in
        let hyphen_particle =
          stop < len
          && s.[stop] = '-'
          &&
          let seg = String.sub s pos (stop - pos) ^ " " in
          Mutil.get_particle particles seg = seg
        in
        if hyphen_particle then scan (stop + 1) ((pos, stop) :: acc)
        else scan (stop + 1) acc
  in
  scan 0 []

let in_spans spans i = List.exists (fun (a, b) -> i >= a && i < b) spans

(* Roman numerals and Mac/Mc/Fitz names may legitimately mix cases. *)
let exempts_allowed_words dict =
  match dict with
  | Fnames | Snames | Fnames_alias | Snames_alias | PubNames | Aliases | Places
    ->
      true
  | Qualifiers | Occupation | Estates | Titles | Sources -> false

(* Every word of a first name or surname must start with a capital letter,
   except particles and a few connecting words ("dit", "y"...). *)
let requires_initial_capital dict =
  match dict with
  | Fnames | Snames | Fnames_alias | Snames_alias -> true
  | PubNames | Aliases | Places | Qualifiers | Occupation | Estates | Titles
  | Sources ->
      false

(* Invalid patterns inside the word [ws, we) of [s]:
   - "AAz": two capitals followed by a lowercase letter ("DUpont")
   - "aZ": a lowercase letter followed by a capital ("LaFayette") *)
let add_pattern_positions s ws we acc =
  let rec scan i acc =
    if i + 1 >= we then acc
    else
      let acc =
        if
          i + 2 < we
          && is_ascii_upper s.[i]
          && is_ascii_upper s.[i + 1]
          && is_ascii_lower s.[i + 2]
        then i :: (i + 1) :: acc
        else if is_ascii_lower s.[i] && is_ascii_upper s.[i + 1] then
          i :: (i + 1) :: acc
        else acc
      in
      scan (i + 1) acc
  in
  scan ws acc

let find_bad_capitalization_positions dict base s =
  match dict with
  | Sources -> []
  | Fnames | Snames | Fnames_alias | Snames_alias | PubNames | Aliases | Places
  | Qualifiers | Occupation | Estates | Titles -> (
      let exempt = exempts_allowed_words dict in
      let initial = requires_initial_capital dict in
      let positions =
        List.fold_left
          (fun acc (ws, we) ->
            let pats = add_pattern_positions s ws we [] in
            let low = initial && is_ascii_lower s.[ws] in
            if pats = [] && not low then acc
            else
              let w = String.sub s ws (we - ws) in
              let acc =
                if pats = [] || (exempt && is_allowed_word w) then acc
                else List.rev_append pats acc
              in
              if low && not (is_lowercase_allowed w) then ws :: acc else acc)
          [] (word_spans s)
      in
      match positions with
      | [] -> []
      | _ ->
          let pspans = particle_spans (Driver.base_particles base) s in
          List.filter (fun i -> not (in_spans pspans i)) positions
          |> List.sort_uniq compare)

let has_bad_capitalization dict base s =
  find_bad_capitalization_positions dict base s <> []

let zero_width_codes =
  List.rev_append
    (Util.get_problem_chars_codes `Invisible)
    (Util.get_problem_chars_codes `ZeroWidth)

let is_zero_width hex =
  match int_of_string_opt ("0x" ^ hex) with
  | Some code -> List.mem code zero_width_codes
  | None -> false

let invisible_chars_tbl =
  let tbl = Hashtbl.create 128 in
  List.iter
    (fun cat ->
      List.iter
        (fun code -> Hashtbl.replace tbl code ())
        (Util.get_problem_chars_codes cat))
    [ `Control; `Invisible; `Space; `ZeroWidth ];
  tbl

let is_invisible_char code = Hashtbl.mem invisible_chars_tbl code

let is_invisible_code code =
  if code < 0x80 then code < 0x20 || code = 0x7F else is_invisible_char code

let has_invisible_chars s =
  let len = String.length s in
  let rec aux i =
    if i >= len then false
    else
      let code, size = Util.get_unicode_point s i in
      if is_invisible_code code then true else aux (i + size)
  in
  aux 0

let find_invisible_positions s =
  let len = String.length s in
  let rec aux acc i =
    if i >= len then List.rev acc
    else
      let code, size = Util.get_unicode_point s i in
      if is_invisible_code code then aux (i :: acc) (i + size)
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

let char_before_parenthesis_pattern =
  Re.seq [ Re.group (Re.set "A-Za-z0-9"); Re.char '(' ]

let breton_trigram_pattern =
  Re.seq [ Re.group (Re.set "cC"); Re.set "’'"; Re.group (Re.set "hH") ]

let dash_replacements = [ (Re.str " -", "-"); (Re.str "- ", "-") ]
let max_misc_match_len = 6

let compile_replacements l =
  List.map (fun (pat, repl) -> (Re.compile pat, repl)) l

let replacements_of_dict =
  let occ_src = lazy (compile_replacements simple_replacements) in
  let std =
    lazy
      (compile_replacements
         (List.rev_append (List.rev simple_replacements) dash_replacements))
  in
  function Occupation | Sources -> Lazy.force occ_src | _ -> Lazy.force std

let char_before_paren_re = lazy (Re.compile char_before_parenthesis_pattern)
let breton_trigram_re = lazy (Re.compile breton_trigram_pattern)

let misc_alt_of_dict =
  let build extra =
    Re.compile
      (Re.alt
         (List.rev_append
            (List.rev_map fst simple_replacements)
            (List.rev_append (List.rev_map fst extra)
               [ char_before_parenthesis_pattern; breton_trigram_pattern ])))
  in
  let std = lazy (build dash_replacements) in
  let occ_src = lazy (build []) in
  function Occupation | Sources -> Lazy.force occ_src | _ -> Lazy.force std

let has_misc_typographic_errors dict_type s =
  let re = misc_alt_of_dict dict_type in
  let check str = Re.execp re str in
  match dict_type with
  | Places -> (
      match place_suburb_sep_range s with
      | Some (brk, main_s) ->
          let sub = String.sub s 1 (brk - 1) in
          let main = String.sub s main_s (String.length s - main_s) in
          check sub || check main
      | None -> check s)
  | _ -> check s

let find_misc_typographic_positions dict_type s conf =
  let replacements = replacements_of_dict dict_type in
  let errors = ref [] in
  let pos = ref 0 in
  let len = String.length s in
  let try_pattern re message_or_key add_positions found_match =
    if !found_match = None then
      match
        Re.exec_opt ~pos:!pos ~len:(min max_misc_match_len (len - !pos)) re s
      with
      | Some result ->
          let start_pos = Re.Group.start result 0 in
          let end_pos = Re.Group.stop result 0 in
          if start_pos = !pos then (
            add_positions start_pos end_pos message_or_key;
            found_match := Some end_pos)
      | None -> ()
  in
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
  while !pos < len do
    let found_match = ref None in
    List.iter
      (fun (re, repl) ->
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
        try_pattern re "" add_pos found_match)
      replacements;
    try_pattern
      (Lazy.force char_before_paren_re)
      "chk_data ponctuation error missing space" add_single_pos found_match;
    try_pattern
      (Lazy.force breton_trigram_re)
      "chk_data ponctuation error breton trigram help" add_range_pos found_match;
    pos := match !found_match with Some p -> p | None -> !pos + 1
  done;
  let errors = List.rev !errors in
  match dict_type with
  | Places -> (
      match place_suburb_sep_range s with
      | Some (sep_s, sep_e) ->
          List.filter (fun e -> e.pos < sep_s || e.pos >= sep_e) errors
      | None -> errors)
  | _ -> errors

let apply_misc_fixes dict_type s =
  let s =
    List.fold_left
      (fun acc (re, repl) -> Re.replace_string re ~by:repl acc)
      s
      (replacements_of_dict dict_type)
  in
  s
  |> Re.replace (Lazy.force char_before_paren_re) ~f:(fun groups ->
      Re.Group.get groups 1 ^ " (")
  |> Re.replace (Lazy.force breton_trigram_re) ~f:(fun groups ->
      Re.Group.get groups 1 ^ "ʼ" ^ Re.Group.get groups 2)

let fix_misc_typographic_errors dict_type s =
  match dict_type with
  | Places -> (
      match place_suburb_sep_range s with
      | Some (brk, main_s) ->
          let sub = String.sub s 1 (brk - 1) in
          let sep = String.sub s brk (main_s - brk) in
          let main = String.sub s main_s (String.length s - main_s) in
          "["
          ^ apply_misc_fixes dict_type sub
          ^ sep
          ^ apply_misc_fixes dict_type main
      | None -> apply_misc_fixes dict_type s)
  | _ -> apply_misc_fixes dict_type s

let script_cache = Hashtbl.create 256

let script_of_code code =
  if code < 0x80 then
    if (code >= 0x41 && code <= 0x5A) || (code >= 0x61 && code <= 0x7A) then
      Some `Latn
    else None
  else
    match Hashtbl.find_opt script_cache code with
    | Some r -> r
    | None ->
        let r =
          if not (Uchar.is_valid code) then None
          else
            match Uucp.Script.script (Uchar.of_int code) with
            | `Zyyy | `Zzzz -> None
            | `Hira | `Kana | `Hang | `Bopo -> Some `Hani
            | sc -> Some sc
        in
        Hashtbl.replace script_cache code r;
        r

let word_script_counts s ws we =
  let rec loop i acc =
    if i >= we then acc
    else
      let code, size = Util.get_unicode_point s i in
      let acc =
        match script_of_code code with
        | None | Some `Zinh -> acc
        | Some sc ->
            let n = Option.value ~default:0 (List.assoc_opt sc acc) in
            (sc, n + 1) :: List.remove_assoc sc acc
      in
      loop (i + size) acc
  in
  loop ws []

let script_runs s =
  let len = String.length s in
  let close start i acc = if i > start then (start, i) :: acc else acc in
  let rec loop i start acc =
    if i >= len then close start i acc
    else
      let code, size = Util.get_unicode_point s i in
      match script_of_code code with
      | None -> loop (i + size) (i + size) (close start i acc)
      | Some _ -> loop (i + size) start acc
  in
  loop 0 0 []

let has_mixed_scripts s =
  String.exists (fun c -> c >= '\xCD') s
  && List.exists
       (fun (ws, we) ->
         match word_script_counts s ws we with
         | _ :: _ :: _ -> true
         | _ -> false)
       (script_runs s)

let find_mixed_scripts_positions s =
  List.fold_left
    (fun acc (ws, we) ->
      match word_script_counts s ws we with
      | _ :: _ :: _ as counts ->
          let major, _ =
            List.fold_left
              (fun (bs, bn) (sc, n) ->
                if n > bn || (n = bn && sc = `Latn) then (sc, n) else (bs, bn))
              (`Zzzz, 0) counts
          in
          let rec mark i acc =
            if i >= we then acc
            else
              let code, size = Util.get_unicode_point s i in
              let acc =
                match script_of_code code with
                | Some sc when sc <> major && sc <> `Zinh -> i :: acc
                | _ -> acc
              in
              mark (i + size) acc
          in
          mark ws acc
      | _ -> acc)
    [] (script_runs s)

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
  let pos_tbl = Hashtbl.create 16 in
  (match highlight_info with
  | SimplePositions pos_list ->
      List.iter (fun p -> Hashtbl.replace pos_tbl p ()) pos_list
  | WithMessages infos ->
      List.iter (fun info -> Hashtbl.replace pos_tbl info.pos ()) infos);
  let rec process_char i =
    if i >= String.length s then Buffer.contents buf
    else begin
      let char_size = char_len s i in
      let escaped = (Util.escape_html (String.sub s i char_size) :> string) in
      if Hashtbl.mem pos_tbl i then
        let code, _ = Util.get_unicode_point s i in
        let code = if Uchar.is_valid code then code else 0xFFFD in
        let hex = Printf.sprintf "%04X" code in
        let name =
          match Util.get_problem_char_name code with
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
          | Some t ->
              Printf.sprintf " title=\"%s\"" (Util.escape_html t :> string)
          | None -> ""
        in
        Printf.bprintf buf "<span class=\"%s\"%s>%s</span>"
          (style.make_class hex) title_attr escaped
      else Buffer.add_string buf escaped;
      process_char (i + char_size)
    end
  in
  process_char 0

let first_word s =
  match String.index_opt s ' ' with Some i -> String.sub s 0 i | None -> s

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
  |> List.sort_uniq Driver.Istr.compare

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
  collect_attributes base p
    ~std_attrs:
      [
        get_birth_src_x;
        get_baptism_src_x;
        get_death_src_x;
        get_burial_src_x;
        get_psources_x;
      ]
    ~epers_attr_get:get_epers_src
    ~marriage_attr:(fun fam ->
      [ Driver.get_marriage_src fam; Driver.get_fsources fam ])
    ~efam_attr_get:get_efam_src

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

let cache_file_exists conf dict_type =
  Cache.exists conf.bname (dict_to_cache_name dict_type)

let read_cache conf dict_type =
  Option.value ~default:[]
    (Cache.read conf.bname (dict_to_cache_name dict_type))

let update_cache_entry conf dict_type ~old_istr ~new_istr new_value =
  Cache.update conf.bname
    (dict_to_cache_name dict_type)
    ~old_istr ~new_istr new_value

let find_dict_type_for_istr conf istr =
  let check_in_cache dict_type =
    if cache_file_exists conf dict_type then
      let entries = read_cache conf dict_type in
      List.exists (fun (i, _) -> Driver.Istr.equal i istr) entries
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
        if errors = [] then process_entries rest acc count
        else process_entries rest ((istr, s, errors) :: acc) (count + 1)
  in
  List.rev (process_entries entries [] 0)

let collect_all_errors ?max_results ?(sel_err_types = []) base dict =
  let seen = Hashtbl.create 4096 in
  let found = ref [] in
  let count = ref 0 in
  let collect_strings = collect_dict_strings base dict in
  let check_errors_set = make_error_set sel_err_types in
  let check istr =
    if not (Driver.Istr.is_empty istr || Hashtbl.mem seen istr) then (
      Hashtbl.replace seen istr ();
      let s = Driver.sou base istr in
      match analyze_string_errors dict base s check_errors_set with
      | [] -> ()
      | errs -> (
          found := (istr, s, errs) :: !found;
          incr count;
          match max_results with
          | Some max when !count >= max -> raise Max_results_reached
          | _ -> ()))
  in
  (try
     Geneweb_db.Collection.iter
       (fun i -> List.iter check (collect_strings (Driver.poi base i)))
       (Driver.ipers base)
   with Max_results_reached -> ());
  List.sort (fun (_, s1, _) (_, s2, _) -> String.compare s1 s2) !found

(* Main function *)
let collect_all_errors_with_cache ?max_results ?(sel_err_types = []) conf base
    dict =
  let use_cache =
    match Util.p_getenv conf.env "nocache" with Some "1" -> false | _ -> true
  in
  if use_cache then
    if cache_file_exists conf dict then
      collect_all_errors_from_cache conf dict base max_results ~sel_err_types ()
    else []
  else
    let is_roglo = List.assoc_opt "roglo" conf.base_env = Some "yes" in
    if is_roglo then []
    else collect_all_errors ?max_results ~sel_err_types base dict
