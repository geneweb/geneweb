(* camlp4r pa_extend.cmo *)
(* $Id: ged2gwb.ml,v 1.26 1998-12-05 11:58:32 ddr Exp $ *)

open Def;
open Gutil;

type record =
  { rlab : string;
    rval : string;
    rcont : string;
    rsons : list record;
    rpos : int }
;

value titles_aurejac = ref False;
value lowercase_first_names = ref False;
value lowercase_surnames = ref False;
value extract_first_names = ref True;
value extract_public_names = ref True;
value ansel_option = ref None;
value ansel_characters = ref True;
value try_negative_dates = ref False;
value no_negative_dates = ref False;
value set_not_dead = ref False;

(* Reading input *)

value line_cnt = ref 1;
value in_file = ref "";

value print_location pos =
  Printf.printf "File \"%s\", line %d:\n" in_file.val pos
;

value buff = ref (String.create 80);
value store len x =
  do if len >= String.length buff.val then
       buff.val := buff.val ^ String.create (String.length buff.val)
     else ();
     buff.val.[len] := x;
  return succ len
;
value get_buff len = String.sub buff.val 0 len;

value rec skip_eol =
  parser
  [ [: `'\n' | '\r'; _ = skip_eol :] -> ()
  | [: :] -> () ]
;

value rec get_to_eoln len =
  parser
  [ [: `'\n' | '\r'; _ = skip_eol :] -> get_buff len
  | [: `c; s :] -> get_to_eoln (store len c) s
  | [: :] -> get_buff len ]
;

value rec skip_to_eoln =
  parser
  [ [: `'\n' | '\r'; _ = skip_eol :] -> ()
  | [: `_; s :] -> skip_to_eoln s
  | [: :] -> () ]
;

value rec get_ident len =
  parser
  [ [: `' ' :] -> get_buff len
  | [: `c when not (List.mem c ['\n'; '\r']); s :] -> get_ident (store len c) s
  | [: :] -> get_buff len ]
;

value skip_space =
  parser
  [ [: `' ' :] -> ()
  | [: :] -> () ]
;

value rec get_lev n =
  parser
  [ [: `c when c == n; _ = skip_space; r1 = get_ident 0; r2 = get_ident 0;
       r3 = get_to_eoln 0 ? "get to eoln";
       l = get_lev_list [] (Char.chr (Char.code n + 1)) ? "get lev list" :] ->
      let (rlab, rval, rcont) =
        if String.length r1 > 0 && r1.[0] = '@' then (r2, r1, r3)
        else
          let rval = if r3 = "" then r2 else r2 ^ " " ^ r3 in
          (r1, rval, "")
      in
      {rlab = rlab;
       rval =
         if ansel_characters.val then rval else Ansel.of_iso_8859_1 rval;
       rcont =
         if ansel_characters.val then rcont else Ansel.of_iso_8859_1 rcont;
       rsons = List.rev l;
       rpos = line_cnt.val} ]
and get_lev_list l n =
  parser
  [ [: x = get_lev n; s :] -> get_lev_list [x :: l] n s
  | [: :] -> l ]
;

(* Error *)

value bad_dates = ref [];

value print_bad_date pos d =
  if List.mem d bad_dates.val then ()
  else
    do bad_dates.val := [d :: bad_dates.val];
       print_location pos;
       Printf.printf "Can't decode date %s\n" d;
       flush stdout;
    return ()
;

(* Decoding fields *)

value rec skip_spaces =
  parser
  [ [: `' '; s :] -> skip_spaces s
  | [: :] -> () ]
;

value rec ident_slash len =
  parser
  [ [: `'/' :] -> get_buff len
  | [: `c; i = ident_slash (store len c) :] -> i
  | [: :] -> get_buff len ]
;

value strip c str =
  let start = loop 0
    where rec loop i =
      if i == String.length str then i
      else if str.[i] == c then loop (i + 1)
      else i
  in
  let stop = loop (String.length str - 1)
    where rec loop i =
      if i == -1 then i + 1
      else if str.[i] == c then loop (i - 1)
      else i + 1
  in
  if start == 0 && stop == String.length str then str
  else if start >= stop then ""
  else String.sub str start (stop - start)
;

value strip_spaces = strip ' ';
value strip_newlines = strip '\n';

value less_greater_escaped s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
      [ '<' | '>' -> True
      | x -> need_code (succ i) ]
    else False
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
        [ '<' | '>' -> i1 + 4
        | _ -> succ i1 ]
      in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
        [ '<' -> do String.blit "&lt;" 0 s1 i1 4; return i1 + 4
        | '>' -> do String.blit "&gt;" 0 s1 i1 4; return i1 + 4
        | c -> do s1.[i1] := c; return succ i1 ]
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (String.create len) 0 0
  else s
;

value parse_name =
  parser
  [ [: _ = skip_spaces;
       invert = parser [ [: `'/' :] -> True | [: :] -> False ];
       f = ident_slash 0; _ = skip_spaces; s = ident_slash 0 :] ->
      let (f, s) = if invert then (s, f) else (f, s) in
      let f = strip_spaces f in
      let s = strip_spaces s in
      (if f = "" then "x" else f, if s = "" then "?" else s) ]
;

value rec find_field lab =
  fun
  [ [r :: rl] -> if r.rlab = lab then Some r else find_field lab rl
  | [] -> None ]
;

value rec find_all_fields lab =
  fun
  [ [r :: rl] ->
      if r.rlab = lab then [r :: find_all_fields lab rl]
      else find_all_fields lab rl
  | [] -> [] ]
;

value rec lexing =
  parser
  [ [: `('0'..'9' as c); n = number (store 0 c) :] -> ("INT", n)
  | [: `('A'..'Z' as c); i = ident (store 0 c) :] -> ("ID", i)
  | [: `'.' :] -> ("", ".")
  | [: `' ' | '\r'; s :] -> lexing s
  | [: _ = Stream.empty :] -> ("EOI", "")
  | [: `x :] -> ("", String.make 1 x) ]
and number len =
  parser
  [ [: `('0'..'9' as c); n = number (store len c) :] -> n
  | [: :] -> get_buff len ]
and ident len =
  parser
  [ [: `('A'..'Z' as c); n = ident (store len c) :] -> n
  | [: :] -> get_buff len ]
;

value make_lexing s = Stream.from (fun _ -> Some (lexing s));

value tparse (p_con, p_prm) =
  if p_prm = "" then parser [: `(con, prm) when con = p_con :] -> prm
  else parser [: `(con, prm) when con = p_con && prm = p_prm :] -> prm
;

value using_token (p_con, p_prm) =
  match p_con with
  [ "" | "INT" | "ID" | "EOI" -> ()
  | _ ->
      raise (Token.Error ("\
the constructor \"" ^ p_con ^ "\" is not recognized by the lexer")) ]
;

value lexer =
  {Token.func = fun s -> (make_lexing s, fun _ -> (0, 0));
   Token.using = using_token;
   Token.removing = fun _ -> ();
   Token.tparse = tparse;
   Token.text = fun _ -> "<tok>"}
;

type title_date 'a = [ TDinterv of 'a and 'a | TDstart of 'a | TDend of 'a ];

value g = Grammar.create lexer;
value date = Grammar.Entry.create g "date";
value title_date = Grammar.Entry.create g "title_date";
value find_year =
  let rec find strm =
    match strm with parser
    [ [: `("INT", n) :] ->
        let n = int_of_string n in
        if n >= 32 && n <= 2500 then n else find strm
    | [: `("EOI", "") :] -> raise Not_found
    | [: `_ :] -> find strm ]
  in
  Grammar.Entry.of_parser g "find_year" find
;
EXTEND
  GLOBAL: date title_date;
  date:
    [[ p = prec; (n1, n2, n3) = simple_date; EOI -> (p, n1, n2, n3) ]];
  simple_date:
    [[ LIST0 "."; n1 = OPT int; LIST0 ".";
       n2 = OPT [ i = int -> abs i | m = month -> m ]; LIST0 ".";
       n3 = OPT int; LIST0 "." ->
         let n3 =
           if no_negative_dates.val then
             match n3 with
             [ Some n3 -> Some (abs n3)
             | None -> None ]
           else n3
         in
         (n1, n2, n3) ]];
  title_date:
    [[ ID "BET"; _ = prec; d1 = simple_date;
       ID "AND"; _ = prec; d2 = simple_date ->
         TDinterv d1 d2
     | ID "BEF"; d = simple_date -> TDend d
     | _ = prec; d = simple_date -> TDstart d ]];
  prec:
    [[ ID "ABT" -> About | ID "ENV" -> About
     | ID "BEF" -> Before | ID "AFT" -> After
     | ID "EST" -> Maybe | -> Sure ]];
  month:
    [[ ID "JAN" -> 1 | ID "FEB" -> 2 | ID "MAR" -> 3
     | ID "APR" -> 4 | ID "MAY" -> 5 | ID "JUN" -> 6
     | ID "JUL" -> 7 | ID "AUG" -> 8 | ID "SEP" -> 9
     | ID "OCT" -> 10 | ID "NOV" -> 11 | ID "DEC" -> 12 ]];
  int:
    [[ i = INT -> int_of_string i
     | "-"; i = INT -> - int_of_string i ]];
END;

value date_of_field pos d =
  if d = "" then None
  else
    let s = Stream.of_string (String.uppercase d) in
    let r =
      try Some (Grammar.Entry.parse date s) with
      [ Stdpp.Exc_located loc e -> None ]
    in
    match r with
    [ Some (p, Some d, Some m, Some y) ->
        Some {day = d; month = m; year = y; prec = p}
    | Some (p, None, Some m, Some y) ->
        Some {day = 0; month = m; year = y; prec = p}
    | Some (p, None, None, Some y) ->
        Some {day = 0; month = 0; year = y; prec = p}
    | Some (p, Some y, None, None) ->
        Some {day = 0; month = 0; year = y; prec = p}
    | _ ->
        try
          let y = Grammar.Entry.parse find_year (Stream.of_string d) in
          Some {day = 0; month = 0; year = y; prec = Maybe}
        with
        [ Stdpp.Exc_located loc e ->
            do print_bad_date pos d; return None ] ]
;

(* Creating base *)

type choice 'a 'b = [ Left of 'a | Right of 'b ];
type tab 'a = {arr : mutable array 'a; tlen : mutable int};

type gen =
  {g_per : tab (choice string base_person);
   g_asc : tab (choice string base_ascend);
   g_fam : tab (choice string base_family);
   g_cpl : tab (choice string base_couple);
   g_str : tab string;
   g_ic : in_channel;
   g_not : Hashtbl.t string int;
   g_src : Hashtbl.t string int;
   g_hper : Hashtbl.t string Adef.iper;
   g_hfam : Hashtbl.t string Adef.ifam;
   g_hstr : Hashtbl.t string Adef.istr;
   g_hnam : Hashtbl.t string (ref int)}
;

value assume_tab name tab none =
  if tab.tlen == Array.length tab.arr then
    let new_len = 2 * Array.length tab.arr + 1 in
    let new_arr = Array.create new_len none in
    do Array.blit tab.arr 0 new_arr 0 (Array.length tab.arr);
       tab.arr := new_arr;
    return ()
  else ()
;

value add_string gen s =
  try Hashtbl.find gen.g_hstr s with
  [ Not_found ->
      let i = gen.g_str.tlen in
      do assume_tab "gen.g_str" gen.g_str "";
         gen.g_str.arr.(i) := s;
         gen.g_str.tlen := gen.g_str.tlen + 1;
         Hashtbl.add gen.g_hstr s (Adef.istr_of_int i);
      return
      Adef.istr_of_int i ]
;        

value per_index gen lab =
  try Hashtbl.find gen.g_hper lab with
  [ Not_found ->
      let i = gen.g_per.tlen in
      do assume_tab "gen.g_per" gen.g_per (Left "");
         gen.g_per.arr.(i) := Left lab;
         gen.g_per.tlen := gen.g_per.tlen + 1;
         assume_tab "gen.g_asc" gen.g_asc (Left "");
         gen.g_asc.arr.(i) := Left lab;
         gen.g_asc.tlen := gen.g_asc.tlen + 1;
         Hashtbl.add gen.g_hper lab (Adef.iper_of_int i);
      return Adef.iper_of_int i ]
;

value fam_index gen lab =
  try Hashtbl.find gen.g_hfam lab with
  [ Not_found ->
      let i = gen.g_fam.tlen in
      do assume_tab "gen.g_fam" gen.g_fam (Left "");
         gen.g_fam.arr.(i) := Left lab;
         gen.g_fam.tlen := gen.g_fam.tlen + 1;
         assume_tab "gen.g_cpl" gen.g_cpl (Left "");
         gen.g_cpl.arr.(i) := Left lab;
         gen.g_cpl.tlen := gen.g_cpl.tlen + 1;
         Hashtbl.add gen.g_hfam lab (Adef.ifam_of_int i);
      return Adef.ifam_of_int i ]
;

value unknown_per gen i =
  let empty = add_string gen "" in
  let what = add_string gen "?" in
  let p =
    {first_name = what;
     surname = what;
     occ = i;
     public_name = empty;
     photo = empty;
     nick_names = []; aliases = []; first_names_aliases = [];
     surnames_aliases = [];
     titles = []; occupation = empty;
     sexe = Neutre; access = IfTitles;
     birth = Adef.codate_None; birth_place = empty; birth_src = empty;
     baptism = Adef.codate_None; baptism_place = empty; baptism_src = empty;
     death = DontKnowIfDead; death_place = empty; death_src = empty;
     burial = UnknownBurial; burial_place = empty; burial_src = empty;
     family = [| |];
     notes = empty;
     psources = empty;
     cle_index = Adef.iper_of_int i}
  and a =
    {parents = None;
     consang = Adef.fix (-1)}
  in
  (p, a)
;

value phony_per gen =
  let i = gen.g_per.tlen in
  let (person, ascend) = unknown_per gen i in
  do assume_tab "gen.g_per" gen.g_per (Left "");
     gen.g_per.tlen := gen.g_per.tlen + 1;
     gen.g_per.arr.(i) := Right person;
     assume_tab "gen.g_asc" gen.g_asc (Left "");
     gen.g_asc.arr.(i) := Right ascend;
     gen.g_asc.tlen := gen.g_asc.tlen + 1;
  return Adef.iper_of_int i
;

value this_year = 1998;

value make_title gen (title, place) =
  {t_name = Tnone;
   t_title = add_string gen title;
   t_place = add_string gen place;
   t_date_start = Adef.codate_None;
   t_date_end = Adef.codate_None;
   t_nth = 0}
;

value string_ini_eq s1 i s2 =
  loop i 0 where rec loop i j =
    if j == String.length s2 then True
    else if i == String.length s1 then False
    else if s1.[i] == s2.[j] then loop (i + 1) (j + 1)
    else False
;

value particle s i =
  string_ini_eq s i "des " || string_ini_eq s i "DES " ||
  string_ini_eq s i "de " || string_ini_eq s i "DE " ||
  string_ini_eq s i "du " || string_ini_eq s i "DU " ||
  string_ini_eq s i "d'" || string_ini_eq s i "D'"
;

value lowercase_name s =
  loop (particle s 0) 0 where rec loop uncap i =
    if i == String.length s then s
    else
      let c = s.[i] in
      let (c, uncap) =
        match c with
        [ 'a'..'z' | 'á'..'ý' ->
            (if uncap then c
             else Char.chr (Char.code c - Char.code 'a' + Char.code 'A'),
             True)
        | 'A'..'Z' | 'À'.. 'Ý' ->
            (if not uncap then c
             else Char.chr (Char.code c - Char.code 'A' + Char.code 'a'),
             True)
        | c -> (c, particle s (i + 1)) ]
      in
      do s.[i] := c; return loop uncap (i + 1)
;

value look_like_a_number s =
  loop 0 where rec loop i =
    if i == String.length s then True
    else
      match s.[i] with
      [ '0'..'9' -> loop (i + 1)
      | _ -> False ]
;

value look_like_a_roman_number s =
  loop 0 where rec loop i =
    if i == String.length s then True
    else
      match s.[i] with
      [ 'I' | 'V' | 'X' | 'L' -> loop (i + 1)
      | _ -> False ]
;

value is_a_name_char =
  fun
  [ 'A'..'Z' | 'a'..'z' | 'À'..'Ö' | 'Ø'..'ö' | 'ø'..'ÿ' | '0'..'9' | '-'
  | ''' -> True
  | _ -> False ]
;

value rec next_word_pos s i =
  if i == String.length s then i
  else if is_a_name_char s.[i] then i
  else next_word_pos s (i + 1)
;

value rec next_sep_pos s i =
  if i == String.length s then String.length s
  else if is_a_name_char s.[i] then next_sep_pos s (i + 1)
  else i
;

value public_name_word =
  ["Ier"; "Ière"; "der"; "die"; "el"; "le"; "la"; "the"]
;

value rec is_a_public_name s i =
  let i = next_word_pos s i in
  if i == String.length s then False
  else
    let j = next_sep_pos s i in
    if j > i then
      let w = String.sub s i (j - i) in
      if look_like_a_number w then True
      else if look_like_a_roman_number w then True
      else if List.mem w public_name_word then True
      else is_a_public_name s j
    else False
;

value get_lev0 =
  parser
  [ [: `'0'; _ = skip_space; r1 = get_ident 0; r2 = get_ident 0;
       r3 = get_to_eoln 0 ? "get to eoln";
       l = get_lev_list [] '1' ? "get lev list" :] ->
      let (rlab, rval) = if r2 = "" then (r1, "") else (r2, r1) in
      let rval =
        if ansel_characters.val then rval else Ansel.of_iso_8859_1 rval
      in
      let rcont =
        if ansel_characters.val then r3 else Ansel.of_iso_8859_1 r3
      in
      {rlab = rlab; rval = rval; rcont = rcont; rsons = List.rev l;
       rpos = line_cnt.val} ]
;

value find_notes_record gen addr =
  match try Some (Hashtbl.find gen.g_not addr) with [ Not_found -> None ] with
  [ Some i ->
      do seek_in gen.g_ic i; return
      Some (get_lev0 (Stream.of_channel gen.g_ic))
  | None -> None ]
;

value find_sources_record gen addr =
  match try Some (Hashtbl.find gen.g_src addr) with [ Not_found -> None ] with
  [ Some i ->
      do seek_in gen.g_ic i; return
      Some (get_lev0 (Stream.of_channel gen.g_ic))
  | None -> None ]
;

value extract_notes gen rl =
  List.fold_right
    (fun r lines ->
       List.fold_right
         (fun r lines ->
            if r.rlab = "NOTE" && r.rval <> "" && r.rval.[0] == '@'
            then
              match find_notes_record gen r.rval with
              [ Some r ->
                  let l = List.map (fun r -> (r.rlab, r.rval)) r.rsons in
                  [("NOTE", r.rcont) :: l @ lines]
              | None ->
                  do print_location r.rpos;
                     Printf.printf "Note %s not found\n" r.rval;
                     flush stdout;
                  return lines ]
            else [(r.rlab, r.rval) :: lines])
         [r :: r.rsons] lines)
    rl []
;

value treat_indi_notes_titles gen rl =
  let lines = extract_notes gen rl in
  let (notes, titles) =
    List.fold_left
      (fun (s, titles) (lab, n) ->
         let n = strip_spaces n in
         let titles = titles @ Aurejac.find_titles n in
         let s =
           if s = "" then n
           else if lab = "CONT" || lab = "NOTE" then s ^ "<br>\n" ^ n
           else if n = "" then s
           else s ^ "\n" ^ n
         in
         (s, titles))
      ("", []) lines
  in
  let titles = List.map (make_title gen) titles in
  (add_string gen (strip_newlines notes), titles)
;

value treat_indi_notes gen rl =
  let lines = extract_notes gen rl in
  let notes =
    List.fold_left
      (fun s (lab, n) ->
         let n = strip_spaces n in
         if s = "" then n
         else if lab = "CONT" || lab = "NOTE" then s ^ "<br>\n" ^ n
         else if n = "" then s
         else s ^ "\n" ^ n)
      "" lines
  in
  add_string gen (strip_newlines notes)
;

value source gen r =
  match find_field "SOUR" r.rsons with
  [ Some r ->
      if String.length r.rval > 0 && r.rval.[0] = '@' then
        match find_sources_record gen r.rval with
        [ Some v -> v.rcont
        | None ->
            do print_location r.rpos;
               Printf.printf "Source %s not found\n" r.rval;
               flush stdout;
            return "" ]
      else r.rval
  | _ -> "" ]
;

value string_empty = ref (Adef.istr_of_int 0);
value string_x = ref (Adef.istr_of_int 0);

value p_index_from s i c =
  if i >= String.length s then String.length s
  else
    try String.index_from s i c with
    [ Not_found -> String.length s ]
;

value strip_sub s beg len =
  strip_spaces (String.sub s beg len)
;

value decode_title s =
  let i1 = p_index_from s 0 ',' in
  let i2 = p_index_from s (i1 + 1) ',' in
  let title = strip_sub s 0 i1 in
  let (place, nth) =
    if i1 == String.length s then ("", 0)
    else if i2 == String.length s then
      let s1 = strip_sub s (i1 + 1) (i2 - i1 - 1) in
      try ("", int_of_string s1) with
      [ Failure _ -> (s1, 0) ]
    else
      let s1 = strip_sub s (i1 + 1) (i2 - i1 - 1) in
      let s2 = strip_sub s (i2 + 1) (String.length s - i2 - 1) in
      try (s1, int_of_string s2) with
      [ Failure _ ->
          (strip_sub s i1 (String.length s - i1), 0) ]
  in
  (title, place, nth)  
;

value date_of_sd =
  fun
  [ (Some d, Some m, Some y) -> {day = d; month = m; year = y; prec = Sure}
  | (None, Some m, Some y) -> {day = 0; month = m; year = y; prec = Sure}
  | (None, None, Some y) -> {day = 0; month = 0; year = y; prec = Sure}
  | (Some y, None, None) -> {day = 0; month = 0; year = y; prec = Sure}
  | _ -> raise Not_found ]
;

value decode_date_interval pos s =
  let strm = Stream.of_string s in
  try
    match Grammar.Entry.parse title_date strm with
    [ TDinterv d1 d2 -> (Some (date_of_sd d1), Some (date_of_sd d2))
    | TDstart d1 -> (Some (date_of_sd d1), None)
    | TDend d2 -> (None, Some (date_of_sd  d2)) ]
  with
  [ Stdpp.Exc_located _ _ | Not_found ->
      do print_bad_date pos s; return (None, None) ]
;

value treat_indi_title gen public_name r =
  let (title, place, nth) = decode_title r.rval in
  let (date_start, date_end) =
    match find_field "DATE" r.rsons with
    [ Some r -> decode_date_interval r.rpos r.rval
    | None -> (None, None) ]
  in
  let name =
    match find_field "NOTE" r.rsons with
    [ Some r ->
        if r.rval = public_name then Tmain
        else Tname (add_string gen (strip_spaces r.rval))
    | None -> Tnone ]
  in
  {t_name = name; t_title = add_string gen title;
   t_place = add_string gen place;
   t_date_start = Adef.codate_of_od date_start;
   t_date_end = Adef.codate_of_od date_end;
   t_nth = nth}
;

value add_indi gen r =
  let i = per_index gen r.rval in
  let name_sons = find_field "NAME" r.rsons in
  let public_name =
    match name_sons with
    [ Some n ->
        match find_field "GIVN" n.rsons with
        [ Some r -> r.rval
        | None -> "" ]
    | None -> "" ]
  in
  let (first_name, surname, occ, public_name, first_name_alias) =
    match name_sons with
    [ Some n ->
        let (f, s) = parse_name (Stream.of_string n.rval) in
        let (f, pn, fa) =
          if extract_public_names.val || extract_first_names.val then
            let i = next_word_pos f 0 in
            let j = next_sep_pos f i in
            if j == String.length f then (f, public_name, "")
            else
              let fn = String.sub f i (j - i) in
              if public_name = "" && extract_public_names.val then
                if is_a_public_name f j then (fn, f, "")
                else if extract_first_names.val then (fn, "", f)
                else (f, "", "")
              else (fn, public_name, f)
          else (f, public_name, "")
        in
        let f = if lowercase_first_names.val then lowercase_name f else f in
        let fa = if lowercase_first_names.val then lowercase_name fa else fa in
        let s = if lowercase_surnames.val then lowercase_name s else s in
        let r =
          let key = Name.strip_lower (f ^ " " ^ s) in
          try Hashtbl.find gen.g_hnam key with
          [ Not_found ->
              let r = ref (-1) in
              do Hashtbl.add gen.g_hnam key r; return r ]
        in
        do incr r; return (f, s, r.val, pn, fa)
    | None -> ("?", "?", Adef.int_of_iper i, public_name, "") ]
  in
  let nick_name =
    match name_sons with
    [ Some n ->
        match find_field "NICK" n.rsons with
        [ Some r -> r.rval
        | None -> "" ]
    | None -> "" ]
  in
  let surname_alias =
    match name_sons with
    [ Some n ->
        match find_field "SURN" n.rsons with
        [ Some r -> r.rval
        | None -> "" ]
    | None -> "" ]
  in
  let sex =
    match find_field "SEX" r.rsons with
    [ Some {rval = "M"} -> Masculin
    | Some {rval = "F"} -> Feminin
    | _ -> Neutre ]
  in
  let photo =
    match find_field "OBJE" r.rsons with
    [ Some r ->
        match find_field "FILE" r.rsons with
        [ Some r -> r.rval
        | None -> "" ]
    | None -> "" ]
  in
  let parents =
    match find_field "FAMC" r.rsons with
    [ Some r -> Some (fam_index gen r.rval)
    | None -> None ]
  in
  let occupation =
    match find_all_fields "OCCU" r.rsons with
    [ [r :: rl] -> List.fold_left (fun s r -> s ^ ", " ^ r.rval) r.rval rl
    | [] -> "" ]
  in
  let (notes, titles) =
    if titles_aurejac.val then
      match find_all_fields "NOTE" r.rsons with
      [ [] -> (string_empty.val, [])
      | rl -> treat_indi_notes_titles gen rl ]
    else
      let notes =
        match find_all_fields "NOTE" r.rsons with
        [ [] -> string_empty.val
        | rl -> treat_indi_notes gen rl ]
      in
      (notes, [])
  in
  let titles =
    if titles_aurejac.val then titles
    else
      List.map (treat_indi_title gen public_name)
        (find_all_fields "TITL" r.rsons)
  in
  let family =
    let rl = find_all_fields "FAMS" r.rsons in
    let rvl =
      List.fold_right
        (fun r rvl -> if List.mem r.rval rvl then [r.rval :: rvl] else rvl)
        rl []
    in
    List.map (fun r -> fam_index gen r) rvl
  in
  let (birth, birth_place, birth_src) =
    match find_field "BIRT" r.rsons with
    [ Some r ->
        let d =
          match find_field "DATE" r.rsons with
          [ Some r -> date_of_field r.rpos r.rval
          | _ -> None ]
        in
        let p =
          match find_field "PLAC" r.rsons with
          [ Some r -> r.rval
          | _ -> "" ]
        in
        (d, p, source gen r)
    | None -> (None, "", "") ]
  in
  let (bapt, bapt_place, bapt_src) =
    let ro =
      match find_field "BAPM" r.rsons with
      [ None -> find_field "CHR" r.rsons
      | x -> x ]
    in
    match ro with
    [ Some r ->
        let d =
          match find_field "DATE" r.rsons with
          [ Some r -> date_of_field r.rpos r.rval
          | _ -> None ]
        in
        let p =
          match find_field "PLAC" r.rsons with
          [ Some r -> r.rval
          | _ -> "" ]
        in
        (Adef.codate_of_od d, p, source gen r)
    | None -> (Adef.codate_None, "", "") ]
  in
  let (death, death_place, death_src) =
    match find_field "DEAT" r.rsons with
    [ Some r ->
        if r.rsons = [] then
          if r.rval = "Y" then (DeadDontKnowWhen, "", "")
          else if set_not_dead.val then (NotDead, "", "")
          else (DontKnowIfDead, "", "")
        else
          let d =
            match find_field "DATE" r.rsons with
            [ Some r ->
                match date_of_field r.rpos r.rval with
                [ Some d -> Death Unspecified (Adef.cdate_of_date d)
                | None -> DeadDontKnowWhen ]
            | _ -> DeadDontKnowWhen ]
          in
          let p =
            match find_field "PLAC" r.rsons with
            [ Some r -> r.rval
            | _ -> "" ]
          in
          (d, p, source gen r)
    | None ->
        match birth with
        [ Some d ->
            let age = this_year - annee d in
            if age >= 100 then (DontKnowIfDead, "", "")
            else (NotDead, "", "")
        | _ -> (NotDead, "", "") ] ]
  in
  let (burial, burial_place, burial_src) =
    let (buri, buri_place, buri_src) =
      match find_field "BURI" r.rsons with
      [ Some r ->
          if r.rsons = [] then
            if r.rval = "Y" then (Buried Adef.codate_None, "", "")
            else (UnknownBurial, "", "")
          else
            let d =
              match find_field "DATE" r.rsons with
              [ Some r -> date_of_field r.rpos r.rval
              | _ -> None ]
            in
            let p =
              match find_field "PLAC" r.rsons with
              [ Some r -> r.rval
              | _ -> "" ]
            in
            (Buried (Adef.codate_of_od d), p, source gen r)
      | None -> (UnknownBurial, "", "") ]
    in
    let (crem, crem_place, crem_src) =
      match find_field "CREM" r.rsons with
      [ Some r ->
          if r.rsons = [] then
            if r.rval = "Y" then (Cremated Adef.codate_None, "", "")
            else (UnknownBurial, "", "")
          else
            let d =
              match find_field "DATE" r.rsons with
              [ Some r -> date_of_field r.rpos r.rval
              | _ -> None ]
            in
            let p =
              match find_field "PLAC" r.rsons with
              [ Some r -> r.rval
              | _ -> "" ]
            in
            (Cremated (Adef.codate_of_od d), p, source gen r)
      | None -> (UnknownBurial, "", "") ]
    in
    match (buri, crem) with
    [ (UnknownBurial, Cremated _) -> (crem, crem_place, crem_src)
    | _ -> (buri, buri_place, buri_src) ]
  in
  let birth = Adef.codate_of_od birth in
  let psources = source gen r in
  let empty = add_string gen "" in
  let person =
    {first_name = add_string gen first_name;
     surname = add_string gen surname;
     occ = occ;
     public_name = add_string gen public_name;
     photo = add_string gen photo;
     nick_names = if nick_name <> "" then [add_string gen nick_name] else [];
     aliases = [];
     first_names_aliases =
       if first_name_alias <> "" then [add_string gen first_name_alias]
       else [];
     surnames_aliases =
       if surname_alias <> "" then [add_string gen surname_alias] else [];
     titles = titles; occupation = add_string gen occupation;
     sexe = sex; access = IfTitles;
     birth = birth; birth_place = add_string gen birth_place;
     birth_src = add_string gen birth_src;
     baptism = bapt; baptism_place = add_string gen bapt_place;
     baptism_src = add_string gen bapt_src;
     death = death; death_place = add_string gen death_place;
     death_src = add_string gen death_src;
     burial = burial; burial_place = add_string gen burial_place;
     burial_src = add_string gen burial_src;
     family = Array.of_list family;
     notes = notes;
     psources = add_string gen psources;
     cle_index = i}
  and ascend =
    {parents = parents;
     consang = Adef.fix (-1)}
  in
  do gen.g_per.arr.(Adef.int_of_iper i) := Right person;
     gen.g_asc.arr.(Adef.int_of_iper i) := Right ascend;
  return ()
;

value add_fam gen r =
  let i = fam_index gen r.rval in
  let fath =
    match find_field "HUSB" r.rsons with
    [ Some r -> per_index gen r.rval
    | None -> phony_per gen ]
  in
  let moth =
    match find_field "WIFE" r.rsons with
    [ Some r -> per_index gen r.rval
    | None -> phony_per gen ]
  in
  do match gen.g_per.arr.(Adef.int_of_iper fath) with
     [ Left _ -> ()
     | Right p ->
         do if not (List.memq i (Array.to_list p.family)) then
              p.family := Array.append p.family [| i |]
            else ();
            if p.sexe = Neutre then p.sexe := Masculin else ();
         return () ];
     match gen.g_per.arr.(Adef.int_of_iper moth) with
     [ Left _ -> ()
     | Right p ->
         do if not (List.memq i (Array.to_list p.family)) then
              p.family := Array.append p.family [| i |]
            else ();
            if p.sexe = Neutre then p.sexe := Feminin else ();
         return () ];
  return
  let children =
    let rl = find_all_fields "CHIL" r.rsons in
    List.map (fun r -> per_index gen r.rval) rl
  in
  let (marr, marr_place, marr_src) =
    match find_field "MARR" r.rsons with
    [ Some r ->
        let d =
          match find_field "DATE" r.rsons with
          [ Some r -> date_of_field r.rpos r.rval
          | _ -> None ]
        in
        let p =
          match find_field "PLAC" r.rsons with
          [ Some r -> r.rval
          | _ -> "" ]
        in
        (d, p, source gen r)
    | None -> (None, "", "") ]
  in
  let div =
    match find_field "DIV" r.rsons with
    [ Some r ->
        match find_field "DATE" r.rsons with
        [ Some d -> Divorced (Adef.codate_of_od (date_of_field r.rpos r.rval))
        | _ ->
            match find_field "PLAC" r.rsons with
            [ Some _ -> NotDivorced
            | _ -> Divorced Adef.codate_None ] ]
    | None -> NotDivorced ]
  in
  let comment =
    match find_field "NOTE" r.rsons with
    [ Some r ->
        if r.rval <> "" && r.rval.[0] == '@' then ""
        else r.rval
    | None -> "" ]
  in
  let empty = add_string gen "" in
  let fsources = source gen r in
  let fam =
    {marriage = Adef.codate_of_od marr;
     marriage_place = add_string gen marr_place;
     marriage_src = add_string gen marr_src;
     divorce = div;
     children = Array.of_list children;
     comment = add_string gen comment; origin_file = empty;
     fsources = add_string gen fsources;
     fam_index = i}
  and cpl =
    {father = fath;
     mother = moth}
  in
  do gen.g_fam.arr.(Adef.int_of_ifam i) := Right fam;
     gen.g_cpl.arr.(Adef.int_of_ifam i) := Right cpl;
  return ()
;

value treat_header r =
  match ansel_option.val with
  [ Some v -> ansel_characters.val := v
  | None ->
      match find_field "CHAR" r.rsons with
      [ Some r ->
          match r.rval with
          [ "ANSEL" -> ansel_characters.val := True
          | _ -> ansel_characters.val := False ]
      | None -> () ] ]
;

value make_gen gen r =
(*
do Printf.printf "%s %s\n" r.rlab r.rval; flush stdout; return
*)
  match r.rlab with
  [ "HEAD" ->
      do Printf.eprintf "*** Header ok\n";
         flush stderr;
         treat_header r;
      return ()
  | "INDI" -> add_indi gen r
  | "FAM" -> add_fam gen r
  | "NOTE" -> ()
  | "SOUR" -> ()
  | "TRLR" ->
      do Printf.eprintf "*** Trailer ok\n";
         flush stderr;
      return ()
  | s ->
      do Printf.printf "Not implemented typ = %s\n" s;
         flush stdout;
      return () ]
;

value rec sortable_by_date proj =
  fun
  [ [] -> True
  | [e :: el] ->
      match proj e with
      [ Some d -> sortable_by_date proj el
      | None -> False ] ]
;

value sort_by_date proj list =
  if sortable_by_date proj list then
    Sort.list
      (fun e1 e2 ->
         match (proj e1, proj e2) with
         [ (Some d1, Some d2) -> not (strictement_apres d1 d2)
         | _ -> False ])
      list
  else list
;

(* Printing check errors *)

value print_base_error base =
  fun
  [ AlreadyDefined p ->
      Printf.printf "%s\nis defined several times\n" (denomination base p)
  | OwnAncestor p ->
      Printf.printf "%s\nis his/her own ancestor\n" (denomination base p)
  | BadSexOfMarriedPerson p ->
      Printf.printf "%s\n  bad sex for a married person\n"
        (denomination base p) ]
;

value print_base_warning base =
  fun
  [ BirthAfterDeath p ->
      Printf.printf "%s\n  born after his/her death\n" (denomination base p)
  | ChangedOrderOfChildren fam _ ->
      let cpl = coi base fam.fam_index in
      Printf.printf "Changed order of children of %s and %s\n"
        (denomination base (poi base cpl.father))
        (denomination base (poi base cpl.mother))
  | ChildrenNotInOrder fam elder x ->
      let cpl = coi base fam.fam_index in
      do Printf.printf
           "The following children of\n  %s\nand\n  %s\nare not in order:\n"
           (denomination base (poi base cpl.father))
           (denomination base (poi base cpl.mother));
         Printf.printf "- %s\n" (denomination base elder);
         Printf.printf "- %s\n" (denomination base x);
      return ()
  | DeadTooEarlyToBeFather father child ->
      do Printf.printf "%s\n" (denomination base child);
         Printf.printf
           "  is born more than 2 years after the death of his/her father\n";
         Printf.printf "%s\n" (denomination base father);
      return ()
  | MarriageDateAfterDeath p ->
      do Printf.printf "%s\n" (denomination base p);
         Printf.printf "married after his/her death\n";
      return ()
  | MarriageDateBeforeBirth p ->
      do Printf.printf "%s\n" (denomination base p);
         Printf.printf "married before his/her birth\n";
      return ()
  | MotherDeadAfterChildBirth mother child ->
      Printf.printf "%s\n  is born after the death of his/her mother\n%s\n"
        (denomination base child) (denomination base mother)
  | ParentBornAfterChild parent child ->
      Printf.printf "%s born after his/her child %s\n"
        (denomination base parent) (denomination base child)
  | ParentTooYoung p a ->
      Printf.printf "%s was parent at age of %d\n" (denomination base p)
        (annee a)
  | TitleDatesError p t ->
      do Printf.printf "%s\n" (denomination base p);
         Printf.printf "has incorrect title dates as:\n";
         Printf.printf "  %s %s\n" (sou base t.t_title)
           (sou base t.t_place);
      return ()
  | YoungForMarriage p a ->
      Printf.printf "%s married at age %d\n" (denomination base p) (annee a) ]
;

value find_lev0 =
  parser bp
  [ [: `'0'; _ = skip_space; r1 = get_ident 0; r2 = get_ident 0;
       _ = skip_to_eoln :] ->
      (bp, r1, r2) ]
;

value pass1 gen fname =
  let ic = open_in fname in
  let strm = Stream.of_channel ic in
  do loop () where rec loop () =
       match try Some (find_lev0 strm) with [ Stream.Failure -> None ] with
       [ Some (bp, r1, r2) ->
           do match r2 with
              [ "NOTE" -> Hashtbl.add gen.g_not r1 bp
              | "SOUR" -> Hashtbl.add gen.g_src r1 bp
              | _ -> () ];
           return loop ()
       | None ->
           match strm with parser
           [ [: `_ :] -> do skip_to_eoln strm; return loop ()
           | [: :] -> () ] ];
     close_in ic;
  return ()
;

value pass2 gen fname =
  let ic = open_in fname in
  do line_cnt.val := 0; return
  let strm =
    Stream.from
      (fun i ->
         try
           let c = input_char ic in
           do if c == '\n' then incr line_cnt else (); return
           Some c
         with
         [ End_of_file -> None ])
  in
  do string_empty.val := add_string gen "";
     string_x.val := add_string gen "x";
     loop () where rec loop () =
       match try Some (get_lev0 strm) with [ Stream.Failure -> None ] with
       [ Some r ->
           do make_gen gen r; return loop ()
       | None ->
           match strm with parser
           [ [: `'1'..'9' :] ->
               do let _ : string = get_to_eoln 0 strm in (); return
               loop ()
           | [: `_ :] ->
               do print_location line_cnt.val;
                  Printf.printf "Strange input.\n";
                  flush stdout;
                  let _ : string = get_to_eoln 0 strm in ();
               return loop ()
           | [: :] -> () ] ];
     close_in ic;
     for i = 0 to gen.g_per.tlen - 1 do
       match gen.g_per.arr.(i) with
       [ Right _ -> ()
       | Left lab ->
           let (p, a) = unknown_per gen i in
           do Printf.printf "Warning: undefined person %s\n" lab;
              gen.g_per.arr.(i) := Right p;
              gen.g_asc.arr.(i) := Right a;
           return () ];
     done;
  return ()
;

value make_arrays in_file =
  let fname =
    if Filename.check_suffix in_file ".ged" then in_file
    else in_file ^ ".ged"
  in
  let gen =
    {g_per = {arr = [| |]; tlen = 0};
     g_asc = {arr = [| |]; tlen = 0};
     g_fam = {arr = [| |]; tlen = 0};
     g_cpl = {arr = [| |]; tlen = 0};
     g_str = {arr = [| |]; tlen = 0};
     g_ic = open_in fname;
     g_not = Hashtbl.create 3001;
     g_src = Hashtbl.create 3001;
     g_hper = Hashtbl.create 3001;
     g_hfam = Hashtbl.create 3001;
     g_hstr = Hashtbl.create 3001;
     g_hnam = Hashtbl.create 3001}
  in
  do Printf.eprintf "*** pass 1\n"; flush stderr;
     pass1 gen fname;
     Printf.eprintf "*** pass 2\n"; flush stderr;
     pass2 gen fname;
     close_in gen.g_ic;
  return
  (gen.g_per, gen.g_asc, gen.g_fam, gen.g_cpl, gen.g_str)
;

value make_subarrays (g_per, g_asc, g_fam, g_cpl, g_str) =
  let persons =
    let a = Array.create g_per.tlen (Obj.magic 0) in
    do for i = 0 to g_per.tlen - 1 do
         match g_per.arr.(i) with
         [ Right p -> a.(i) := p
         | Left lab -> failwith ("undefined person " ^ lab) ];
       done;
    return a
  in
  let ascends =
    let a = Array.create g_asc.tlen (Obj.magic 0) in
    do for i = 0 to g_asc.tlen - 1 do
         match g_asc.arr.(i) with
         [ Right p -> a.(i) := p
         | Left lab -> failwith ("undefined person " ^ lab) ];
       done;
    return a
  in
  let families =
    let a = Array.create g_fam.tlen (Obj.magic 0) in
    do for i = 0 to g_fam.tlen - 1 do
         match g_fam.arr.(i) with
         [ Right f -> a.(i) := f
         | Left lab -> failwith ("undefined family " ^ lab) ];
       done;
    return a
  in
  let couples =
    let a = Array.create g_cpl.tlen (Obj.magic 0) in
    do for i = 0 to g_cpl.tlen - 1 do
         match g_cpl.arr.(i) with
         [ Right c -> a.(i) := c
         | Left lab -> failwith ("undefined family " ^ lab) ];
       done;
    return a
  in
  let strings = Array.sub g_str.arr 0 g_str.tlen in
  (persons, ascends, families, couples, strings)
;

value cache_of tab =
  let c = {array = fun _ -> tab; get = fun []; len = Array.length tab} in
  do c.get := fun i -> (c.array ()).(i); return c
;

value make_base (persons, ascends, families, couples, strings) =
  {persons = cache_of persons;
   ascends = cache_of ascends;
   families = cache_of families;
   couples = cache_of couples;
   strings = cache_of strings;
   has_family_patches = False;
   persons_of_name = fun [];
   strings_of_fsname = fun [];
   index_of_string = fun [];
   persons_of_surname = {find = fun []; cursor = fun []; next = fun []};
   persons_of_first_name = {find = fun []; cursor = fun []; next = fun []};
   patch_person = fun []; patch_ascend = fun [];
   patch_family = fun []; patch_couple = fun [];
   patch_string = fun []; patch_name = fun []; commit_patches = fun [];
   cleanup = fun () -> ()}
;

value array_memq x a =
  loop 0 where rec loop i =
    if i == Array.length a then False
    else if x == a.(i) then True
    else loop (i + 1)
;

value check_parents_children base =
  let to_delete = ref [] in
  do for i = 0 to base.persons.len - 1 do
       let a = base.ascends.get i in
       match a.parents with
       [ Some ifam ->
           let fam = foi base ifam in
           let cpl = coi base ifam in
           if array_memq (Adef.iper_of_int i) fam.children then ()
           else
             let p = base.persons.get i in
             do Printf.printf "%s is not the child of his/her parents\n"
                  (denomination base p);
                Printf.printf "- %s\n"
                  (denomination base (poi base cpl.father));
                Printf.printf "- %s\n"
                  (denomination base (poi base cpl.mother));
                Printf.printf "=> no more parents for him/her\n";
                Printf.printf "\n";
                flush stdout;
                a.parents := None;
             return ()
       | None -> () ];
     done;
     for i = 0 to base.families.len - 1 do
       to_delete.val := [];
       let fam = base.families.get i in
       let cpl = base.couples.get i in
       do for j = 0 to Array.length fam.children - 1 do
            let a = aoi base fam.children.(j) in
            let p = poi base fam.children.(j) in
            match a.parents with
            [ Some ifam ->
                if Adef.int_of_ifam ifam <> i then
                  do Printf.printf "Other parents for %s\n"
                       (denomination base p);
                     Printf.printf "- %s\n"
                       (denomination base (poi base cpl.father));
                     Printf.printf "- %s\n"
                       (denomination base (poi base cpl.mother));
                     Printf.printf "=> deleted in this family\n";
                     Printf.printf "\n";
                     flush stdout;
                     to_delete.val := [p.cle_index :: to_delete.val];
                  return ()
                else ()
            | None ->
                do Printf.printf "%s has no parents but is the child of\n"
                     (denomination base p);
                   Printf.printf "- %s\n"
                     (denomination base (poi base cpl.father));
                   Printf.printf "- %s\n"
                     (denomination base (poi base cpl.mother));
                   Printf.printf "=> added parents\n";
                   Printf.printf "\n";
                   flush stdout;
                   a.parents := Some fam.fam_index;
                return () ];
          done;
          if to_delete.val <> [] then
            let l =
              List.fold_right
                (fun ip l ->
                   if List.memq ip to_delete.val then l else [ip :: l])
                (Array.to_list fam.children) []
            in
            fam.children := Array.of_list l
          else ();
       return ();
     done;
  return ()
;

value kill_family base fam ip =
  let p = poi base ip in
  let l =
    List.fold_right
      (fun ifam ifaml ->
         if ifam == fam.fam_index then ifaml else [ifam :: ifaml])
      (Array.to_list p.family) []
  in
  p.family := Array.of_list l
;

value kill_parents base ip =
  let a = aoi base ip in
  a.parents := None
;

value effective_del_fam base fam cpl =
  let ifam = fam.fam_index in
  do kill_family base fam cpl.father;
     kill_family base fam cpl.mother;
     Array.iter (kill_parents base) fam.children;
     cpl.father := Adef.iper_of_int (-1);
     cpl.mother := Adef.iper_of_int (-1);
     fam.children := [| |];
     fam.fam_index := Adef.ifam_of_int (-1);
  return ()
;

value check_parents_sex base =
  for i = 0 to base.couples.len - 1 do
    let cpl = base.couples.get i in
    let fath = poi base cpl.father in
    let moth = poi base cpl.mother in
    if fath.sexe <> Masculin || moth.sexe <> Feminin then
      do Printf.printf "Bad sex for parents\n";
         Printf.printf "- father: %s\n" (denomination base fath);
         Printf.printf "- mother: %s\n" (denomination base moth);
         Printf.printf "=> family deleted\n\n";
         flush stdout;
         effective_del_fam base (base.families.get i) cpl;
      return ()
    else ();
  done
;

value neg_year =
  fun
  [ {day = d; month = m; year = y; prec = OrYear y2} ->
      {day = d; month = m; year = - abs y; prec = OrYear (- abs y2)}
  | {day = d; month = m; year = y; prec = YearInt y2} ->
      {day = d; month = m; year = - abs y; prec = YearInt (- abs y2)}
  | {day = d; month = m; year = y; prec = p} ->
      {day = d; month = m; year = - abs y; prec = p} ]
;

value neg_year_cdate cd =
  Adef.cdate_of_date (neg_year (Adef.date_of_cdate cd))
;

value rec negative_date_ancestors base p =
  do match Adef.od_of_codate p.birth with
     [ Some d1 -> p.birth := Adef.codate_of_od (Some (neg_year d1))
     | _ -> () ];
     match p.death with
     [ Death dr cd2 -> p.death := Death dr (neg_year_cdate cd2)
     | _ -> () ];
     for i = 0 to Array.length p.family - 1 do
       let fam = foi base p.family.(i) in
       match Adef.od_of_codate fam.marriage with
       [ Some d ->
           fam.marriage := Adef.codate_of_od (Some (neg_year d))
       | None -> () ];
     done;
  return
  let a = aoi base p.cle_index in
  match a.parents with
  [ Some ifam ->
      let cpl = coi base ifam in
      do negative_date_ancestors base (poi base cpl.father);
         negative_date_ancestors base (poi base cpl.mother);
      return ()
  | _ -> () ]
;

value negative_dates base =
  for i = 0 to base.persons.len - 1 do
    let p = base.persons.get i in
    match (Adef.od_of_codate p.birth, p.death) with
    [ (Some d1, Death dr cd2) ->
        let d2 = Adef.date_of_cdate cd2 in
        if annee d1 > 0 && annee d2 > 0 && strictement_avant d2 d1 then
          negative_date_ancestors base (base.persons.get i)
        else ()
    | _ -> () ];
  done
;

value finish_base base =
  let persons = base.persons.array () in
  let ascends = base.ascends.array () in
  let families = base.families.array () in
  let strings = base.strings.array () in
  do for i = 0 to Array.length families - 1 do
       let fam = families.(i) in
       let children =
         sort_by_date
           (fun ip -> Adef.od_of_codate (persons.(Adef.int_of_iper ip).birth))
           (Array.to_list fam.children)
       in
       fam.children := Array.of_list children;  
     done;
     for i = 0 to Array.length persons - 1 do
       let p = persons.(i) in
       let family =
         sort_by_date
           (fun ifam ->
              Adef.od_of_codate
                families.(Adef.int_of_ifam ifam).marriage)
           (Array.to_list p.family)
       in
       p.family := Array.of_list family;
     done;
     for i = 0 to Array.length persons - 1 do
       let p = persons.(i) in
       let a = ascends.(i) in
       if a.parents <> None && Array.length p.family != 0
       || p.notes <> string_empty.val then
         do if sou base p.first_name = "?" then
              do p.first_name := string_x.val;
                 p.occ := i;
              return ()
            else ();
            if sou base p.surname = "?" then
              do p.surname := string_x.val;
                 p.occ := i;
              return ()
            else ();
         return ()
       else ();
     done;
     check_parents_sex base;
     check_parents_children base;
     if try_negative_dates.val then negative_dates base else ();
     check_base base
       (fun x -> do print_base_error base x; return Printf.printf "\n")
       (fun x -> do print_base_warning base x; return Printf.printf "\n");
     flush stdout;
  return ()
;

value output_command_line bname =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  let oc = open_out (Filename.concat bdir "command.txt") in
  do Printf.fprintf oc "%s" Sys.argv.(0);
     for i = 1 to Array.length Sys.argv - 1 do
       Printf.fprintf oc " %s" Sys.argv.(i);
     done;
     Printf.fprintf oc "\n";
     close_out oc;
  return ()
;

(* Main *)

value out_file = ref "a";
value speclist =
  [("-o", Arg.String (fun s -> out_file.val := s), "<file>
       Output data base (defaut: \"a\").");
   ("-lf", Arg.Set lowercase_first_names, "   - Lowercase first names -
       Force lowercase first names keeping only their initials as uppercase
       characters.");
   ("-ls", Arg.Set lowercase_surnames, "   - Lowercase surnames -
       Force lowercase surnames keeping only their initials as uppercase
       characters. Try to keep lowercase particles.");
   ("-efn", Arg.Set extract_first_names, "  - Extract first names - [default] -
       When creating a person, if the GEDCOM first name part holds several
       names, the first of this names becomes the person \"first name\" and
       the complete GEDCOM first name part a \"first name alias\".");
   ("-no_efn", Arg.Clear extract_first_names, "
       Cancels the previous option.");
   ("-epn", Arg.Set extract_public_names, "  - Extract public names - [default]
       When creating a person, if the GEDCOM first name part looks like a
       public name, i.e. holds:
       * a number or a roman number, supposed to be a number of a
         nobility title,
       * one of the words: \"der\", \"die\", \"el\", \"le\", \"la\", \"the\",
         supposed to be the beginning of a qualifier,
       then the GEDCOM first name part becomes the person \"public name\"
       and its first word his \"first name\".");
   ("-no_epn", Arg.Clear extract_public_names, "
       Cancels the previous option.");
   ("-tnd", Arg.Set try_negative_dates, "  - Try negative dates -
       Set negative dates when inconsistency (e.g. birth after death)");
   ("-no_nd", Arg.Set no_negative_dates, "  - No negative dates -
       Don't interpret a year preceded by a minus sign as a negative year");
   ("-ansel", Arg.Unit (fun () -> ansel_option.val := Some True),
       "  - ANSEL encoding -
       Force ANSEL encoding, overriding the possible setting in GEDCOM.");
   ("-no_ansel", Arg.Unit (fun () -> ansel_option.val := Some False),
       "  - No ANSEL encoding -
       No ANSEL encoding, overriding the possible setting in GEDCOM.");
   ("-nd", Arg.Set set_not_dead, "
       Set \"not dead\" instead of \"don't know\" when existing but
       empty fields DEAT");
   ("-ta", Arg.Set titles_aurejac, "
       [This option is ad hoc; please do not use it]")]
;

value errmsg = "Usage: ged2gwb [<ged>] [options] where options are:";

value main () =
  do Argl.parse speclist (fun s -> in_file.val := s) errmsg; return
(*
  let r = Gc.get () in
  do r.Gc.max_overhead := 10; Gc.set r; return
*)
  let arrays = make_arrays in_file.val in
  do Gc.compact (); return
  let arrays = make_subarrays arrays in
  let base = make_base arrays in
  do finish_base base;
     Iobase.output out_file.val base;
     output_command_line out_file.val;
  return ()
;

Printexc.catch main ();
