(* camlp4r pa_extend.cmo *)
(* $Id: ged2gwb.ml,v 3.13 2000-03-31 15:13:26 ddr Exp $ *)
(* Copyright (c) INRIA *)

open Def;
open Gutil;

value log_oc = ref stdout;

type record =
  { rlab : string;
    rval : string;
    rcont : string;
    rsons : list record;
    rpos : int }
;

type choice 'a 'b = [ Left of 'a | Right of 'b ];
type choice3 'a 'b 'c 'd = [ Left3 of 'a | Right3 of 'b and 'c and 'd ];
type month_number_dates =
  [ MonthDayDates | DayMonthDates | NoMonthNumberDates
  | MonthNumberHappened of string ]
;

type charset = [ Ansel | Ascii | Msdos ];

value lowercase_first_names = ref False;
value lowercase_surnames = ref False;
value extract_first_names = ref False;
value extract_public_names = ref True;
value charset_option = ref None;
value charset = ref Ascii;
value try_negative_dates = ref False;
value no_negative_dates = ref False;
value month_number_dates = ref NoMonthNumberDates;
value no_public_if_titles = ref False;
value first_names_brackets = ref None;
value untreated_in_notes = ref False;
value force = ref False;
value default_source = ref "";

(* Reading input *)

value line_cnt = ref 1;
value in_file = ref "";

value print_location pos =
  Printf.fprintf log_oc.val "File \"%s\", line %d:\n" in_file.val pos
;

value rec skip_eol =
  parser [ [: `'\n' | '\r'; _ = skip_eol :] -> () | [: :] -> () ]
;

value rec get_to_eoln len =
  parser
  [ [: `'\n' | '\r'; _ = skip_eol :] -> Buff.get len
  | [: `c; s :] -> get_to_eoln (Buff.store len c) s
  | [: :] -> Buff.get len ]
;

value rec skip_to_eoln =
  parser
  [ [: `'\n' | '\r'; _ = skip_eol :] -> ()
  | [: `_; s :] -> skip_to_eoln s
  | [: :] -> () ]
;

value rec get_ident len =
  parser
  [ [: `' ' :] -> Buff.get len
  | [: `c when not (List.mem c ['\n'; '\r']); s :] ->
      get_ident (Buff.store len c) s
  | [: :] -> Buff.get len ]
;

value skip_space = parser [ [: `' ' :] -> () | [: :] -> () ];

value rec line_start num =
  parser [ [: `' '; s :] -> line_start num s | [: `x when x = num :] -> () ]
;

value ascii_of_msdos s =
  let need_copy =
    loop 0 where rec loop i =
      if i == String.length s then False
      else
        match Char.code s.[i] with
        [ 0o200 | 0o201 | 0o202 | 0o203 | 0o204 | 0o205 | 0o206 | 0o207
        | 0o210 | 0o211 | 0o212 | 0o213 | 0o214 | 0o215 | 0o216 | 0o217
        | 0o220 | 0o221 | 0o222 | 0o223 | 0o224 | 0o225 | 0o226 | 0o227
        | 0o230 | 0o231 | 0o232 | 0o233 | 0o234 | 0o235 | 0o240 | 0o241
        | 0o242 | 0o243 | 0o244 | 0o245 | 0o246 | 0o247 | 0o250 | 0o252
        | 0o253 | 0o254 | 0o255 | 0o256 | 0o257 | 0o346 | 0o361 | 0o366
        | 0o370 | 0o372 | 0o375 -> True
        | _ -> loop (i + 1) ]
  in
  if need_copy then
    let s' = String.create (String.length s) in
    do for i = 0 to String.length s - 1 do
         let cc =
           match Char.code s.[i] with
           [ 0o200 -> 0o307 | 0o201 -> 0o374 | 0o202 -> 0o351 | 0o203 -> 0o342
           | 0o204 -> 0o344 | 0o205 -> 0o340 | 0o206 -> 0o345 | 0o207 -> 0o347
           | 0o210 -> 0o352 | 0o211 -> 0o353 | 0o212 -> 0o350 | 0o213 -> 0o357
           | 0o214 -> 0o356 | 0o215 -> 0o354 | 0o216 -> 0o304 | 0o217 -> 0o305
           | 0o220 -> 0o311 | 0o221 -> 0o346 | 0o222 -> 0o306 | 0o223 -> 0o364
           | 0o224 -> 0o366 | 0o225 -> 0o362 | 0o226 -> 0o373 | 0o227 -> 0o371
           | 0o230 -> 0o377 | 0o231 -> 0o326 | 0o232 -> 0o334 | 0o233 -> 0o242
           | 0o234 -> 0o243 | 0o235 -> 0o245 | 0o240 -> 0o341 | 0o241 -> 0o355
           | 0o242 -> 0o363 | 0o243 -> 0o372 | 0o244 -> 0o361 | 0o245 -> 0o321
           | 0o246 -> 0o252 | 0o247 -> 0o272 | 0o250 -> 0o277 | 0o252 -> 0o254
           | 0o253 -> 0o275 | 0o254 -> 0o274 | 0o255 -> 0o241 | 0o256 -> 0o253
           | 0o257 -> 0o273 | 0o346 -> 0o265 | 0o361 -> 0o261 | 0o366 -> 0o367
           | 0o370 -> 0o260 | 0o372 -> 0o267 | 0o375 -> 0o262
           | c -> c ]
         in
         s'.[i] := Char.chr cc;
       done;
    return s'
  else s
;

value ascii_of_string s =
  match charset.val with
  [ Ansel -> Ansel.to_iso_8859_1 s
  | Ascii -> s
  | Msdos -> ascii_of_msdos s ]
;

value rec get_lev n =
  parser
    [: _ = line_start n; _ = skip_space; r1 = get_ident 0; strm :] ->
      let (rlab, rval, rcont, l) =
        if String.length r1 > 0 && r1.[0] = '@' then parse_address n r1 strm
        else parse_text n r1 strm
      in
      {rlab = rlab;
       rval = ascii_of_string rval;
       rcont = ascii_of_string rcont;
       rsons = List.rev l; rpos = line_cnt.val}
and parse_address n r1 =
  parser
  [ [: r2 = get_ident 0; r3 = get_to_eoln 0 ? "get to eoln";
       l = get_lev_list [] (Char.chr (Char.code n + 1)) ? "get lev list" :] ->
      (r2, r1, r3, l) ]
and parse_text n r1 =
  parser
  [ [: r2 = get_to_eoln 0;
       l = get_lev_list [] (Char.chr (Char.code n + 1)) ? "get lev list" :] ->
      (r1, r2, "", l) ]
and get_lev_list l n =
  parser [ [: x = get_lev n; s :] -> get_lev_list [x :: l] n s | [: :] -> l ]
;

(* Error *)

value bad_dates_warned = ref False;

value print_bad_date pos d =
  if bad_dates_warned.val then ()
  else
    do bad_dates_warned.val := True;
       print_location pos;
       Printf.fprintf log_oc.val "Can't decode date %s\n" d;
       flush log_oc.val;
    return ()
;

value check_month m =
  if m < 1 || m > 12 then
    do Printf.fprintf log_oc.val "Bad (numbered) month in date: %d\n" m;
       flush log_oc.val;
    return ()
  else ()
;

value warning_month_number_dates () =
  match month_number_dates.val with
  [ MonthNumberHappened s ->
      do flush log_oc.val;
         Printf.eprintf
           "
  Warning: the file holds dates with numbered months (like: 12/05/1912).

  GEDCOM standard *requires* that months in dates be identifiers. The
  correct form for this example would be 12 MAY 1912 or 5 DEC 1912.

  Consider restarting with option \"-dates_dm\" or \"-dates_md\".
  Use option -help to see what they do.

  (example found in gedcom: \"%s\")
" s;
         flush stderr;
      return ()
  | _ -> () ]
;

(* Decoding fields *)

value rec skip_spaces =
  parser [ [: `' '; s :] -> skip_spaces s | [: :] -> () ]
;

value rec ident_slash len =
  parser
  [ [: `'/' :] -> Buff.get len
  | [: `c; a = ident_slash (Buff.store len c) :] -> a
  | [: :] -> Buff.get len ]
;

value strip c str =
  let start =
    loop 0 where rec loop i =
      if i == String.length str then i
      else if str.[i] == c then loop (i + 1)
      else i
  in
  let stop =
    loop (String.length str - 1) where rec loop i =
      if i == -1 then i + 1 else if str.[i] == c then loop (i - 1) else i + 1
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
    [: _ = skip_spaces;
       invert = parser [ [: `'/' :] -> True | [: :] -> False ];
       f = ident_slash 0; _ = skip_spaces; s = ident_slash 0 :] ->
      let (f, s) = if invert then (s, f) else (f, s) in
      let f = strip_spaces f in
      let s = strip_spaces s in
      (if f = "" then "x" else f, if s = "" then "?" else s)
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
  [ [: `('0'..'9' as c); n = number (Buff.store 0 c) :] -> ("INT", n)
  | [: `('A'..'Z' as c); i = ident (Buff.store 0 c) :] -> ("ID", i)
  | [: `'.' :] -> ("", ".")
  | [: `' ' | '\r'; s :] -> lexing s
  | [: _ = Stream.empty :] -> ("EOI", "")
  | [: `x :] -> ("", String.make 1 x) ]
and number len =
  parser
  [ [: `('0'..'9' as c); a = number (Buff.store len c) :] -> a
  | [: :] -> Buff.get len ]
and ident len =
  parser
  [ [: `('A'..'Z' as c); a = ident (Buff.store len c) :] -> a
  | [: :] -> Buff.get len ]
;

value make_lexing s = Stream.from (fun _ -> Some (lexing s));

value tparse (p_con, p_prm) =
  ifdef CAMLP4_300 then None
  else
    if p_prm = "" then parser [: `(con, prm) when con = p_con :] -> prm
    else parser [: `(con, prm) when con = p_con && prm = p_prm :] -> prm
;

value using_token (p_con, p_prm) =
  match p_con with
  [ "" | "INT" | "ID" | "EOI" -> ()
  | _ ->
      raise
        (Token.Error
           ("the constructor \"" ^ p_con ^
              "\" is not recognized by the lexer")) ]
;

value lexer =
  {Token.func = fun s -> (make_lexing s, fun _ -> (0, 0));
   Token.using = using_token; Token.removing = fun _ -> ();
   Token.tparse = tparse; Token.text = fun _ -> "<tok>"}
;

type range 'a =
  [ Begin of 'a
  | End of 'a
  | BeginEnd of 'a and 'a ]
; 

value g = Grammar.create lexer;
value date_value = Grammar.Entry.create g "date value";
value date_interval = Grammar.Entry.create g "date interval";

value roman_int_decode s =
  let decode_digit one five ten r =
    loop 0 where rec loop cnt i =
      if i >= String.length s then (10 * r + cnt, i)
      else if s.[i] = one then
        loop (cnt + 1) (i + 1)
      else if s.[i] = five then
        if cnt = 0 then loop 5 (i + 1)
        else (10 * r + 5 - cnt, i + 1)
      else if s.[i] = ten then
        (10 * r + 10 - cnt, i + 1)
      else (10 * r + cnt, i)
  in
  let (r, i) = decode_digit 'M' 'M' 'M' 0 0 in
  let (r, i) = decode_digit 'C' 'D' 'M' r i in
  let (r, i) = decode_digit 'X' 'L' 'C' r i in
  let (r, i) = decode_digit 'I' 'V' 'X' r i in
  if i = String.length s then r else raise Not_found
;

value is_roman_int x =
  try let _ = roman_int_decode x in True with [ Not_found -> False ]
;

value roman_int =
  let p =
    parser
    [ [: `("ID", x) when is_roman_int x :] -> roman_int_decode x ]
  in
  Grammar.Entry.of_parser g "roman int" p
;

value date_str = ref "";

value make_date n1 n2 n3 =
  let n3 =
    if no_negative_dates.val then
      match n3 with
      [ Some n3 -> Some (abs n3)
      | None -> None ]
    else n3
  in
  match (n1, n2, n3) with
  [ (Some d, Some m, Some y) ->
      let (d, m) =
        match m with
        [ Right m -> (d, m)
        | Left m ->
            match month_number_dates.val with
            [ DayMonthDates -> do check_month m; return (d, m)
            | MonthDayDates -> do check_month d; return (m, d)
            | _ ->
                if d >= 1 && m >= 1 && d <= 31 && m <= 31 then
                  if d > 13 && m <= 13 then (d, m)
                  else if m > 13 && d <= 13 then (m, d)
                  else if d > 13 && m > 13 then (0, 0)
                  else
                    do month_number_dates.val :=
                         MonthNumberHappened date_str.val;
                    return (0, 0)
                else (0, 0) ] ]
      in
      let (d, m) = if m < 1 || m > 13 then (0, 0) else (d, m) in
      {day = d; month = m; year = y; prec = Sure; delta = 0}
  | (None, Some m, Some y) ->
      let m =
        match m with
        [ Right m -> m
        | Left m -> m ]
      in
      {day = 0; month = m; year = y; prec = Sure; delta = 0}
  | (None, None, Some y) ->
      {day = 0; month = 0; year = y; prec = Sure; delta = 0}
  | (Some y, None, None) ->
      {day = 0; month = 0; year = y; prec = Sure; delta = 0}
  | _ -> raise (Stream.Error "bad date") ]
;

EXTEND
  GLOBAL: date_value date_interval;
  date_value:
    [ [ dr = date_range; EOI ->
          match dr with
          [ Begin (d, cal) -> Dgreg {(d) with prec = After} cal
          | End (d, cal) -> Dgreg {(d) with prec = Before} cal
          | BeginEnd (d1, cal) (d2, _) ->
              Dgreg {(d1) with prec = YearInt d2.year} cal ]
      | (d, cal) = date; EOI -> Dgreg d cal ] ]
  ;
  date_interval:
    [ [ dr = date_range; EOI -> dr
      | (d, cal) = date; EOI -> Begin (d, cal) ] ]
  ;
  date_range:
    [ [ ID "BEF"; dt = date -> End dt
      | ID "AFT"; dt = date -> Begin dt
      | ID "BET"; dt = date; ID "AND"; dt1 = date -> BeginEnd dt dt1
      | ID "TO"; dt = date -> End dt
      | ID "FROM"; dt = date -> Begin dt
      | ID "FROM"; dt = date; ID "TO"; dt1 = date -> BeginEnd dt dt1 ] ]
  ;
  date:
    [ [ ID "ABT" ; (d, cal) = date_calendar -> ({(d) with prec = About}, cal)
      | ID "ENV"; (d, cal) = date_calendar -> ({(d) with prec = About}, cal)
      | ID "EST"; (d, cal) = date_calendar -> ({(d) with prec = Maybe}, cal)
      | ID "AFT"; (d, cal) = date_calendar -> ({(d) with prec = Before}, cal)
      | ID "BEF"; (d, cal) = date_calendar -> ({(d) with prec = After}, cal)
      | (d, cal) = date_calendar -> (d, cal) ] ]
  ;
  date_calendar:
    [ [ "@"; "#"; ID "DGREGORIAN"; "@"; d = date_greg -> (d, Dgregorian)
      | "@"; "#"; ID "DJULIAN"; "@"; d = date_greg ->
          (Calendar.gregorian_of_julian d, Djulian)
      | "@"; "#"; ID "DFRENCH"; ID "R"; "@"; d = date_fren ->
          (Calendar.gregorian_of_french d, Dfrench)
      | "@"; "#"; ID "DHEBREW"; "@"; d = date_hebr ->
          (Calendar.gregorian_of_hebrew d, Dhebrew)
      | d = date_greg -> (d, Dgregorian) ] ]
  ;
  date_greg:
    [ [ LIST0 "."; n1 = OPT int; LIST0 [ "." -> () | "/" -> () ];
        n2 = OPT gen_month; LIST0 [ "." -> () | "/" -> () ]; n3 = OPT int;
        LIST0 "." ->
          make_date n1 n2 n3 ] ]
  ;
  date_fren:
    [ [ LIST0 "."; n1 = int; (n2, n3) = date_fren_kont ->
          make_date (Some n1) n2 n3
      | LIST0 "."; n1 = year_fren ->
          make_date (Some n1) None None
      | LIST0 "."; (n2, n3) = date_fren_kont ->
          make_date None n2 n3 ] ]
  ;
  date_fren_kont:
    [ [ LIST0 [ "." -> () | "/" -> () ];
        n2 = OPT gen_french; LIST0 [ "." -> () | "/" -> () ];
        n3 = OPT year_fren; LIST0 "." ->
          (n2, n3) ] ]
  ;
  date_hebr:
    [ [ LIST0 "."; n1 = OPT int; LIST0 [ "." -> () | "/" -> () ];
        n2 = OPT gen_hebr; LIST0 [ "." -> () | "/" -> () ]; n3 = OPT int;
        LIST0 "." ->
          make_date n1 n2 n3 ] ]
  ;
  gen_month:
    [ [ i = int -> Left (abs i)
      | m = month -> Right m ] ]
  ;
  month:
    [ [ ID "JAN" -> 1
      | ID "FEB" -> 2
      | ID "MAR" -> 3
      | ID "APR" -> 4
      | ID "MAY" -> 5
      | ID "JUN" -> 6
      | ID "JUL" -> 7
      | ID "AUG" -> 8
      | ID "SEP" -> 9
      | ID "OCT" -> 10
      | ID "NOV" -> 11
      | ID "DEC" -> 12 ] ]
  ;
  gen_french:
    [ [ m = french -> Right m ] ]
  ;
  french:
    [ [ ID "VEND" -> 1
      | ID "BRUM" -> 2
      | ID "FRIM" -> 3
      | ID "NIVO" -> 4
      | ID "PLUV" -> 5
      | ID "VENT" -> 6
      | ID "GERM" -> 7
      | ID "FLOR" -> 8
      | ID "PRAI" -> 9
      | ID "MESS" -> 10
      | ID "THER" -> 11
      | ID "FRUC" -> 12
      | ID "COMP" -> 13 ] ]
  ;
  year_fren:
    [ [ i = int -> i
      | ID "AN"; i = roman_int -> i
      | i = roman_int -> i ] ]
  ;
  gen_hebr:
    [ [ m = hebr -> Right m ] ]
  ;
  hebr:
    [ [ ID "TSH" -> 1
      | ID "CSH" -> 2
      | ID "KSL" -> 3
      | ID "TVT" -> 4
      | ID "SHV" -> 5
      | ID "ADR" -> 6
      | ID "ADS" -> 7
      | ID "NSN" -> 8
      | ID "IYR" -> 9
      | ID "SVN" -> 10
      | ID "TMZ" -> 11
      | ID "AAV" -> 12
      | ID "ELL" -> 13 ] ]
  ;
  int:
    [ [ i = INT -> int_of_string i | "-"; i = INT -> - int_of_string i ] ]
  ;
END;

value date_of_field pos d =
  if d = "" then None
  else
    let s = Stream.of_string (String.uppercase d) in
    do date_str.val := d; return
    try Some (Grammar.Entry.parse date_value s) with
    [ Stdpp.Exc_located loc (Stream.Error _) -> 
        let d =
          if d.[0] = '(' && d.[String.length d - 1] = ')' then
            String.sub d 1 (String.length d - 2)
          else d
        in
        Some (Dtext d) ]
;

(* Creating base *)

type tab 'a = { arr : mutable array 'a; tlen : mutable int };

type gen =
  { g_per : tab (choice3 string person ascend union);
    g_fam : tab (choice3 string family couple descend);
    g_str : tab string;
    g_bnot : mutable string;
    g_ic : in_channel;
    g_not : Hashtbl.t string int;
    g_src : Hashtbl.t string int;
    g_hper : Hashtbl.t string Adef.iper;
    g_hfam : Hashtbl.t string Adef.ifam;
    g_hstr : Hashtbl.t string Adef.istr;
    g_hnam : Hashtbl.t string (ref int);
    g_adop : Hashtbl.t string (Adef.iper * string);
    g_godp : mutable list (Adef.iper * Adef.iper) }
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
      return Adef.istr_of_int i ]
;        

value extract_addr addr =
  if String.length addr > 0 && addr.[0] = '@' then
    try
      let r = String.index_from addr 1 '@' in
      String.sub addr 0 (r + 1)
    with
    [ Not_found -> addr ]
  else addr
;

value per_index gen lab =
  let lab = extract_addr lab in
  try Hashtbl.find gen.g_hper lab with
  [ Not_found ->
      let i = gen.g_per.tlen in
      do assume_tab "gen.g_per" gen.g_per (Left3 "");
         gen.g_per.arr.(i) := Left3 lab;
         gen.g_per.tlen := gen.g_per.tlen + 1;
         Hashtbl.add gen.g_hper lab (Adef.iper_of_int i);
      return Adef.iper_of_int i ]
;

value fam_index gen lab =
  let lab = extract_addr lab in
  try Hashtbl.find gen.g_hfam lab with
  [ Not_found ->
      let i = gen.g_fam.tlen in
      do assume_tab "gen.g_fam" gen.g_fam (Left3 "");
         gen.g_fam.arr.(i) := Left3 lab;
         gen.g_fam.tlen := gen.g_fam.tlen + 1;
         Hashtbl.add gen.g_hfam lab (Adef.ifam_of_int i);
      return Adef.ifam_of_int i ]
;

value unknown_per gen i =
  let empty = add_string gen "" in
  let what = add_string gen "?" in
  let p =
    {first_name = what; surname = what; occ = i; public_name = empty;
     image = empty; nick_names = []; aliases = []; first_names_aliases = [];
     surnames_aliases = []; titles = []; rparents = []; related = [];
     occupation = empty; sex = Neuter; access = IfTitles;
     birth = Adef.codate_None; birth_place = empty; birth_src = empty;
     baptism = Adef.codate_None; baptism_place = empty; baptism_src = empty;
     death = DontKnowIfDead; death_place = empty; death_src = empty;
     burial = UnknownBurial; burial_place = empty; burial_src = empty;
     notes = empty; psources = empty; cle_index = Adef.iper_of_int i}
  and a = {parents = None; consang = Adef.fix (-1)}
  and u = {family = [| |]} in
  (p, a, u)
;

value phony_per gen sex =
  let i = gen.g_per.tlen in
  let (person, ascend, union) = unknown_per gen i in
  do person.sex := sex;
     assume_tab "gen.g_per" gen.g_per (Left3 "");
     gen.g_per.tlen := gen.g_per.tlen + 1;
     gen.g_per.arr.(i) := Right3 person ascend union;
  return Adef.iper_of_int i
;

value unknown_fam gen i =
  let empty = add_string gen "" in
  let father = phony_per gen Male in
  let mother = phony_per gen Female in
  let f =
    {marriage = Adef.codate_None; marriage_place = empty;
     marriage_src = empty; witnesses = [| |]; not_married = False;
     divorce = NotDivorced;
     comment = empty; origin_file = empty; fsources = empty;
     fam_index = Adef.ifam_of_int i}
  and c = {father = father; mother = mother}
  and d = {children = [| |]} in
  (f, c, d)
;

value this_year =
  let tm = Unix.localtime (Unix.time ()) in
  tm.Unix.tm_year + 1900
;

value infer_death birth =
  match birth with
  [ Some (Dgreg d _) ->
      let a = this_year - d.year in
      if a > 120 then DeadDontKnowWhen
      else if a <= 80 then NotDead
      else DontKnowIfDead
  | _ -> DontKnowIfDead ]
;

value make_title gen (title, place) =
  {t_name = Tnone; t_ident = add_string gen title;
   t_place = add_string gen place; t_date_start = Adef.codate_None;
   t_date_end = Adef.codate_None; t_nth = 0}
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
  let s = String.copy s in
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
        | 'A'..'Z' | 'À'..'Ý' ->
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

value is_a_name_char =
  fun
  [ 'A'..'Z' | 'a'..'z' | 'À'..'Ö' | 'Ø'..'ö' | 'ø'..'ÿ' | '0'..'9' | '-' |
    ''' ->
      True
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
  ["Ier"; "Ière"; "der"; "den"; "die"; "el"; "le"; "la"; "the"]
;

value rec is_a_public_name s i =
  let i = next_word_pos s i in
  if i == String.length s then False
  else
    let j = next_sep_pos s i in
    if j > i then
      let w = String.sub s i (j - i) in
      if look_like_a_number w then True
      else if is_roman_int w then True
      else if List.mem w public_name_word then True
      else is_a_public_name s j
    else False
;

value get_lev0 =
  parser
    [: _ = line_start '0'; _ = skip_space; r1 = get_ident 0; r2 = get_ident 0;
       r3 = get_to_eoln 0 ? "get to eoln";
       l = get_lev_list [] '1' ? "get lev list" :] ->
      let (rlab, rval) = if r2 = "" then (r1, "") else (r2, r1) in
      let rval = ascii_of_string rval in
      let rcont = ascii_of_string r3 in
      {rlab = rlab; rval = rval; rcont = rcont; rsons = List.rev l;
       rpos = line_cnt.val}
;

value find_notes_record gen addr =
  match try Some (Hashtbl.find gen.g_not addr) with [ Not_found -> None ] with
  [ Some i ->
      do seek_in gen.g_ic i; return
      try Some (get_lev0 (Stream.of_channel gen.g_ic)) with
      [ Stream.Failure | Stream.Error _ -> None ]
  | None -> None ]
;

value find_sources_record gen addr =
  match try Some (Hashtbl.find gen.g_src addr) with [ Not_found -> None ] with
  [ Some i ->
      do seek_in gen.g_ic i; return
      try Some (get_lev0 (Stream.of_channel gen.g_ic)) with
      [ Stream.Failure | Stream.Error _ -> None ]
  | None -> None ]
;

value extract_notes gen rl =
  List.fold_right
    (fun r lines ->
       List.fold_right
         (fun r lines ->
            if r.rlab = "NOTE" && r.rval <> "" && r.rval.[0] == '@' then
              let addr = extract_addr r.rval in
              match find_notes_record gen addr with
              [ Some r ->
                  let l = List.map (fun r -> (r.rlab, r.rval)) r.rsons in
                  [("NOTE", r.rcont) :: l @ lines]
              | None ->
                  do print_location r.rpos;
                     Printf.fprintf log_oc.val "Note %s not found\n" addr;
                     flush log_oc.val;
                  return lines ]
            else [(r.rlab, r.rval) :: lines])
         [r :: r.rsons] lines)
    rl []
;

value treat_notes gen rl =
  let lines = extract_notes gen rl in
  let notes =
    List.fold_left
      (fun s (lab, n) ->
         let spc = String.length n > 0 && n.[0] == ' ' in
         let end_spc = String.length n > 1 && n.[String.length n - 1] == ' ' in
         let n = strip_spaces n in
         if s = "" then n ^ (if end_spc then " " else "")
         else if lab = "CONT" || lab = "NOTE" then
           s ^ "<br>\n" ^ n ^ (if end_spc then " " else "")
         else if n = "" then s
         else
           s ^ (if spc then "\n" else "") ^ n ^ (if end_spc then " " else ""))
      "" lines
  in
  strip_newlines notes
;

value source gen r =
  match find_field "SOUR" r.rsons with
  [ Some r ->
      if String.length r.rval > 0 && r.rval.[0] = '@' then
        match find_sources_record gen r.rval with
        [ Some v -> v.rcont
        | None ->
            do print_location r.rpos;
               Printf.fprintf log_oc.val "Source %s not found\n" r.rval;
               flush log_oc.val;
            return "" ]
      else r.rval
  | _ -> "" ]
;

value string_empty = ref (Adef.istr_of_int 0);
value string_x = ref (Adef.istr_of_int 0);

value p_index_from s i c =
  if i >= String.length s then String.length s
  else try String.index_from s i c with [ Not_found -> String.length s ]
;

value strip_sub s beg len = strip_spaces (String.sub s beg len);

value decode_title s =
  let i1 = p_index_from s 0 ',' in
  let i2 = p_index_from s (i1 + 1) ',' in
  let title = strip_sub s 0 i1 in
  let (place, nth) =
    if i1 == String.length s then ("", 0)
    else if i2 == String.length s then
      let s1 = strip_sub s (i1 + 1) (i2 - i1 - 1) in
      try ("", int_of_string s1) with [ Failure _ -> (s1, 0) ]
    else
      let s1 = strip_sub s (i1 + 1) (i2 - i1 - 1) in
      let s2 = strip_sub s (i2 + 1) (String.length s - i2 - 1) in
      try (s1, int_of_string s2) with
      [ Failure _ -> (strip_sub s i1 (String.length s - i1), 0) ]
  in
  (title, place, nth)
;

value decode_date_interval pos s =
  let strm = Stream.of_string s in
  try
    match Grammar.Entry.parse date_interval strm with
    [ BeginEnd (d1, cal1) (d2, cal2) ->
        (Some (Dgreg d1 cal1), Some (Dgreg d2 cal2))
    | Begin (d, cal) -> (Some (Dgreg d cal), None)
    | End (d, cal) -> (None, Some (Dgreg d cal)) ]
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
  let (name, title, place) =
    match find_field "NOTE" r.rsons with
    [ Some r ->
        if title = "" then (Tnone, strip_spaces r.rval, "")
        else if r.rval = public_name then (Tmain, title, place)
        else (Tname (add_string gen (strip_spaces r.rval)), title, place)
    | None -> (Tnone, title, place) ]
  in
  {t_name = name; t_ident = add_string gen title;
   t_place = add_string gen place;
   t_date_start = Adef.codate_of_od date_start;
   t_date_end = Adef.codate_of_od date_end; t_nth = nth}
;

value forward_adop gen ip lab which_parent =
  let which_parent =
    match which_parent with
    [ Some r -> r.rval
    | _ -> "" ]
  in
  Hashtbl.add gen.g_adop lab (ip, which_parent)
;

value adop_parent gen ip r =
  let i = per_index gen r.rval in
  match gen.g_per.arr.(Adef.int_of_iper i) with
  [ Left3 _ -> None
  | Right3 p _ _ ->
      do if List.memq ip p.related then ()
         else p.related := [ip :: p.related];
      return Some p.cle_index ]
;

value set_adop_fam gen ip which_parent fath moth =
  match gen.g_per.arr.(Adef.int_of_iper ip) with
  [ Left3 _ -> ()
  | Right3 per _ _ ->
      let r_fath =
        match (which_parent, fath) with
        [ (("HUSB" | "BOTH"), Some r) -> adop_parent gen ip r
        | _ -> None ]
      in
      let r_moth =
        match (which_parent, moth) with
        [ (("WIFE" | "BOTH"), Some r) -> adop_parent gen ip r
        | _ -> None ]
      in
      let r =
        {r_type = Adoption; r_fath = r_fath; r_moth = r_moth;
         r_sources = add_string gen ""}
      in
      per.rparents := [r :: per.rparents] ]
;

value forward_godp gen ip ipp =
  gen.g_godp := [(ipp, ip) :: gen.g_godp]
;

value glop = ref [];

value indi_lab =
  fun
  [ "ADOP" | "ASSO" | "BAPM" | "BIRT" | "BURI" | "CHR" | "CREM" | "DEAT"
  | "FAMC" | "FAMS" | "NAME" | "NOTE" | "OBJE" | "OCCU" | "SEX" | "SOUR"
  | "TITL" -> True
  | c -> do if List.mem c glop.val then () else do glop.val := [c :: glop.val]; Printf.eprintf "untreated tag %s -> in notes\n" c; flush stderr; return (); return
      False ]
;

value html_text_of_tags rl =
  let rec tot len lev r =
    let len = Buff.mstore len (string_of_int lev) in
    let len = Buff.store len ' ' in
    let len = Buff.mstore len r.rlab in
    let len =
      if r.rval = "" then len else Buff.mstore (Buff.store len ' ') r.rval
    in
    let len =
      if r.rcont = "" then len else Buff.mstore (Buff.store len ' ') r.rcont
    in
    totl len (lev + 1) r.rsons
  and totl len lev rl =
    List.fold_left
      (fun len r ->
         let len = Buff.store len '\n' in
         tot len lev r)
      len rl
  in
  let len = 0 in
  let len = Buff.mstore len "<pre>\n" in
  let len = Buff.mstore len "-- GEDCOM --" in
  let len = totl len 1 rl in
  let len = Buff.mstore len "\n</pre>" in
  Buff.get len
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
  let (first_name, surname, occ, public_name, first_names_aliases) =
    match name_sons with
    [ Some n ->
        let (f, s) = parse_name (Stream.of_string n.rval) in
        let (f, pn, fal) =
          if extract_public_names.val || extract_first_names.val then
            let i = next_word_pos f 0 in
            let j = next_sep_pos f i in
            if j == String.length f then (f, public_name, [])
            else
              let fn = String.sub f i (j - i) in
              if public_name = "" && extract_public_names.val then
                if is_a_public_name f j then (fn, f, [])
                else if extract_first_names.val then (fn, "", [f])
                else (f, "", [])
              else (fn, public_name, [f])
          else (f, public_name, [])
        in
        let f = if lowercase_first_names.val then lowercase_name f else f in
        let fal =
          if lowercase_first_names.val then List.map lowercase_name fal
          else fal
        in
        let (f, fal) =
          match first_names_brackets.val with
          [ Some (bb, eb) ->
              try
                let i = String.index f bb in
                let j =
                  if i + 2 == String.length f then raise Not_found
                  else String.index_from f (i + 2) eb
                in
                if j > i then
                  let fn = String.sub f (i + 1) (j - i - 1) in
                  let fa =
                    String.sub f 0 i ^ fn ^
                    String.sub f (j + 1) (String.length f - j - 1)
                  in
                  if fn = fa then (fn, fal) else (fn, [fa :: fal])
                else raise Not_found
              with
              [ Not_found -> (f, fal) ]
          | None -> (f, fal) ]
        in
        let pn = if lowercase_name pn = f then "" else pn in
        let fal =
          List.fold_right (fun fa fal -> if fa = pn then fal else [fa :: fal])
            fal []
        in
        let s = if lowercase_surnames.val then lowercase_name s else s in
        let r =
          let key = Name.strip_lower (f ^ " " ^ s) in
          try Hashtbl.find gen.g_hnam key with
          [ Not_found ->
              let r = ref (-1) in do Hashtbl.add gen.g_hnam key r; return r ]
        in
        do incr r; return (f, s, r.val, pn, fal)
    | None -> ("?", "?", Adef.int_of_iper i, public_name, []) ]
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
        [ Some r ->
            let x =
              if lowercase_surnames.val then lowercase_name r.rval else r.rval
            in
            if x <> surname then x else ""
        | _ -> "" ]
    | None -> "" ]
  in
  let aliases =
    match find_all_fields "NAME" r.rsons with
    [ [_ :: l] -> List.map (fun r -> r.rval) l
    | _ -> [] ]
  in
  let sex =
    match find_field "SEX" r.rsons with
    [ Some {rval = "M"} -> Male
    | Some {rval = "F"} -> Female
    | _ -> Neuter ]
  in
  let image =
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
  let notes =
    match find_all_fields "NOTE" r.rsons with
    [ [] -> ""
    | rl -> treat_notes gen rl ]
  in
  let titles =
    List.map (treat_indi_title gen public_name)
      (find_all_fields "TITL" r.rsons)
  in
  let family =
    let rl = find_all_fields "FAMS" r.rsons in
    let rvl =
      List.fold_right
        (fun r rvl -> if List.mem r.rval rvl then [r.rval :: rvl] else rvl) rl
        []
    in
    List.map (fun r -> fam_index gen r) rvl
  in
  let rparents = [] in
  let rparents =
    let rl = find_all_fields "ASSO" r.rsons in
    let rec find_rela n =
      fun
      [ [] -> None
      | [r :: rl] ->
          match find_field "RELA" r.rsons with
          [ Some r1 ->
              if String.length r1.rval >= 4
              && String.lowercase (String.sub r1.rval 0 4) = n then
                let ipp = per_index gen r.rval in
                do forward_godp gen i ipp; return
                Some ipp
              else find_rela n rl
          | None -> find_rela n rl ] ]
    in
    let godf = find_rela "godf" r.rsons in
    let godm = find_rela "godm" r.rsons in
    if godf <> None || godm <> None then
      let r =
        {r_type = GodParent; r_fath = godf; r_moth = godm;
         r_sources = add_string gen ""}
      in
      [r :: rparents]
    else rparents
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
          else (infer_death birth, "", "")
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
    | None -> (infer_death birth, "", "") ]
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
  let empty = add_string gen "" in
  let psources =
    let s = source gen r in
    if s = "" then default_source.val else s
  in
  let ext_notes =
    if untreated_in_notes.val then
      let remain_tags =
        List.fold_left
          (fun list r -> if indi_lab r.rlab then list else [r :: list])
          [] r.rsons
      in
      if remain_tags = [] then ""
      else
        let s = if notes = "" then "" else "\n" in
        s ^ html_text_of_tags (List.rev remain_tags)
    else ""
  in
  let person =
    {first_name = add_string gen first_name; surname = add_string gen surname;
     occ = occ; public_name = add_string gen public_name;
     image = add_string gen image;
     nick_names = if nick_name <> "" then [add_string gen nick_name] else [];
     aliases = List.map (add_string gen) aliases;
     first_names_aliases = List.map (add_string gen) first_names_aliases;
     surnames_aliases =
       if surname_alias <> "" then [add_string gen surname_alias] else [];
     titles = titles; rparents = rparents; related = [];
     occupation = add_string gen occupation;
     sex = sex;
     access =
       if no_public_if_titles.val && titles <> [] then Private else IfTitles;
     birth = birth;
     birth_place = add_string gen birth_place;
     birth_src = add_string gen birth_src; baptism = bapt;
     baptism_place = add_string gen bapt_place;
     baptism_src = add_string gen bapt_src; death = death;
     death_place = add_string gen death_place;
     death_src = add_string gen death_src; burial = burial;
     burial_place = add_string gen burial_place;
     burial_src = add_string gen burial_src;
     notes = add_string gen (notes ^ ext_notes);
     psources = add_string gen psources; cle_index = i}
  and ascend = {parents = parents; consang = Adef.fix (-1)}
  and union = {family = Array.of_list family} in
  do gen.g_per.arr.(Adef.int_of_iper i) := Right3 person ascend union;
     match find_field "ADOP" r.rsons with
     [ Some r ->
         match find_field "FAMC" r.rsons with
         [ Some r -> forward_adop gen i r.rval (find_field "ADOP" r.rsons)
         | _ -> () ]
     | _ -> () ];
  return ()
;

value add_fam_norm gen r =
  let i = fam_index gen r.rval in
  let fath =
    match find_field "HUSB" r.rsons with
    [ Some r -> per_index gen r.rval
    | None -> phony_per gen Male ]
  in
  let moth =
    match find_field "WIFE" r.rsons with
    [ Some r -> per_index gen r.rval
    | None -> phony_per gen Female ]
  in
  do match gen.g_per.arr.(Adef.int_of_iper fath) with
     [ Left3 lab -> ()
     | Right3 p _ u ->
         do if not (List.memq i (Array.to_list u.family)) then
              u.family := Array.append u.family [| i |]
            else ();
            if p.sex = Neuter then p.sex := Male else ();
         return () ];
     match gen.g_per.arr.(Adef.int_of_iper moth) with
     [ Left3 lab -> ()
     | Right3 p _ u ->
         do if not (List.memq i (Array.to_list u.family)) then
              u.family := Array.append u.family [| i |]
            else ();
            if p.sex = Neuter then p.sex := Female else ();
         return () ];
  return
  let children =
    let rl = find_all_fields "CHIL" r.rsons in
    List.map (fun r -> per_index gen r.rval) rl
  in
  let (not_married, marr, marr_place, marr_src) =
    match find_field "MARR" r.rsons with
    [ Some r ->
        let (u, p) =
          match find_field "PLAC" r.rsons with
          [ Some r ->
              if String.uncapitalize r.rval = "unmarried" then (True, "")
              else (False, r.rval)
          | _ -> (False, "") ]
        in
        let d =
          if u then None
          else
            match find_field "DATE" r.rsons with
            [ Some r -> date_of_field r.rpos r.rval
            | _ -> None ]
        in
        (u, d, p, source gen r)
    | None -> (False, None, "", "") ]
  in
  let div =
    match find_field "DIV" r.rsons with
    [ Some r ->
        match find_field "DATE" r.rsons with
        [ Some d -> Divorced (Adef.codate_of_od (date_of_field r.rpos r.rval))
        | _ ->
            match find_field "PLAC" r.rsons with
            [ Some _ -> Divorced Adef.codate_None
            | _ ->
                if r.rval = "Y" then Divorced Adef.codate_None
                else NotDivorced ] ]
    | None -> NotDivorced ]
  in
  let comment =
    match find_field "NOTE" r.rsons with
    [ Some r -> if r.rval <> "" && r.rval.[0] == '@' then "" else r.rval
    | None -> "" ]
  in
  let empty = add_string gen "" in
  let fsources =
    let s = source gen r in
    if s = "" then default_source.val else s
  in
  let fam =
    {marriage = Adef.codate_of_od marr;
     marriage_place = add_string gen marr_place;
     marriage_src = add_string gen marr_src;
     witnesses = [| |];
     not_married = not_married;
     divorce = div;
     comment = add_string gen comment; origin_file = empty;
     fsources = add_string gen fsources; fam_index = i}
  and cpl = {father = fath; mother = moth}
  and des = {children = Array.of_list children} in
  gen.g_fam.arr.(Adef.int_of_ifam i) := Right3 fam cpl des
;

value add_fam gen r =
  try
    let (ip, which_parent) = Hashtbl.find gen.g_adop r.rval in
    set_adop_fam gen ip which_parent (find_field "HUSB" r.rsons)
      (find_field "WIFE" r.rsons)
  with
  [ Not_found -> add_fam_norm gen r ]
;

value treat_header2 gen r =
  match charset_option.val with
  [ Some v -> charset.val := v
  | None ->
      match find_field "CHAR" r.rsons with
      [ Some r ->
          match r.rval with
          [ "ANSEL" -> charset.val := Ansel
          | "ASCII" | "IBMPC" -> charset.val := Ascii
          | _ -> charset.val := Ascii ]
      | None -> () ] ]
;

value treat_header3 gen r =
   match find_all_fields "NOTE" r.rsons with
   [ [] -> ()
   | rl -> gen.g_bnot := treat_notes gen rl ]
;

value make_gen2 gen r =
  match r.rlab with
  [ "HEAD" -> treat_header2 gen r
  | "INDI" -> add_indi gen r
  | _ -> () ]
;

value make_gen3 gen r =
  match r.rlab with
  [ "HEAD" -> treat_header3 gen r
  | "SUBM" -> ()
  | "INDI" -> ()
  | "FAM" -> add_fam gen r
  | "NOTE" -> ()
  | "SOUR" -> ()
  | "TRLR" -> do Printf.eprintf "*** Trailer ok\n"; flush stderr; return ()
  | s ->
      do Printf.fprintf log_oc.val "Not implemented typ = %s\n" s;
         flush log_oc.val;
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
      Printf.fprintf log_oc.val "%s\nis defined several times\n"
        (denomination base p)
  | OwnAncestor p ->
      Printf.fprintf log_oc.val "%s\nis his/her own ancestor\n"
        (denomination base p)
  | BadSexOfMarriedPerson p ->
      Printf.fprintf log_oc.val "%s\n  bad sex for a married person\n"
        (denomination base p) ]
;

value print_base_warning base =
  fun
  [ BirthAfterDeath p ->
      Printf.fprintf log_oc.val "%s\n  born after his/her death\n"
        (denomination base p)
  | ChangedOrderOfChildren ifam des _ ->
      let cpl = coi base ifam in
      Printf.fprintf log_oc.val "Changed order of children of %s and %s\n"
        (denomination base (poi base cpl.father))
        (denomination base (poi base cpl.mother))
  | ChildrenNotInOrder ifam des elder x ->
      let cpl = coi base ifam in
      do Printf.fprintf log_oc.val
           "The following children of\n  %s\nand\n  %s\nare not in order:\n"
           (denomination base (poi base cpl.father))
           (denomination base (poi base cpl.mother));
         Printf.fprintf log_oc.val "- %s\n" (denomination base elder);
         Printf.fprintf log_oc.val "- %s\n" (denomination base x);
      return ()
  | DeadTooEarlyToBeFather father child ->
      do Printf.fprintf log_oc.val "%s\n" (denomination base child);
         Printf.fprintf log_oc.val
           "  is born more than 2 years after the death of his/her father\n";
         Printf.fprintf log_oc.val "%s\n" (denomination base father);
      return ()
  | MarriageDateAfterDeath p ->
      do Printf.fprintf log_oc.val "%s\n" (denomination base p);
         Printf.fprintf log_oc.val "married after his/her death\n";
      return ()
  | MarriageDateBeforeBirth p ->
      do Printf.fprintf log_oc.val "%s\n" (denomination base p);
         Printf.fprintf log_oc.val "married before his/her birth\n";
      return ()
  | MotherDeadAfterChildBirth mother child ->
      Printf.fprintf log_oc.val
        "%s\n  is born after the death of his/her mother\n%s\n"
        (denomination base child) (denomination base mother)
  | ParentBornAfterChild parent child ->
      Printf.fprintf log_oc.val "%s born after his/her child %s\n"
        (denomination base parent) (denomination base child)
  | ParentTooYoung p a ->
      Printf.fprintf log_oc.val "%s was parent at age of %d\n"
        (denomination base p) (annee a)
  | TitleDatesError p t ->
      do Printf.fprintf log_oc.val "%s\n" (denomination base p);
         Printf.fprintf log_oc.val "has incorrect title dates as:\n";
         Printf.fprintf log_oc.val "  %s %s\n" (sou base t.t_ident)
           (sou base t.t_place);
      return ()
  | YoungForMarriage p a ->
      Printf.fprintf log_oc.val "%s married at age %d\n" (denomination base p)
        (annee a) ]
;

value find_lev0 =
  parser bp
    [: _ = line_start '0'; _ = skip_space; r1 = get_ident 0; r2 = get_ident 0;
       _ = skip_to_eoln :] ->
      (bp, r1, r2)
;

value pass1 gen fname =
  let ic = open_in_bin fname in
  let strm = Stream.of_channel ic in
  do let rec loop () =
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
           | [: :] -> () ] ]
     in
     loop ();
     close_in ic;
  return ()
;

value pass2 gen fname =
  let ic = open_in_bin fname in
  do line_cnt.val := 0; return
  let strm =
    Stream.from
      (fun i ->
         try
           let c = input_char ic in
           do if c == '\n' then incr line_cnt else (); return Some c
         with
         [ End_of_file -> None ])
  in
  do let rec loop () =
       match try Some (get_lev0 strm) with [ Stream.Failure -> None ] with
       [ Some r -> do make_gen2 gen r; return loop ()
       | None ->
           match strm with parser
           [ [: `'1'..'9' :] ->
               do let _ : string = get_to_eoln 0 strm in (); return loop ()
           | [: `_ :] ->
               do let _ : string = get_to_eoln 0 strm in (); return loop ()
           | [: :] -> () ] ]
     in
     loop ();
     List.iter
       (fun (ipp, ip) ->
          match gen.g_per.arr.(Adef.int_of_iper ipp) with
          [ Right3 p _ _ ->
              if List.memq ip p.related then ()
              else p.related := [ip :: p.related]
          | _ -> () ])
       gen.g_godp;
     close_in ic;
  return ()
;

value pass3 gen fname =
  let ic = open_in_bin fname in
  do line_cnt.val := 0; return
  let strm =
    Stream.from
      (fun i ->
         try
           let c = input_char ic in
           do if c == '\n' then incr line_cnt else (); return Some c
         with
         [ End_of_file -> None ])
  in
  do let rec loop () =
       match try Some (get_lev0 strm) with [ Stream.Failure -> None ] with
       [ Some r -> do make_gen3 gen r; return loop ()
       | None ->
           match strm with parser
           [ [: `'1'..'9' :] ->
               do let _ : string = get_to_eoln 0 strm in (); return loop ()
           | [: `_ :] ->
               do print_location line_cnt.val;
                  Printf.fprintf log_oc.val "Strange input.\n";
                  flush log_oc.val;
                  let _ : string = get_to_eoln 0 strm in ();
               return loop ()
           | [: :] -> () ] ]
     in
     loop ();
     close_in ic;
  return ()
;

value check_undefined gen =
  do for i = 0 to gen.g_per.tlen - 1 do
       match gen.g_per.arr.(i) with
       [ Right3 _ _ _ -> ()
       | Left3 lab ->
           let (p, a, u) = unknown_per gen i in
           do Printf.fprintf log_oc.val "Warning: undefined person %s\n" lab;
              gen.g_per.arr.(i) := Right3 p a u;
           return () ];
     done;
     for i = 0 to gen.g_fam.tlen - 1 do
       match gen.g_fam.arr.(i) with
       [ Right3 _ _ _ -> ()
       | Left3 lab ->
           let (f, c, d) = unknown_fam gen i in
           do Printf.fprintf log_oc.val "Warning: undefined family %s\n" lab;
              gen.g_fam.arr.(i) := Right3 f c d;
           return () ];
     done;
  return ()
;

value make_arrays in_file =
  let fname =
    if Filename.check_suffix in_file ".ged" then in_file
    else if Filename.check_suffix in_file ".GED" then in_file
    else in_file ^ ".ged"
  in
  let gen =
    {g_per = {arr = [| |]; tlen = 0}; g_fam = {arr = [| |]; tlen = 0};
     g_str = {arr = [| |]; tlen = 0}; g_bnot = "";
     g_ic = open_in_bin fname;
     g_not = Hashtbl.create 3001; g_src = Hashtbl.create 3001;
     g_hper = Hashtbl.create 3001; g_hfam = Hashtbl.create 3001;
     g_hstr = Hashtbl.create 3001; g_hnam = Hashtbl.create 3001;
     g_adop = Hashtbl.create 3001; g_godp = []}
  in
  do string_empty.val := add_string gen "";
     string_x.val := add_string gen "x";
     Printf.eprintf "*** pass 1 (note)\n";
     flush stderr;
     pass1 gen fname;
     Printf.eprintf "*** pass 2 (indi)\n";
     flush stderr;
     pass2 gen fname;
     Printf.eprintf "*** pass 3 (fam)\n";
     flush stderr;
     pass3 gen fname;
     close_in gen.g_ic;
     check_undefined gen;
  return (gen.g_per, gen.g_fam, gen.g_str, gen.g_bnot)
;

value make_subarrays (g_per, g_fam, g_str, g_bnot) =
  let persons =
    let pa = Array.create g_per.tlen (Obj.magic 0) in
    let aa = Array.create g_per.tlen (Obj.magic 0) in
    let ua = Array.create g_per.tlen (Obj.magic 0) in
    do for i = 0 to g_per.tlen - 1 do
         match g_per.arr.(i) with
         [ Right3 p a u ->
             do pa.(i) := p; aa.(i) := a; ua.(i) := u; return ()
         | Left3 lab -> failwith ("undefined person " ^ lab) ];
       done;
    return (pa, aa, ua)
  in
  let families =
    let fa = Array.create g_fam.tlen (Obj.magic 0) in
    let ca = Array.create g_fam.tlen (Obj.magic 0) in
    let da = Array.create g_fam.tlen (Obj.magic 0) in
    do for i = 0 to g_fam.tlen - 1 do
         match g_fam.arr.(i) with
         [ Right3 f c d ->
             do fa.(i) := f; ca.(i) := c; da.(i) := d; return ()
         | Left3 lab -> failwith ("undefined family " ^ lab) ];
       done;
    return (fa, ca, da)
  in
  let strings = Array.sub g_str.arr 0 g_str.tlen in
  (persons, families, strings, g_bnot)
;

value cache_of tab =
  let c =
    {array = fun _ -> tab; get = fun []; len = Array.length tab;
     clear_array = fun x -> x}
  in
  do c.get := fun i -> (c.array ()).(i); return c
;

value make_base (persons, families, strings, bnotes) =
  let (persons, ascends, unions) = persons in
  let (families, couples, descends) = families in
  let bnotes = {nread = fun _ -> bnotes; norigin_file = ""} in
  let base_data =
    {persons = cache_of persons; ascends = cache_of ascends;
     unions = cache_of unions;
     families = cache_of families; couples = cache_of couples;
     descends = cache_of descends;
     strings = cache_of strings; bnotes = bnotes}
  in
  let base_func =
    {persons_of_name = fun []; strings_of_fsname = fun [];
     index_of_string = fun [];
     persons_of_surname = {find = fun []; cursor = fun []; next = fun []};
     persons_of_first_name = {find = fun []; cursor = fun []; next = fun []};
     patch_person = fun []; patch_ascend = fun []; patch_union = fun [];
     patch_family = fun []; patch_couple = fun []; patch_descend = fun [];
     patch_string = fun []; patch_name = fun [];
     commit_patches = fun []; commit_notes = fun [];
     patched_ascends = fun []; cleanup = fun () -> ()}
  in
  {data = base_data; func = base_func}
;

value array_memq x a =
  loop 0 where rec loop i =
    if i == Array.length a then False
    else if x == a.(i) then True
    else loop (i + 1)
;

value check_parents_children base =
  let to_delete = ref [] in
  do for i = 0 to base.data.persons.len - 1 do
       let a = base.data.ascends.get i in
       match a.parents with
       [ Some ifam ->
           let fam = foi base ifam in
           if fam.fam_index == Adef.ifam_of_int (-1) then a.parents := None
           else
             let cpl = coi base ifam in
             let des = doi base ifam in
             if array_memq (Adef.iper_of_int i) des.children then ()
             else
               let p = base.data.persons.get i in
               do Printf.fprintf log_oc.val
                    "%s is not the child of his/her parents\n"
                    (denomination base p);
                  Printf.fprintf log_oc.val "- %s\n"
                    (denomination base (poi base cpl.father));
                  Printf.fprintf log_oc.val "- %s\n"
                    (denomination base (poi base cpl.mother));
                  Printf.fprintf log_oc.val "=> no more parents for him/her\n";
                  Printf.fprintf log_oc.val "\n";
                  flush log_oc.val;
                  a.parents := None;
               return ()
       | None -> () ];
     done;
     for i = 0 to base.data.families.len - 1 do
       to_delete.val := [];
       let fam = base.data.families.get i in
       let cpl = base.data.couples.get i in
       let des = base.data.descends.get i in
       do for j = 0 to Array.length des.children - 1 do
            let a = aoi base des.children.(j) in
            let p = poi base des.children.(j) in
            match a.parents with
            [ Some ifam ->
                if Adef.int_of_ifam ifam <> i then
                  do Printf.fprintf log_oc.val "Other parents for %s\n"
                       (denomination base p);
                     Printf.fprintf log_oc.val "- %s\n"
                       (denomination base (poi base cpl.father));
                     Printf.fprintf log_oc.val "- %s\n"
                       (denomination base (poi base cpl.mother));
                     Printf.fprintf log_oc.val "=> deleted in this family\n";
                     Printf.fprintf log_oc.val "\n";
                     flush log_oc.val;
                     to_delete.val := [p.cle_index :: to_delete.val];
                  return ()
                else ()
            | None ->
                do Printf.fprintf log_oc.val
                     "%s has no parents but is the child of\n"
                     (denomination base p);
                   Printf.fprintf log_oc.val "- %s\n"
                     (denomination base (poi base cpl.father));
                   Printf.fprintf log_oc.val "- %s\n"
                     (denomination base (poi base cpl.mother));
                   Printf.fprintf log_oc.val "=> added parents\n";
                   Printf.fprintf log_oc.val "\n";
                   flush log_oc.val;
                   a.parents := Some fam.fam_index;
                return () ];
          done;
          if to_delete.val <> [] then
            let l =
              List.fold_right
                (fun ip l ->
                   if List.memq ip to_delete.val then l else [ip :: l])
                (Array.to_list des.children) []
            in
            des.children := Array.of_list l
          else ();
       return ();
     done;
  return ()
;

value kill_family base fam ip =
  let u = uoi base ip in
  let l =
    List.fold_right
      (fun ifam ifaml ->
         if ifam == fam.fam_index then ifaml else [ifam :: ifaml])
      (Array.to_list u.family) []
  in
  u.family := Array.of_list l
;

value kill_parents base ip = let a = aoi base ip in a.parents := None;

value effective_del_fam base fam cpl des =
  let ifam = fam.fam_index in
  do kill_family base fam cpl.father;
     kill_family base fam cpl.mother;
     Array.iter (kill_parents base) des.children;
     cpl.father := Adef.iper_of_int (-1);
     cpl.mother := Adef.iper_of_int (-1);
     des.children := [| |];
     fam.fam_index := Adef.ifam_of_int (-1);
  return ()
;

value string_of_sex =
  fun
  [ Male -> "M"
  | Female -> "F"
  | Neuter -> "N" ]
;

value check_parents_sex base =
  for i = 0 to base.data.couples.len - 1 do
    let cpl = base.data.couples.get i in
    let fath = poi base cpl.father in
    let moth = poi base cpl.mother in
    if fath.sex = Female || moth.sex = Male then
      do Printf.fprintf log_oc.val "Bad sex for parents\n";
         Printf.fprintf log_oc.val "- father: %s (sex: %s)\n"
           (denomination base fath) (string_of_sex fath.sex);
         Printf.fprintf log_oc.val "- mother: %s (sex: %s)\n"
           (denomination base moth) (string_of_sex moth.sex);
         Printf.fprintf log_oc.val "=> family deleted\n\n";
         flush log_oc.val;
         effective_del_fam base (base.data.families.get i) cpl
           (base.data.descends.get i);
      return ()
    else do fath.sex := Male; moth.sex := Female; return ();
  done
;

value neg_year_dmy =
  fun
  [ {day = d; month = m; year = y; prec = OrYear y2} ->
      {day = d; month = m; year = - abs y; prec = OrYear (- abs y2); delta = 0}
  | {day = d; month = m; year = y; prec = YearInt y2} ->
      {day = d; month = m; year = - abs y; prec = YearInt (- abs y2);
       delta = 0}
  | {day = d; month = m; year = y; prec = p} ->
      {day = d; month = m; year = - abs y; prec = p; delta = 0} ]
;

value neg_year =
  fun
  [ Dgreg d cal -> Dgreg (neg_year_dmy d) cal
  | x -> x ]
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
     let u = uoi base p.cle_index in
     for i = 0 to Array.length u.family - 1 do
       let fam = foi base u.family.(i) in
       match Adef.od_of_codate fam.marriage with
       [ Some d -> fam.marriage := Adef.codate_of_od (Some (neg_year d))
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
  for i = 0 to base.data.persons.len - 1 do
    let p = base.data.persons.get i in
    match (Adef.od_of_codate p.birth, date_of_death p.death) with
    [ (Some (Dgreg d1 _), Some (Dgreg d2 _)) ->
        if annee d1 > 0 && annee d2 > 0 && strictement_avant_dmy d2 d1 then
          negative_date_ancestors base (base.data.persons.get i)
        else ()
    | _ -> () ];
  done
;

value finish_base base =
  let persons = base.data.persons.array () in
  let ascends = base.data.ascends.array () in
  let unions = base.data.unions.array () in
  let families = base.data.families.array () in
  let descends = base.data.descends.array () in
  let strings = base.data.strings.array () in
  do for i = 0 to Array.length descends - 1 do
       let des = descends.(i) in
       let children =
         sort_by_date
           (fun ip -> Adef.od_of_codate persons.(Adef.int_of_iper ip).birth)
           (Array.to_list des.children)
       in
       des.children := Array.of_list children;
     done;
     for i = 0 to Array.length unions - 1 do
       let u = unions.(i) in
       let family =
         sort_by_date
           (fun ifam ->
              Adef.od_of_codate families.(Adef.int_of_ifam ifam).marriage)
           (Array.to_list u.family)
       in
       u.family := Array.of_list family;
     done;
     for i = 0 to Array.length persons - 1 do
       let p = persons.(i) in
       let a = ascends.(i) in
       let u = unions.(i) in
       if a.parents <> None && Array.length u.family != 0 ||
          p.notes <> string_empty.val then
         do if sou base p.first_name = "?" then
              do p.first_name := string_x.val; p.occ := i; return ()
            else ();
            if sou base p.surname = "?" then
              do p.surname := string_x.val; p.occ := i; return ()
            else ();
         return ()
       else ();
     done;
     check_parents_sex base;
     check_parents_children base;
     if try_negative_dates.val then negative_dates base else ();
     check_base base
       (fun x ->
          do print_base_error base x; return Printf.fprintf log_oc.val "\n")
       (fun x ->
          do print_base_warning base x; return
          Printf.fprintf log_oc.val "\n");
     flush log_oc.val;
  return ()
;

value output_command_line bname =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
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
  [("-o", Arg.String (fun s -> out_file.val := s),
    "<file>\n       Output data base (default: \"a\").");
   ("-f", Arg.Set force,
    "\n       Remove data base if already existing");
   ("-log", Arg.String (fun s -> log_oc.val := open_out s),
    "<file>\n       Redirect log trace to this file.");
   ("-lf", Arg.Set lowercase_first_names,
    "   \
- Lowercase first names -
       Convert first names to lowercase letters, with initials in
       uppercase."
      );
   ("-ls", Arg.Set lowercase_surnames,
    "   \
- Lowercase surnames -
       Convert surnames to lowercase letters, with initials in
       uppercase. Try to keep lowercase particles."
      );
   ("-fne",
    Arg.String
      (fun s ->
         if String.length s = 2 then
           first_names_brackets.val := Some (s.[0], s.[1])
         else
           raise
             (Arg.Bad
                "-fne option must be followed by a 2 characters string")),
    "be \
- First names enclosed -
       When creating a person, if the GEDCOM first name part holds
       a part between 'b' (any character) and 'e' (any character), it
       is considered to be the usual first name: e.g. -fne '\"\"' or
       -fne \"()\".");
   ("-efn", Arg.Set extract_first_names,
    "  \
- Extract first names -
       When creating a person, if the GEDCOM first name part holds several
       names, the first of this names becomes the person \"first name\" and
       the complete GEDCOM first name part a \"first name alias\"."
      );
   ("-no_efn", Arg.Clear extract_first_names,
    "  \
- Dont extract first names - [default]
       Cancels the previous option.");
   ("-epn", Arg.Set extract_public_names,
    "  \
- Extract public names - [default]
       When creating a person, if the GEDCOM first name part looks like a
       public name, i.e. holds:
       * a number or a roman number, supposed to be a number of a
         nobility title,
       * one of the words: \"der\", \"den\", \"die\", \"el\", \"le\", \"la\",
         \"the\", supposed to be the beginning of a qualifier,
       then the GEDCOM first name part becomes the person \"public name\"
       and its first word his \"first name\"."
      );
   ("-no_epn", Arg.Clear extract_public_names,
    "\n       Cancels the previous option.");
   ("-no_pit", Arg.Set no_public_if_titles,
    " \
- No public if titles -
       Do not consider persons having titles as public");
   ("-tnd", Arg.Set try_negative_dates,
    "  \
- Try negative dates -
       Set negative dates when inconsistency (e.g. birth after death)"
      );
   ("-no_nd", Arg.Set no_negative_dates,
    " \
- No negative dates -
       Don't interpret a year preceded by a minus sign as a negative year"
      );
   ("-uin", Arg.Set untreated_in_notes,
    " \
- Untreated in notes -
       Put untreated GEDCOM tags in notes");
   ("-ds", Arg.String (fun s -> default_source.val := s),
    " \
- Default source -
       Set the source field for persons and families without source data");
   ("-dates_dm", Arg.Unit (fun () -> month_number_dates.val := DayMonthDates),
    "\n       Interpret months-numbered dates as day/month/year");
   ("-dates_md", Arg.Unit (fun () -> month_number_dates.val := MonthDayDates),
    "\n       Interpret months-numbered dates as month/day/year");
   ("-charset",
    Arg.String
      (fun
       [ "ANSEL" -> charset_option.val := Some Ansel
       | "ASCII" -> charset_option.val := Some Ascii
       | "MSDOS" -> charset_option.val := Some Msdos
       | _ -> raise (Arg.Bad "bad -charset value") ]),
    "[ANSEL|ASCII|MSDOS] \
- charset decoding -
       Force given charset decoding, overriding the possible setting in
       GEDCOM")]
;

value errmsg = "Usage: ged2gwb [<ged>] [options] where options are:";

value main () =
  do Argl.parse speclist (fun s -> in_file.val := s) errmsg;
     let bdir =
       if Filename.check_suffix out_file.val ".gwb" then out_file.val
       else out_file.val ^ ".gwb"
     in
     if not force.val && Sys.file_exists bdir then
       do Printf.printf "\
The data base \"%s\" already exists. Use option -f to overwrite it.\n"
            out_file.val;
          flush stdout;
       return exit 2
     else ();
  return
  let arrays = make_arrays in_file.val in
  do Gc.compact (); return
  let arrays = make_subarrays arrays in
  let base = make_base arrays in
  do finish_base base;
     Iobase.output out_file.val base;
     output_command_line out_file.val;
     warning_month_number_dates ();
     if log_oc.val != stdout then close_out log_oc.val else ();
  return ()
;

Printexc.catch main ();
