(* camlp5r pa_extend.cmo ../src/pa_lock.cmo *)
(* $Id: ged2gwb2.ml,v 5.7 2008-01-14 03:51:54 ddr Exp $ *)
(* Copyright (c) 1998-2008 INRIA *)

open Def;
open Mutil;
open Printf;

type person = gen_person iper Adef.istr;
type ascend = gen_ascend ifam;
type union = gen_union ifam;
type family = gen_family iper Adef.istr;
type couple = gen_couple iper;
type descend = gen_descend iper;

value get_first_name p = p.Def.first_name;
value get_key_index p = p.Def.key_index;
value get_notes p = p.Def.notes;
value get_occ p = p.Def.occ;
value get_related p = p.Def.related;
value get_rparents p = p.Def.rparents;
value get_sex p = p.Def.sex;
value get_surname p = p.Def.surname;
value person_with_related p r = {(p) with related = r};
value person_with_rparents p r = {(p) with rparents = r};
value person_with_sex p s = {(p) with sex = s};
value person_of_gen_person p = p;
value gen_person_of_person p = p;

value get_parents a = a.Def.parents;

value ascend_with_parents a p = {parents = p; consang = a.consang};
value ascend_of_gen_ascend a = a;

value get_family u = u.Def.family;

value union_of_gen_union u = u;

value get_witnesses f = f.Def.witnesses;

value family_of_gen_family f = f;
value gen_family_of_family f = f;

value get_father c = Adef.father c;
value couple_of_gen_couple c = c;

value descend_of_gen_descend d = d;

value couple _ x y = Adef.couple x y;

value log_oc = ref stdout;

type record =
  { rlab : string;
    rval : string;
    rcont : string;
    rsons : list record;
    rpos : int;
    rused : mutable bool }
;

type choice3 'a 'b 'c 'd =
  [ Left3 of 'a
  | Right3 of 'b and 'c and 'd ]
;
type month_number_dates =
  [ MonthDayDates
  | DayMonthDates
  | NoMonthNumberDates
  | MonthNumberHappened of string ]
;

type charset =
  [ Ansel
  | Ascii
  | Msdos
  | MacIntosh
  | Utf8 ]
;

type case =
  [ NoCase
  | LowerCase
  | UpperCase ]
;

value lowercase_first_names = ref False;
value case_surnames = ref NoCase;
value extract_first_names = ref False;
value extract_public_names = ref True;
value charset_option = ref None;
value charset = ref Ascii;
value alive_years = ref 80;
value dead_years = ref 120;
value try_negative_dates = ref False;
value no_negative_dates = ref False;
value month_number_dates = ref NoMonthNumberDates;
value no_public_if_titles = ref False;
value first_names_brackets = ref None;
value untreated_in_notes = ref False;
value force = ref False;
value default_source = ref "";
value default_name = ref "?";
value relation_status = ref Married;
value no_picture = ref False;

(* Reading input *)

value line_cnt = ref 1;
value in_file = ref "";

value print_location pos =
  fprintf log_oc.val "File \"%s\", line %d:\n" in_file.val pos
;

value rec skip_eol =
  parser
  [ [: `'\010' | '\013'; _ = skip_eol :] -> ()
  | [: :] -> () ]
;

value rec get_to_eoln len =
  parser
  [ [: `'\010' | '\013'; _ = skip_eol :] -> Buff.get len
  | [: `'\t'; s :] -> get_to_eoln (Buff.store len ' ') s
  | [: `c; s :] -> get_to_eoln (Buff.store len c) s
  | [: :] -> Buff.get len ]
;

value rec skip_to_eoln =
  parser
  [ [: `'\010' | '\013'; _ = skip_eol :] -> ()
  | [: `_; s :] -> skip_to_eoln s
  | [: :] -> () ]
;

value eol_chars = ['\010'; '\013'];
value rec get_ident len =
  parser
  [ [: `' ' | '\t' :] -> Buff.get len
  | [: `c when not (List.mem c eol_chars); s :] ->
      get_ident (Buff.store len c) s
  | [: :] -> Buff.get len ]
;

value skip_space =
  parser
  [ [: `' ' | '\t' :] -> ()
  | [: :] -> () ]
;

value rec line_start num =
  parser
  [ [: `' '; s :] -> line_start num s
  | [: `x when x = num :] -> () ]
;

value ascii_of_msdos s =
  String.init (String.length s) conv_char
    where conv_char i =
      let cc =
        match Char.code s.[i] with
        [ 0o200 -> 0o307
        | 0o201 -> 0o374
        | 0o202 -> 0o351
        | 0o203 -> 0o342
        | 0o204 -> 0o344
        | 0o205 -> 0o340
        | 0o206 -> 0o345
        | 0o207 -> 0o347
        | 0o210 -> 0o352
        | 0o211 -> 0o353
        | 0o212 -> 0o350
        | 0o213 -> 0o357
        | 0o214 -> 0o356
        | 0o215 -> 0o354
        | 0o216 -> 0o304
        | 0o217 -> 0o305
        | 0o220 -> 0o311
        | 0o221 -> 0o346
        | 0o222 -> 0o306
        | 0o223 -> 0o364
        | 0o224 -> 0o366
        | 0o225 -> 0o362
        | 0o226 -> 0o373
        | 0o227 -> 0o371
        | 0o230 -> 0o377
        | 0o231 -> 0o326
        | 0o232 -> 0o334
        | 0o233 -> 0o242
        | 0o234 -> 0o243
        | 0o235 -> 0o245
        | 0o240 -> 0o341
        | 0o241 -> 0o355
        | 0o242 -> 0o363
        | 0o243 -> 0o372
        | 0o244 -> 0o361
        | 0o245 -> 0o321
        | 0o246 -> 0o252
        | 0o247 -> 0o272
        | 0o250 -> 0o277
        | 0o252 -> 0o254
        | 0o253 -> 0o275
        | 0o254 -> 0o274
        | 0o255 -> 0o241
        | 0o256 -> 0o253
        | 0o257 -> 0o273
        | 0o346 -> 0o265
        | 0o361 -> 0o261
        | 0o366 -> 0o367
        | 0o370 -> 0o260
        | 0o372 -> 0o267
        | 0o375 -> 0o262
        | c -> c ]
      in
      Char.chr cc
;

value ascii_of_macintosh s =
  String.init (String.length s) conv_char
    where conv_char i =
      let cc =
        match Char.code s.[i] with
        [ 0o200 -> 0o304
        | 0o201 -> 0o305
        | 0o202 -> 0o307
        | 0o203 -> 0o311
        | 0o204 -> 0o321
        | 0o205 -> 0o326
        | 0o206 -> 0o334
        | 0o207 -> 0o341
        | 0o210 -> 0o340
        | 0o211 -> 0o342
        | 0o212 -> 0o344
        | 0o213 -> 0o343
        | 0o214 -> 0o345
        | 0o215 -> 0o347
        | 0o216 -> 0o351
        | 0o217 -> 0o350
        | 0o220 -> 0o352
        | 0o221 -> 0o353
        | 0o222 -> 0o355
        | 0o223 -> 0o354
        | 0o224 -> 0o356
        | 0o225 -> 0o357
        | 0o226 -> 0o361
        | 0o227 -> 0o363
        | 0o230 -> 0o362
        | 0o231 -> 0o364
        | 0o232 -> 0o366
        | 0o233 -> 0o365
        | 0o234 -> 0o372
        | 0o235 -> 0o371
        | 0o236 -> 0o373
        | 0o237 -> 0o374
        | 0o241 -> 0o260
        | 0o244 -> 0o247
        | 0o245 -> 0o267
        | 0o246 -> 0o266
        | 0o247 -> 0o337
        | 0o250 -> 0o256
        | 0o256 -> 0o306
        | 0o257 -> 0o330
        | 0o264 -> 0o245
        | 0o273 -> 0o252
        | 0o274 -> 0o272
        | 0o276 -> 0o346
        | 0o277 -> 0o370
        | 0o300 -> 0o277
        | 0o301 -> 0o241
        | 0o302 -> 0o254
        | 0o307 -> 0o253
        | 0o310 -> 0o273
        | 0o312 -> 0o040
        | 0o313 -> 0o300
        | 0o314 -> 0o303
        | 0o315 -> 0o325
        | 0o320 -> 0o255
        | 0o326 -> 0o367
        | 0o330 -> 0o377
        | 0o345 -> 0o302
        | 0o346 -> 0o312
        | 0o347 -> 0o301
        | 0o350 -> 0o313
        | 0o351 -> 0o310
        | 0o352 -> 0o315
        | 0o353 -> 0o316
        | 0o354 -> 0o317
        | 0o355 -> 0o314
        | 0o356 -> 0o323
        | 0o357 -> 0o324
        | 0o361 -> 0o322
        | 0o362 -> 0o332
        | 0o363 -> 0o333
        | 0o364 -> 0o331
        | c -> c ]
      in
      Char.chr cc
;

value utf8_of_string s =
  match charset.val with
  [ Ansel -> utf_8_of_iso_8859_1 (Ansel.to_iso_8859_1 s)
  | Ascii -> Mutil.utf_8_of_iso_8859_1 s
  | Msdos -> Mutil.utf_8_of_iso_8859_1 (ascii_of_msdos s)
  | MacIntosh -> Mutil.utf_8_of_iso_8859_1 (ascii_of_macintosh s)
  | Utf8 -> s ]
;

value rec get_lev n =
  parser
    [: _ = line_start n; _ = skip_space; r1 = get_ident 0; strm :] ->
      let (rlab, rval, rcont, l) =
        if String.length r1 > 0 && r1.[0] = '@' then parse_address n r1 strm
        else parse_text n r1 strm
      in
      {rlab = rlab; rval = utf8_of_string rval;
       rcont = utf8_of_string rcont; rsons = List.rev l; rpos = line_cnt.val;
       rused = False}
and parse_address n r1 =
  parser
    [: r2 = get_ident 0; r3 = get_to_eoln 0 ? "get to eoln";
       l = get_lev_list [] (Char.chr (Char.code n + 1)) ? "get lev list" :] ->
      (r2, r1, r3, l)
and parse_text n r1 =
  parser
    [: r2 = get_to_eoln 0;
       l = get_lev_list [] (Char.chr (Char.code n + 1)) ? "get lev list" :] ->
      (r1, r2, "", l)
and get_lev_list l n =
  parser
  [ [: x = get_lev n; s :] -> get_lev_list [x :: l] n s
  | [: :] -> l ]
;

(* Error *)

value bad_dates_warned = ref False;

value print_bad_date pos d =
  if bad_dates_warned.val then ()
  else do {
    bad_dates_warned.val := True;
    print_location pos;
    fprintf log_oc.val "Can't decode date %s\n" d;
    flush log_oc.val
  }
;

value check_month m =
  if m < 1 || m > 12 then do {
    fprintf log_oc.val "Bad (numbered) month in date: %d\n" m;
    flush log_oc.val
  }
  else ()
;

value warning_month_number_dates () =
  match month_number_dates.val with
  [ MonthNumberHappened s ->
      do {
        fprintf log_oc.val "
  Warning: the file holds dates with numbered months (like: 12/05/1912).

  GEDCOM standard *requires* that months in dates be identifiers. The
  correct form for this example would be 12 MAY 1912 or 5 DEC 1912.

  Consider restarting with option \"-dates_dm\" or \"-dates_md\".
  Use option -help to see what they do.

  (example found in gedcom: \"%s\")
"
          s;
        flush log_oc.val
      }
  | _ -> () ]
;

(* Decoding fields *)

value rec skip_spaces =
  parser
  [ [: `' '; s :] -> skip_spaces s
  | [: :] -> () ]
;

value rec ident_slash len =
  parser
  [ [: `'/' :] -> Buff.get len
  | [: `'\t'; a = ident_slash (Buff.store len ' ') :] -> a
  | [: `c; a = ident_slash (Buff.store len c) :] -> a
  | [: :] -> Buff.get len ]
;

value strip c str =
  let start =
    loop 0 where rec loop i =
      if i = String.length str then i
      else if str.[i] = c then loop (i + 1)
      else i
  in
  let stop =
    loop (String.length str - 1) where rec loop i =
      if i = -1 then i + 1 else if str.[i] = c then loop (i - 1) else i + 1
  in
  if start = 0 && stop = String.length str then str
  else if start >= stop then ""
  else String.sub str start (stop - start)
;

value strip_spaces = strip ' ';
value strip_newlines = strip '\n';

value parse_name =
  parser
    [: _ = skip_spaces;
       invert =
         parser
         [ [: `'/' :] -> True
         | [: :] -> False ];
       f = ident_slash 0; _ = skip_spaces; s = ident_slash 0 :] ->
      let (f, s) = if invert then (s, f) else (f, s) in
      let f = strip_spaces f in
      let s = strip_spaces s in
      (if f = "" then "x" else f, if s = "" then "?" else s)
;

value rec find_field lab =
  fun
  [ [r :: rl] ->
      if r.rlab = lab then do { r.rused := True; Some r }
      else find_field lab rl
  | [] -> None ]
;

value rec find_all_fields lab =
  fun
  [ [r :: rl] ->
      if r.rlab = lab then do {
        r.rused := True; [r :: find_all_fields lab rl]
      }
      else find_all_fields lab rl
  | [] -> [] ]
;

value rec find_field_with_value lab v =
  fun
  [ [r :: rl] ->
      if r.rlab = lab && r.rval = v then
        do { r.rused := True; True }
      else find_field_with_value lab v rl
  | [] -> False ]
;

value rec lexing_date =
  parser
  [ [: `('0'..'9' as c); n = number (Buff.store 0 c) :] -> ("INT", n)
  | [: `('A'..'Z' as c); i = ident (Buff.store 0 c) :] -> ("ID", i)
  | [: `'('; len = text 0 :] -> ("TEXT", Buff.get len)
  | [: `'.' :] -> ("", ".")
  | [: `' ' | '\t' | '\013'; s :] -> lexing_date s
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
and text len =
  parser
  [ [: `')' :] -> len
  | [: `'('; len = text (Buff.store len '('); s :] ->
      text (Buff.store len ')') s
  | [: `c; s :] -> text (Buff.store len c) s
  | [: :] -> len ]
;

value make_date_lexing s = Stream.from (fun _ -> Some (lexing_date s));

value tparse = Token.default_match;

value using_token (p_con, p_prm) =
  match p_con with
  [ "" | "INT" | "ID" | "TEXT" | "EOI" -> ()
  | _ ->
      raise
        (Token.Error
           ("the constructor \"" ^ p_con ^
              "\" is not recognized by the lexer")) ]
;

value date_lexer =
  {Token.tok_func s = (make_date_lexing s, fun _ -> Token.dummy_loc);
   Token.tok_using = using_token; Token.tok_removing _ = ();
   Token.tok_match = tparse; Token.tok_text _ = "<tok>";
   Token.tok_comm = None}
;

type range 'a =
  [ Begin of 'a
  | End of 'a
  | BeginEndInt of 'a and 'a
  | BeginEndDur of 'a and 'a ]
;

value date_g = Grammar.gcreate date_lexer;
value date_value = Grammar.Entry.create date_g "date value";
value date_interval = Grammar.Entry.create date_g "date interval";
value date_value_recover = Grammar.Entry.create date_g "date value";

value is_roman_int x =
  try
    let _ = Mutil.arabian_of_roman x in
    True
  with
  [ Not_found -> False ]
;

value start_with_int x =
  try
    let s = String.sub x 0 1 in
    let _ = int_of_string s in
    True
  with
  [ _ -> False ]
;

value roman_int =
  let p =
    parser [: `("ID", x) when is_roman_int x :] -> Mutil.arabian_of_roman x
  in
  Grammar.Entry.of_parser date_g "roman int" p
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
            [ DayMonthDates -> do { check_month m; (d, m) }
            | MonthDayDates -> do { check_month d; (m, d) }
            | _ ->
                if d >= 1 && m >= 1 && d <= 31 && m <= 31 then
                  if d > 13 && m <= 13 then (d, m)
                  else if m > 13 && d <= 13 then (m, d)
                  else if d > 13 && m > 13 then (0, 0)
                  else do {
                    month_number_dates.val :=
                      MonthNumberHappened date_str.val;
                    (0, 0)
                  }
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

value recover_date cal =
  fun
  [ Dgreg d Dgregorian ->
      let d =
        match cal with
        [ Dgregorian -> d
        | Djulian -> Calendar.gregorian_of_julian d
        | Dfrench -> Calendar.gregorian_of_french d
        | Dhebrew -> Calendar.gregorian_of_hebrew d ]
      in
      Dgreg d cal
  | d -> d ]
;

EXTEND
  GLOBAL: date_value date_interval date_value_recover;
  date_value:
    [ [ d = date_or_text; EOI -> d ] ]
  ;
  date_value_recover:
    [ [ "@"; "#"; ID "DGREGORIAN"; "@"; d = date_value ->
          recover_date Dgregorian d
      | "@"; "#"; ID "DJULIAN"; "@"; d = date_value ->
          recover_date Djulian d
      | "@"; "#"; ID "DFRENCH"; ID "R"; "@"; d = date_value ->
          recover_date Dfrench d
      | "@"; "#"; ID "DHEBREW"; "@"; d = date_value ->
          recover_date Dhebrew d ] ]
  ;
  date_interval:
    [ [ ID "BEF"; dt = date_or_text; EOI -> End dt
      | ID "AFT"; dt = date_or_text; EOI -> Begin dt
      | ID "BET"; dt = date_or_text; ID "AND"; dt1 = date_or_text; EOI ->
          BeginEndInt dt dt1
      | ID "TO"; dt = date_or_text; EOI -> End dt
      | ID "FROM"; dt = date_or_text; EOI -> Begin dt
      | ID "FROM"; dt = date_or_text; ID "TO"; dt1 = date_or_text; EOI ->
          BeginEndDur dt dt1
      | dt = date_or_text; EOI -> Begin dt ] ]
  ;
  date_or_text:
    [ [ dr = date_range ->
          match dr with
          [ Begin (d, cal) -> Dgreg {(d) with prec = After} cal
          | End (d, cal) -> Dgreg {(d) with prec = Before} cal
          | BeginEndInt (d1, cal1) (d2, cal2) ->
              let dmy2 =
                match cal2 with
                [ Dgregorian ->
                    {day2 = d2.day; month2 = d2.month;
                     year2 = d2.year; delta2 = 0}
                | Djulian ->
                    let dmy2 = Calendar.julian_of_gregorian d2 in
                    {day2 = dmy2.day; month2 = dmy2.month;
                     year2 = dmy2.year; delta2 = 0}
                | Dfrench ->
                    let dmy2 = Calendar.french_of_gregorian d2 in
                    {day2 = dmy2.day; month2 = dmy2.month;
                     year2 = dmy2.year; delta2 = 0}
                | Dhebrew ->
                    let dmy2 = Calendar.hebrew_of_gregorian d2 in
                    {day2 = dmy2.day; month2 = dmy2.month;
                     year2 = dmy2.year; delta2 = 0} ]
              in
              Dgreg {(d1) with prec = YearInt dmy2} cal1
          | BeginEndDur (d1, cal1) (d2, cal2) ->
              let dmy2 =
                match cal2 with
                [ Dgregorian ->
                    {day2 = d2.day; month2 = d2.month;
                     year2 = d2.year; delta2 = 0}
                | Djulian ->
                    let dmy2 = Calendar.julian_of_gregorian d2 in
                    {day2 = dmy2.day; month2 = dmy2.month;
                     year2 = dmy2.year; delta2 = 0}
                | Dfrench ->
                    let dmy2 = Calendar.french_of_gregorian d2 in
                    {day2 = dmy2.day; month2 = dmy2.month;
                     year2 = dmy2.year; delta2 = 0}
                | Dhebrew ->
                    let dmy2 = Calendar.hebrew_of_gregorian d2 in
                    {day2 = dmy2.day; month2 = dmy2.month;
                     year2 = dmy2.year; delta2 = 0} ]
              in
              Dgreg {(d1) with prec = YearDur dmy2} cal1 ]
      | (d, cal) = date -> Dgreg d cal
      | s = TEXT -> Dtext s ] ]
  ;
  date_range:
    [ [ ID "BEF"; dt = date -> End dt
      | ID "AFT"; dt = date -> Begin dt
      | ID "BET"; dt = date; ID "AND"; dt1 = date -> BeginEndInt dt dt1
      | ID "TO"; dt = date -> End dt
      | ID "FROM"; dt = date -> Begin dt
      | ID "FROM"; dt = date; ID "TO"; dt1 = date -> BeginEndDur dt dt1 ] ]
  ;
  date:
    [ [ ID "ABT"; (d, cal) = date_calendar -> ({(d) with prec = About}, cal)
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
    [ [ LIST0 "."; n1 = OPT int; LIST0 [ "." | "/" ]; n2 = OPT gen_month;
        LIST0 [ "." | "/" ]; n3 = OPT int; LIST0 "." ->
          make_date n1 n2 n3 ] ]
  ;
  date_fren:
    [ [ LIST0 "."; n1 = int; (n2, n3) = date_fren_kont ->
          make_date (Some n1) n2 n3
      | LIST0 "."; n1 = year_fren -> make_date (Some n1) None None
      | LIST0 "."; (n2, n3) = date_fren_kont -> make_date None n2 n3 ] ]
  ;
  date_fren_kont:
    [ [ LIST0 [ "." | "/" ]; n2 = OPT gen_french; LIST0 [ "." | "/" ];
        n3 = OPT year_fren; LIST0 "." ->
          (n2, n3) ] ]
  ;
  date_hebr:
    [ [ LIST0 "."; n1 = OPT int; LIST0 [ "." | "/" ]; n2 = OPT gen_hebr;
        LIST0 [ "." | "/" ]; n3 = OPT int; LIST0 "." ->
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
    [ [ i = INT ->
          try int_of_string i with [ Failure _ -> raise Stream.Failure ]
      | "-"; i = INT ->
          try (- int_of_string i) with
          [ Failure _ -> raise Stream.Failure ] ] ]
  ;
END;

value date_of_field pos d =
  if d = "" then None
  else do {
    let s = Stream.of_string (String.uppercase d) in
    date_str.val := d;
    try Some (Grammar.Entry.parse date_value s) with
    [ Ploc.Exc loc (Stream.Error _) ->
        let s = Stream.of_string (String.uppercase d) in
        try Some (Grammar.Entry.parse date_value_recover s) with
        [ Ploc.Exc loc (Stream.Error _) -> Some (Dtext d) ] ]
  }
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
    g_godp : mutable list (Adef.iper * Adef.iper);
    g_prelated : mutable list (Adef.iper * Adef.iper);
    g_frelated : mutable list (Adef.iper * Adef.iper);
    g_witn : mutable list (Adef.ifam * Adef.iper) }
;

value assume_tab name tab none =
  if tab.tlen = Array.length tab.arr then do {
    let new_len = 2 * Array.length tab.arr + 1 in
    let new_arr = Array.make new_len none in
    Array.blit tab.arr 0 new_arr 0 (Array.length tab.arr);
    tab.arr := new_arr
  }
  else ()
;

value add_string gen s =
  try Hashtbl.find gen.g_hstr s with
  [ Not_found ->
      let i = gen.g_str.tlen in
      do {
        assume_tab "gen.g_str" gen.g_str "";
        gen.g_str.arr.(i) := s;
        gen.g_str.tlen := gen.g_str.tlen + 1;
        Hashtbl.add gen.g_hstr s (Adef.istr_of_int i);
        Adef.istr_of_int i
      } ]
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
      do {
        assume_tab "gen.g_per" gen.g_per (Left3 "");
        gen.g_per.arr.(i) := Left3 lab;
        gen.g_per.tlen := gen.g_per.tlen + 1;
        Hashtbl.add gen.g_hper lab (Adef.iper_of_int i);
        Adef.iper_of_int i
      } ]
;

value fam_index gen lab =
  let lab = extract_addr lab in
  try Hashtbl.find gen.g_hfam lab with
  [ Not_found ->
      let i = gen.g_fam.tlen in
      do {
        assume_tab "gen.g_fam" gen.g_fam (Left3 "");
        gen.g_fam.arr.(i) := Left3 lab;
        gen.g_fam.tlen := gen.g_fam.tlen + 1;
        Hashtbl.add gen.g_hfam lab (Adef.ifam_of_int i);
        Adef.ifam_of_int i
      } ]
;

value string_empty = Adef.istr_of_int 0;
value string_quest = Adef.istr_of_int 1;
value string_x = Adef.istr_of_int 2;

value unknown_per gen i sex =
  let empty = string_empty in
  let what = string_quest in
  let p =
    person_of_gen_person
      {first_name = what; surname = what; occ = i; public_name = empty;
       image = empty; qualifiers = []; aliases = []; first_names_aliases = [];
       surnames_aliases = []; titles = []; rparents = []; related = [];
       occupation = empty; sex = sex; access = IfTitles;
       birth = Adef.codate_None; birth_place = empty; birth_note = empty;
       birth_src = empty; baptism = Adef.codate_None; baptism_place = empty;
       baptism_note = empty; baptism_src = empty; death = DontKnowIfDead;
       death_place = empty; death_note = empty; death_src = empty;
       burial = UnknownBurial; burial_place = empty; burial_note = empty;
       burial_src = empty; pevents = []; notes = empty; psources = empty;
       key_index = Adef.iper_of_int i}
  and a = ascend_of_gen_ascend {parents = None; consang = Adef.fix (-1)}
  and u = union_of_gen_union {family = [| |]} in
  (p, a, u)
;

value phony_per gen sex =
  let i = gen.g_per.tlen in
  let (person, ascend, union) = unknown_per gen i sex in
  do {
    assume_tab "gen.g_per" gen.g_per (Left3 "");
    gen.g_per.tlen := gen.g_per.tlen + 1;
    gen.g_per.arr.(i) := Right3 person ascend union;
    Adef.iper_of_int i
  }
;

value unknown_fam gen i =
  let empty = string_empty in
  let father = phony_per gen Male in
  let mother = phony_per gen Female in
  let f =
    family_of_gen_family
      {marriage = Adef.codate_None; marriage_place = empty;
       marriage_note = empty; marriage_src = empty; witnesses = [| |];
       relation = relation_status.val; divorce = NotDivorced;
       fevents = []; comment = empty; origin_file = empty; fsources = empty;
       fam_index = Adef.ifam_of_int i}
  and c = couple_of_gen_couple (couple False father mother)
  and d = descend_of_gen_descend {children = [| |]} in
  (f, c, d)
;

value phony_fam gen =
  let i = gen.g_fam.tlen in
  let (fam, cpl, des) = unknown_fam gen i in
  do {
    assume_tab "gen.g_fam" gen.g_fam (Left3 "");
    gen.g_fam.tlen := gen.g_fam.tlen + 1;
    gen.g_fam.arr.(i) := Right3 fam cpl des;
    Adef.ifam_of_int i
  }
;

value this_year =
  let tm = Unix.localtime (Unix.time ()) in
  tm.Unix.tm_year + 1900
;

value infer_death birth bapt =
  match (birth, bapt) with
  [ (Some (Dgreg d _), _) ->
      let a = this_year - d.year in
      if a > dead_years.val then DeadDontKnowWhen
      else if a <= alive_years.val then NotDead
      else DontKnowIfDead
  | (_, Some (Dgreg d _)) ->
      let a = this_year - d.year in
      if a > dead_years.val then DeadDontKnowWhen
      else if a <= alive_years.val then NotDead
      else DontKnowIfDead
  | _ -> DontKnowIfDead ]
;

(* Fonctions utiles pour la mise en forme des noms. *)

(* Hashtbl (utf8.ml) qui font la correspondance entre : *)
(*   - l'encoding -> le nom                             *)
(*   - le nom     -> l'encoding                         *)
value (ht_e_n, ht_n_e) =
  do {
    let ht_e_n = Hashtbl.create 5003 in
    let ht_n_e = Hashtbl.create 5003 in
    List.iter
      (fun (encoding, name) ->
        do {
          Hashtbl.add ht_n_e name encoding;
          Hashtbl.add ht_e_n encoding name;
        })
      Utf8List.utf8_list;
    (ht_e_n, ht_n_e)
};

value string_ini_eq s1 i s2 =
  loop i 0 where rec loop i j =
    if j = String.length s2 then True
    else if i = String.length s1 then False
    else if s1.[i] = s2.[j] then loop (i + 1) (j + 1)
    else False
;

value particle s i =
  let particles =
    ["af "; "d'"; "d’"; "dal "; "de "; "des "; "di "; "du "; "of ";
     "van "; "von und zu "; "von "; "y "; "zu "; "zur ";
     "AF "; "D'"; "D’"; "DAL "; "DE "; "DES "; "DI "; "DU "; "OF ";
     "VAN "; "VON UND ZU "; "VON "; "Y "; "ZU "; "ZUR "]
  in
  List.exists (string_ini_eq s i) particles
;

value look_like_a_number s =
  loop 0 where rec loop i =
    if i = String.length s then True
    else
      match s.[i] with
      [ '0'..'9' -> loop (i + 1)
      | _ -> False ]
;

value is_a_name_char =
  fun
  [ 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | ''' -> True
  | c -> Char.code c > 127 ]
;

value rec next_word_pos s i =
  if i = String.length s then i
  else if is_a_name_char s.[i] then i
  else next_word_pos s (i + 1)
;

value rec next_sep_pos s i =
  if i = String.length s then String.length s
  else if is_a_name_char s.[i] then next_sep_pos s (i + 1)
  else i
;

value public_name_word =
  ["Ier"; "Ière"; "der"; "den"; "die"; "el"; "le"; "la"; "the"]
;

value rec is_a_public_name s i =
  let i = next_word_pos s i in
  if i = String.length s then False
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

value gen_lowercase_uppercase_utf8_letter lower s =
  (* liste des code hexa correspondant à l'encodage du caractère e. *)
  let list_of_encodings e =
    let rec loop len e l =
      if e = "" then l
      else
        let i = String.index e '/' in
        let j =
          try String.index_from e (i+1) '/'
          with [ Not_found -> String.length e ]
        in
        let k = "0" ^ String.sub e (i+1) (j-1) in
        loop (len+1)
          (String.sub e j (String.length e - j)) [int_of_string k :: l]
    in
    let l = loop 0 e [] in
    List.rev l
  in
  (* l'encodage du caractère s. *)
  let encoding =
    loop 0 s "" where rec loop i s e =
      if i = String.length s then e
      else
        let e = e ^ Printf.sprintf "/x%x" (Char.code s.[i]) in
        loop (i + 1) s e
  in
  try
    let name = Hashtbl.find ht_e_n encoding in
    let name =
      if lower then Str.replace_first (Str.regexp "CAPITAL") "SMALL" name
      else Str.replace_first (Str.regexp "SMALL") "CAPITAL" name
    in
    let new_encoding = Hashtbl.find ht_n_e name in
    let (el, len) =
      let l = list_of_encodings new_encoding in
      (l, List.length l)
    in
    let s = Bytes.create len in
    loop 0 el s where rec loop i el s =
      match el with
      [ [] -> Bytes.unsafe_to_string s
      | [e :: ell] ->
          let _s = Bytes.set s i (Char.chr e) in
          loop (i + 1) ell s ]
  with [ Not_found -> s ]
;

value lowercase_utf8_letter = gen_lowercase_uppercase_utf8_letter True;
value uppercase_utf8_letter = gen_lowercase_uppercase_utf8_letter False;

value capitalize_word s =
  copy False 0 0 (particle s 0) where rec copy special i len uncap =
    if i = String.length s then Buff.get len
    else
      match s.[i] with
      [ 'a'..'z' as c ->
          let c =
            if uncap then c
            else Char.chr (Char.code c - Char.code 'a' + Char.code 'A')
          in
          copy False (i + 1) (Buff.store len c) True
      | 'A'..'Z' as c ->
          let c =
            if not uncap then c
            else Char.chr (Char.code c - Char.code 'A' + Char.code 'a')
          in
          copy False (i + 1) (Buff.store len c) True
      | c ->
          if Char.code c < 128 then
            copy False (i + 1) (Buff.store len c) (particle s (i+1))
          else
            let nbc = Name.nbc s.[i] in
            if nbc = 1 || nbc < 0 || i + nbc > String.length s then
              copy False (i + 1) (Buff.store len s.[i]) True
            else
              let s = String.sub s i nbc in
              let s =
                if not uncap then uppercase_utf8_letter s
                else lowercase_utf8_letter s
              in
              let (t, j) = (s, i + nbc) in
              copy False j (Buff.mstore len t) True ]
;

value uppercase_word s =
  copy False 0 0 (particle s 0) where rec copy special i len uncap =
    if i = String.length s then Buff.get len
    else
      match s.[i] with
      [ 'a'..'z' as c ->
          let c =
            if uncap then c
            else Char.chr (Char.code c - Char.code 'a' + Char.code 'A')
          in
          copy False (i + 1) (Buff.store len c) uncap
      | 'A'..'Z' as c ->
          let c =
            if not uncap then c
            else Char.chr (Char.code c - Char.code 'A' + Char.code 'a')
          in
          copy False (i + 1) (Buff.store len c) uncap
      | c ->
          if Char.code c < 128 then
            copy False (i + 1) (Buff.store len c) (particle s (i+1))
          else
            let nbc = Name.nbc s.[i] in
            if nbc = 1 || nbc < 0 || i + nbc > String.length s then
              copy False (i + 1) (Buff.store len s.[i]) False
            else
              let s = String.sub s i nbc in
              let s = if uncap then s else uppercase_utf8_letter s in
              let (t, j) = (s, i + nbc) in
              copy False j (Buff.mstore len t) False ]
;

module Buff2 = Buff.Make (struct end);

value capitalize_name s =
  (* On initialise le buffer à la valeur de s. *)
  let _ = Buff2.mstore 0 s in
  loop 0 0 where rec loop len k =
    let i = next_word_pos s k in
    if i = String.length s then Buff2.get (String.length s)
    else
      let j = next_sep_pos s i in
      if j > i then
        let w = String.sub s i (j - i) in
        let w =
          if is_roman_int w || particle s i || List.mem w public_name_word ||
             start_with_int w
          then w
          else capitalize_word w
        in
        let len =
          loop len k where rec loop len k =
            if k = i then len else loop (Buff2.store len s.[k]) (k + 1)
        in
        loop (Buff2.mstore len w) j
      else Buff2.get len
;

value uppercase_name s =
  (* On initialise le buffer à la valeur de s. *)
  let _ = Buff2.mstore 0 s in
  loop 0 0 where rec loop len k =
    let i = next_word_pos s k in
    if i = String.length s then Buff2.get (String.length s)
    else
      let j = next_sep_pos s i in
      if j > i then
        let w = String.sub s i (j - i) in
        let w =
          if is_roman_int w || particle s i || List.mem w public_name_word ||
             start_with_int w
          then w
          else uppercase_word w
        in
        let len =
          loop len k where rec loop len k =
            if k = i then len else loop (Buff2.store len s.[k]) (k + 1)
        in
        loop (Buff2.mstore len w) j
      else Buff2.get len
;

value get_lev0 =
  parser
    [: _ = line_start '0'; _ = skip_space; r1 = get_ident 0; r2 = get_ident 0;
       r3 = get_to_eoln 0 ? "get to eoln";
       l = get_lev_list [] '1' ? "get lev list" :] ->
      let (rlab, rval) = if r2 = "" then (r1, "") else (r2, r1) in
      let rval = utf8_of_string rval in
      let rcont = utf8_of_string r3 in
      {rlab = rlab; rval = rval; rcont = rcont; rsons = List.rev l;
       rpos = line_cnt.val; rused = False}
;

value find_notes_record gen addr =
  match try Some (Hashtbl.find gen.g_not addr) with [ Not_found -> None ] with
  [ Some i ->
      do {
        seek_in gen.g_ic i;
        try Some (get_lev0 (Stream.of_channel gen.g_ic)) with
        [ Stream.Failure | Stream.Error _ -> None ]
      }
  | None -> None ]
;

value find_sources_record gen addr =
  match try Some (Hashtbl.find gen.g_src addr) with [ Not_found -> None ] with
  [ Some i ->
      do {
        seek_in gen.g_ic i;
        try Some (get_lev '0' (Stream.of_channel gen.g_ic)) with
        [ Stream.Failure | Stream.Error _ -> None ]
      }
  | None -> None ]
;

value rec flatten_notes =
  fun
  [ [r :: rl] ->
      let n = flatten_notes rl in
      match r.rlab with
      [ "CONC" | "CONT" | "NOTE" ->
          [(r.rlab, r.rval) :: flatten_notes r.rsons @ n]
      | _ -> n ]
  | [] -> [] ]
;

value extract_notes gen rl =
  List.fold_right
    (fun r lines ->
       List.fold_right
         (fun r lines ->
            do {
              r.rused := True;
              if r.rlab = "NOTE" && r.rval <> "" && r.rval.[0] = '@' then
                let addr = extract_addr r.rval in
                match find_notes_record gen addr with
                [ Some r ->
                    let l = flatten_notes r.rsons in
                    [("NOTE", r.rcont) :: l @ lines]
                | None ->
                    do {
                      print_location r.rpos;
                      fprintf log_oc.val "Note %s not found\n" addr;
                      flush log_oc.val;
                      lines
                    } ]
              else [(r.rlab, r.rval) :: lines]
            })
         [r :: r.rsons] lines)
    rl []
;

value rebuild_text r =
  let s = strip_spaces r.rval in
  List.fold_left
    (fun s e ->
       let _ = do { e.rused := True } in
       let n = e.rval in
       let end_spc =
         if String.length n > 1 && n.[String.length n - 1] = ' ' then " "
         else ""
       in
       let n = strip_spaces n in
       match e.rlab with
       [ "CONC" -> s ^ n ^ end_spc
       | "CONT" -> s ^ "<br>\n" ^ n ^ end_spc
       | _ -> s ])
    s r.rsons
;

value notes_from_source_record rl =
  let title =
    match find_field "TITL" rl with
    [ Some l ->
        let s = rebuild_text l in
        if s = "" then ""
        else "<b>" ^ s ^"</b>"
    | None -> "" ]
  in
  let text =
    match find_field "TEXT" rl with
    [ Some l ->
        let s = rebuild_text l in
        if title = "" then s
        else "<br>\n" ^ s
    | None -> "" ]
  in
  title ^ text
;

value treat_notes gen rl =
  let lines = extract_notes gen rl in
  let buf = Buffer.create (List.length lines) in
  let () =
    List.iter
      (fun (lab, n) ->
         let spc = String.length n > 0 && n.[0] = ' ' in
         let end_spc =
           String.length n > 1 && n.[String.length n - 1] = ' '
         in
         let n = strip_spaces n in
         if Buffer.length buf = 0 then do {
           Buffer.add_string buf n;
           Buffer.add_string buf (if end_spc then " " else "")
         }
         else if lab = "CONT" || lab = "NOTE" then do {
           Buffer.add_string buf "<br>\n";
           Buffer.add_string buf n;
           Buffer.add_string buf (if end_spc then " " else "")
         }
         else if n = "" then ()
         else do {
           Buffer.add_string buf (if spc then "\n" else "");
           Buffer.add_string buf n;
           Buffer.add_string buf (if end_spc then " " else "")
         })
      lines
  in
  strip_newlines (Buffer.contents buf)
;

value note gen r =
  match find_field "NOTE" r.rsons with
  [ Some r ->
      if String.length r.rval > 0 && r.rval.[0] = '@' then
        match find_notes_record gen r.rval with
        [ Some v -> (strip_spaces v.rcont, v.rsons)
        | None ->
            do {
              print_location r.rpos;
              fprintf log_oc.val "Note %s not found\n" r.rval;
              flush log_oc.val;
              ("", [])
            } ]
      else (strip_spaces r.rval, r.rsons)
  | _ -> ("", []) ]
;

value treat_source gen r =
  if String.length r.rval > 0 && r.rval.[0] = '@' then
    match find_sources_record gen r.rval with
    [ Some v -> (strip_spaces v.rcont, v.rsons)
    | None ->
        do {
          print_location r.rpos;
          fprintf log_oc.val "Source %s not found\n" r.rval;
          flush log_oc.val;
          ("", [])
        } ]
  else (strip_spaces r.rval, r.rsons)
;

value source gen r =
  match find_field "SOUR" r.rsons with
  [ Some r -> treat_source gen r
  | _ -> ("", []) ]
;

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
    if i1 = String.length s then ("", 0)
    else if i2 = String.length s then
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

value list_of_string s =
  loop 0 0 [] where rec loop i len list =
    if i = String.length s then List.rev [Buff.get len :: list]
    else
      match s.[i] with
      [ ',' -> loop (i + 1) 0 [Buff.get len :: list]
      | c -> loop (i + 1) (Buff.store len c) list ]
;

value purge_list list =
  List.fold_right
    (fun s list ->
       match strip_spaces s with
       [ "" -> list
       | s -> [s :: list] ])
    list []
;

value decode_date_interval pos s =
  let strm = Stream.of_string s in
  try
    match Grammar.Entry.parse date_interval strm with
    [ BeginEndInt d1 d2 -> (Some d1, Some d2)
    | BeginEndDur d1 d2 -> (Some d1, Some d2)
    | Begin d -> (Some d, None)
    | End d -> (None, Some d) ]
  with
  [ Ploc.Exc _ _ | Not_found ->
      do { print_bad_date pos s; (None, None) } ]
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

value forward_pevent_witn gen ip rval =
  let ipp = per_index gen rval in
  do { gen.g_prelated := [(ipp, ip) :: gen.g_prelated]; ipp }
;

value forward_fevent_witn gen ip rval =
  let ipp = per_index gen rval in
  do { gen.g_frelated := [(ipp, ip) :: gen.g_frelated]; ipp }
;

value find_event_witness gen tag ip r =
  let rec find_witnesses =
    fun
    [ [] -> []
    | [ r :: asso_l ] ->
        if find_field_with_value "TYPE" tag r.rsons then
          let witness = forward_pevent_witn gen ip (strip_spaces r.rval) in
          let witness_kind =
            match find_field "RELA" r.rsons with
            [ Some rr ->
                if rr.rval = "GODP" then Witness_GodParent
                else Witness
            | _ -> Witness ]
          in
          [ (witness, witness_kind) :: find_witnesses asso_l ]
        else do {
          let witness = forward_pevent_witn gen ip (strip_spaces r.rval) in
          let witness_kind =
            match find_field "RELA" r.rsons with
            [ Some rr ->
                if rr.rval = "GODP" then Witness_GodParent
                else Witness
            | _ -> Witness ]
          in
          [ (witness, witness_kind) :: find_witnesses asso_l ]
        } ]
  in
  let witnesses =
    match find_all_fields "ASSO" r.rsons with
    [ [] -> []
    | wl -> find_witnesses wl ]
  in
  Array.of_list witnesses
;

value find_fevent_witness gen tag ifath imoth r =
  let rec find_witnesses =
    fun
    [ [] -> []
    | [ r :: asso_l ] ->
        if find_field_with_value "TYPE" tag r.rsons then
          let witness = forward_fevent_witn gen ifath (strip_spaces r.rval) in
          let witness_kind =
            match find_field "RELA" r.rsons with
            [ Some rr ->
                if rr.rval = "GODP" then Witness_GodParent
                else Witness
            | _ -> Witness ]
          in
          [ (witness, witness_kind) :: find_witnesses asso_l ]
        else do {
          let witness = forward_fevent_witn gen ifath (strip_spaces r.rval) in
          let witness_kind =
            match find_field "RELA" r.rsons with
            [ Some rr ->
                if rr.rval = "GODP" then Witness_GodParent
                else Witness
            | _ -> Witness ]
          in
          [ (witness, witness_kind) :: find_witnesses asso_l ]
        } ]
  in
  let witnesses =
    match find_all_fields "ASSO" r.rsons with
    [ [] -> []
    | wl -> find_witnesses wl ]
  in
  Array.of_list witnesses
;

value find_pevent_name_from_tag gen tag tagv =
  match tag with
  [ "BIRT" -> Epers_Birth
  | "BAPM" | "CHR" -> Epers_Baptism
  | "DEAT" -> Epers_Death
  | "BURI" -> Epers_Burial
  | "CREM" -> Epers_Cremation
  | "accomplishment" -> Epers_Accomplishment
  | "acquisition" -> Epers_Acquisition
  | "award" | "distinction" -> Epers_Distinction
  | "BAPL" | "lds baptism" -> Epers_BaptismLDS
  | "BARM" -> Epers_BarMitzvah
  | "BASM" -> Epers_BatMitzvah
  | "BLES" -> Epers_Benediction
  | "CENS" -> Epers_Recensement
  | "circumcision" -> Epers_Circumcision
  | "CONF" -> Epers_Confirmation
  | "CONL" | "lds confirmation" -> Epers_ConfirmationLDS
  | "degree" -> Epers_Diploma
  | "DECO" -> Epers_Decoration
  | "lds dotation" | "lds endowment" -> Epers_DotationLDS
  | "EDUC" -> Epers_Education
  | "election" -> Epers_Election
  | "EMIG" -> Epers_Emigration
  | "ENDL" -> Epers_Dotation
  | "excommunication" -> Epers_Excommunication
  | "family link lds" -> Epers_FamilyLinkLDS
  | "FCOM" -> Epers_FirstCommunion
  | "funeral" -> Epers_Funeral
  | "GRAD" -> Epers_Graduate
  | "hospitalization" -> Epers_Hospitalisation
  | "illness" -> Epers_Illness
  | "IMMI" -> Epers_Immigration
  | "membership" -> Epers_Adhesion
  | "military discharge" -> Epers_DemobilisationMilitaire
  | "military distinction" -> Epers_MilitaryDistinction
  | "military promotion" -> Epers_MilitaryPromotion
  | "military service" -> Epers_MilitaryService
  | "military mobilization" -> Epers_MobilisationMilitaire
  | "change name" -> Epers_ChangeName
  | "NATU" -> Epers_Naturalisation
  | "OCCU" | "occupation" -> Epers_Occupation
  | "ORDN" -> Epers_Ordination
  | "passenger list" -> Epers_ListePassenger
  | "PROP" -> Epers_Property
  | "RESI" | "residence" -> Epers_Residence
  | "RETI" -> Epers_Retired
  | "scellent parent lds" -> Epers_ScellentParentLDS
  | "SLGC" | "lds sealing child" -> Epers_ScellentChildLDS
  | "SLGS" | "lds sealing spouse" -> Epers_ScellentSpouseLDS
  | "property sale" -> Epers_VenteBien
  | "WILL" -> Epers_Will
  | _ -> Epers_Name (add_string gen (strip_spaces tagv)) ]
;

value primary_pevents =
  [ "BAPM"; "CHR"; "BAPL"; "BARM"; "BASM"; "BIRT"; "BLES"; "BURI";
    "CENS"; "CONF"; "CONL"; "CREM"; "DEAT"; "DECO"; "EDUC"; "EMIG";
    "ENDL"; "FCOM"; "GRAD"; "IMMI"; "NATU"; "OCCU"; "ORDN"; "PROP";
    "RETI"; "RESI"; "SLGS"; "SLGC"; "WILL" ]
;

value treat_indi_pevent gen ip r =
  let prim_events =
    List.fold_left
      (fun events tag ->
        List.fold_left
          (fun events r ->
            let name = find_pevent_name_from_tag gen tag tag in
            let date =
              match find_field "DATE" r.rsons with
              [ Some r -> date_of_field r.rpos r.rval
              | None -> None ]
            in
            let place =
              match find_field "PLAC" r.rsons with
              [ Some r -> strip_spaces r.rval
              | _ -> "" ]
            in
            let reason = "" in
            let note =
              match find_all_fields "NOTE" r.rsons with
              [ [] -> ""
              | rl -> treat_notes gen rl ]
            in
            (* Si le tag 1 XXX a des infos, on les ajoutes. *)
            let note =
              let name_info = strip_spaces r.rval in
              if name_info = "" || r.rval = "Y" then note
              else name_info ^ "<br>\n" ^ note
            in
            let src =
              match find_all_fields "SOUR" r.rsons with
              [ [] -> ""
              | rl ->
                  loop True "" rl where rec loop first src rl =
                    match rl with
                    [ [] -> src
                    | [ r :: rl ] ->
                        let (src_cont, _) = treat_source gen r in
                        let src =
                          if first then src ^ src_cont else src ^ " " ^ src_cont
                        in
                        loop False src rl ]]
            in
            let witnesses = find_event_witness gen "INDI" ip r in
            let evt =
              {epers_name = name; epers_date = Adef.codate_of_od date;
               epers_place = add_string gen place;
               epers_reason = add_string gen reason;
               epers_note = add_string gen note;
               epers_src = add_string gen src;
               epers_witnesses = witnesses}
            in
            (* On ajoute que les évènements non vides, sauf *)
            (* s'il est spécifié qu'il faut l'ajouter.      *)
            if date <> None || place <> "" || note <> "" ||
               src <> "" || witnesses <> [| |] || r.rval = "Y"
            then
              (* On ajoute les professions que s'il y a des info en plus. *)
              if name = Epers_Occupation then
                if r.rsons <> [] then [ evt :: events ]
                else events
              else [ evt :: events ]
            else events )
          events (find_all_fields tag r.rsons) )
      [] primary_pevents
  in
  let second_events =
    List.fold_left
      (fun events r ->
         match find_field "TYPE" r.rsons with
         [ Some rr ->
             if rr.rval <> "" then
               let name =
                 if List.mem rr.rval primary_pevents then
                   find_pevent_name_from_tag gen rr.rval rr.rval
                 else
                   find_pevent_name_from_tag gen (String.lowercase rr.rval) rr.rval
               in
               let date =
                 match find_field "DATE" r.rsons with
                 [ Some r -> date_of_field r.rpos r.rval
                 | None -> None ]
               in
               let place =
                 match find_field "PLAC" r.rsons with
                 [ Some r -> strip_spaces r.rval
                 | _ -> "" ]
               in
               let reason = "" in
               let note =
                 match find_all_fields "NOTE" r.rsons with
                 [ [] -> ""
                 | rl -> treat_notes gen rl ]
               in
               (* Si le tag 1 XXX a des infos, on les ajoutes. *)
               let note =
                 let name_info = strip_spaces r.rval in
                 if name_info = "" || r.rval = "Y" then note
                 else name_info ^ "<br>\n" ^ note
               in
               let src =
                 match find_all_fields "SOUR" r.rsons with
                 [ [] -> ""
                 | rl ->
                     loop True "" rl where rec loop first src rl =
                       match rl with
                       [ [] -> src
                       | [ r :: rl ] ->
                           let (src_cont, _) = treat_source gen r in
                           let src =
                             if first then src ^ src_cont else src ^ " " ^ src_cont
                           in
                           loop False src rl ]]
               in
               let witnesses = find_event_witness gen "INDI" ip r in
               let evt =
                 {epers_name = name; epers_date = Adef.codate_of_od date;
                  epers_place = add_string gen place;
                  epers_reason = add_string gen reason;
                  epers_note = add_string gen note;
                  epers_src = add_string gen src;
                  epers_witnesses = witnesses}
               in
               (* On ajoute que les évènements non vides, *)
               (* sauf si évènement personnalisé !        *)
               let has_epers_name =
                 match name with
                 [ Epers_Name n -> n <> string_empty
                 | _ -> False ]
               in
               if has_epers_name || date <> None || place <> "" || note <> "" ||
                  src <> "" || witnesses <> [| |]
               then
                 [ evt :: events ]
               else events
             else events
         | None -> events ] )
      [] (find_all_fields "EVEN" r.rsons)
  in
  List.rev_append prim_events second_events
;

value forward_adop gen ip lab which_parent =
  let which_parent =
    match which_parent with
    [ Some r -> r.rval
    | _ -> "" ]
  in
  let which_parent = if which_parent = "" then "BOTH" else which_parent in
  Hashtbl.add gen.g_adop lab (ip, which_parent)
;

value adop_parent gen ip r =
  let i = per_index gen r.rval in
  match gen.g_per.arr.(Adef.int_of_iper i) with
  [ Left3 _ -> None
  | Right3 p a u ->
      do {
        if List.mem ip (get_related p) then ()
        else
          let p = person_with_related p [ip :: get_related p] in
          gen.g_per.arr.(Adef.int_of_iper i) := Right3 p a u;
        Some (get_key_index p)
      } ]
;

value set_adop_fam gen ip which_parent fath moth =
  match gen.g_per.arr.(Adef.int_of_iper ip) with
  [ Left3 _ -> ()
  | Right3 per asc uni ->
      let r_fath =
        match (which_parent, fath) with
        [ ("HUSB" | "BOTH", Some r) -> adop_parent gen ip r
        | _ -> None ]
      in
      let r_moth =
        match (which_parent, moth) with
        [ ("WIFE" | "BOTH", Some r) -> adop_parent gen ip r
        | _ -> None ]
      in
      let r =
        {r_type = Adoption; r_fath = r_fath; r_moth = r_moth;
         r_sources = string_empty}
      in
      let per = person_with_rparents per [r :: get_rparents per] in
      gen.g_per.arr.(Adef.int_of_iper ip) := Right3 per asc uni ]
;

value forward_godp gen ip rval =
  let ipp = per_index gen rval in
  do { gen.g_godp := [(ipp, ip) :: gen.g_godp]; ipp }
;

value forward_witn gen ip rval =
  let ifam = fam_index gen rval in
  do { gen.g_witn := [(ifam, ip) :: gen.g_witn]; ifam }
;

value glop = ref [];

value indi_lab =
  fun
  [ "ADOP" | "ASSO" | "BAPM" | "BIRT" | "BURI" | "CHR" | "CREM" | "DEAT" |
    "FAMC" | "FAMS" | "NAME" | "NOTE" | "OBJE" | "OCCU" | "SEX" | "SOUR" |
    "TITL" ->
      True
  | c ->
      do {
        if List.mem c glop.val then ()
        else do {
          glop.val := [c :: glop.val];
          eprintf "untreated tag %s -> in notes\n" c;
          flush stderr
        };
        False
      } ]
;

value html_text_of_tags text rl =
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
  let title =
    if text = "" then "-- GEDCOM --"
    else "-- GEDCOM (" ^ text ^ ") --"
  in
  let len = 0 in
  let len = Buff.mstore len title in
  let len = totl len 1 rl in
  Buff.get len
;

value rec find_all_rela nl =
  fun
  [ [] -> []
  | [r :: rl] ->
      match find_field "RELA" r.rsons with
      [ Some r1 ->
          loop nl where rec loop =
            fun
            [ [n :: nl1] ->
                let len = String.length n in
                if String.length r1.rval >= len &&
                   String.lowercase (String.sub r1.rval 0 len) = n
                then [(n, r.rval) :: find_all_rela nl rl]
                else loop nl1
            | [] -> find_all_rela nl rl ]
      | None -> find_all_rela nl rl ] ]
;

value find_event_witness gen r =
  let rec find_witnesses =
    fun
    [ [] -> []
    | [ r :: asso_l ] ->
        if find_field_with_value "TYPE" "INDI" r.rsons then
          let witness = per_index gen r.rval in
          [ witness :: find_witnesses asso_l ]
        else do {
          r.rused := False;
          find_witnesses asso_l
        } ]
  in
  let witnesses =
    match find_all_fields "ASSO" r.rsons with
    [ [] -> []
    | wl -> find_witnesses wl ]
  in
  let witnesses = List.map (fun ip -> (ip, Witness)) witnesses in
  Array.of_list witnesses
;

value find_pevent_name_from_tag gen tag =
  match tag with
  [ "BIRT" -> Epers_Birth
  | "BAPT" -> Epers_Baptism
  | "DEAT" -> Epers_Death
  | "BURI" -> Epers_Burial
  | "CREM" -> Epers_Cremation
  | "accomplishment" -> Epers_Accomplishment
  | "acquisition" -> Epers_Acquisition
  | "award" -> Epers_Decoration
  | "BAPL" -> Epers_BaptismLDS
  | "BARM" -> Epers_BarMitzvah
  | "BASM" -> Epers_BatMitzvah
  | "BLES" -> Epers_Benediction
  | "CENS" -> Epers_Recensement
  | "circumcision" -> Epers_Circumcision
  | "CONF" -> Epers_Confirmation
  | "CONL" -> Epers_ConfirmationLDS
  | "degree" -> Epers_Diploma
  | "distinction" -> Epers_Distinction
  | "dotation LDS" -> Epers_DotationLDS
  | "EDUC" -> Epers_Education
  | "election" -> Epers_Election
  | "EMIG" -> Epers_Emigration
  | "ENDL" -> Epers_Dotation
  | "excommunication" -> Epers_Excommunication
  | "family link LDS" -> Epers_FamilyLinkLDS
  | "FCOM" -> Epers_FirstCommunion
  | "funeral" -> Epers_Funeral
  | "GRAD" -> Epers_Graduate
  | "hospitalization" -> Epers_Hospitalisation
  | "Illness" -> Epers_Illness
  | "IMMI" -> Epers_Immigration
  | "membership" -> Epers_Adhesion
  | "military discharge" -> Epers_DemobilisationMilitaire
  | "military distinction" -> Epers_MilitaryDistinction
  | "military promotion" -> Epers_MilitaryPromotion
  | "military service" -> Epers_MilitaryService
  | "mobilisation militaire" -> Epers_MobilisationMilitaire
  | "name change" -> Epers_ChangeName
  | "NATU" -> Epers_Naturalisation
  | "occupation" -> Epers_Occupation
  | "ORDN" -> Epers_Ordination
  | "passenger list" -> Epers_ListePassenger
  | "PROP" -> Epers_Property
  | "RESI" -> Epers_Residence
  | "RETI" -> Epers_Retired
  | "scellent parent LDS" -> Epers_ScellentParentLDS
  | "SLGC" -> Epers_ScellentChildLDS
  | "SLGS" -> Epers_ScellentSpouseLDS
  | "property sale" -> Epers_VenteBien
  | "WILL" -> Epers_Will
  | _ -> Epers_Name (add_string gen (strip_spaces tag)) ]
;

value primary_pevents =
  [ "BAPT"; "BAPL"; "BARM"; "BASM"; "BIRT"; "BLES"; "BURI";
    "CENS"; "CONF"; "CONL"; "CREM"; "DEAT"; "EDUC"; "EMIG";
    "ENDL"; "FCOM"; "GRAD"; "IMMI"; "NATU"; "ORDN"; "PROP";
    "RETI"; "RESI"; "SLGS"; "SLGC"; "WILL" ]
;

value treat_indi_pevent gen r =
  let prim_events =
    List.fold_left
      (fun events tag ->
        List.fold_left
          (fun events r ->
            let name = find_pevent_name_from_tag gen tag in
            let date =
              match find_field "DATE" r.rsons with
              [ Some r -> date_of_field r.rpos r.rval
              | None -> None ]
            in
            let place =
              match find_field "PLAC" r.rsons with
              [ Some r -> strip_spaces r.rval
              | _ -> "" ]
            in
            let reason = "" in
            let note =
              match find_all_fields "NOTE" r.rsons with
              [ [] -> ""
              | rl -> treat_notes gen rl ]
            in
            let (src, _) = source gen r in
            let witnesses = find_event_witness gen r in
            let evt =
              {epers_name = name; epers_date = Adef.codate_of_od date;
               epers_place = add_string gen place;
               epers_reason = add_string gen reason;
               epers_note = add_string gen note;
               epers_src = add_string gen src;
               epers_witnesses = witnesses}
            in
            [ evt :: events ] )
          events (find_all_fields tag r.rsons) )
      [] primary_pevents
  in
  let second_events =
    List.fold_left
      (fun events r ->
         match find_field "TYPE" r.rsons with
         [ Some rr ->
             if rr.rval <> "" then
               let name =
                 find_pevent_name_from_tag gen (String.lowercase rr.rval)
               in
               let date =
                 match find_field "DATE" r.rsons with
                 [ Some r -> date_of_field r.rpos r.rval
                 | None -> None ]
               in
               let place =
                 match find_field "PLAC" r.rsons with
                 [ Some r -> strip_spaces r.rval
                 | _ -> "" ]
               in
               let reason = "" in
               let note =
                 match find_all_fields "NOTE" r.rsons with
                 [ [] -> ""
                 | rl -> treat_notes gen rl ]
               in
               let (src, _) = source gen r in
               let witnesses = find_event_witness gen r in
               let evt =
                 {epers_name = name; epers_date = Adef.codate_of_od date;
                  epers_place = add_string gen place;
                  epers_reason = add_string gen reason;
                  epers_note = add_string gen note;
                  epers_src = add_string gen src;
                  epers_witnesses = witnesses}
               in
               [ evt :: events ]
             else events
         | None -> events ] )
      [] (find_all_fields "EVEN" r.rsons)
  in
  List.rev_append prim_events second_events
;

value rec build_remain_tags =
  fun
  [ [] -> []
  | [r :: rest] ->
     let rsons =
       if indi_lab r.rlab then [] else build_remain_tags r.rsons
     in
      let rest = build_remain_tags rest in
      if r.rused = True && rsons = [] then rest
      else
        [{rlab = r.rlab; rval = r.rval; rcont = r.rcont; rsons = rsons;
          rpos = r.rpos; rused = r.rused} ::
          rest] ]
;

value applycase_surname s =
  match case_surnames.val with
  [ NoCase -> s
  | LowerCase -> capitalize_name s
  | UpperCase ->
      if charset.val = Utf8 then uppercase_name s
      else String.uppercase s ]
;

value reconstitute_from_pevents pevents bi bp de bu =
  let found_birth = ref False in
  let found_baptism = ref False in
  let found_death = ref False in
  let found_burial = ref False in
  loop pevents bi bp de bu where rec loop pevents bi bp de bu =
    match pevents with
    [ [] -> (bi, bp, de, bu)
    | [evt :: l] ->
        match evt.epers_name with
        [ Epers_Birth ->
            if found_birth.val then loop l bi bp de bu
            else
              let bi =
                (evt.epers_date, evt.epers_place,
                 evt.epers_note, evt.epers_src)
              in
              let () = found_birth.val := True in
              loop l bi bp de bu
        | Epers_Baptism ->
            if found_baptism.val then loop l bi bp de bu
            else
              let bp =
                (evt.epers_date, evt.epers_place,
                 evt.epers_note, evt.epers_src)
              in
              let () = found_baptism.val := True in
              loop l bi bp de bu
        | Epers_Death ->
            if found_death.val then loop l bi bp de bu
            else
              let death =
                match Adef.od_of_codate evt.epers_date with
                [ Some d -> Death Unspecified (Adef.cdate_of_date d)
                | None -> DeadDontKnowWhen ]
              in
              let de =
                (death, evt.epers_place,
                 evt.epers_note, evt.epers_src)
              in
              let () = found_death.val := True in
              loop l bi bp de bu
        | Epers_Burial ->
            if found_burial.val then loop l bi bp de bu
            else
              let bu =
                (Buried evt.epers_date, evt.epers_place,
                 evt.epers_note, evt.epers_src)
              in
              let () = found_burial.val := True in
              loop l bi bp de bu
        | Epers_Cremation ->
            if found_burial.val then loop l bi bp de bu
            else
              let bu =
                (Cremated evt.epers_date, evt.epers_place,
                 evt.epers_note, evt.epers_src)
              in
              let () = found_burial.val := True in
              loop l bi bp de bu
        | _ -> loop l bi bp de bu ] ]
;

value add_indi gen r =
  let ip = per_index gen r.rval in
  let name_sons = find_field "NAME" r.rsons in
  let givn =
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
        let pn = "" in
        let fal = if givn = f then [] else [givn] in
        let (f, fal) =
          match first_names_brackets.val with
(*
          [ Some (' ', eb) ->
              let first_enclosed f =
                let j = String.index f eb in
                let i =
                  try String.rindex_from f (j - 1) ' ' with
                  [ Not_found -> -1 ]
                in
                let fn = String.sub f (i + 1) (j - i - 1) in
                let fa =
                  String.sub f 0 j ^ fn ^
                    String.sub f (j + 1) (String.length f - j - 1)
                in
                (fn, fa)
              in
              loop True f "" where rec loop first ff accu =
                try
                  let (fn, fa) = first_enclosed ff in
                  let accu =
                    if first then fn
                    else if fn <> "" then accu ^ " " ^ fn
                    else accu
                  in
                  loop False fa accu
                with
                [ Not_found ->
                    if f = ff then (f, fal) else (accu, [ff :: fal]) ]
*)
          [ Some (bb, eb) ->
              let first_enclosed f =
                let i = String.index f bb in
                let j =
                  if i + 2 >= String.length f then raise Not_found
                  else String.index_from f (i + 2) eb
                in
                let fn = String.sub f (i + 1) (j - i - 1) in
                let fa =
                  String.sub f 0 i ^ fn ^
                    String.sub f (j + 1) (String.length f - j - 1)
                in
                (fn, fa)
              in
              loop True f "" where rec loop first ff accu =
                try
                  let (fn, fa) = first_enclosed ff in
                  let accu =
                    if first then fn
                    else if fn <> "" then accu ^ " " ^ fn
                    else accu
                  in
                  loop False fa accu
                with
                [ Not_found ->
                    if f = ff then (f, fal) else (accu, [ff :: fal]) ]
          | None -> (f, fal) ]
        in
        let (f, pn, fal) =
          if extract_public_names.val || extract_first_names.val then
            let i = next_word_pos f 0 in
            let j = next_sep_pos f i in
            if j = String.length f then (f, pn, fal)
            else
              let fn = String.sub f i (j - i) in
              if pn = "" && extract_public_names.val then
                if is_a_public_name f j then (fn, f, fal)
                else if extract_first_names.val then (fn, "", [f :: fal])
                else (f, "", fal)
              else (fn, pn, [f :: fal])
          else (f, pn, fal)
        in
        let f = if lowercase_first_names.val then capitalize_name f else f in
        let fal =
          if lowercase_first_names.val then List.map capitalize_name fal
          else fal
        in
        let pn = if capitalize_name pn = f then "" else pn in
        let pn =
          if lowercase_first_names.val then capitalize_name pn else pn
        in
        let fal =
          List.fold_right (fun fa fal -> if fa = pn then fal else [fa :: fal])
            fal []
        in
        let s = applycase_surname s in
        let r =
          let key = Name.strip_lower (nominative f ^ " " ^ nominative s) in
          try Hashtbl.find gen.g_hnam key with
          [ Not_found ->
              let r = ref (-1) in
              do { Hashtbl.add gen.g_hnam key r; r } ]
        in
        do { incr r; (f, s, r.val, pn, fal) }
    | None -> ("?", "?", Adef.int_of_iper ip, givn, []) ]
  in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    (Name.strip_c first_name ':', Name.strip_c surname ':')
  in
  (* Si le prénom ou le nom est vide *)
  let (first_name, surname) =
    (if first_name = "" then default_name.val else first_name,
     if surname = "" then default_name.val else surname)
  in
  let qualifier =
    match name_sons with
    [ Some n ->
        match find_field "NICK" n.rsons with
        [ Some r -> r.rval
        | None -> "" ]
    | None -> "" ]
  in
  let surname_aliases =
    match name_sons with
    [ Some n ->
        match find_field "SURN" n.rsons with
        [ Some r ->
            let list = purge_list (list_of_string r.rval) in
            List.fold_right
              (fun x list ->
                 let x = applycase_surname x in
                 if x <> surname then [x :: list] else list)
              list []
        | _ -> [] ]
    | None -> [] ]
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
        [ Some r -> if no_picture.val then "" else r.rval
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
    [ [r :: rl] ->
        List.fold_left (fun s r -> s ^ ", " ^ strip_spaces r.rval)
          (strip_spaces r.rval) rl
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
  let pevents = treat_indi_pevent gen r in
  let family =
    let rl = find_all_fields "FAMS" r.rsons in
    let rvl =
      List.fold_right
        (fun r rvl -> if List.mem r.rval rvl then rvl else [r.rval :: rvl]) rl
        []
    in
    List.map (fun r -> fam_index gen r) rvl
  in
  let rasso = find_all_fields "ASSO" r.rsons in
  let rparents =
    let godparents = find_all_rela ["godf"; "godm"; "godp"] rasso in
    let godparents =
      if godparents = [] then
        let ro =
          match find_field "BAPM" r.rsons with
          [ None -> find_field "CHR" r.rsons
          | x -> x ]
        in
        match ro with
        [ Some r -> find_all_rela ["godf"; "godm"; "godp"] rasso
        | None -> [] ]
      else godparents
    in
    loop godparents where rec loop rl =
      if rl <> [] then
        let (r_fath, rl) =
          match rl with
          [ [("godf", r) :: rl] -> (Some (forward_godp gen ip r), rl)
          | _ -> (None, rl) ]
        in
        let (r_moth, rl) =
          match rl with
          [ [("godm", r) :: rl] -> (Some (forward_godp gen ip r), rl)
          | _ -> (None, rl) ]
        in
        let (r_fath, r_moth, rl) =
          if r_fath <> None || r_moth <> None then (r_fath, r_moth, rl)
          else
            let (r_fath, rl) =
              match rl with
              [ [("godp", r) :: rl] -> (Some (forward_godp gen ip r), rl)
              | _ -> (None, rl) ]
            in
            (r_fath, None, rl)
        in
        let r =
          {r_type = GodParent; r_fath = r_fath; r_moth = r_moth;
           r_sources = string_empty}
        in
        [r :: loop rl]
      else []
  in
  let witn = find_all_rela ["witness"] rasso in
  let _ = List.map (fun (n, rval) -> forward_witn gen ip rval) witn in
  let (birth, birth_place, (birth_note, _), (birth_src, birth_nt)) =
    match find_field "BIRT" r.rsons with
    [ Some r ->
        let d =
          match find_field "DATE" r.rsons with
          [ Some r -> date_of_field r.rpos r.rval
          | _ -> None ]
        in
        let p =
          match find_field "PLAC" r.rsons with
          [ Some r -> strip_spaces r.rval
          | _ -> "" ]
        in
        let note =
          match find_all_fields "NOTE" r.rsons with
          [ [] -> ""
          | rl -> treat_notes gen rl ]
        in
        (d, p, (note, []), source gen r)
    | None -> (None, "", ("", []), ("", [])) ]
  in
  let (bapt, bapt_place, (bapt_note, _), (bapt_src, bapt_nt)) =
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
          [ Some r -> strip_spaces r.rval
          | _ -> "" ]
        in
        let note =
          match find_all_fields "NOTE" r.rsons with
          [ [] -> ""
          | rl -> treat_notes gen rl ]
        in
        (d, p, (note, []), source gen r)
    | None -> (None, "", ("", []), ("", [])) ]
  in
  let (death, death_place, (death_note, _), (death_src, death_nt)) =
    match find_field "DEAT" r.rsons with
    [ Some r ->
        if r.rsons = [] then
          if r.rval = "Y" then (DeadDontKnowWhen, "", ("", []), ("", []))
          else (infer_death birth bapt, "", ("", []), ("", []))
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
            [ Some r -> strip_spaces r.rval
            | _ -> "" ]
          in
          let note =
            match find_all_fields "NOTE" r.rsons with
            [ [] -> ""
            | rl -> treat_notes gen rl ]
          in
          (d, p, (note, []), source gen r)
    | None -> (infer_death birth bapt, "", ("", []), ("", [])) ]
  in
  let (burial, burial_place, (burial_note, _), (burial_src, burial_nt)) =
    let (buri, buri_place, (buri_note, _), (buri_src, buri_nt)) =
      match find_field "BURI" r.rsons with
      [ Some r ->
          if r.rsons = [] then
            if r.rval = "Y" then (Buried Adef.codate_None, "", ("", []), ("", []))
            else (UnknownBurial, "", ("", []), ("", []))
          else
            let d =
              match find_field "DATE" r.rsons with
              [ Some r -> date_of_field r.rpos r.rval
              | _ -> None ]
            in
            let p =
              match find_field "PLAC" r.rsons with
              [ Some r -> strip_spaces r.rval
              | _ -> "" ]
            in
            let note =
              match find_all_fields "NOTE" r.rsons with
              [ [] -> ""
              | rl -> treat_notes gen rl ]
            in
            (Buried (Adef.codate_of_od d), p, (note, []), source gen r)
      | None -> (UnknownBurial, "", ("", []), ("", [])) ]
    in
    let (crem, crem_place, (crem_note, _), (crem_src, crem_nt)) =
      match find_field "CREM" r.rsons with
      [ Some r ->
          if r.rsons = [] then
            if r.rval = "Y" then (Cremated Adef.codate_None, "", ("", []), ("", []))
            else (UnknownBurial, "", ("", []), ("", []))
          else
            let d =
              match find_field "DATE" r.rsons with
              [ Some r -> date_of_field r.rpos r.rval
              | _ -> None ]
            in
            let p =
              match find_field "PLAC" r.rsons with
              [ Some r -> strip_spaces r.rval
              | _ -> "" ]
            in
            let note =
              match find_all_fields "NOTE" r.rsons with
              [ [] -> ""
              | rl -> treat_notes gen rl ]
            in
            (Cremated (Adef.codate_of_od d), p, (note, []), source gen r)
      | None -> (UnknownBurial, "", ("", []), ("", [])) ]
    in
    match (buri, crem) with
    [ (UnknownBurial, Cremated _) -> (crem, crem_place, (crem_note, []), (crem_src, crem_nt))
    | _ -> (buri, buri_place, (buri_note, []), (buri_src, buri_nt)) ]
  in
  let birth = Adef.codate_of_od birth in
  let bapt = Adef.codate_of_od bapt in
  let (psources, psources_nt) =
    let (s, s_nt) = source gen r in
    if s = "" then (default_source.val, s_nt) else (s, s_nt)
  in
  let ext_notes =
    let concat_text s1 s2 s_sep =
      let s = if s1 = "" && notes = "" || s2 = "" then "" else s_sep in
      s1 ^ s ^ s2
    in
    let text = concat_text "" (notes_from_source_record birth_nt) "<br>\n" in
    let text = concat_text text (notes_from_source_record bapt_nt) "<br>\n" in
    let text = concat_text text (notes_from_source_record death_nt) "<br>\n" in
    let text = concat_text text (notes_from_source_record burial_nt) "<br>\n" in
    let text = concat_text text (notes_from_source_record psources_nt) "<br>\n" in
    if untreated_in_notes.val then
      let remain_tags_in_notes text init rtl =
        let rtl = build_remain_tags rtl in
        if rtl = [] then init
        else concat_text init (html_text_of_tags text rtl) "\n"
      in
      let nt = remain_tags_in_notes "INDI" "" r.rsons in
      let nt = remain_tags_in_notes "BIRT SOUR" nt birth_nt in
      let nt = remain_tags_in_notes "BAPT SOUR" nt bapt_nt in
      let nt = remain_tags_in_notes "DEAT SOUR" nt death_nt in
      let nt = remain_tags_in_notes "BURI/CREM SOUR" nt burial_nt in
      let nt = remain_tags_in_notes "SOUR SOUR" nt psources_nt in
      if nt = "" then text
      else text ^ "<pre>\n" ^ nt ^ "\n</pre>"
    else text
  in
  (* Mise à jour des évènements principaux. *)
  let (birth_place, birth_note, birth_src) =
    (add_string gen birth_place, add_string gen birth_note,
     add_string gen birth_src)
  in
  let (bapt_place, bapt_note, bapt_src) =
    (add_string gen bapt_place, add_string gen bapt_note,
     add_string gen bapt_src)
  in
  let (death_place, death_note, death_src) =
    (add_string gen death_place, add_string gen death_note,
     add_string gen death_src)
  in
  let (burial_place, burial_note, burial_src) =
    (add_string gen burial_place, add_string gen burial_note,
     add_string gen burial_src)
  in
  (* On tri les évènements pour être sûr. *)
  let pevents =
    CheckItem.sort_events
      ((fun evt -> CheckItem.Psort evt.epers_name),
       (fun evt -> evt.epers_date))
      pevents
  in
  let (bi, bp, de, bu) =
    reconstitute_from_pevents pevents
      (birth, birth_place, birth_note, birth_src)
      (bapt, bapt_place, bapt_note, bapt_src)
      (death, death_place, death_note, death_src)
      (burial, burial_place, burial_note, burial_src)
  in
  let (birth, birth_place, birth_note, birth_src) = bi in
  let (bapt, bapt_place, bapt_note, bapt_src) = bp in
  let (death, death_place, death_note, death_src) = de in
  let (burial, burial_place, burial_note, burial_src) = bu in
  let person =
    person_of_gen_person
      {first_name = add_string gen first_name;
       surname = add_string gen surname;
       occ = occ; public_name = add_string gen public_name;
       image = add_string gen image;
       qualifiers =
         if qualifier <> "" then [add_string gen qualifier] else [];
       aliases = List.map (add_string gen) aliases;
       first_names_aliases = List.map (add_string gen) first_names_aliases;
       surnames_aliases = List.map (add_string gen) surname_aliases;
       titles = titles; rparents = rparents; related = [];
       occupation = add_string gen occupation; sex = sex;
       access =
         if no_public_if_titles.val && titles <> [] then Private
         else IfTitles;
       birth = birth; birth_place = birth_place;
       birth_note = birth_note;
       birth_src = birth_src;
       baptism = bapt; baptism_place = bapt_place;
       baptism_note = bapt_note;
       baptism_src = bapt_src; death = death;
       death_place = death_place;
       death_note = death_note;
       death_src = death_src; burial = burial;
       burial_place = burial_place;
       burial_note = burial_note;
       burial_src = burial_src;
       pevents = pevents;
       notes = add_string gen (notes ^ ext_notes);
       psources = add_string gen psources; key_index = ip}
  in
  let ascend =
    ascend_of_gen_ascend {parents = parents; consang = Adef.fix (-1)}
  in
  let union = union_of_gen_union {family = Array.of_list family} in
  do {
    gen.g_per.arr.(Adef.int_of_iper ip) := Right3 person ascend union;
    match find_field "ADOP" r.rsons with
    [ Some r ->
        match find_field "FAMC" r.rsons with
        [ Some r -> forward_adop gen ip r.rval (find_field "ADOP" r.rsons)
        | _ -> () ]
    | _ -> () ];
    r.rused := True
  }
;

value find_fevent_name_from_tag gen tag tagv =
  match tag with
  [ "MARR" -> Efam_Marriage
  | "unmarried" -> Efam_NoMarriage
  | "nomen" -> Efam_NoMention
  | "ENGA" -> Efam_Engage
  | "DIV" -> Efam_Divorce
  | "SEP" | "separation" -> Efam_Separated
  | "ANUL" -> Efam_Annulation
  | "MARB" -> Efam_MarriageBann
  | "MARC" -> Efam_MarriageContract
  | "MARL" -> Efam_MarriageLicense
  | "pacs" -> Efam_PACS
  | "RESI" | "residence" -> Efam_Residence
  | n -> Efam_Name (add_string gen (strip_spaces tagv)) ]
;

value primary_fevents =
  [ "ANUL"; "DIV"; "ENGA"; "MARR";
    "MARB"; "MARC"; "MARL"; "RESI"; "SEP" ]
;

value treat_fam_fevent gen ifath imoth r =
  let check_place_unmarried efam_name place r =
    match find_all_fields "PLAC" r.rsons with
    [ [r :: rl] ->
        if String.uncapitalize r.rval = "unmarried" then
          (Efam_NoMarriage, "")
        else
          let place = strip_spaces r.rval in
          loop rl where rec loop =
            fun
            [ [r :: rl] ->
                if String.uncapitalize r.rval = "unmarried" then
                  (Efam_NoMarriage, place)
                else loop rl
            | [] -> (efam_name, place) ]
    | [] -> (efam_name, place) ]
  in
  let prim_events =
    List.fold_left
      (fun events tag ->
        List.fold_left
          (fun events r ->
            let name = find_fevent_name_from_tag gen tag tag in
            let date =
              match find_field "DATE" r.rsons with
              [ Some r -> date_of_field r.rpos r.rval
              | None -> None ]
            in
            let place =
              match find_field "PLAC" r.rsons with
              [ Some r -> strip_spaces r.rval
              | _ -> "" ]
            in
            let reason = "" in
            let note =
              match find_all_fields "NOTE" r.rsons with
              [ [] -> ""
              | rl -> treat_notes gen rl ]
            in
            (* Si le tag 1 XXX a des infos, on les ajoutes. *)
            let note =
              let name_info = strip_spaces r.rval in
              if name_info = "" || r.rval = "Y" then note
              else name_info ^ "<br>\n" ^ note
            in
            let src =
              match find_all_fields "SOUR" r.rsons with
              [ [] -> ""
              | rl ->
                  loop True "" rl where rec loop first src rl =
                    match rl with
                    [ [] -> src
                    | [ r :: rl ] ->
                        let (src_cont, _) = treat_source gen r in
                        let src =
                          if first then src ^ src_cont else src ^ " " ^ src_cont
                        in
                        loop False src rl ]]
            in
            let witnesses = find_fevent_witness gen "INDI" ifath imoth r in
            (* Vérification du mariage. *)
            let (name, place) =
              match name with
              [ Efam_Marriage ->
                  match find_field "TYPE" r.rsons with
                  [ Some r ->
                      if String.uncapitalize r.rval = "unmarried" then
                        (Efam_NoMarriage, place)
                      else
                        check_place_unmarried name place r
                  | None -> check_place_unmarried name place r ]
              | _ -> (name, place) ]
            in
            let evt =
              {efam_name = name; efam_date = Adef.codate_of_od date;
               efam_place = add_string gen place;
               efam_reason = add_string gen reason;
               efam_note = add_string gen note;
               efam_src = add_string gen src;
               efam_witnesses = witnesses}
            in
            (* On ajoute toujours les évènements principaux liés à la   *)
            (* famille, sinon, on peut avoir un problème si on supprime *)
            (* l'évènement, celui ci sera remplacé par la relation par  *)
            (* défaut.                                                  *)
            [ evt :: events ])
          events (find_all_fields tag r.rsons) )
      [] primary_fevents
  in
  let second_events =
    List.fold_left
      (fun events r ->
         match find_field "TYPE" r.rsons with
         [ Some rr ->
             if rr.rval <> "" then
               let name =
                 if List.mem rr.rval primary_fevents then
                   find_fevent_name_from_tag gen rr.rval rr.rval
                 else
                   find_fevent_name_from_tag gen (String.lowercase rr.rval) rr.rval
               in
               let date =
                 match find_field "DATE" r.rsons with
                 [ Some r -> date_of_field r.rpos r.rval
                 | None -> None ]
               in
               let place =
                 match find_field "PLAC" r.rsons with
                 [ Some r -> strip_spaces r.rval
                 | _ -> "" ]
               in
               let reason = "" in
               let note =
                 match find_all_fields "NOTE" r.rsons with
                 [ [] -> ""
                 | rl -> treat_notes gen rl ]
               in
               (* Si le tag 1 XXX a des infos, on les ajoutes. *)
               let note =
                 let name_info = strip_spaces r.rval in
                 if name_info = "" || r.rval = "Y" then note
                 else name_info ^ "<br>\n" ^ note
               in
               let src =
                 match find_all_fields "SOUR" r.rsons with
                 [ [] -> ""
                 | rl ->
                     loop True "" rl where rec loop first src rl =
                       match rl with
                       [ [] -> src
                       | [ r :: rl ] ->
                           let (src_cont, _) = treat_source gen r in
                           let src =
                             if first then src ^ src_cont else src ^ " " ^ src_cont
                           in
                           loop False src rl ]]
               in
               let witnesses = find_fevent_witness gen "INDI" ifath imoth r in
               let evt =
                 {efam_name = name; efam_date = Adef.codate_of_od date;
                  efam_place = add_string gen place;
                  efam_reason = add_string gen reason;
                  efam_note = add_string gen note;
                  efam_src = add_string gen src;
                  efam_witnesses = witnesses}
               in
               (* On ajoute que les évènements non vides, *)
               (* sauf si évènement personnalisé !        *)
               let has_efam_name =
                 match name with
                 [ Efam_Name n -> n <> string_empty
                 | _ -> False ]
               in
               if has_efam_name || date <> None || place <> "" || note <> "" ||
                  src <> "" || witnesses <> [| |]
               then
                 [ evt :: events ]
               else events
             else events
         | None -> events ] )
      [] (find_all_fields "EVEN" r.rsons)
  in
  List.rev_append prim_events second_events
;

value reconstitute_from_fevents gen gay fevents marr witn div =
  let found_marriage = ref False in
  let found_divorce = ref False in
  (* On veut cette fois ci que ce soit le dernier évènement *)
  (* qui soit mis dans les évènements principaux.           *)
  let rec loop fevents marr witn div =
    match fevents with
    [ [] -> (marr, witn, div)
    | [evt :: l] ->
        match evt.efam_name with
        [ Efam_Engage ->
            if found_marriage.val then loop l marr witn div
            else
              let witn = Array.map fst evt.efam_witnesses in
              let marr =
                (Engaged, evt.efam_date, evt.efam_place,
                 evt.efam_note, evt.efam_src)
              in
              let () = found_marriage.val := True in
              loop l marr witn div
        | Efam_Marriage ->
              let witn = Array.map fst evt.efam_witnesses in
              let marr =
                (Married, evt.efam_date, evt.efam_place,
                 evt.efam_note, evt.efam_src)
            in
            let () = found_marriage.val := True in
            (marr, witn, div)
        | Efam_MarriageContract ->
            if found_marriage.val then loop l marr witn div
            else
              let witn = Array.map fst evt.efam_witnesses in
              (* Pour différencier le fait qu'on recopie le *)
              (* mariage, on met une précision "vers".      *)
              let date =
                match Adef.od_of_codate evt.efam_date with
                [ Some (Dgreg dmy cal) ->
                    let dmy = {(dmy) with prec = About} in
                    Adef.codate_of_od (Some (Dgreg dmy cal))
                | _ -> evt.efam_date ]
              in
              (* Pour différencier le fait qu'on recopie le *)
              (* mariage, on ne met pas de lieu.            *)
              let place = add_string gen "" in
              let marr =
                (Married, date, place, evt.efam_note, evt.efam_src)
              in
              let () = found_marriage.val := True in
              loop l marr witn div
        | Efam_NoMention | Efam_MarriageBann | Efam_MarriageLicense |
          Efam_Annulation | Efam_PACS ->
            if found_marriage.val then loop l marr witn div
            else
              let witn = Array.map fst evt.efam_witnesses in
              let marr =
                (NoMention, evt.efam_date, evt.efam_place,
                 evt.efam_note, evt.efam_src)
              in
              let () = found_marriage.val := True in
              loop l marr witn div
        | Efam_NoMarriage ->
            if found_marriage.val then loop l marr witn div
            else
              let witn = Array.map fst evt.efam_witnesses in
              let marr =
                (NotMarried, evt.efam_date, evt.efam_place,
                 evt.efam_note, evt.efam_src)
              in
              let () = found_marriage.val := True in
              loop l marr witn div
        | Efam_Divorce ->
            if found_divorce.val then loop l marr witn div
            else
              let div = Divorced evt.efam_date in
              let () = found_divorce.val := True in
              loop l marr witn div
        | Efam_Separated ->
            if found_divorce.val then loop l marr witn div
            else
              let div = Separated in
              let () = found_divorce.val := True in
              loop l marr witn div
        | _ -> loop l marr witn div ] ]
  in
  let (marr, witn, div) = loop (List.rev fevents) marr witn div in
  (* Parents de même sexe. *)
  if gay then
    let (relation, date, place, note, src) = marr in
    let relation =
      match relation with
        [ Married | NoSexesCheckMarried -> NoSexesCheckMarried
        | _  -> NoSexesCheckNotMarried ]
    in
    let marr = (relation, date, place, note, src) in
    (marr, witn, div)
  else (marr, witn, div)
;

value add_fam_norm gen r adop_list =
  let i = fam_index gen r.rval in
  let (fath, moth, gay) =
    match (find_all_fields "HUSB" r.rsons, find_all_fields "WIFE" r.rsons) with
    [ ([f1], [m1]) -> (per_index gen f1.rval, per_index gen m1.rval, False)
    | ([f1; f2 :: []], []) ->
        (per_index gen f1.rval, per_index gen f2.rval, True)
    | ([], [m1; m2 :: []]) ->
        (per_index gen m1.rval, per_index gen m2.rval, True)
    | _ ->
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
        (fath, moth, False) ]
  in
  do {
    match gen.g_per.arr.(Adef.int_of_iper fath) with
    [ Left3 lab -> ()
    | Right3 p a u ->
        let u =
          if not (List.mem i (Array.to_list (get_family u))) then
            union_of_gen_union {family = Array.append (get_family u) [| i |]}
          else u
        in
        let p = if get_sex p = Neuter then person_with_sex p Male else p in
        gen.g_per.arr.(Adef.int_of_iper fath) := Right3 p a u ];
    match gen.g_per.arr.(Adef.int_of_iper moth) with
    [ Left3 lab -> ()
    | Right3 p a u ->
        let u =
          if not (List.mem i (Array.to_list (get_family u))) then
            union_of_gen_union {family = Array.append (get_family u) [| i |]}
          else u
        in
        let p = if get_sex p = Neuter then person_with_sex p Female else p in
        gen.g_per.arr.(Adef.int_of_iper moth) := Right3 p a u ];
    let children =
      let rl = find_all_fields "CHIL" r.rsons in
      List.fold_right
        (fun r ipl ->
           let ip = per_index gen r.rval in
           if List.mem_assoc ip adop_list then
             match gen.g_per.arr.(Adef.int_of_iper ip) with
             [ Right3 p a u ->
                 match get_parents a with
                 [ Some ifam ->
                     if ifam = i then do {
                       let a = ascend_with_parents a None in
                       gen.g_per.arr.(Adef.int_of_iper ip) := Right3 p a u;
                       ipl
                     }
                     else [ip :: ipl]
                 | None -> [ip :: ipl] ]
             | _ -> [ip :: ipl] ]
           else [ip :: ipl])
        rl []
    in
    let (relation, marr, marr_place, (marr_note, _), (marr_src, marr_nt), witnesses) =
      let (relation, sons) =
        match find_field "MARR" r.rsons with
        [ Some r ->
            if gay then (NoSexesCheckMarried, Some r) else (Married, Some r)
        | None ->
            match find_field "ENGA" r.rsons with
            [ Some r -> (Engaged, Some r)
            | None -> (relation_status.val, None) ] ]
      in
      match sons with
      [ Some r ->
          let (u, p) =
            match find_all_fields "PLAC" r.rsons with
            [ [r :: rl] ->
                if String.uncapitalize r.rval = "unmarried" then
                  (NotMarried, "")
                else
                  let p = strip_spaces r.rval in
                  loop rl where rec loop =
                    fun
                    [ [r :: rl] ->
                        if String.uncapitalize r.rval = "unmarried" then
                          (NotMarried, p)
                        else loop rl
                    | [] -> (relation, p) ]
            | [] -> (relation, "") ]
          in
          let u =
            match find_field "TYPE" r.rsons with
            [ Some r ->
                if String.uncapitalize r.rval = "gay" then
                  NoSexesCheckNotMarried
                else u
            | None -> u ]
          in
          let d =
            match find_field "DATE" r.rsons with
            [ Some r -> date_of_field r.rpos r.rval
            | _ -> None ]
          in
          let rec heredis_witnesses =
            fun
            [ [] -> []
            | [ r :: asso_l ] ->
                if find_field_with_value "RELA" "Witness" r.rsons &&
                   find_field_with_value "TYPE" "INDI" r.rsons then
                  let witness = per_index gen r.rval in
                  [ witness :: heredis_witnesses asso_l ]
                else do {
                  r.rused := False;
                  heredis_witnesses asso_l
                } ]
          in
          let witnesses =
            match find_all_fields "ASSO" r.rsons with
            [ [] -> []
            | wl -> heredis_witnesses wl ]
          in
          let note =
            match find_all_fields "NOTE" r.rsons with
            [ [] -> ""
            | rl -> treat_notes gen rl ]
          in
          (u, d, p, (note, []), source gen r, witnesses)
      | None -> (relation, None, "", ("", []), ("", []), []) ]
    in
    let witnesses = Array.of_list witnesses in
    let div =
      match find_field "DIV" r.rsons with
      [ Some r ->
          match find_field "DATE" r.rsons with
          [ Some d ->
              Divorced (Adef.codate_of_od (date_of_field d.rpos d.rval))
          | _ ->
              match find_field "PLAC" r.rsons with
              [ Some _ -> Divorced Adef.codate_None
              | _ ->
                  if r.rval = "Y" then Divorced Adef.codate_None
                  else NotDivorced ] ]
      | None -> NotDivorced ]
    in
    let fevents = treat_fam_fevent gen fath moth r in
    let comment =
      match find_all_fields "NOTE" r.rsons with
      [ [] -> ""
      | rl -> treat_notes gen rl ]
    in
    let (fsources, fsources_nt) =
      let (s, s_nt) = source gen r in
      if s = "" then (default_source.val, s_nt) else (s, s_nt)
    in
    let concat_text s1 s2 s_sep =
      let s = if s1 = "" then "" else s_sep in
      s1 ^ s ^ s2
    in
    let ext_sources =
      let text = concat_text "" (notes_from_source_record marr_nt) "<br>\n" in
      concat_text text (notes_from_source_record fsources_nt) "<br>\n"
    in
    let ext_notes =
      if untreated_in_notes.val then
        let remain_tags_in_notes text init rtl =
          let rtl = build_remain_tags rtl in
          if rtl = [] then init
          else concat_text init (html_text_of_tags text rtl) "\n"
        in
        let nt = remain_tags_in_notes "FAM" "" r.rsons in
        let nt = remain_tags_in_notes "MARR SOUR" nt marr_nt in
        let nt = remain_tags_in_notes "SOUR SOUR" nt fsources_nt in
        if nt = "" then ""
        else "<pre>\n" ^ nt ^ "\n</pre>"
      else ""
    in
    let add_in_person_notes iper =
      match gen.g_per.arr.(Adef.int_of_iper iper) with
      [ Left3 _ -> ()
      | Right3 p a u ->
          let notes = gen.g_str.arr.(Adef.int_of_istr (get_notes p)) in
          let notes =
            if notes = "" then ext_sources ^ ext_notes
            else if ext_sources = "" then notes ^ "\n" ^ ext_notes
            else notes ^ "<br>\n" ^ ext_sources ^ ext_notes
          in
          let new_notes = add_string gen notes in
          let p =
            person_of_gen_person
              {(gen_person_of_person p) with notes = new_notes}
          in
          gen.g_per.arr.(Adef.int_of_iper iper) := Right3 p a u ]
    in
    let _ =
      if ext_notes = "" then ()
      else do {
        add_in_person_notes fath;
        add_in_person_notes moth;
      }
    in
    (* Mise à jour des évènements principaux. *)
    let (marr, marr_place, marr_note, marr_src) =
      (Adef.codate_of_od marr, add_string gen marr_place,
       add_string gen marr_note, add_string gen marr_src)
    in
    (* On tri les évènements pour être sûr. *)
    let fevents =
      CheckItem.sort_events
        ((fun evt -> CheckItem.Fsort evt.efam_name),
         (fun evt -> evt.efam_date))
        fevents
    in
    let (marr, witn, div) =
      reconstitute_from_fevents gen gay fevents
        (relation, marr, marr_place, marr_note, marr_src)
        witnesses div
    in
    let (relation, marr, marr_place, marr_note, marr_src) = marr in
    let witnesses = witn in
    let div = div in
    let fam =
      family_of_gen_family
        {marriage = marr; marriage_place = marr_place;
         marriage_note = marr_note; marriage_src = marr_src;
         witnesses = witnesses; relation = relation; divorce = div;
         fevents = fevents; comment = add_string gen comment;
         origin_file = string_empty; fsources = add_string gen fsources;
         fam_index = i}
    and cpl = couple_of_gen_couple (couple False fath moth)
    and des = descend_of_gen_descend {children = Array.of_list children} in
    gen.g_fam.arr.(Adef.int_of_ifam i) := Right3 fam cpl des
  }
;

value add_fam gen r =
  let list = Hashtbl.find_all gen.g_adop r.rval in
  match list with
  [ [] -> add_fam_norm gen r []
  | list ->
      let husb = find_field "HUSB" r.rsons in
      let wife = find_field "WIFE" r.rsons in
      do {
        List.iter
          (fun (ip, which_parent) ->
             set_adop_fam gen ip which_parent husb wife)
          list;
        match find_field "CHIL" r.rsons with
        [ Some _ -> add_fam_norm gen r list
        | _ -> () ]
      } ]
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
          | "MACINTOSH" -> charset.val := MacIntosh
          | "UTF-8" -> charset.val := Utf8
          | _ -> charset.val := Ascii ]
      | None -> () ] ]
;

value treat_header3 gen r =
  match find_all_fields "NOTE" r.rsons with
  [ [] -> ()
  | rl -> gen.g_bnot := treat_notes gen rl ]
;

value turn_around_genealogos_bug r =
  if String.length r.rlab > 0 && r.rlab.[0] = '@' then
    {(r) with rlab = r.rval; rval = r.rlab}
  else r
;

value make_gen2 gen r =
  let r = turn_around_genealogos_bug r in
  match r.rlab with
  [ "HEAD" -> treat_header2 gen r
  | "INDI" -> add_indi gen r
  | _ -> () ]
;

value make_gen3 gen r =
  let r = turn_around_genealogos_bug r in
  match r.rlab with
  [ "HEAD" -> treat_header3 gen r
  | "SUBM" -> ()
  | "INDI" -> ()
  | "FAM" -> add_fam gen r
  | "NOTE" -> ()
  | "SOUR" -> ()
  | "TRLR" -> do { eprintf "*** Trailer ok\n"; flush stderr }
  | s ->
      do {
        fprintf log_oc.val "Not implemented typ = %s\n" s;
        flush log_oc.val
      } ]
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
  let rec loop () =
    match try Some (find_lev0 strm) with [ Stream.Failure -> None ] with
    [ Some (bp, r1, r2) ->
        do {
          match r2 with
          [ "NOTE" -> Hashtbl.add gen.g_not r1 bp
          | "SOUR" -> Hashtbl.add gen.g_src r1 bp
          | _ -> () ];
          loop ()
        }
    | None ->
        match strm with parser
        [ [: `_ :] -> do { skip_to_eoln strm; loop () }
        | [: :] -> () ] ]
  in
  do { loop (); close_in ic }
;

value pass2 gen fname =
  let ic = open_in_bin fname in
  do {
    line_cnt.val := 0;
    let strm =
      Stream.from
        (fun i ->
           try
             let c = input_char ic in
             do { if c = '\n' then incr line_cnt else (); Some c }
           with
           [ End_of_file -> None ])
    in
    let rec loop () =
      match try Some (get_lev0 strm) with [ Stream.Failure -> None ] with
      [ Some r -> do { make_gen2 gen r; loop () }
      | None ->
          match strm with parser
          [ [: `'1'..'9' :] ->
              let _ : string = get_to_eoln 0 strm in
              loop ()
          | [: `_ :] ->
              let _ : string = get_to_eoln 0 strm in
              loop ()
          | [: :] -> () ] ]
    in
    loop ();
    List.iter
      (fun (ipp, ip) ->
         match gen.g_per.arr.(Adef.int_of_iper ipp) with
         [ Right3 p a u ->
             if List.mem ip (get_related p) then ()
             else
               let p = person_with_related p [ip :: get_related p] in
               gen.g_per.arr.(Adef.int_of_iper ipp) := Right3 p a u
         | _ -> () ])
      gen.g_godp;
    List.iter
      (fun (ipp, ip) ->
         match gen.g_per.arr.(Adef.int_of_iper ipp) with
         [ Right3 p a u ->
               if List.mem ip (get_related p) then ()
               else
                 let p = person_with_related p [ip :: get_related p] in
                 gen.g_per.arr.(Adef.int_of_iper ipp) := Right3 p a u
         | _ -> () ])
      gen.g_prelated;
    close_in ic
  }
;

value pass3 gen fname =
  let ic = open_in_bin fname in
  do {
    line_cnt.val := 0;
    let strm =
      Stream.from
        (fun i ->
           try
             let c = input_char ic in
             do { if c = '\n' then incr line_cnt else (); Some c }
           with
           [ End_of_file -> None ])
    in
    let rec loop () =
      match try Some (get_lev0 strm) with [ Stream.Failure -> None ] with
      [ Some r -> do { make_gen3 gen r; loop () }
      | None ->
          match strm with parser
          [ [: `'1'..'9' :] ->
              let _ : string = get_to_eoln 0 strm in
              loop ()
          | [: `_ :] ->
              do {
                print_location line_cnt.val;
                fprintf log_oc.val "Strange input.\n";
                flush log_oc.val;
                let _ : string = get_to_eoln 0 strm in
                loop ()
              }
          | [: :] -> () ] ]
    in
    loop ();
    List.iter
      (fun (ifam, ip) ->
         match gen.g_fam.arr.(Adef.int_of_ifam ifam) with
         [ Right3 fam cpl des ->
             match
               (gen.g_per.arr.(Adef.int_of_iper (get_father cpl)),
                gen.g_per.arr.(Adef.int_of_iper ip))
             with
             [ (Right3 pfath _ _, Right3 p a u) ->
                 do {
                   if List.mem (get_father cpl) (get_related p) then ()
                   else
                     let p =
                       person_with_related p
                         [get_father cpl :: get_related p]
                     in
                     gen.g_per.arr.(Adef.int_of_iper ip) := Right3 p a u;
                   if List.mem ip (Array.to_list (get_witnesses fam)) then ()
                   else
                     let fam =
                       family_of_gen_family
                         {(gen_family_of_family fam) with
                          witnesses = Array.append (get_witnesses fam) [| ip |]}
                     in
                     gen.g_fam.arr.(Adef.int_of_ifam ifam) := Right3 fam cpl des
                 }
             | _ -> () ]
         | _ -> () ])
      gen.g_witn;
    List.iter
      (fun (ipp, ip) ->
         match gen.g_per.arr.(Adef.int_of_iper ipp) with
         [ Right3 p a u ->
             if List.mem ip (get_related p) then ()
             else
               let p = person_with_related p [ip :: get_related p] in
               gen.g_per.arr.(Adef.int_of_iper ipp) := Right3 p a u
         | _ -> () ])
      gen.g_frelated;
    close_in ic
  }
;

value check_undefined gen =
  do {
    for i = 0 to gen.g_per.tlen - 1 do {
      match gen.g_per.arr.(i) with
      [ Right3 _ _ _ -> ()
      | Left3 lab ->
          let (p, a, u) = unknown_per gen i Neuter in
          do {
            fprintf log_oc.val "Warning: undefined person %s\n" lab;
            gen.g_per.arr.(i) := Right3 p a u
          } ]
    };
    for i = 0 to gen.g_fam.tlen - 1 do {
      match gen.g_fam.arr.(i) with
      [ Right3 _ _ _ -> ()
      | Left3 lab ->
          let (f, c, d) = unknown_fam gen i in
          do {
            fprintf log_oc.val "Warning: undefined family %s\n" lab;
            gen.g_fam.arr.(i) := Right3 f c d
          } ]
    }
  }
;

value add_parents_to_isolated gen =
  for i = 0 to gen.g_per.tlen - 1 do {
    match gen.g_per.arr.(i) with
    [ Right3 p a u ->
        if get_parents a = None &&
           Array.length (get_family u) = 0 && get_rparents p = [] &&
           get_related p = []
        then
          let fn = gen.g_str.arr.(Adef.int_of_istr (get_first_name p)) in
          let sn = gen.g_str.arr.(Adef.int_of_istr (get_surname p)) in
          if fn = "?" && sn = "?" then ()
          else do {
            fprintf log_oc.val
              "Adding parents to isolated person: %s.%d %s\n" fn (get_occ p)
              sn;
            let ifam = phony_fam gen in
            match gen.g_fam.arr.(Adef.int_of_ifam ifam) with
            [ Right3 fam cpl des -> do {
                let des =
                  descend_of_gen_descend
                    {children = [| get_key_index p |]}
                in
                gen.g_fam.arr.(Adef.int_of_ifam ifam) := Right3 fam cpl des;
                let a = ascend_with_parents a (Some ifam) in
                gen.g_per.arr.(i) := Right3 p a u
              }
            | _ -> () ];
          }
        else ()
    | Left3 _ -> () ]
  }
;

value make_arrays in_file =
  let fname =
    if Filename.check_suffix in_file ".ged" then in_file
    else if Filename.check_suffix in_file ".GED" then in_file
    else in_file ^ ".ged"
  in
  let gen =
    {g_per = {arr = [| |]; tlen = 0}; g_fam = {arr = [| |]; tlen = 0};
     g_str = {arr = [| |]; tlen = 0}; g_bnot = ""; g_ic = open_in_bin fname;
     g_not = Hashtbl.create 3001; g_src = Hashtbl.create 3001;
     g_hper = Hashtbl.create 3001; g_hfam = Hashtbl.create 3001;
     g_hstr = Hashtbl.create 3001; g_hnam = Hashtbl.create 3001;
     g_adop = Hashtbl.create 3001; g_godp = []; g_prelated = [];
     g_frelated = []; g_witn = []}
  in
  do {
    assert (add_string gen "" = string_empty);
    assert (add_string gen "?" = string_quest);
    assert (add_string gen "x" = string_x);
    eprintf "*** pass 1 (note)\n";
    flush stderr;
    pass1 gen fname;
    eprintf "*** pass 2 (indi)\n";
    flush stderr;
    pass2 gen fname;
    eprintf "*** pass 3 (fam)\n";
    flush stderr;
    pass3 gen fname;
    close_in gen.g_ic;
    check_undefined gen;
    add_parents_to_isolated gen;
    (gen.g_per, gen.g_fam, gen.g_str, gen.g_bnot)
  }
;

value make_subarrays (g_per, g_fam, g_str, g_bnot) =
  let persons =
    let pa = Array.make g_per.tlen (Obj.magic 0) in
    let aa = Array.make g_per.tlen (Obj.magic 0) in
    let ua = Array.make g_per.tlen (Obj.magic 0) in
    do {
      for i = 0 to g_per.tlen - 1 do {
        match g_per.arr.(i) with
        [ Right3 p a u -> do { pa.(i) := p; aa.(i) := a; ua.(i) := u }
        | Left3 lab -> failwith ("undefined person " ^ lab) ]
      };
      (pa, aa, ua)
    }
  in
  let families =
    let fa = Array.make g_fam.tlen (Obj.magic 0) in
    let ca = Array.make g_fam.tlen (Obj.magic 0) in
    let da = Array.make g_fam.tlen (Obj.magic 0) in
    do {
      for i = 0 to g_fam.tlen - 1 do {
        match g_fam.arr.(i) with
        [ Right3 f c d -> do { fa.(i) := f; ca.(i) := c; da.(i) := d }
        | Left3 lab -> failwith ("undefined family " ^ lab) ]
      };
      (fa, ca, da)
    }
  in
  let strings = Array.sub g_str.arr 0 g_str.tlen in
  (persons, families, strings, g_bnot)
;

(* Converting to Gwcomp.gw_syntax *)

value key_of_person sa p =
  {Gwcomp.pk_first_name = sa.(Adef.int_of_istr p.first_name);
   pk_surname = sa.(Adef.int_of_istr p.surname); pk_occ = p.occ}
;

value somebody_of_person def pa sa ip =
  let p = pa.(Adef.int_of_iper ip) in
  let somebody =
    if def.(Adef.int_of_iper ip) then
      let pk = key_of_person sa p in
      Gwcomp.Undefined pk
    else do {
      let p = {(p) with rparents = []; related = []; notes = string_empty} in
      let p =
        Futil.map_person_ps (fun p -> failwith "somebody_of_person")
          (fun i -> sa.(Adef.int_of_istr i)) p
      in
      def.(Adef.int_of_iper ip) := True;
      Gwcomp.Defined p
    }
  in
  (somebody, p.sex)
;

value string_person_of_person pa sa ip =
  Futil.map_person_ps (fun p -> p) (fun i -> sa.(Adef.int_of_istr i))
    pa.(Adef.int_of_iper ip)
;

value make_gwsyntax ((pa, aa, ua), (fa, ca, da), sa, g_bnot) = do {
  let rev_list = ref [] in
  let def = Array.make (Array.length pa) False in
  for ifam = 0 to Array.length fa - 1 do {
    let des = da.(ifam) in
    for i = 0 to Array.length des.children - 1 do {
      def.(Adef.int_of_iper des.children.(i)) := True
    };
  };
  for ifam = 0 to Array.length fa - 1 do {
    let cpl = ca.(ifam) in
    let (fath, s1) = somebody_of_person def pa sa (Adef.father cpl) in
    let (moth, s2) = somebody_of_person def pa sa (Adef.mother cpl) in
    let cpl = Adef.couple fath moth in
    let witn =
      List.map (somebody_of_person def pa sa)
        (Array.to_list fa.(ifam).witnesses)
    in
    let fevents =
      List.map
        (fun evt ->
           let name =
             match evt.efam_name with
             [ Efam_Name n -> Efam_Name sa.(Adef.int_of_istr n)
             | Efam_Marriage | Efam_NoMarriage | Efam_NoMention | Efam_Engage |
               Efam_Divorce  | Efam_Separated | Efam_Annulation |
               Efam_MarriageBann | Efam_MarriageContract |
               Efam_MarriageLicense | Efam_PACS | Efam_Residence as e -> e ]
           in
           let witl =
             List.map
               (fun (ip, wk) ->
                  let (sb, sex) = somebody_of_person def pa sa ip in
                  (sb, sex, wk))
               (Array.to_list evt.efam_witnesses)
           in
           (name, evt.efam_date,
            sa.(Adef.int_of_istr evt.efam_place),
            sa.(Adef.int_of_istr evt.efam_reason),
            sa.(Adef.int_of_istr evt.efam_note),
            sa.(Adef.int_of_istr evt.efam_src),
            witl))
        fa.(ifam).fevents
    in
    let fam =
      let fam = {(fa.(ifam)) with witnesses = [| |]; fevents = []} in
      Futil.map_family_ps (fun p -> failwith "make_gwsyntax 1")
        (fun i -> sa.(Adef.int_of_istr i)) fam
    in
    let des =
      Futil.map_descend_p (string_person_of_person pa sa) da.(ifam)
    in
    rev_list.val :=
      [Gwcomp.Family cpl s1 s2 witn fevents fam des :: rev_list.val]
  };
  for i = 0 to Array.length pa - 1 do {
    match pa.(i).rparents with
    [ [] -> ()
    | rl ->
        let (x, sex) = somebody_of_person def pa sa (Adef.iper_of_int i) in
        let rl =
          List.map
            (fun r ->
               Futil.map_relation_ps
                 (fun ip -> fst (somebody_of_person def pa sa ip))
                 (fun i -> sa.(Adef.int_of_istr i)) r)
            rl
        in
        rev_list.val := [Gwcomp.Relations x sex rl :: rev_list.val] ];
  };
  for i = 0 to Array.length pa - 1 do {
    let p = pa.(i) in
    let n = sa.(Adef.int_of_istr p.notes) in
    if n = "" then ()
    else
      let pk = key_of_person sa p in
      rev_list.val := [Gwcomp.Notes pk n :: rev_list.val];
  };
  if g_bnot = "" then ()
  else rev_list.val := [Gwcomp.Bnotes "" g_bnot :: rev_list.val];
  List.rev rev_list.val
};

(* Providing gw_syntax stream *)

value next_family_fun_templ gw_syntax in_file fi = do {
  fi.Db2link.f_curr_gwo_file := in_file;
  let gw_syntax = ref gw_syntax in
  fun () ->
    match gw_syntax.val with
    [ [x :: l] -> do { gw_syntax.val := l; Some x }
    | [] -> None ]
};

(* Main *)

value set_undefined_death_interval s =
  try
    match Stream.of_string s with parser
    [ [: a = number 0; `'-'; b = number 0 :] ->
        do {
          eprintf "ay %s dy %s\n" a b;
          flush stderr;
          let a = if a = "" then alive_years.val else int_of_string a in
          let b =
            max a (if b = "" then dead_years.val else int_of_string b)
          in
          alive_years.val := a;
          dead_years.val := b;
          eprintf "ay %d dy %d\n" a b;
          flush stderr
        } ]
  with
  [ Stream.Error _ -> raise (Arg.Bad "bad parameter for -udi")
  | e -> raise e ]
;

value out_file = ref "a";
value speclist =
  [("-o", Arg.String (fun s -> out_file.val := s),
    "<file>\n       Output database (default: \"a\").");
   ("-f", Arg.Set force, "\n       Remove database if already existing");
   ("-log", Arg.String (fun s -> log_oc.val := open_out s),
    "<file>\n       Redirect log trace to this file.");
   ("-lf", Arg.Set lowercase_first_names, "   \
- Lowercase first names -
       Convert first names to lowercase letters, with initials in
       uppercase.");
   ("-ls", Arg.Unit (fun () -> case_surnames.val := LowerCase), "   \
- Lowercase surnames -
       Convert surnames to lowercase letters, with initials in
       uppercase. Try to keep lowercase particles.");
   ("-us", Arg.Unit (fun () -> case_surnames.val := UpperCase), "   \
- Uppercase surnames -
       Convert surnames to uppercase letters.");
   ("-fne",
    Arg.String
      (fun s ->
         if String.length s = 2 then
           first_names_brackets.val := Some (s.[0], s.[1])
         else
           raise
             (Arg.Bad
                "-fne option must be followed by a 2 characters string")),
    "\
be - First names enclosed -
       When creating a person, if the GEDCOM first name part holds
       a part between 'b' (any character) and 'e' (any character), it
       is considered to be the usual first name: e.g. -fne '\"\"' or
       -fne \"()\".");
   ("-efn", Arg.Set extract_first_names, "  \
- Extract first names -
       When creating a person, if the GEDCOM first name part holds several
       names, the first of this names becomes the person \"first name\" and
       the complete GEDCOM first name part a \"first name alias\".");
   ("-no_efn", Arg.Clear extract_first_names, "  \
- Dont extract first names - [default]
       Cancels the previous option.");
   ("-epn", Arg.Set extract_public_names, "  \
- Extract public names - [default]
       When creating a person, if the GEDCOM first name part looks like a
       public name, i.e. holds:
       * a number or a roman number, supposed to be a number of a
         nobility title,
       * one of the words: \"der\", \"den\", \"die\", \"el\", \"le\", \"la\",
         \"the\", supposed to be the beginning of a qualifier,
       then the GEDCOM first name part becomes the person \"public name\"
       and its first word his \"first name\".");
   ("-no_epn", Arg.Clear extract_public_names,
    "\n       Cancels the previous option.");
   ("-no_pit", Arg.Set no_public_if_titles, " \
- No public if titles -
       Do not consider persons having titles as public");
   ("-tnd", Arg.Set try_negative_dates, "  \
- Try negative dates -
       Set negative dates when inconsistency (e.g. birth after death)");
   ("-no_nd", Arg.Set no_negative_dates, " \
- No negative dates -
       Don't interpret a year preceded by a minus sign as a negative year");
   ("-nc", Arg.Clear Db2link.do_check, "\n       No consistency check");
   ("-nopicture", Arg.Set no_picture, " \
- Don't extract individual picture.");
   ("-udi", Arg.String set_undefined_death_interval, "\
x-y   - Undefined death interval -
       Set the interval for persons whose death part is undefined:
       - if before x years, they are considered as alive
       - if after y year, they are considered as death
       - between x and y year, they are considered as \"don't know\"
       Default x is " ^ string_of_int alive_years.val ^ " and y is " ^ string_of_int dead_years.val);
   ("-uin", Arg.Set untreated_in_notes,
    " - Untreated in notes -\n       Put untreated GEDCOM tags in notes");
   ("-ds", Arg.String (fun s -> default_source.val := s), " \
- Default source -
       Set the source field for persons and families without source data");
   ("-dn", Arg.String (fun s -> default_name.val := s), " \
- Default name -
       Set the first name or surname field for persons without name");
   ("-dates_dm", Arg.Unit (fun () -> month_number_dates.val := DayMonthDates),
    "\n       Interpret months-numbered dates as day/month/year");
   ("-dates_md", Arg.Unit (fun () -> month_number_dates.val := MonthDayDates),
    "\n       Interpret months-numbered dates as month/day/year");
   ("-rs_no_mention", Arg.Unit (fun () -> relation_status.val := NoMention),
    "\n       Force relation status to NoMention (default is Married)");
   ("-charset",
    Arg.String
      (fun
       [ "ANSEL" -> charset_option.val := Some Ansel
       | "ASCII" -> charset_option.val := Some Ascii
       | "MSDOS" -> charset_option.val := Some Msdos
       | _ -> raise (Arg.Bad "bad -charset value") ]),
    "\
[ANSEL|ASCII|MSDOS] - charset decoding -
       Force given charset decoding, overriding the possible setting in
       GEDCOM")]
;

value anonfun s =
  if in_file.val = "" then in_file.val := s
  else raise (Arg.Bad "Cannot treat several GEDCOM files")
;

value errmsg = "Usage: ged2gwb2 [<ged>] [options] where options are:";

value main () =
  do {
    Argl.parse speclist anonfun errmsg;
    Secure.set_base_dir (Filename.dirname out_file.val);
    let bdir =
      if Filename.check_suffix out_file.val ".gwb" then out_file.val
      else out_file.val ^ ".gwb"
    in
    if not force.val && Sys.file_exists bdir then do {
      printf "\
The database \"%s\" already exists. Use option -f to overwrite it.
"
        out_file.val;
      flush stdout;
      exit 2
    }
    else ();
    let gw_syntax = do {
      let arrays = make_arrays in_file.val in
      Gc.compact ();
      let arrays = make_subarrays arrays in
      flush log_oc.val;
      make_gwsyntax arrays
    }
    in
    lock Mutil.lock_file out_file.val with
    [ Accept ->
        let next_family_fun = next_family_fun_templ gw_syntax in_file.val in
        if Db2link.link next_family_fun bdir then ()
        else do {
          eprintf "*** database not created\n";
          flush stderr;
          exit 2;
        }
    | Refuse -> do {
        printf "Base is locked: cannot write it\n";
        flush stdout;
        exit 2
      } ];
(*
    finish_base base arrays;
*)
    warning_month_number_dates ();
    if log_oc.val != stdout then close_out log_oc.val else ()
  }
;

try main () with e ->
  let e =
    match e with
    [ Ploc.Exc _ e -> e
    |  _ -> e ]
  in
  do {
    fprintf log_oc.val "Uncaught exception: %s\n"
      (Printexc.to_string e);
    if log_oc.val != stdout then close_out log_oc.val else ();
    exit 2
  };
