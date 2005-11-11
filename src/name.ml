(* $Id: name.ml,v 4.25 2005-11-11 22:59:09 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

value utf_8_db = ref True;

(* Name.lower *)

value unaccent_iso_8859_1 =
  fun
  [ 'à' | 'á' | 'â' | 'ã' | 'ä' | 'å' | 'æ' -> 'a'
  | 'ç' -> 'c'
  | 'è' | 'é' | 'ê' | 'ë' -> 'e'
  | 'ì' | 'í' | 'î' | 'ï' -> 'i'
  | 'ð' -> 'd'
  | 'ñ' -> 'n'
  | 'ò' | 'ó' | 'ô' | 'õ' | 'ö' | 'ø' -> 'o'
  | 'ù' | 'ú' | 'û' | 'ü' -> 'u'
  | 'ý' | 'ÿ' -> 'y'
  | 'þ' -> 'p'
  | 'ß' -> 's'
  | c -> c ]
;

value nbc c =
  if Char.code c < 0b10000000 then 1
  else if Char.code c < 0b10000000 then -1
  else if Char.code c < 0b11100000 then 2
  else if Char.code c < 0b11110000 then 3
  else if Char.code c < 0b11111000 then 4
  else if Char.code c < 0b11111100 then 5
  else if Char.code c < 0b11111110 then 6
  else -1
;

value unaccent_utf_8 s i =
  let nbc = nbc s.[i] in
  if nbc = 1 || nbc < 0 || i + nbc > String.length s then
    (String.make 1 (Char.lowercase s.[i]), i + 1)
  else
    let c = Char.code s.[i] in
    let s =
      match c with
      [ 0xC2 -> String.make 1 (Char.lowercase s.[i+1])
      | 0xC3 ->
          match Char.code s.[i+1] with
          [ 0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 -> "a"
          | 0x87 -> "c"
          | 0x88 | 0x89 | 0x8A | 0x8B -> "e"
          | 0x8C | 0x8D | 0x8E | 0x8F -> "i"
          | 0x91 -> "n"
          | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x98 -> "o"
          | 0x99 | 0x9A | 0x9B | 0x9C -> "u"
          | 0x9F -> "z"
          | 0xA0 | 0xA1 | 0xA2 | 0xA3 | 0xA4 | 0xA5 | 0xA6 -> "a"
          | 0xA7 -> "c"
          | 0xA8 | 0xA9 | 0xAA | 0xAB -> "e"
          | 0xAC | 0xAD | 0xAE | 0xAF -> "i"
          | 0xB1 -> "n"
          | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB8 -> "o"
          | 0xB9 | 0xBA | 0xBB | 0xBC -> "u"
          | 0xBF -> "y"
          | _ ->
              let c = Char.lowercase (Char.chr (Char.code s.[i+1] + 0x40)) in
              String.make 1 c ]
      | 0xC4 ->
          match Char.code s.[i+1] with
          [ 0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 -> "a"
          | 0x86 | 0x87 | 0x88 | 0x89
          | 0x8A | 0x8B | 0x8C | 0x8D -> "c"
          | 0x8E | 0x8F | 0x90 | 0x91 -> "d"
          | 0x92 | 0x93 | 0x94 | 0x95 | 0x96
          | 0x97 | 0x98 | 0x99 | 0x9A | 0x9B -> "e"
          | 0x9C | 0x9D | 0x9E | 0x9F
          | 0xA0 | 0xA1 | 0xA2 | 0xA3 -> "g"
          | 0xA4 | 0xA5 | 0xA6 | 0xA7 -> "h"
          | 0xA8 | 0xA9 | 0xAA | 0xAB | 0xAC
          | 0xAD | 0xAE | 0xAF | 0xB0 | 0xB1 -> "i"
          | 0xB2 | 0xB3 -> "ij"
          | 0xB4 | 0xB5 -> "i"
          | 0xB6 | 0xB7 | 0xB8 -> "k"
          | 0xB9 | 0xBA | 0xBB | 0xBC
          | 0xBD | 0xBE | 0xBF -> "l"
          | _ -> String.sub s i nbc ]
      | 0xC5 ->
          match Char.code s.[i+1] with
          [ 0x80 | 0x81 | 0x82 -> "l"
          | 0x83 | 0x84 | 0x85 | 0x86 | 0x87
          | 0x88 | 0x89 | 0x8A | 0x8B -> "n"
          | 0x8C | 0x8D | 0x8E | 0x8F | 0x90 | 0x91 -> "o"
          | 0x92 | 0x93 -> "oe"
          | 0x94 | 0x95 | 0x96 | 0x97 | 0x98 | 0x99 -> "r"
          | 0x9A | 0x9B | 0x9C | 0x9D
          | 0x9E | 0x9F | 0xA0 | 0xA1 -> "s"
          | 0xA2 | 0xA3 | 0xA4 | 0xA5 | 0xA6 | 0xA7 -> "t"
          | 0xA8 | 0xA9 | 0xAA | 0xAB | 0xAC | 0xAD
          | 0xAE | 0xAF | 0xB0 | 0xB1 | 0xB2 | 0xB3 -> "u"
          | 0xB4 | 0xB5 -> "w"
          | 0xB6 | 0xB7 | 0xB8 -> "y"
          | 0xB9 | 0xBA | 0xBB | 0xBC | 0xBD | 0xBE -> "z"
          | _ -> String.sub s i nbc ]
      | 0xCE ->
          match Char.code s.[i+1] with
          [ 0x91 | 0xB1 -> "a"
          | 0x92 | 0xB2 -> "b"
          | 0x93 | 0xB3 -> "g"
          | 0x94 | 0xB4 -> "d"
          | 0x95 | 0xB5 -> "e"
          | 0x96 | 0xB6 -> "dz"
          | 0x97 | 0xB7 -> "e"
          | 0x98 | 0xB8 -> "th"
          | 0x99 | 0xB9 -> "i"
          | 0x9A | 0xBA -> "k"
          | 0x9B | 0xBB -> "l"
          | 0x9C | 0xBC -> "m"
          | 0x9D | 0xBD -> "n"
          | 0x9E | 0xBE -> "x"
          | 0x9F | 0xBF -> "o"
          | 0xA0 -> "p"
          | 0xA1 -> "r"
          | 0xA2 | 0xA3 -> "s"
          | 0xA4 -> "t"
          | 0xA5 -> "u"
          | 0xA6 -> "ph"
          | 0xA7 -> "kh"
          | 0xA8 -> "ps"
          | 0xA9 -> "o"
          | _ -> String.sub s i nbc ]
      | 0xCF ->
          match Char.code s.[i+1] with
          [ 0x80 -> "p"
          | 0x81 -> "r"
          | 0x82 | 0x83 -> "s"
          | 0x84 -> "t"
          | 0x85 -> "u"
          | 0x86 -> "ph"
          | 0x87 -> "kh"
          | 0x88 -> "ps"
          | 0x89 -> "o"
          | _ -> String.sub s i nbc ]
      | 0xD0 ->
          match Char.code s.[i+1] with
          [ 0x90 | 0xB0 -> "a"
          | 0x91 | 0xB1 -> "b"
          | 0x92 | 0xB2 -> "v"
          | 0x93 | 0xB3 -> "g"
          | 0x94 | 0xB4 -> "d"
          | 0x95 | 0xB5 -> "e"
          | 0x96 | 0xB6 -> "j"
          | 0x97 | 0xB7 -> "z"
          | 0x98 | 0xB8 -> "i"
          | 0x9A | 0xBA -> "k"
          | 0x9B | 0xBB -> "l"
          | 0x9C | 0xBC -> "m"
          | 0x9D | 0xBD -> "n"
          | 0x9E | 0xBE -> "o"
          | 0x9F | 0xBF -> "p"
          | 0xA0 -> "r"
          | 0xA1 -> "s"
          | 0xA2 -> "t"
          | 0xA3 -> "u"
          | 0xA4 -> "f"
          | 0xA7 -> "tch"
          | 0xAE -> "you"
          | 0xAF -> "ya"
          | _ -> String.sub s i nbc ]
      | 0xD1 ->
          match Char.code s.[i+1] with
          [ 0x80 -> "r"
          | 0x81 -> "s"
          | 0x82 -> "t"
          | 0x83 -> "u"
          | 0x84 -> "f"
          | 0x87 -> "tch"
          | 0x8E -> "you"
          | 0x8F -> "ya"
          | _ -> String.sub s i nbc ]
      | c ->
          String.sub s i nbc ]
    in
    (s, i + nbc)
;

(*
value char_iso_8859_1_of_utf_8 s i =
  if Char.code s.[i] < 0x80 then Some (s.[i], i + 1)
  else if Char.code s.[i] = 0xC2 then
    let c = Char.chr (Char.code s.[i+1]) in
    Some (c, i + 2)
  else if Char.code s.[i] = 0xC3 then
    let c = Char.chr (Char.code s.[i+1] + 0x40) in
    Some (c, i + 2)
  else None
;
*)

value next_chars_if_equiv s i t j =
  if i >= String.length s || j >= String.length t then None
  else if utf_8_db.val then
(*
    match (char_iso_8859_1_of_utf_8 s i, char_iso_8859_1_of_utf_8 t j) with
    [ (Some (c1, i), Some (c2, j)) ->
        if unaccent_iso_8859_1 (Char.lowercase c1) =
           unaccent_iso_8859_1 (Char.lowercase c2) then Some (i, j)
        else None
    | (None, None) ->
        if s.[i] = t.[j] then Some (i + 1, j + 1) else None
    | _ -> None ]
*)
    let (s1, i1) = unaccent_utf_8 s i in
    let (t1, j1) = unaccent_utf_8 t j in
    if s1 = t1 then Some (i1, j1) else None
(**)
  else if s.[i] = t.[j] then Some (i + 1, j + 1)
  else if
    unaccent_iso_8859_1 (Char.lowercase s.[i]) =
    unaccent_iso_8859_1 (Char.lowercase t.[j])
  then Some (i + 1, j + 1)
  else None
;

value lower s =
  copy False 0 0 where rec copy special i len =
    if i == String.length s then Buff.get len
    else if not utf_8_db.val || Char.code s.[i] < 0x80 then
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | 'à'..'ÿ' | 'À'..'Ý' | '0'..'9' | '.' as c
        ->
          let len = if special then Buff.store len ' ' else len in
          let c = unaccent_iso_8859_1 (Char.lowercase c) in
          copy False (i + 1) (Buff.store len c)
      | c ->
          copy (len <> 0) (i + 1) len ]
    else (* start of utf-8 multi-byte char *)
      let len = if special then Buff.store len ' ' else len in
      let (t, j) = unaccent_utf_8 s i in
      match t.[0] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' ->
          copy False j (Buff.mstore len t)
      | _ -> copy (len <> 0) j len ]
;

(* Name.abbrev *)

value abbrev_list =
  [("a", None); ("af", None); ("d", None); ("de", None); ("di", None);
   ("ier", Some "i"); ("of", None); ("saint", Some "st");
   ("sainte", Some "ste"); ("van", None); ("von", None); ("zu", None);
   ("zur", None)]
;

value rec is_word s i p ip =
  if ip == String.length p then
    if i == String.length s then True
    else if s.[i] = ' ' then True
    else False
  else
    if i == String.length s then False
    else if s.[i] == p.[ip] then is_word s (i+1) p (ip+1)
    else False
;

value rec search_abbrev s i =
  fun
  [ [(w, a) :: pl] ->
      if is_word s i w 0 then Some (String.length w, a)
      else search_abbrev s i pl
  | [] -> None ]
;

value abbrev s =
  copy True 0 0 where rec copy can_start_abbrev i len =
    if i >= String.length s then Buff.get len
    else
      match s.[i] with
      [ ' ' -> copy True (i + 1) (Buff.store len ' ')
      | c ->
          if can_start_abbrev then
            match search_abbrev s i abbrev_list with
            [ None -> copy False (i + 1) (Buff.store len c)
            | Some (n, Some a) -> copy False (i + n) (Buff.mstore len a)
            | Some (n, None) -> copy True (i + n + 1) len ]
          else copy False (i + 1) (Buff.store len c) ]
;

(* Name.strip *)

value strip s =
  copy 0 0 where rec copy i len =
    if i == String.length s then Buff.get len
    else if s.[i] == ' ' then copy (i + 1) len
    else copy (i + 1) (Buff.store len s.[i])
;

(* Name.crush *)

value roman_number s i =
  let rec loop i =
    if i == String.length s then Some i
    else if s.[i] == ' ' then Some i
    else
      match s.[i] with
      [ 'i' | 'v' | 'x' | 'l' -> loop (i + 1)
      | _ -> None ]
  in
  if i == 0 || s.[i-1] == ' ' then loop i else None
;

value crush s =
  copy 0 0 True where rec copy i len first_vowel =
    if i == String.length s then Buff.get len
    else if s.[i] == ' ' then copy (i + 1) len True
    else
      match roman_number s i with
      [ Some j ->
          loop i len where rec loop i len =
            if i = j then copy j len True
            else loop (i + 1) (Buff.store len s.[i])
      | _ ->
          match s.[i] with
          [ 'a' | 'e' | 'i' | 'o' | 'u' | 'y' ->
              let len = if first_vowel then Buff.store len 'e' else len in
              copy (i + 1) len False
          | 'h' ->
              let len =
                if i > 0 && s.[i-1] == 'p' then Buff.store (len - 1) 'f'
                else len
              in
              copy (i + 1) len first_vowel
          | 's' | 'z' when
            utf_8_db.val && (i == String.length s - 1 || s.[i + 1] == ' ') ->
              let len =
                loop (i - 1) (len - 1) where rec loop i len =
                  if i > 0 && len > 0 && s.[i] = Buff.buff.val.[len] &&
                    (s.[i] = 's' || s.[i] = 'z') then
                    loop (i - 1) (len - 1)
                  else len + 1
              in
              copy (i + 1) len False
          | 's' when i == String.length s - 1 || s.[i + 1] == ' ' ->
              copy (i + 1) len False
          | c ->
              if i > 0 && s.[i-1] == c then copy (i + 1) len False
              else
                let c =
                  match c with
                  [ 'k' | 'q' -> 'c'
                  | 'z' -> 's'
                  | c -> c ]
                in
                copy (i + 1) (Buff.store len c) False ] ]
;

(* strip_lower *)

value strip_lower s = strip (lower s);

(* crush_lower *)

value crush_lower s = crush (abbrev (lower s));
