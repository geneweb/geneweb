(* $Id: name.ml,v 4.23 2005-09-05 00:57:07 ddr Exp $ *)
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

value next_chars_if_equiv s i t j =
  if i >= String.length s || j >= String.length t then None
  else if utf_8_db.val then
    match (char_iso_8859_1_of_utf_8 s i, char_iso_8859_1_of_utf_8 t j) with
    [ (Some (c1, i), Some (c2, j)) ->
        if unaccent_iso_8859_1 (Char.lowercase c1) =
           unaccent_iso_8859_1 (Char.lowercase c2) then Some (i, j)
        else None
    | (None, None) ->
        if s.[i] = t.[j] then Some (i + 1, j + 1) else None
    | _ -> None ]
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
      let c = Char.code s.[i] in
      let nbc =
        if c < 0b11100000 then 2
        else if c < 0b11110000 then 3
        else if c < 0b11111000 then 4
        else if c < 0b11111100 then 5
        else if c < 0b11111110 then 6
        else (* bad utf-8 *) 1
      in
      if i + nbc > String.length s then (* bad utf-8 *) Buff.get len
      else
        let len = if special then Buff.store len ' ' else len in
        match c with
        [ 0xC2 | 0xC3 ->
            let c1 =
              if c = 0xC2 then Char.chr (Char.code s.[i+1])
              else Char.chr (Char.code s.[i+1] + 0x40)
            in
            let c2 = unaccent_iso_8859_1 (Char.lowercase c1) in
            match c2 with
            [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' ->
                copy False (i + 2) (Buff.store len c2)
            | _ -> copy (len <> 0) (i + 2) len ]
        | 0xC4 ->
            let len =
              match Char.code s.[i+1] with
              [ 0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 -> Buff.store len 'a'
              | 0x86 | 0x87 | 0x88 | 0x89
              | 0x8A | 0x8B | 0x8C | 0x8D -> Buff.store len 'c'
              | 0x8E | 0x8F | 0x90 | 0x91 -> Buff.store len 'd'
              | 0x92 | 0x93 | 0x94 | 0x95 | 0x96
              | 0x97 | 0x98 | 0x99 | 0x9A | 0x9B -> Buff.store len 'e'
              | 0x9C | 0x9D | 0x9E | 0x9F
              | 0xA0 | 0xA1 | 0xA2 | 0xA3 -> Buff.store len 'g'
              | 0xA4 | 0xA5 | 0xA6 | 0xA7 -> Buff.store len 'h'
              | 0xA8 | 0xA9 | 0xAA | 0xAB | 0xAC
              | 0xAD | 0xAE | 0xAF | 0xB0 | 0xB1 -> Buff.store len 'i'
              | 0xB2 | 0xB3 -> Buff.mstore len "ij"
              | 0xB4 | 0xB5 -> Buff.store len 'j'
              | 0xB6 | 0xB7 | 0xB8 -> Buff.store len 'k'
              | 0xB9 | 0xBA | 0xBB | 0xBC
              | 0xBD | 0xBE | 0xBF -> Buff.store len 'l'
              | _ -> Buff.gstore len s i nbc ]
            in
            copy False (i + nbc) len
        | 0xC5 ->
            let len =
              match Char.code s.[i+1] with
              [ 0x80 | 0x81 | 0x82 -> Buff.store len 'l'
              | 0x83 | 0x84 | 0x85 | 0x86 | 0x87
              | 0x88 | 0x89 | 0x8A | 0x8B -> Buff.store len 'n'
              | 0x8C | 0x8D | 0x8E | 0x8F | 0x90 | 0x91 -> Buff.store len 'o'
              | 0x92 | 0x93 -> Buff.mstore len "oe"
              | 0x94 | 0x95 | 0x96 | 0x97 | 0x98 | 0x99 -> Buff.store len 'r'
              | 0x9A | 0x9B | 0x9C | 0x9D
              | 0x9E | 0x9F | 0xA0 | 0xA1 -> Buff.store len 's'
              | 0xA2 | 0xA3 | 0xA4 | 0xA5 | 0xA6 | 0xA7 -> Buff.store len 't'
              | 0xA8 | 0xA9 | 0xAA | 0xAB | 0xAC | 0xAD
              | 0xAE | 0xAF | 0xB0 | 0xB1 | 0xB2 | 0xB3 -> Buff.store len 'u'
              | 0xB4 | 0xB5 -> Buff.store len 'w'
              | 0xB6 | 0xB7 | 0xB8 -> Buff.store len 'y'
              | 0xB9 | 0xBA | 0xBB | 0xBC | 0xBD | 0xBE -> Buff.store len 'z'
              | _ -> Buff.gstore len s i nbc ]
            in
            copy False (i + nbc) len
        | 0xD0 ->
            let len =
              match Char.code s.[i+1] with
              [ 0x90 | 0xB0 -> Buff.store len 'a'
              | 0x91 | 0xB1 -> Buff.store len 'b'
              | 0x92 | 0xB2 -> Buff.store len 'v'
              | 0x93 | 0xB3 -> Buff.store len 'g'
              | 0x95 | 0xB5 -> Buff.store len 'e'
              | 0x97 | 0xB7 -> Buff.store len 'z'
              | 0x98 | 0xB8 -> Buff.store len 'i'
              | 0x9A | 0xBA -> Buff.store len 'k'
              | 0x9B | 0xBB -> Buff.store len 'l'
              | 0x9C | 0xBC -> Buff.store len 'm'
              | 0x9D | 0xBD -> Buff.store len 'n'
              | 0x9E | 0xBE -> Buff.store len 'o'
              | _ -> Buff.gstore len s i nbc ]
            in
            copy False (i + nbc) len
        | 0xD1 ->
            let len =
              match Char.code s.[i+1] with
              [ 0x81 -> Buff.mstore len "s"
              | 0x84 -> Buff.mstore len "f"
              | 0x87 -> Buff.mstore len "tch"
              | 0x8F -> Buff.mstore len "ya"
              | _ -> Buff.gstore len s i nbc ]
            in
            copy False (i + nbc) len
        | c ->
            copy False (i + nbc) (Buff.gstore len s i nbc) ]
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
