(* $Id: name.ml,v 4.13 2005-02-04 20:18:59 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

module Buff =
  struct
    value buff = ref (String.create 80);
    value store len x =
      do {
        if len >= String.length buff.val then
          buff.val := buff.val ^ String.create (String.length buff.val)
        else ();
        buff.val.[len] := x;
        succ len
      }
    ;
    value mstore len s =
      add_rec len 0 where rec add_rec len i =
        if i == String.length s then len
        else add_rec (store len s.[i]) (succ i)
    ;
    value gstore len s si slen =
      let iend = si + slen in
      add_rec len si where rec add_rec len i =
        if i == iend || i == String.length s then len
        else add_rec (store len s.[i]) (succ i)
    ;
    value get len = String.sub buff.val 0 len;
  end
;

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
  | c -> c ]
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
	[ 0xC2 ->
            let c1 = Char.chr (Char.code s.[i+1]) in
            let c2 = unaccent_iso_8859_1 c1 in
            if c2 <> c1 then copy False (i + 2) (Buff.store len c2)
            else copy False (i + 2) (Buff.gstore len s i 2)
        | 0xC3 ->
            let c1 = Char.chr (Char.code s.[i+1] + 0x40) in
            let c2 = unaccent_iso_8859_1 c1 in
            if c2 <> c1 then copy False (i + 2) (Buff.store len c2)
            else copy False (i + 2) (Buff.gstore len s i 2)
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
(*
  In case of need of major release, I can add this feature also and add the
  following comment in the CHANGES file. Not done now (3 May 2001) because
  it is a minor change, but with a probleming incompatibility.

  - [date] Changed index: names ending with s, z, sz, ss (s-tsett?) are now
    treated like if only ended by one s (therefore could be ignored in
    search). The index being different, databases must be rebuild (radical
    cleanup or consang program, option -i), otherwise the requests concerning
    people whose names end with z, sz or ss may not return the good result.

          | 's' | 'z' when i == String.length s - 1 || s.[i + 1] == ' ' ->
              let len =
                loop (i - 1) (len - 1) where rec loop i len =
                  if i > 0 && len > 0 && s.[i] = Buff.buff.val.[len] &&
                    (s.[i] = 's' || s.[i] = 'z') then
                    loop (i - 1) (len - 1)
                  else len + 1
              in
              copy (i + 1) len False
*)
          | 's' when i == String.length s - 1 || s.[i + 1] == ' ' ->
              copy (i + 1) len False
(**)
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
