(* $Id: name.ml,v 3.4 2001-01-06 09:55:57 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

(* Name.lower *)

value lower s =
  let rec name_len special i len =
    if i == String.length s then len
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | 'à'..'ÿ' | 'À'..'Ý' | '0'..'9' | '.' ->
          name_len 0 (i + 1) (len + special + 1)
      | _ ->
          if len == 0 then name_len 0 (i + 1) 0
          else name_len 1 (i + 1) len ]
  in
  let s' = String.create (name_len 0 0 0) in
  let rec copy special i i' =
    if i == String.length s then s'
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | 'à'..'ÿ' | 'À'..'Ý' | '0'..'9' | '.' as c
        ->
          let i' =
            if special then do s'.[i'] := ' '; return i' + 1 else i'
          in
          let c =
            match Char.lowercase c with
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
          in
          do s'.[i'] := c; return copy False (i + 1) (i' + 1)
      | c ->
          if i' == 0 then copy False (i + 1) 0
          else copy True (i + 1) i' ]
  in
  copy False 0 0
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
  let rec name_len can_start_abbrev i i' =
    if i >= String.length s then i'
    else
      match s.[i] with
      [ ' ' -> name_len True (i + 1) (i' + 1)
      | c ->
          if can_start_abbrev then
            match search_abbrev s i abbrev_list with
            [ None -> name_len False (i + 1) (i' + 1)
            | Some (n, Some a) -> name_len False (i + n) (i' + String.length a)
            | Some (n, None) -> name_len True (i + n + 1) i' ]
          else name_len False (i + 1) (i' + 1) ]
  in
  let len = name_len True 0 0 in
  if len == String.length s then s
  else
    let s' = String.make len '@' in
    let rec copy can_start_abbrev i i' =
      if i >= String.length s then s'
      else
        match s.[i] with
        [ ' ' -> do s'.[i'] := ' '; return copy True (i + 1) (i' + 1)
        | c ->
            if can_start_abbrev then
              match search_abbrev s i abbrev_list with
              [ None -> do s'.[i'] := c; return copy False (i + 1) (i' + 1)
              | Some (n, Some a) ->
                  do String.blit a 0 s' i' (String.length a); return
                  copy False (i + n) (i' + String.length a)
              | Some (n, None) -> copy True (i + n + 1) i' ]
            else do s'.[i'] := c; return copy False (i + 1) (i' + 1) ]
    in
    copy True 0 0
;

(* Name.strip *)

value strip s =
  let rec name_len i len =
    if i == String.length s then len
    else if s.[i] == ' ' then name_len (i + 1) len
    else name_len (i + 1) (len + 1)
  in
  let len = name_len 0 0 in
  if len == String.length s then s
  else
    let s' = String.create len in
    let rec copy i i' =
      if i == String.length s then s'
      else if s.[i] == ' ' then copy (i + 1) i'
      else do s'.[i'] := s.[i]; return copy (i + 1) (i' + 1)
    in
    copy 0 0
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
  let rec name_len i len first_vowel =
    if i == String.length s then len
    else if s.[i] == ' ' then name_len (i + 1) len True
    else
      match roman_number s i with
      [ Some j -> name_len j (len + j - i) True
      | _ ->
          match s.[i] with
          [ 'a' | 'e' | 'i' | 'o' | 'u' | 'y' ->
              if first_vowel then name_len (i + 1) (len + 1) False
              else name_len (i + 1) len False
          | 'h' -> name_len (i + 1) len first_vowel
          | 's' when i == String.length s - 1 || s.[i + 1] == ' ' ->
              name_len (i + 1) len False
          | c ->
              if i > 0 && s.[i-1] == c then name_len (i + 1) len False
              else name_len (i + 1) (len + 1) False ] ]
  in
  let len = name_len 0 0 True in
  let s' = String.create len in
  let rec copy i i' first_vowel =
    if i == String.length s then s'
    else if s.[i] == ' ' then copy (i + 1) i' True
    else
      match roman_number s i with
      [ Some j ->
          do for k = i to j - 1 do s'.[k+i'-i] := s.[k]; done; return
          copy j (i' + j - i) True
      | _ ->
          match s.[i] with
          [ 'a' | 'e' | 'i' | 'o' | 'u' | 'y' ->
              if first_vowel then
                do s'.[i'] := 'e'; return copy (i + 1) (i' + 1) False
              else copy (i + 1) i' False
          | 'h' ->
              do if i > 0 && s.[i-1] == 'p' then s'.[i'-1] := 'f' else ();
              return copy (i + 1) i' first_vowel
          | 's' when i == String.length s - 1 || s.[i + 1] == ' ' ->
              copy (i + 1) i' False          
          | c ->
              if i > 0 && s.[i-1] == c then copy (i + 1) i' False
              else
                let c =
                  match c with
                  [ 'k' | 'q' -> 'c'
                  | 'z' -> 's'
                  | c -> c ]
                in
                do s'.[i'] := c; return copy (i + 1) (i' + 1) False ] ]
  in
  copy 0 0 True
;

(* strip_lower *)

value strip_lower s = strip (lower s);

(* crush_lower *)

value crush_lower s = crush (abbrev (lower s));
