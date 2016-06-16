(* $Id: ansel.ml,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

value no_accent =
  fun
  [ '\224'..'\229' -> 'a'
  | '\231' -> 'c'
  | '\232'..'\235' -> 'e'
  | '\236'..'\239' -> 'i'
  | '\241' -> 'n'
  | '\242'..'\246' | '\248' -> 'o'
  | '\249'..'\252' -> 'u'
  | '\253' | '\255' -> 'y'
  | '\192'..'\197' -> 'A'
  | '\199' -> 'C'
  | '\200'..'\203' -> 'E'
  | '\204'..'\207' -> 'I'
  | '\209' -> 'N'
  | '\210'..'\214' | '\216' -> 'O'
  | '\217'..'\220' -> 'U'
  | '\221' -> 'Y'
  | c -> c ]
;

value accent_code =
  fun
    [ '\192' | '\200' | '\204' | '\210' | '\217'
    | '\224' | '\232' | '\236' | '\242' | '\249' -> 255 (* grave *)
    | '\193' | '\201' | '\205' | '\211' | '\218' | '\221'
    | '\225' | '\233' | '\237' | '\243' | '\250' | '\253' -> 226 (* acute *)
    | '\194' | '\202' | '\206' | '\212' | '\219'
    | '\226' | '\234' | '\238' | '\244' | '\251' -> 227 (* circum *)
    | '\195' | '\209' | '\213' | '\227' | '\241' | '\245' -> 228 (* tilde *)
    | '\196' | '\203' | '\207' | '\214' | '\220'
    | '\228' | '\235' | '\239' | '\246' | '\252' | '\255' -> 232 (* uml *)
    | '\197' | '\229' -> 234 (* circle *)
    | '\199' | '\231' -> 240 (* cedil *)
    | '\216' -> 162
    | '\248' -> 178
    | '\223' -> 207
    | _ -> 0 ]
;

value of_iso_8859_1 s =
  let (len, identical) =
    loop 0 0 True where rec loop i len identical =
      if i = String.length s then (len, identical)
      else
        match s.[i] with
        [ '\192'..'\197' | '\199'.. '\207' | '\209'..'\214' | '\217'..'\221'
        | '\224'..'\229' | '\231'.. '\239' | '\241'..'\246' | '\249'..'\253' | '\255' ->
            loop (i + 1) (len + 2) False
        | '\216' | '\248' | '\223' -> loop (i + 1) (len + 1) False
        | _ -> loop (i + 1) (len + 1) identical ]
  in
  if identical then s
  else
    let s' = Bytes.create len in
    loop 0 0 where rec loop i i' =
      if i = String.length s then Bytes.unsafe_to_string s'
      else
        let i' =
          let a = accent_code s.[i] in
          if a > 0 then do {
            Bytes.set s' i' (Char.chr a);
            let n = no_accent s.[i] in
            if n = s.[i] then i'
            else do { Bytes.set s' (i'+1) n; i'+1 }
          } else do { Bytes.set s' i' s.[i]; i' } 
        in
        loop (i + 1) (i' + 1)
;

value grave =
  fun
  [ 'a' -> '\224'
  | 'e' -> '\232'
  | 'i' -> '\236'
  | 'o' -> '\242'
  | 'u' -> '\249'
  | 'A' -> '\192'
  | 'E' -> '\200'
  | 'I' -> '\204'
  | 'O' -> '\210'
  | 'U' -> '\217'
  | x -> x ]
;

value acute =
  fun
  [ 'a' -> '\225'
  | 'e' -> '\233'
  | 'i' -> '\237'
  | 'o' -> '\243'
  | 'u' -> '\250'
  | 'y' -> '\253'
  | 'A' -> '\193'
  | 'E' -> '\201'
  | 'I' -> '\205'
  | 'O' -> '\211'
  | 'U' -> '\218'
  | 'Y' -> '\221'
  | x -> x ]
;

value circum =
  fun
  [ 'a' -> '\226'
  | 'e' -> '\234'
  | 'i' -> '\238'
  | 'o' -> '\244'
  | 'u' -> '\251'
  | 'A' -> '\194'
  | 'E' -> '\202'
  | 'I' -> '\206'
  | 'O' -> '\212'
  | 'U' -> '\219'
  | x -> x ]
;

value uml =
  fun
  [ 'a' -> '\228'
  | 'e' -> '\235'
  | 'i' -> '\239'
  | 'o' -> '\246'
  | 'u' -> '\252'
  | 'y' -> '\255'
  | 'A' -> '\196'
  | 'E' -> '\203'
  | 'I' -> '\207'
  | 'O' -> '\214'
  | 'U' -> '\220'
  | x -> x ]
;

value circle =
  fun
  [ 'a' -> '\229'
  | 'A' -> '\197'
  | x -> x ]
;

value tilde =
  fun
  [ 'a' -> '\227'
  | 'n' -> '\241'
  | 'o' -> '\245'
  | 'A' -> '\195'
  | 'N' -> '\209'
  | 'O' -> '\213'
  | x -> x ]
;

value cedil =
  fun
  [ 'c' -> '\231'
  | 'C' -> '\199'
  | x -> x ]
;

value to_iso_8859_1 s =
  let (len, identical) =
    loop 0 0 True where rec loop i len identical =
      if i = String.length s then (len, identical)
      else if i = String.length s - 1 then (len + 1, identical)
      else
        match Char.code s.[i] with
        [ 225 | 226 | 227 | 228 | 232 | 234 | 240 ->
            loop (i + 2) (len + 1) False
        | 162 | 178 | 207 -> loop (i + 1) (len + 1) False
        | _ -> loop (i + 1) (len + 1) identical]
  in
  if identical then s
  else
    let s' = Bytes.create len in
    loop 0 0 where rec loop i i' =
      if i = String.length s then Bytes.unsafe_to_string s'
      else if i = String.length s - 1 then
        do { Bytes.set s' i' s.[i]; Bytes.unsafe_to_string s' }
      else
        let (c, i) =
          match Char.code s.[i] with
          [ 162 -> ('\216', i)
          | 178 -> ('\248', i)
          | 207 -> ('\223', i)
          | 225 -> (grave s.[i+1], i + 1)
          | 226 -> (acute s.[i+1], i + 1)
          | 227 -> (circum s.[i+1], i + 1)
          | 228 -> (tilde s.[i+1], i + 1)
          | 232 -> (uml s.[i+1], i + 1)
          | 234 -> (circle s.[i+1], i + 1)
          | 240 -> (cedil s.[i+1], i + 1)
          | _ -> (s.[i], i) ]
        in
        do { Bytes.set s' i' c;
             loop (i + 1) (i' + 1) }
;
