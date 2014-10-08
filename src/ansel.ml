(* $Id: ansel.ml,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

value no_accent =
  fun
  [ 'à' | 'á' | 'â' | 'ã' | 'ä' | 'å' -> 'a'
  | 'ç' -> 'c'
  | 'è' | 'é' | 'ê' | 'ë' -> 'e'
  | 'ì' | 'í' | 'î' | 'ï' -> 'i'
  | 'ñ' -> 'n'
  | 'ò' | 'ó' | 'ô' | 'õ' | 'ö' | 'ø' -> 'o'
  | 'ù' | 'ú' | 'û' | 'ü' -> 'u'
  | 'ý' | 'ÿ' -> 'y'
  | 'À' | 'Á' | 'Â' | 'Ã' | 'Ä' | 'Å' -> 'A'
  | 'Ç' -> 'C'
  | 'È' | 'É' | 'Ê' | 'Ë' -> 'E'
  | 'Ì' | 'Í' | 'Î' | 'Ï' -> 'I'
  | 'Ñ' -> 'N'
  | 'Ò' | 'Ó' | 'Ô' | 'Õ' | 'Ö' | 'Ø' -> 'O'
  | 'Ù' | 'Ú' | 'Û' | 'Ü' -> 'U'
  | 'Ý' -> 'Y'
  | c -> c ]
;

value accent_code =
  fun
    [ 'À' | 'È' | 'Ì' | 'Ò' | 'Ù'
    | 'à' | 'è' | 'ì' | 'ò' | 'ù' -> 255
    | 'Á' | 'É' | 'Í' | 'Ó' | 'Ú' | 'Ý'
    | 'á' | 'é' | 'í' | 'ó' | 'ú' | 'ý' -> 226
    | 'Â' | 'Ê' | 'Î' | 'Ô' | 'Û'
    | 'â' | 'ê' | 'î' | 'ô' | 'û' -> 227
    | 'Ã' | 'Ñ' | 'Õ' | 'ã' | 'ñ' | 'õ' -> 228
    | 'Ä' | 'Ë' | 'Ï' | 'Ö' | 'Ü'
    | 'ä' | 'ë' | 'ï' | 'ö' | 'ü' | 'ÿ' -> 232
    | 'Å' | 'å' -> 234
    | 'Ç' | 'ç' -> 240
    | 'Ø' -> 162
    | 'ø' -> 178
    | 'ß' -> 207
    | _ -> 0 ]
;

value of_iso_8859_1 s =
  let (len, identical) =
    loop 0 0 True where rec loop i len identical =
      if i = String.length s then (len, identical)
      else
        match s.[i] with
        [ 'À'..'Å' | 'Ç'.. 'Ï' | 'Ñ'..'Ö' | 'Ù'..'Ý'
        | 'à'..'å' | 'ç'.. 'ï' | 'ñ'..'ö' | 'ù'..'ý' | 'ÿ' ->
            loop (i + 1) (len + 2) False
        | 'Ø' | 'ø' | 'ß' -> loop (i + 1) (len + 1) False
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
  [ 'a' -> 'à'
  | 'e' -> 'è'
  | 'i' -> 'ì'
  | 'o' -> 'ò'
  | 'u' -> 'ù'
  | 'A' -> 'À'
  | 'E' -> 'È'
  | 'I' -> 'Ì'
  | 'O' -> 'Ò'
  | 'U' -> 'Ù'
  | x -> x ]
;

value acute =
  fun
  [ 'a' -> 'á'
  | 'e' -> 'é'
  | 'i' -> 'í'
  | 'o' -> 'ó'
  | 'u' -> 'ú'
  | 'y' -> 'ý'
  | 'A' -> 'Á'
  | 'E' -> 'É'
  | 'I' -> 'Í'
  | 'O' -> 'Ó'
  | 'U' -> 'Ú'
  | 'Y' -> 'Ý'
  | x -> x ]
;

value circum =
  fun
  [ 'a' -> 'â'
  | 'e' -> 'ê'
  | 'i' -> 'î'
  | 'o' -> 'ô'
  | 'u' -> 'û'
  | 'A' -> 'Â'
  | 'E' -> 'Ê'
  | 'I' -> 'Î'
  | 'O' -> 'Ô'
  | 'U' -> 'Û'
  | x -> x ]
;

value uml =
  fun
  [ 'a' -> 'ä'
  | 'e' -> 'ë'
  | 'i' -> 'ï'
  | 'o' -> 'ö'
  | 'u' -> 'ü'
  | 'y' -> 'ÿ'
  | 'A' -> 'Ä'
  | 'E' -> 'Ë'
  | 'I' -> 'Ï'
  | 'O' -> 'Ö'
  | 'U' -> 'Ü'
  | x -> x ]
;

value circle =
  fun
  [ 'a' -> 'å'
  | 'A' -> 'Å'
  | x -> x ]
;

value tilde =
  fun
  [ 'a' -> 'ã'
  | 'n' -> 'ñ'
  | 'o' -> 'õ'
  | 'A' -> 'Ã'
  | 'N' -> 'Ñ'
  | 'O' -> 'Õ'
  | x -> x ]
;

value cedil =
  fun
  [ 'c' -> 'ç'
  | 'C' -> 'Ç'
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
          [ 162 -> ('Ø', i)
          | 178 -> ('ø', i)
          | 207 -> ('ß', i)
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
