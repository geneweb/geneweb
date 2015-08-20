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

value of_iso_8859_1 s =
  let (len, identical) =
    loop 0 0 True where rec loop i len identical =
      if i = String.length s then (len, identical)
      else
        match s.[i] with
        [ 'À'..'Å' | 'Ç'.. 'Ï' | 'Ñ'..'Ö' | 'Ù'..'Ý'
        | 'à'..'å' | 'ç'.. 'ï' | 'ñ'..'ö' | 'ù'..'ý' | 'ÿ' ->
            loop (i + 1) (len + 2) False
        | 'Ø' -> loop (i + 1) (len + 1) False
        | 'ø' -> loop (i + 1) (len + 1) False
        | 'ß' -> loop (i + 1) (len + 1) False
        | _ -> loop (i + 1) (len + 1) identical ]
  in
  if identical then s
  else
    let s' = Bytes.create len in
    loop 0 0 where rec loop i i' =
      if i = String.length s then s'
      else
        let i' =
          match s.[i] with
          [ 'À' | 'È' | 'Ì' | 'Ò' | 'Ù'
          | 'à' | 'è' | 'ì' | 'ò' | 'ù' ->
              do {
                Bytes.set s' i' (Char.chr 225); Bytes.set s' (i'+1) (no_accent s.[i]);
                i' + 1
              }
          | 'Á' | 'É' | 'Í' | 'Ó' | 'Ú' | 'Ý'
          | 'á' | 'é' | 'í' | 'ó' | 'ú' | 'ý' ->
              do {
                Bytes.set s' i' (Char.chr 226); Bytes.set s' (i'+1) (no_accent s.[i]);
                i' + 1
              }
          | 'Â' | 'Ê' | 'Î' | 'Ô' | 'Û'
          | 'â' | 'ê' | 'î' | 'ô' | 'û' ->
              do {
                Bytes.set s' i' (Char.chr 227); Bytes.set s' (i'+1) (no_accent s.[i]);
                i' + 1
              }
          | 'Ã' | 'Ñ' | 'Õ' | 'ã' | 'ñ' | 'õ' ->
              do {
                Bytes.set s' i' (Char.chr 228); Bytes.set s' (i'+1) (no_accent s.[i]);
                i' + 1
              }
          | 'Ä' | 'Ë' | 'Ï' | 'Ö' | 'Ü'
          | 'ä' | 'ë' | 'ï' | 'ö' | 'ü' | 'ÿ' ->
              do {
                Bytes.set s' i' (Char.chr 232); Bytes.set s' (i'+1) (no_accent s.[i]);
                i' + 1
              }
          | 'Å' | 'å' ->
              do {
                Bytes.set s' i' (Char.chr 234); Bytes.set s' (i'+1) (no_accent s.[i]);
                i' + 1
              }
          | 'Ç' | 'ç' ->
              do {
                Bytes.set s' i' (Char.chr 240); Bytes.set s' (i'+1) (no_accent s.[i]);
                i' + 1
              }
          | 'Ø' -> do { Bytes.set s' i' (Char.chr 162); i' }
          | 'ø' -> do { Bytes.set s' i' (Char.chr 178); i' }
          | 'ß' -> do { Bytes.set s' i' (Char.chr 207); i' }
          | c -> do { Bytes.set s' i' c; i' } ]
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
        | 162 -> loop (i + 1) (len + 1) False
        | 178 -> loop (i + 1) (len + 1) False
        | 207 -> loop (i + 1) (len + 1) False
        | _ -> loop (i + 1) (len + 1) identical]
  in
  if identical then s
  else
    let s' = Bytes.create len in
    loop 0 0 where rec loop i i' =
      if i = String.length s then s'
      else if i = String.length s - 1 then
        do { Bytes.set s' i' s.[i]; s' }
      else
        let i =
          match Char.code s.[i] with
          [ 162 -> do { Bytes.set s' i' 'Ø'; i }
          | 178 -> do { Bytes.set s' i' 'ø'; i }
          | 207 -> do { Bytes.set s' i' 'ß'; i }
          | 225 -> do { Bytes.set s' i' (grave s.[i+1]); i + 1 }
          | 226 -> do { Bytes.set s' i' (acute s.[i+1]); i + 1 }
          | 227 -> do { Bytes.set s' i' (circum s.[i+1]); i + 1 }
          | 228 -> do { Bytes.set s' i' (tilde s.[i+1]); i + 1 }
          | 232 -> do { Bytes.set s' i' (uml s.[i+1]); i + 1 }
          | 234 -> do { Bytes.set s' i' (circle s.[i+1]); i + 1 }
          | 240 -> do { Bytes.set s' i' (cedil s.[i+1]); i + 1 }
          | _ -> do { Bytes.set s' i' s.[i]; i } ]
        in
        loop (i + 1) (i' + 1)
;
