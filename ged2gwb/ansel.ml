(* $Id: ansel.ml,v 1.1 1998-09-01 14:32:13 ddr Exp $ *)

value acute =
  fun
  [ 'a' -> 'á'
  | 'e' -> 'é'
  | 'i' -> 'í'
  | 'E' -> 'É'
  | x -> x ]
;

value grave =
  fun
  [ 'a' -> 'à'
  | 'e' -> 'è'
  | 'u' -> 'ù'
  | x -> x ]
;

value circ =
  fun
  [ 'a' -> 'â'
  | 'e' -> 'ê'
  | 'i' -> 'î'
  | 'o' -> 'ô'
  | x -> x ]
;

value uml =
  fun
  [ 'a' -> 'ä'
  | 'e' -> 'ë'
  | 'i' -> 'ï'
  | 'o' -> 'ö'
  | 'O' -> 'Ö'
  | x -> x ]
;

value cedil =
  fun
  [ 'c' -> 'ç'
  | x -> x ]
;

value translate s =
  let len =
    loop 0 0 where rec loop i len =
      if i == String.length s then len
      else if i == String.length s - 1 then len + 1
      else
        match Char.code s.[i] with
        [ 225 | 226 | 227 | 232 | 240 -> loop (i + 1) len
        | _ -> loop (i + 1) (len + 1) ]
  in
  if len == String.length s then s
  else
    let s' = String.create len in
    loop 0 0 where rec loop i i' =
      if i == String.length s then s'
      else if i == String.length s - 1 then
        do s'.[i'] := s.[i]; return s'
      else
        let i =
          match Char.code s.[i] with
          [ 225 -> do s'.[i'] := grave s.[i+1]; return i + 1
          | 226 -> do s'.[i'] := acute s.[i+1]; return i + 1
          | 227 -> do s'.[i'] := circ s.[i+1]; return i + 1
          | 232 -> do s'.[i'] := uml s.[i+1]; return i + 1
          | 240 -> do s'.[i'] := cedil s.[i+1]; return i + 1
          | _ -> do s'.[i'] := s.[i]; return i ]
        in
        loop (i + 1) (i' + 1)
;
