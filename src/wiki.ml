(* camlp4r *)
(* $Id: wiki.ml,v 4.1 2005-07-07 11:24:41 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

value check_file_name s =
  loop 0 where rec loop i =
    if i = String.length s then True
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> loop (i + 1)
      | _ -> False ]
;

value ext_file_link s i =
  let slen = String.length s in
  let j =
    loop (i + 3) where rec loop j =
      if j = slen then j
      else if
        j < slen - 2 && s.[j] = ']' && s.[j+1] = ']' && s.[j+2] = ']'
      then j + 3
      else loop (j + 1)
  in
  if j > i + 6 then
    let b = String.sub s (i + 3) (j - i - 6) in
    let (fname, sname, text) =
      try
        let k = String.index b '/' in
        let j = try String.index b '#' with [ Not_found -> k ] in
        (String.sub b 0 j, String.sub b j (k - j),
         String.sub b (k + 1) (String.length b - k - 1))
      with
      [ Not_found -> (b, "", b) ]
    in
    if check_file_name fname then Some (j, fname, sname, text)
    else None
  else None
;

