(* $Id: httpEnv.ml,v 5.1 2006-10-15 15:39:38 ddr Exp $ *)

open Printf;

(* Decode/Encode for URLs *)

value hexa_val conf =
  match conf with
  [ '0'..'9' -> Char.code conf - Char.code '0'
  | 'a'..'f' -> Char.code conf - Char.code 'a' + 10
  | 'A'..'F' -> Char.code conf - Char.code 'A' + 10
  | _ -> 0 ]
;

value decode s =
  let rec need_decode i =
    if i < String.length s then
      match s.[i] with
      [ '%' | '+' -> True
      | _ -> need_decode (succ i) ]
    else False
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
        [ '%' when i + 2 < String.length s -> i + 3
        | _ -> succ i ]
      in
      compute_len i (succ i1)
    else i1
  in
  let rec copy_decode_in s1 i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
        [ '%' when i + 2 < String.length s ->
            let v = hexa_val s.[i + 1] * 16 + hexa_val s.[i + 2] in
            do { Bytes.set s1 i1 (Char.chr v); i + 3 }
        | '+' -> do { Bytes.set s1 i1 ' '; succ i }
        | x -> do { Bytes.set s1 i1 x; succ i } ]
      in
      copy_decode_in s1 i (succ i1)
    else s1
  in
  let rec strip_heading_and_trailing_spaces s =
    if String.length s > 0 then
      if s.[0] = ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 1 (String.length s - 1))
      else if s.[String.length s - 1] = ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 0 (String.length s - 1))
      else s
    else s
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = Bytes.create len in
    strip_heading_and_trailing_spaces (copy_decode_in s1 0 0)
  else s
;

value hexa_digit x =
  if x >= 10 then Char.chr (Char.code 'A' + x - 10)
  else Char.chr (Char.code '0' + x)
;

value special =
  fun
  [ '\000'..'\031' | '\127'..'ÿ' | '<' | '>' | '"' | '#' | '%' | '{' | '}' |
    '|' | '\\' | '^' | '~' | '[' | ']' | '`' | ';' | '/' | '?' | ':' | '@' |
    '=' | '&' ->
      True
  | _ -> False ]
;

value encode s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
      [ ' ' -> True
      | x -> if special x then True else need_code (succ i) ]
    else False
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 = if special s.[i] then i1 + 3 else succ i1 in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
        [ ' ' -> do { Bytes.set s1 i1 '+'; succ i1 }
        | c ->
            if special c then do {
              Bytes.set s1 i1 '%';
              Bytes.set s1 (i1 + 1) (hexa_digit (Char.code c / 16));
              Bytes.set s1 (i1 + 2) (hexa_digit (Char.code c mod 16));
              i1 + 3
            }
            else do { Bytes.set s1 i1 c; succ i1 } ]
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (Bytes.create len) 0 0
  else s
;

(* Env from a string *)

value rec skip_spaces s i =
  if i < String.length s && s.[i] = ' ' then skip_spaces s (i + 1) else i
;

value create_env s =
  let rec get_assoc beg i =
    if i = String.length s then
      if i = beg then [] else [String.sub s beg (i - beg)]
    else if s.[i] = ';' || s.[i] = '&' then
      let next_i = skip_spaces s (succ i) in
      [String.sub s beg (i - beg) :: get_assoc next_i next_i]
    else get_assoc beg (succ i)
  in
  let rec separate i s =
    if i = String.length s then (s, "")
    else if s.[i] = '=' then
      (String.sub s 0 i, String.sub s (succ i) (String.length s - succ i))
    else separate (succ i) s
  in
  List.map (separate 0) (get_assoc 0 0)
;

value getenv env label =
  try Some (decode (List.assoc label env)) with [ Not_found -> None ]
;

(* Multipart env *)

value is_multipart_form =
  let s = "multipart/form-data" in
  fun content_type ->
    let rec loop i =
      if i >= String.length content_type then False
      else if i >= String.length s then True
      else if content_type.[i] = Char.lowercase s.[i] then loop (i + 1)
      else False
    in
    loop 0
;

value extract_boundary content_type =
  let e = create_env content_type in List.assoc "boundary" e
;

value strip_quotes s =
  let i0 = if String.length s > 0 && s.[0] = '"' then 1 else 0 in
  let i1 =
    if String.length s > 0 && s.[String.length s - 1] = '"' then
      String.length s - 1
    else String.length s
  in
  String.sub s i0 (i1 - i0)
;

value extract_multipart boundary str =
  let rec skip_nl i =
    if i < String.length str && str.[i] = '\r' then skip_nl (i + 1)
    else if i < String.length str && str.[i] = '\n' then i + 1
    else i
  in
  let next_line i =
    let i = skip_nl i in
    let rec loop s i =
      if i = String.length str || str.[i] = '\n' || str.[i] = '\r' then
        (s, i)
      else loop (s ^ String.make 1 str.[i]) (i + 1)
    in
    loop "" i
  in
  let boundary = "--" ^ boundary in
  let rec loop list i =
    if i = String.length str then list
    else
      let (s, i) = next_line i in
      if s = boundary then
        let (s, i) = next_line i in
        let s = String.lowercase s in
        let env = create_env s in
        match (getenv env "name", getenv env "filename") with
        [ (Some var, Some filename) ->
            let var = strip_quotes var in
            let filename = strip_quotes filename in
            let i = skip_nl i in
            let i1 =
              loop i where rec loop i =
                if i < String.length str then
                  if i > String.length boundary &&
                     String.sub str (i - String.length boundary)
                       (String.length boundary) =
                       boundary then
                    i - String.length boundary
                  else loop (i + 1)
                else i
            in
            let v = String.sub str i (i1 - i) in
            let list =
              [(var, v, False); (var ^ "_name", filename, True) :: list]
            in
            loop list i1
        | (Some var, None) ->
            let var = strip_quotes var in
            let (s, i) = next_line i in
            if s = "" then
              let (s, i) = next_line i in loop [(var, s, True) :: list] i
            else loop list i
        | _ -> loop list i ]
      else if s = boundary ^ "--" then list
      else loop list i
  in
  let env = loop [] 0 in
  let (str, env, _) =
    List.fold_left
      (fun (str, env, sep) (v, x, b) ->
         let (str, sep) =
           if b then (str ^ sep ^ v ^ "=" ^ x, ";") else (str, sep)
         in
         (str, [(v, x) :: env], sep))
      ("", [], "") env
  in
  (str, env)
;

value make content_type str =
  if is_multipart_form content_type then
    let boundary = extract_boundary content_type in
    let (str, env) = extract_multipart boundary str in (str, env)
  else (str, create_env str)
;
