(* $Id: httpEnv.ml,v 1.2 2000-07-25 13:23:04 ddr Exp $ *)

open Printf;

(* Env from a string *)

value rec skip_spaces s i =
  if i < String.length s && s.[i] == ' ' then skip_spaces s (i + 1)
  else i
;

value create_env s =
  let rec get_assoc beg i =
    if i == String.length s then
      if i == beg then [] else [String.sub s beg (i - beg)]
    else if s.[i] == ';' || s.[i] == '&' then
      let next_i = skip_spaces s (succ i) in
      [String.sub s beg (i - beg) :: get_assoc next_i next_i]
    else get_assoc beg (succ i)
  in
  let rec separate i s =
    if i = String.length s then (s, "")
    else if s.[i] == '=' then
      (String.sub s 0 i, String.sub s (succ i) (String.length s - succ i))
    else separate (succ i) s
  in
  List.map (separate 0) (get_assoc 0 0)
;

value getenv env label =
  try Some (List.assoc label env) with [ Not_found -> None ]
;

(* Multipart env *)

value is_multipart_form =
  let s = "multipart/form-data" in
  fun content_type ->
    loop 0 where rec loop i =
      if i >= String.length content_type then False
      else if i >= String.length s then True
      else if content_type.[i] == s.[i] then loop (i + 1)
      else False
;

value extract_boundary content_type =
  let e = create_env content_type in
  List.assoc "boundary" e
;

value strip_quotes s =
  let i0 =
    if String.length s > 0 && s.[0] == '"' then 1 else 0
  in
  let i1 =
    if String.length s > 0
    && s.[String.length s - 1] == '"' then
      String.length s - 1
    else String.length s
  in
  String.sub s i0 (i1 - i0)
;

value extract_multipart boundary str =
  let rec skip_nl i =
    if i < String.length str && str.[i] == '\r' then skip_nl (i + 1)
    else if i < String.length str && str.[i] == '\n' then i + 1
    else i
  in
  let next_line i =
    let i = skip_nl i in
    loop "" i where rec loop s i =
      if i == String.length str || str.[i] == '\n' || str.[i] == '\r' then
        (s, i)
      else loop (s ^ String.make 1 str.[i]) (i + 1)
  in
  let boundary = "--" ^ boundary in
  let rec loop i =
    if i == String.length str then []
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
                  if i > String.length boundary
                  && String.sub str (i - String.length boundary)
                       (String.length boundary) = boundary
                  then i - String.length boundary
                  else loop (i + 1)
                else i
            in
            let v = String.sub str i (i1 - i) in
            [(var ^ "_name", filename); (var, v) :: loop i1]
        | (Some var, None) ->
            let var = strip_quotes var in
            let (s, i) = next_line i in
            if s = "" then
              let (s, i) = next_line i in
              [(var, s) :: loop i]
            else loop i
        | _ -> loop i ]
      else if s = boundary ^ "--" then []
      else loop i
  in
  let env = loop 0 in
  let (str, _) =
    List.fold_left
      (fun (str, sep) (v, x) ->
         if v = "file" then (str, sep) else (str ^ sep ^ v ^ "=" ^ x, ";"))
      ("", "") env
  in
  (str, env)
;

value make content_type str =
  if is_multipart_form content_type then
    let boundary = extract_boundary content_type in
    let (str, env) = extract_multipart boundary str in
    (str, env)
  else
    (str, create_env str)
;
