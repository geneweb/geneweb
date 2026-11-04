module Boundary = struct
  type t = string

  let of_string = Fun.id
  let to_string = Fun.id
end

module Headers = struct
  type t = string list

  let of_lines = Fun.id
  let to_lines = Fun.id

  let get t header =
    Option.map String.trim
      (Mutil.extract_param (Printf.sprintf "%s:" header) '\n' t)
end

module Header = struct
  let get ~request header = Headers.get (Headers.of_lines request) header
end

type meth =
  | GET
  | POST
  | HEAD
  | PUT
  | DELETE
  | OPTIONS
  | TRACE
  | CONNECT
  | PATCH
  | OTHER of string

let meth_of_string = function
  | "GET" -> GET
  | "POST" -> POST
  | "HEAD" -> HEAD
  | "PUT" -> PUT
  | "DELETE" -> DELETE
  | "OPTIONS" -> OPTIONS
  | "TRACE" -> TRACE
  | "CONNECT" -> CONNECT
  | "PATCH" -> PATCH
  | s -> (
      match String.uppercase_ascii s with
      | "GET" -> GET
      | "POST" -> POST
      | "HEAD" -> HEAD
      | "PUT" -> PUT
      | "DELETE" -> DELETE
      | "OPTIONS" -> OPTIONS
      | "TRACE" -> TRACE
      | "CONNECT" -> CONNECT
      | "PATCH" -> PATCH
      | other -> OTHER other)

let string_of_meth = function
  | GET -> "GET"
  | POST -> "POST"
  | HEAD -> "HEAD"
  | PUT -> "PUT"
  | DELETE -> "DELETE"
  | OPTIONS -> "OPTIONS"
  | TRACE -> "TRACE"
  | CONNECT -> "CONNECT"
  | PATCH -> "PATCH"
  | OTHER s -> s

type http_version = Http_1_0 | Http_1_1 | Http_2_0 | Unknown_version of string

let http_version_of_string = function
  | "HTTP/1.0" -> Http_1_0
  | "HTTP/1.1" -> Http_1_1
  | "HTTP/2.0" | "HTTP/2" -> Http_2_0
  | s -> Unknown_version s

let string_of_http_version = function
  | Http_1_0 -> "HTTP/1.0"
  | Http_1_1 -> "HTTP/1.1"
  | Http_2_0 -> "HTTP/2.0"
  | Unknown_version s -> s

type path = string

let path_of_string = Fun.id
let string_of_path = Fun.id

type query_string = string

let query_of_string = Fun.id
let string_of_query = Fun.id

type request_line = {
  meth : meth;
  raw_target : string;
  path : path;
  query : query_string option;
  version : http_version option;
}

let is_valid_method_token s =
  s <> ""
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true | _ -> false)
       s

let parse_request_line_content line =
  match String.split_on_char ' ' (String.trim line) with
  | meth_str :: target :: rest
    when is_valid_method_token meth_str
         && String.length target > 0
         && (target.[0] = '/' || target.[0] = '*') ->
      let meth = meth_of_string meth_str in
      let target_no_slash =
        if target.[0] = '/' then String.sub target 1 (String.length target - 1)
        else target
      in
      let path, query =
        match String.index_opt target_no_slash '?' with
        | Some i ->
            let p = String.sub target_no_slash 0 i in
            let q =
              String.sub target_no_slash (i + 1)
                (String.length target_no_slash - i - 1)
            in
            (p, if q = "" then None else Some q)
        | None -> (target_no_slash, None)
      in
      let version =
        match rest with
        | [ v ] when v <> "" -> Some (http_version_of_string v)
        | _ -> None
      in
      Some { meth; raw_target = target; path; query; version }
  | _ -> None

let parse_request_line request =
  match List.find_opt (fun s -> s <> "") request with
  | Some line -> parse_request_line_content line
  | None -> None

let rec skip_spaces s i =
  if i < String.length s && s.[i] = ' ' then skip_spaces s (i + 1) else i

let create_env s =
  let s = (s : Adef.encoded_string :> string) in
  let rec get_assoc beg i =
    if i = String.length s then
      if i = beg then [] else [ String.sub s beg (i - beg) ]
    else if s.[i] = ';' || s.[i] = '&' then
      let next_i = skip_spaces s (succ i) in
      String.sub s beg (i - beg) :: get_assoc next_i next_i
    else get_assoc beg (succ i)
  in
  let separate s =
    match String.index_opt s '=' with
    | Some i ->
        ( String.sub s 0 i,
          Adef.encoded (String.sub s (succ i) (String.length s - succ i)) )
    | None -> (s, Adef.encoded "")
  in
  List.map separate (get_assoc 0 0)

let p_getenv env label = Option.map Mutil.decode (List.assoc_opt label env)

let merge_env url_params body_params =
  url_params
  @ List.filter (fun (k, _) -> not (List.mem_assoc k url_params)) body_params

module Env = struct
  type t = (string * Adef.encoded_string) list

  let empty = []
  let of_encoded_string s = create_env s
  let of_query_string q = if q = "" then [] else create_env (Adef.encoded q)
  let get t label = p_getenv t label
  let merge ~url_env ~body_env = merge_env url_env body_env
  let to_list = Fun.id
  let of_list = Fun.id
end

let start_with_case_insensitive ~prefix s =
  let len_p = String.length prefix in
  let len_s = String.length s in
  if len_s < len_p then false
  else
    let rec loop i =
      if i = len_p then true
      else if Char.lowercase_ascii s.[i] = Char.lowercase_ascii prefix.[i] then
        loop (i + 1)
      else false
    in
    loop 0

let is_multipart_form =
  start_with_case_insensitive ~prefix:"multipart/form-data"

let strip_quotes s =
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then String.sub s 1 (len - 2)
  else s

type content_type =
  | UrlEncoded
  | MultipartForm of { boundary : Boundary.t }
  | Other of string

let parse_content_type header =
  let header = String.trim header in
  if is_multipart_form header then
    let env = create_env (Adef.encoded header) in
    match
      List.find_opt (fun (k, _) -> String.lowercase_ascii k = "boundary") env
    with
    | Some (_, b) ->
        MultipartForm
          {
            boundary =
              Boundary.of_string
                (strip_quotes (b : Adef.encoded_string :> string));
          }
    | None -> Other header
  else if
    start_with_case_insensitive ~prefix:"application/x-www-form-urlencoded"
      header
  then UrlEncoded
  else Other header

let extract_boundary_opt content_type =
  match parse_content_type (content_type : Adef.encoded_string :> string) with
  | MultipartForm { boundary } -> Some boundary
  | _ -> None

let extract_boundary content_type =
  match extract_boundary_opt content_type with
  | Some boundary -> boundary
  | None -> raise Not_found

let rec skip_nl str i =
  if i < String.length str && str.[i] = '\r' then skip_nl str (i + 1)
  else if i < String.length str && str.[i] = '\n' then i + 1
  else i

let next_line str i =
  let i = skip_nl str i in
  let rec find_end j =
    if j = String.length str || str.[j] = '\n' || str.[j] = '\r' then j
    else find_end (j + 1)
  in
  let end_idx = find_end i in
  (String.sub str i (end_idx - i), end_idx)

let find_boundary str boundary start_pos =
  let b_len = String.length boundary in
  let s_len = String.length str in
  let rec check idx k =
    if k = b_len then true
    else if str.[idx + k] = boundary.[k] then check idx (k + 1)
    else false
  in
  let rec find j =
    match String.index_from_opt str j boundary.[0] with
    | None -> s_len
    | Some idx ->
        if idx + b_len <= s_len then if check idx 1 then idx else find (idx + 1)
        else s_len
  in
  find start_pos

let parse_part_headers str i =
  let rec read_headers curr_i headers =
    let line, next_i = next_line str curr_i in
    if line = "" then (headers, next_i)
    else read_headers next_i (line :: headers)
  in
  let part_headers, body_i = read_headers i [] in
  let env =
    List.concat_map
      (fun h -> create_env (String.lowercase_ascii h |> Adef.encoded))
      part_headers
  in
  (env, body_i)

let parse_part_content str boundary body_i env loop =
  match (p_getenv env "name", p_getenv env "filename") with
  | Some var, Some filename ->
      let var = strip_quotes var in
      let filename = strip_quotes filename in
      let body_i = skip_nl str body_i in
      let i1 = find_boundary str boundary body_i in
      let v = String.sub str body_i (i1 - body_i) in
      (var ^ "_name", Mutil.encode filename) :: (var, Adef.encoded v) :: loop i1
  | Some var, None ->
      let var = strip_quotes var in
      let val_line, next_i = next_line str body_i in
      if val_line = boundary || val_line = boundary ^ "--" then
        (var, Adef.encoded "") :: loop body_i
      else (var, Adef.encoded val_line) :: loop next_i
  | None, _ -> loop body_i

let extract_multipart boundary str =
  let str = (str : Adef.encoded_string :> string) in
  let boundary = "--" ^ Boundary.to_string boundary in
  let rec loop i =
    if i = String.length str then []
    else
      let s, i = next_line str i in
      if s = boundary then
        let env, body_i = parse_part_headers str i in
        parse_part_content str boundary body_i env loop
      else if s = boundary ^ "--" then []
      else loop i
  in
  let env = loop 0 in
  let str, _ =
    List.fold_left
      (fun (str, sep) (v, x) ->
        if v = "file" then (str, sep)
        else
          let open Def in
          (str ^^^ sep ^<^ v ^<^ "=" ^<^ x, "&"))
      (Adef.encoded "", "")
      env
  in
  (str, env)

let parse_query_params q = Env.of_query_string q
