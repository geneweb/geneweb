module Ast = Geneweb_templ.Ast
module Parser = Geneweb_templ.Parser

let ( // ) dir f = dir ^ "/" ^ f

let key_functions =
  [
    "transl";
    "transl_nth";
    "transl_decline";
    "ftransl";
    "ftransl_nth";
    "cftransl";
    "transl_args";
    "eval_transl";
  ]

let keywords =
  [
    "and";
    "as";
    "begin";
    "do";
    "done";
    "else";
    "end";
    "fun";
    "function";
    "if";
    "in";
    "let";
    "match";
    "of";
    "rec";
    "then";
    "type";
    "val";
    "when";
    "with";
  ]

let find_sub s sub from =
  let n = String.length sub and len = String.length s in
  let rec at i k = k = n || (s.[i + k] = sub.[k] && at i (k + 1)) in
  let rec loop i =
    if i + n > len then None else if at i 0 then Some i else loop (i + 1)
  in
  loop from

let replace_all s sub by =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    match find_sub s sub i with
    | Some j ->
        Buffer.add_substring b s i (j - i);
        Buffer.add_string b by;
        loop (j + String.length sub)
    | None -> Buffer.add_substring b s i (String.length s - i)
  in
  loop 0;
  Buffer.contents b

let read_string s i =
  let len = String.length s in
  let b = Buffer.create 64 in
  let number prefix j n =
    if j + n > len then None else int_of_string_opt (prefix ^ String.sub s j n)
  in
  let rec loop i nl =
    if i >= len then (Buffer.contents b, i, nl)
    else
      match s.[i] with
      | '"' -> (Buffer.contents b, i + 1, nl)
      | '\\' when i + 1 < len -> escape (i + 1) nl
      | c ->
          Buffer.add_char b c;
          loop (i + 1) (if c = '\n' then nl + 1 else nl)
  and escape i nl =
    let add c n =
      Buffer.add_char b c;
      loop (i + n) nl
    in
    match s.[i] with
    | 'n' -> add '\n' 1
    | 't' -> add '\t' 1
    | 'r' -> add '\r' 1
    | 'b' -> add '\b' 1
    | '\n' -> blanks (i + 1) (nl + 1)
    | '0' .. '9' -> (
        match number "" i 3 with
        | Some c when c < 256 -> add (Char.chr c) 3
        | Some _ | None -> add s.[i] 1)
    | 'x' -> (
        match number "0x" (i + 1) 2 with
        | Some c -> add (Char.chr c) 3
        | None -> add 'x' 1)
    | 'u' -> (
        match String.index_from_opt s i '}' with
        | Some j when j > i + 2 && s.[i + 1] = '{' -> (
            match number "0x" (i + 2) (j - i - 2) with
            | Some c when Uchar.is_valid c ->
                Buffer.add_utf_8_uchar b (Uchar.of_int c);
                loop (j + 1) nl
            | Some _ | None -> add 'u' 1)
        | Some _ | None -> add 'u' 1)
    | c -> add c 1
  and blanks i nl =
    if i < len && (s.[i] = ' ' || s.[i] = '\t') then blanks (i + 1) nl
    else loop i nl
  in
  loop i 0

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' | '.' -> true
  | _ -> false

let is_affix s =
  let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
  String.length s >= 4
  && String.fold_left (fun n c -> if is_letter c then n + 1 else n) 0 s >= 2

let scan_ml src =
  let len = String.length src in
  let strings = ref [] and keys = ref [] and affixes = ref [] in
  let caret = ref false in
  let rec next_caret j =
    j < len
    &&
    match src.[j] with
    | ' ' | '\t' | '\n' | '\r' -> next_caret (j + 1)
    | c -> c = '^'
  in
  let literal lnum pending v j =
    strings := v :: !strings;
    if pending then keys := (lnum, v) :: !keys;
    (if is_affix v then
       match (!caret, next_caret j) with
       | true, false -> affixes := ("\002" ^ v) :: !affixes
       | false, true -> affixes := (v ^ "\002") :: !affixes
       | true, true | false, false -> ());
    caret := false
  in
  let newlines s =
    String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 s
  in
  let rec code i lnum pending =
    if i < len then
      match src.[i] with
      | '\n' -> code (i + 1) (lnum + 1) pending
      | ' ' | '\t' | '\r' -> code (i + 1) lnum pending
      | '~' | '?' | ':' | '0' .. '9' ->
          caret := false;
          code (i + 1) lnum pending
      | '^' ->
          caret := true;
          code (i + 1) lnum false
      | '"' ->
          let v, j, nl = read_string src (i + 1) in
          literal lnum pending v j;
          code j (lnum + nl) false
      | '(' when i + 1 < len && src.[i + 1] = '*' ->
          let j, nl = comment (i + 2) 1 0 in
          code j (lnum + nl) pending
      | '{' -> quoted i lnum pending
      | '\'' -> char_lit i lnum pending
      | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
          caret := false;
          let j = ident_end (i + 1) in
          let id = String.sub src i (j - i) in
          let name =
            match String.rindex_opt id '.' with
            | Some k -> String.sub id (k + 1) (String.length id - k - 1)
            | None -> id
          in
          let pending =
            (not (List.mem id keywords))
            && (pending || List.mem name key_functions)
          in
          code j lnum pending
      | _ ->
          caret := false;
          code (i + 1) lnum false
  and ident_end j =
    if j < len && is_ident_char src.[j] then ident_end (j + 1) else j
  and comment i depth nl =
    if i + 1 >= len then (len, nl)
    else
      match (src.[i], src.[i + 1]) with
      | '*', ')' ->
          if depth = 1 then (i + 2, nl) else comment (i + 2) (depth - 1) nl
      | '(', '*' -> comment (i + 2) (depth + 1) nl
      | '"', _ ->
          let _, j, n = read_string src (i + 1) in
          comment j depth (nl + n)
      | '\'', '"' when i + 2 < len && src.[i + 2] = '\'' ->
          comment (i + 3) depth nl
      | '\n', _ -> comment (i + 1) depth (nl + 1)
      | _ -> comment (i + 1) depth nl
  and quoted i lnum pending =
    let rec id_end j =
      if j < len && ((src.[j] >= 'a' && src.[j] <= 'z') || src.[j] = '_') then
        id_end (j + 1)
      else j
    in
    let j = id_end (i + 1) in
    if j < len && src.[j] = '|' then
      let close = "|" ^ String.sub src (i + 1) (j - i - 1) ^ "}" in
      match find_sub src close (j + 1) with
      | Some k ->
          let v = String.sub src (j + 1) (k - j - 1) in
          let k = k + String.length close in
          literal lnum pending v k;
          code k (lnum + newlines v) false
      | None -> ()
    else (
      caret := false;
      code (i + 1) lnum false)
  and char_lit i lnum pending =
    caret := false;
    if i + 3 < len && src.[i + 1] = '\\' then
      match String.index_from_opt src (i + 3) '\'' with
      | Some j -> code (j + 1) lnum false
      | None -> ()
    else if i + 2 < len && src.[i + 2] = '\'' then code (i + 3) lnum false
    else code (i + 1) lnum pending
  in
  code 0 1 false;
  (List.rev !strings, List.rev !keys, !affixes)

let split_at_coloncolon s =
  let rec loop i =
    if i >= String.length s - 1 then None
    else if s.[i] = ':' && s.[i + 1] = ':' then
      Some (String.sub s 0 i, String.sub s (i + 2) (String.length s - i - 2))
    else loop (i + 1)
  in
  loop 0

type mode = Html | Code of string

type state = {
  mutable mode : mode;
  mutable keys : string list;
  mutable literals : string list;
  mutable patterns : string list;
}

let new_state () = { mode = Html; keys = []; literals = []; patterns = [] }

let openers =
  [ ("<script", "</script"); ("<style", "</style"); ("pattern=\"", "\"") ]

let rec advance mode s i =
  match mode with
  | Code close -> (
      match find_sub s close i with
      | Some j -> advance Html s (j + String.length close)
      | None -> mode)
  | Html -> (
      let next (o, close) =
        Option.map (fun j -> (j + String.length o, close)) (find_sub s o i)
      in
      match List.sort compare (List.filter_map next openers) with
      | (j, close) :: _ -> advance (Code close) s j
      | [] -> Html)

let is_phrase k =
  String.contains k ' '
  && not (String.exists (fun c -> String.contains "'\"=$,{}()#^\\.<>+" c) k)

let has_word s =
  let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
  let rec loop i n =
    n = 3
    || i < String.length s
       && loop (i + 1) (if is_letter s.[i] then n + 1 else 0)
  in
  loop 0 0

let rec walk st params (a : Ast.t) =
  match a.Ast.desc with
  | Ast.Atext s -> st.mode <- advance st.mode s 0
  | Ast.Atransl (_, s, c) -> List.iter (add_key st params) (transl_keys s c)
  | Ast.Avar _ | Ast.Awid_hei _ | Ast.Aint _ | Ast.Ainclude _ -> ()
  | Ast.Aif (e, l1, l2) ->
      walk st params e;
      walks st params l1;
      walks st params l2
  | Ast.Aforeach (_, ll, l) ->
      List.iter (walks st params) ll;
      walks st params l
  | Ast.Afor (_, e1, e2, l) ->
      walk st params e1;
      walk st params e2;
      walks st params l
  | Ast.Adefine (_, ps, l1, l2) ->
      List.iter (fun (_, d) -> Option.iter (walk st params) d) ps;
      walks st (List.rev_append (List.map fst ps) params) l1;
      walks st params l2
  | Ast.Aapply (_, args) ->
      List.iter
        (fun (_, l) ->
          (match l with
          | [ { Ast.desc = Ast.Atext s; _ } ] -> st.literals <- s :: st.literals
          | _ -> ());
          walks st params l)
        args
  | Ast.Alet (_, l1, l2) ->
      walks st params l1;
      walks st params l2
  | Ast.Aop1 (_, e) -> walk st params e
  | Ast.Aop2 (_, e1, e2) ->
      walk st params e1;
      walk st params e2
  | Ast.Apack l -> walks st params l

and walks st params l = List.iter (walk st params) l

and add_key st params k =
  let w = List.fold_left (fun k p -> replace_all k p "\002") k params in
  if k = "" then ()
  else if w <> k then (if has_word w then st.patterns <- w :: st.patterns)
  else
    match st.mode with
    | Code _ when not (is_phrase k) -> st.literals <- k :: st.literals
    | Code _ | Html -> st.keys <- k :: st.keys

and transl_keys s c =
  if c = "" && s <> "" && s.[0] = '\n' then []
  else
    match split_at_coloncolon s with
    | None -> word_keys s
    | Some (s1, s2) -> (
        let keys = word_keys s1 in
        if s2 = "" || s2.[0] = ':' then keys
        else if s2.[0] <> '|' then List.rev_append (word_keys s2) keys
        else
          match String.rindex_opt s2 '|' with
          | Some j when j > 0 ->
              List.rev_append (text_keys (String.sub s2 1 (j - 1))) keys
          | Some _ | None -> keys)

and word_keys w = if String.contains w '[' then text_keys w else [ w ]

and text_keys s =
  let st = new_state () and on_exn _ _ = () and resolve_include _ f = f in
  match Parser.parse ~on_exn ~resolve_include (`Raw s) with
  | l ->
      walks st [] l;
      st.keys
  | exception _ -> []

let rec files ?(skip = []) ext dir =
  if List.mem dir skip || not (Sys.file_exists dir && Sys.is_directory dir) then
    []
  else
    Sys.readdir dir |> Array.to_list |> List.sort String.compare
    |> List.concat_map (fun f ->
        let p = dir // f in
        if Sys.is_directory p then files ~skip ext p
        else if Filename.check_suffix f ext then [ p ]
        else [])

let ml_pattern s =
  if
    find_sub s "%d" 0 <> None
    && find_sub s "%s" 0 = None
    && find_sub s "%t" 0 = None
  then Some (replace_all s "%d" "\001")
  else None

let matches p k =
  let lp = String.length p and lk = String.length k in
  let rec go i j =
    if i = lp then j = lk
    else
      j < lk
      &&
      match p.[i] with
      | '\001' ->
          k.[j] >= '0' && k.[j] <= '9' && (go (i + 1) (j + 1) || go i (j + 1))
      | '\002' -> go (i + 1) (j + 1) || go i (j + 1)
      | c -> c = k.[j] && go (i + 1) (j + 1)
  in
  go 0 0

let orphans lexicon =
  let lex = Lex.read lexicon in
  let known = Hashtbl.create 4096 in
  let add_known (lex : Lex.t) =
    List.iter (fun e -> Hashtbl.replace known e.Lex.key ()) lex.Lex.entries
  in
  add_known lex;
  let plugins =
    if Sys.file_exists "plugins" && Sys.is_directory "plugins" then
      Sys.readdir "plugins" |> Array.to_list |> List.sort String.compare
      |> List.map (fun p -> "plugins" // p)
      |> List.filter Sys.is_directory
    else []
  in
  List.iter
    (fun p ->
      let f = p // "assets" // "lex" // "lexicon.txt" in
      if Sys.file_exists f then add_known (Lex.read f))
    plugins;
  let used = Hashtbl.create 8192 and unknown = Hashtbl.create 64 in
  let use file lnum k =
    Hashtbl.replace used k ();
    if not (Hashtbl.mem known k) then Hashtbl.replace unknown (file, lnum, k) ()
  in
  let patterns = ref [] in
  [ "lib"; "bin"; "plugins" ]
  |> List.concat_map (files ~skip:[ "bin" // "setup"; "bin" // "gwlex" ] ".ml")
  |> List.iter (fun f ->
      let strings, keys, affixes =
        scan_ml (In_channel.with_open_bin f In_channel.input_all)
      in
      patterns := List.rev_append affixes !patterns;
      List.iter
        (fun s ->
          Hashtbl.replace used s ();
          Option.iter (fun p -> patterns := p :: !patterns) (ml_pattern s))
        strings;
      List.iter
        (fun (lnum, k) -> List.iter (use f lnum) (transl_keys k "0"))
        keys);
  ("hd" // "etc") :: List.map (fun p -> p // "assets" // "etc") plugins
  |> List.concat_map (files ".txt")
  |> List.iter (fun f ->
      match Parser.parse_file ~src:(`File f) f with
      | l ->
          let st = new_state () in
          walks st [] l;
          List.iter (use f 0) st.keys;
          List.iter (fun k -> Hashtbl.replace used k ()) st.literals;
          patterns := List.rev_append st.patterns !patterns
      | exception exn -> Printf.eprintf "%s: %s\n" f (Printexc.to_string exn));
  List.iter
    (fun e ->
      List.iter
        (fun l -> if l.Lex.lang = "->" then Hashtbl.replace used l.Lex.text ())
        e.Lex.lines)
    lex.Lex.entries;
  let is_used k =
    Hashtbl.mem used k || List.exists (fun p -> matches p k) !patterns
  in
  print_endline "Unused entries:";
  List.iter
    (fun e ->
      if not (is_used e.Lex.key) then
        Printf.printf "%s:%d: %s\n" lexicon e.Lex.key_lnum e.Lex.key)
    lex.Lex.entries;
  print_endline "\nKeys missing from the lexicon:";
  Hashtbl.fold (fun k () acc -> k :: acc) unknown []
  |> List.sort compare
  |> List.iter (fun (f, lnum, k) ->
      if lnum = 0 then Printf.printf "%s: %s\n" f k
      else Printf.printf "%s:%d: %s\n" f lnum k)
