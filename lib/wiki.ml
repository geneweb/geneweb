let _bench name fn =
  let p1 = Sys.time () in
  let t1 = Unix.gettimeofday () in
  let res = fn () in
  let t2 = Unix.gettimeofday () in
  let p2 = Sys.time () in
  Printf.printf "[%s]: %f seconds (~%f seconds of CPU time).\n" name (t2 -. t1) (p2 -. p1);
  res

(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util

module W = Wikitext

type notelink =
  | WLpage of (string list * string) * string * string * string
  | WLperson of int * Def.NLDB.key * string * string option
  | WLwizard of string * string

type document = block list

and block =
  | Header of int * inline list
  | Paragraph of inline list
  | List of block list list
  | NumList of block list list
  | DefList of (inline list * block list) list
  | Table of inline list * table_block list list
  | Hrule
  | Raw of string

and table_block =
  | TableHead of inline list
  | TableItem of inline list

and inline =
  | Bold of inline list
  | Italic of inline list
  | String of string
  | WLpage of (string list * string) * string * string * string
  | WLperson of int * Def.NLDB.key * string * string option
  | WLwizard of string * string

let notelinks : document -> notelink list =
  let rec inline (acc  : notelink list) : inline list -> notelink list = function
    | WLpage (a, b, c, d) :: tl -> inline (WLpage (a, b, c, d) :: acc) tl
    | WLperson (a, b, c, d) :: tl -> inline (WLperson (a, b, c, d) :: acc) tl
    | WLwizard (a, b) :: tl -> inline (WLwizard (a, b) :: acc) tl
    | (String _ | Italic _ | Bold _) :: tl -> inline acc tl
    | [] -> acc
  and block (acc  : notelink list) = function
    | Header (_, content) :: tl ->
      block (inline acc content) tl
    | Paragraph content :: tl ->
      block (inline acc content) tl
    | List blocks :: tl ->
      block (List.fold_left block acc blocks) tl
    | NumList blocks :: tl ->
      block (List.fold_left block acc blocks) tl
    | DefList list :: tl ->
      block (List.fold_left (fun acc (t, d) -> block (inline acc t) d) acc list) tl
    | Table (inlines, tb) :: tl ->
      block (List.fold_left tblock (inline acc inlines) tb) tl
    | Hrule :: tl -> block acc tl
    | Raw _ :: tl -> block acc tl
    | [] -> acc
  and tblock (acc  : notelink list) = function
    | TableHead content :: tl -> tblock (inline acc content) tl
    | TableItem content :: tl -> tblock (inline acc content) tl
    | [] -> acc
  in
  block []

let empty = []

let notelinks doc = List.rev @@ notelinks doc

let char_dir_sep = ':'

let check_file_name s =
  let rec loop path ibeg i =
    if i = String.length s
    then
      if i > ibeg
      then Some (List.rev path, String.sub s ibeg (i - ibeg))
      else None
    else
      match s.[i] with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' | '.' ->
        loop path ibeg (i + 1)
      | c ->
        if c = char_dir_sep
        then if i > ibeg
          then loop (String.sub s ibeg (i - ibeg) :: path) (i + 1) (i + 1)
          else None
        else None
  in
  loop [] 0 0

let of_wktxt ?(cnt = ref 0) =
  let rec inline = function
    | W.Type.Link s ->
      if String.length s > 4
      && String.unsafe_get s 0 = '['
      && String.unsafe_get s 1 = '['
      && String.unsafe_get s (String.length s - 1) = ']'
      && String.unsafe_get s (String.length s - 2) = ']'
      then
      let (fname, anchor, text) =
        match String.rindex_opt s '/' with
        | Some k ->
          let j = Opt.default k (String.index_opt s '#') in
          ( String.sub s 2 (j - 2)
          , String.sub s j (k - j)
          , String.sub s (k + 1) (String.length s - k - 3)
          )
        | None ->
          ( String.sub s 2 (String.length s - 3)
          , ""
          , String.sub s 2 (String.length s - 3) )
      in
      match check_file_name fname with
        | Some pg_path -> (WLpage (pg_path, fname, anchor, text) : inline)
        | None -> failwith fname
      else if String.length s > 2
      && String.unsafe_get s 0 = '['
      && String.unsafe_get s (String.length s - 1) = ']'
      then
        let (spe, b) =
          match String.index_opt s ':' with
          | Some i ->
            ( Some (String.sub s 1 (i - 1))
            , String.sub s (i + 1) (String.length s - i - 2) )
          | None -> None, String.sub s 1 (String.length s - 2)
        in
        let (b, text) =
          match String.rindex_opt b ';' with
          | Some i ->
            ( String.sub b 0 i
            , Some (String.sub b (i + 1) (String.length b - i - 1) ) )
          | None -> b, None
        in
        if spe = Some "w"
        then
          let (wiz, name) =
            match String.index_opt b '/' with
            | Some i ->
              ( String.sub b 0 i
              , String.sub b (i + 1) (String.length b - i - 1) )
            | None -> b, ""
          in
          WLwizard (wiz, name)
        else
          match String.index_opt b '/' with
          | Some l  ->
            let fn = String.sub b 0 l in
            let k = l + 1 in
            let (fn, sn, oc, name) =
              match String.index_from_opt b k '/' with
              | Some l  ->
                let sn = String.sub b k (l - k) in
                let (oc, name) =
                  let k = l + 1 in
                  begin match String.index_from_opt b k '/' with
                    | Some l ->
                      let x = String.sub b k (l - k) in
                      x, String.sub b (l + 1) (String.length b - l - 1)
                    | None ->
                      "", String.sub b (l + 1) (String.length b - l - 1)
                  end
                in
                let oc1 = try int_of_string name with Failure _ -> -1 in
                let oc = try int_of_string oc with Failure _ -> 0 in
                if oc1 = -1 then (fn, sn, oc, name)
                else (fn, sn, oc1, fn ^ " " ^ sn)
              | None ->
                let sn = String.sub b k (String.length b - k) in
                let name = fn ^ " " ^ sn in
                fn, sn, 0, name
            in
            let fn = Name.lower fn in
            let sn = Name.lower sn in
            let id = incr cnt ; !cnt in
            WLperson (id, (fn, sn, oc), name, text)
          | None ->
            assert false
      else
        let link url txt =
          String (Printf.sprintf {|<a href="%s">%s</a>|} url txt)
        in
        begin match String.index_opt s ' ' with
          | None -> link s s
          | Some pos ->
            link
              (String.sub s 0 pos)
              (String.sub s (pos + 1) (String.length s - pos - 1))
        end
    | W.Type.Bold x -> Bold (List.map inline x)
    | W.Type.Italic x -> Italic (List.map inline x)
    | W.Type.String x | W.Type.NoWiki x -> String x
  and block = function
    | W.Type.Header (n, content) ->
      Header (n, List.map inline content)
    | W.Type.Paragraph content ->
      Paragraph (List.map inline content)
    | W.Type.List blocks ->
      List (List.map (List.map block) blocks)
    | W.Type.NumList blocks ->
      NumList (List.map (List.map block) blocks)
    | W.Type.DefList list ->
      DefList begin List.map begin fun (t, d) ->
          (List.map inline t, List.map block d)
        end list end
    | W.Type.Table (inlines, tb) ->
      Table (List.map inline inlines, List.map (List.map table_block) tb)
    | W.Type.Hrule -> Hrule
    | W.Type.NoWikiBlock s -> Raw s
  and table_block = function
    | W.Type.TableHead content -> TableHead (List.map inline content)
    | W.Type.TableItem content -> TableItem (List.map inline content)
  in
  List.map block

let notes_aliases conf =
  let fname =
    match p_getenv conf.base_env "notes_alias_file" with
    | Some f -> Util.base_path [] f
    | None ->
      Filename.concat (Util.base_path [] (conf.bname ^ ".gwb"))
        "notes.alias"
  in
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
  | Some ic ->
    let rec loop list =
      match try Some (input_line ic) with End_of_file -> None with
        Some s ->
        let list =
          try
            let i = String.index s ' ' in
            (String.sub s 0 i,
             String.sub s (i + 1) (String.length s - i - 1)) ::
            list
          with Not_found -> list
        in
        loop list
      | None -> close_in ic; list
    in
    loop []
  | None -> []

let map_notes aliases f = try List.assoc f aliases with Not_found -> f

let fname_of_path (dirs, file) = List.fold_right Filename.concat dirs file

let file_path conf base fname =
  Util.base_path []
    (List.fold_left Filename.concat (conf.bname ^ ".gwb")
       [Gwdb.base_notes_dir base ; fname ^ ".txt"])

let to_wktxt conf base =
  let rec inline : inline -> _ = function
    | WLpage (fpath1, fname1, anchor, text) ->
      let (fpath, fname) =
        let aliases = notes_aliases conf in
        let fname = map_notes aliases fname1 in
        match check_file_name fname with
        | Some fpath -> fpath, fname
        | None -> fpath1, fname1
      in
      let t =
        if conf.cancel_links then text
        else
          let c =
            let f = file_path conf base (fname_of_path fpath) in
            if Sys.file_exists f then "" else " style=\"color:red\""
          in
          Printf.sprintf "<a href=\"%sm=NOTES&f=%s%s\"%s>%s</a>"
            (commd conf) fname anchor c text
      in
      W.Type.String t
    | WLperson (id, (fn, sn, oc), name, _) ->
      let t =
        if conf.cancel_links then name
        else if Util.person_exists conf base (fn, sn, oc)
        then
          Printf.sprintf {|<a id="p_%d" href="%sp=%s&n=%s%s">%s</a>|}
            id (commd conf) (code_varenv fn) (code_varenv sn)
            (if oc = 0 then "" else "&oc=" ^ string_of_int oc) name
        else if conf.wizard || conf.friend then
          Printf.sprintf {|<a id="p_%d" href="%sp=%s&n=%s%s" style="color:red">%s</a>|}
            id (commd conf) (code_varenv fn) (code_varenv sn)
            (if oc = 0 then "" else "&oc=" ^ string_of_int oc) name
        else
          Printf.sprintf "<a href=\"%s\" style=\"color:red\">%s</a>" (commd conf)
            (if conf.hide_names then "x x" else name)
      in
      W.Type.String t
    | WLwizard (wiz, name) ->
      let t =
        let s = if name <> "" then name else wiz in
        if conf.cancel_links then s
        else
          Printf.sprintf "<a href=\"%sm=WIZNOTES&f=%s\">%s</a>" (commd conf) wiz
            s
      in
      W.Type.String t
    | Bold x ->
      W.Type.Bold (List.map inline x)
    | Italic x ->
      W.Type.Italic (List.map inline x)
    | String x ->
      W.Type.String x
  and block = function
    | Header (n, content) ->
      W.Type.Header (n, List.map inline content)
    | Paragraph content ->
      W.Type.Paragraph (List.map inline content)
    | List blocks ->
      W.Type.List (List.map (List.map block) blocks)
    | NumList blocks ->
      W.Type.NumList (List.map (List.map block) blocks)
    | DefList list ->
      W.Type.DefList begin List.map begin fun (t, d) ->
          (List.map inline t, List.map block d)
        end list end
    | Table (inlines, tb) ->
      W.Type.Table (List.map inline inlines, List.map (List.map table_block) tb)
    | Hrule ->
      W.Type.Hrule
    | Raw s ->
      W.Type.NoWikiBlock s
  and table_block = function
    | TableHead content ->
      W.Type.TableHead (List.map inline content)
    | TableItem content ->
      W.Type.TableItem (List.map inline content)
  in
  List.map block

let need_toc doc =
  let rec loop acc = function
    | [] -> if acc > 1 then `yes else `no
    | W.Type.Header _ :: tl -> loop (acc + 1) tl
    | W.Type.Paragraph [ W.Type.String "__NOTOC__" ] :: _ -> `notoc
    | W.Type.Paragraph [ W.Type.String "__SHORT_TOC__" ] :: _ -> `short_toc
    | W.Type.Paragraph [ W.Type.String "__TOC__" ] :: _ -> `toc
    | _ :: tl -> loop acc tl
  in loop 0 doc

let toc doc =
  W.Mapper.toc doc

let first_cnt = 1

let edit_link conf cnt (can_edit, mode, sfn) =
  if conf.wizard then
    let mode_pref = if can_edit then "MOD" else "VIEW" in
    Some
      (Printf.sprintf
         {|<div style="font-size:80%%;float:%s;margin-%s:3em">(<a href="%sm=%s_%s&v=%d%s">%s</a>)</div>|}
         conf.right conf.left
         (commd conf) mode_pref mode cnt
         (if sfn = "" then "" else "&f=" ^ sfn)
         (if can_edit then transl_decline conf "modify" "" else transl conf "view source")
      )
  else None

let doc_of_string s =
  W.doc_from_string s
  |> of_wktxt

let html_of_wiki_inline conf base s =
  match to_wktxt conf base @@ doc_of_string s with
  | [ W.Type.Paragraph content ] ->
    let b = Buffer.create 1024 in
    List.iter
      (Wikitext__Wktxt_output.output_inline (Buffer.add_string b)) content ;
    Buffer.contents b
  | _ -> s

let doc_to_string conf base doc =
  W.doc_to_string (to_wktxt conf base doc)

let add_h1 s doc =
  Header (1, [ String s ]) :: doc

let replace_toc s doc =
  Paragraph [ String "__NOTOC__" ]
  :: List.fold_right begin fun x acc -> match x with
    | Paragraph [ String "__TOC__" ]
    | Paragraph [ String "__SHORT_TOC__" ] ->
      Raw s :: acc
    | _ -> x :: acc
  end doc []

let html_of_wiki ?edit conf base s =
  try
    let doc = _bench __LOC__ @@ fun () -> W.doc_from_string s in
    let need_toc = need_toc doc in
    let doc, toc =
      match need_toc with
      | `no | `notoc -> doc, None
      | `yes | `toc | `short_toc ->
        match toc doc with
        | Some (doc, toc) -> doc, Some toc
        | None -> doc, None
    in
    let doc = of_wktxt doc in
    let doc =
      match edit with
      | None -> doc
      | Some opt ->
        match doc with
        | Raw _
          :: Paragraph [ String ("__TOC__"|"__SHORT_TOC__"|"__NOTOC__") ]
          :: Header _
          :: _
        | Raw _ :: Header _ :: _
        | Header _ :: _
          -> doc
        | Raw x :: _ ->
          begin match edit_link conf 0 opt with
            | Some edit -> Raw x :: Raw edit :: doc
            | None -> doc
          end
        | doc -> begin match edit_link conf 0 opt with
            | Some edit -> Raw edit :: doc
            | None -> doc
          end
    in
    let doc =
      match need_toc with
      | `yes ->
        begin match toc with
          | Some t -> List.rev_append (of_wktxt [t]) doc
          | None -> doc
        end
      | `toc ->
        List.fold_right begin fun x acc -> match x with
          | Paragraph [ String "__TOC__" ] ->
            begin match toc with
              | Some t -> (List.hd @@ of_wktxt [t]) :: acc
              | None -> acc
            end
          | x -> x :: acc
        end doc []
      | `short_toc ->
        List.fold_right begin fun x acc -> match x with
          | Paragraph [ String "__SHORT_TOC__" ] ->
            begin match toc with
              | Some t -> (List.hd @@ of_wktxt [t]) :: acc
              | None -> acc
            end
          | x -> x :: acc
        end doc []
      | `no | `notoc -> doc
    in
    let doc =
      match edit with
      | None -> doc
      | Some opt ->
        let cnt = ref 0 in
        List.fold_left begin fun acc -> function
          | Header _ as x ->
            let cnt = incr cnt ; !cnt in
            begin match edit_link conf cnt opt with
              | Some link -> Raw link :: x :: acc
              | None -> x :: acc
            end
          | x -> x :: acc
        end [] doc
        |> List.rev
    in
    doc_to_string conf base doc
  with W.ParsingError (line, column, lexeme) ->
    failwith @@ Printf.sprintf "Wikitext.ParsingError (%d, %d, %s)"
      line column (String.escaped lexeme)

let title_regexp = Str.regexp "^\\(=+\\).*[^=]\\(=+\\)$"
let extract_sub_part s v =
  if v = 0 then
    try
      let j = Str.search_forward title_regexp s 0 in
      if j = 0 then s else String.sub s 0 j
    with Not_found -> s
  else
  let rec loop beg i lev cnt =
    try
      let j = Str.search_forward title_regexp s i in
      let k = Str.match_end () in
      let h = Str.matched_group 1 s in
      let n = String.length h in
      if n = String.length (Str.matched_group 2 s)
      then
        if cnt = v then
          loop (Some j) k n (cnt + 1)
        else if cnt > v
        then
          if n > lev then
            loop beg k lev (cnt + 1)
          else
            match beg with
            | None -> raise Not_found
            | Some b -> String.sub s b (j - b)
        else
          loop beg k lev (cnt + 1)
      else
        loop beg k lev cnt
    with Not_found ->
    match beg with
    | None -> raise Not_found
    | Some b -> String.sub s b (String.length s - b - 1)
  in loop None 0 0 1

let interp ?edit conf base ?(env = []) ?(unsafe = false) s =
  let s = html_of_wiki ?edit conf base (Util.string_with_macros conf env s) in
  if unsafe then s else Util.safe_html s

let interp_inline conf base ?(env = []) ?(unsafe = false) s =
  let s = html_of_wiki_inline conf base (Util.string_with_macros conf env s) in
  if unsafe then s else Util.safe_html s

let rec find_env s i =
  match
    try Some (String.index_from s i '=', String.index_from s i '\n') with
      Not_found -> None
  with
    Some (j, k) ->
    if j > i && j < k then
      let is_key =
        let rec loop i =
          if i = j then true
          else
            match s.[i] with
            'A'..'Z' -> loop (i + 1)
            | _ -> false
        in
        loop i
      in
      if is_key then
        let key = String.sub s i (j - i) in
        let v = String.sub s (j + 1) (k - j - 1) in
        let (env, i) = find_env s (k + 1) in (key, v) :: env, i
      else [], i
    else [], i
  | None -> [], i

let split_title_and_text s =
  let (env, i) = find_env s 0 in
  let s = if i = 0 then s else String.sub s i (String.length s - i) in
  if (try List.assoc "TITLE" env with Not_found -> "") = "" then
    let (tit, txt) =
      try
        let i = String.index s '\n' in
        let tit = String.sub s 0 i in
        let txt = String.sub s (i + 1) (String.length s - i - 1) in tit, txt
      with Not_found -> s, ""
    in
    let (tit, txt) =
      if String.length tit > 0 && tit.[0] = '=' || String.contains tit '<' ||
         String.contains tit '['
      then
        "", s
      else tit, txt
    in
    let env = if tit <> "" then ("TITLE", tit) :: env else env in env, txt
  else env, s
