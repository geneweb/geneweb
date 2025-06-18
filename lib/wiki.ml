(* Copyright (c) 1998-2007 INRIA *)

open Def
open Config
open Util

(* TLSW: Text Language Stolen to Wikipedia
   = title level 1 =
   == title level 2 ==
   ...
   ====== title level 6 ======
   * list ul/li item
   * list ul/li item
   ** list ul/li item 2nd level
   ** list ul/li item 2nd level
   ...
   # list ol/li item
   : indentation list dl/dd item
   ; list dl dt item ; dd item
   ''italic''
   '''bold'''
   '''''bold+italic'''''
   [[first_name/surname/oc/text]] link; 'text' displayed
   [[first_name/surname/text]] link (oc = 0); 'text' displayed
   [[first_name/surname]] link (oc = 0); 'first_name surname' displayed
   [[[notes_subfile/text]]] link to a sub-file; 'text' displayed
   [[[notes_subfile]]] link to a sub-file; 'notes_subfile' displayed
   empty line : new paragraph
   lines starting with space : displayed as they are (providing 1/ there
     are at least two 2/ there is empty lines before and after the group
     of lines).
   __TOC__ : summary
   __SHORT_TOC__ : short summary (unnumbered)
   __NOTOC__ : no (automatic) numbered summary *)

let first_cnt = 1
let tab lev s = String.make (2 * lev) ' ' ^ s

let section_level s len =
  let rec loop i j k =
    if i > 5 then i
    else if len > k && s.[i] = '=' && s.[j] = '=' then
      loop (i + 1) (j - 1) (k + 2)
    else i
  in
  loop 1 (len - 2) 4

(* Creates an edit button with consistent styling *)
let make_edit_button conf ?(mode = "") fnotes ?(cnt = None) () =
  let href =
    Printf.sprintf "%sm=MOD_%s&f=%s%s"
      (commd conf :> string)
      mode
      (Mutil.encode fnotes :> string)
      (match cnt with None -> "" | Some n -> "&v=" ^ string_of_int n)
  in
  let title =
    match cnt with
    | None -> Utf8.capitalize_fst (transl conf "modify note")
    | Some n ->
        Utf8.capitalize_fst
          (Printf.sprintf (ftransl conf "modify note section %d") n)
  in
  Format.sprintf
    {|<a href="%s" class="align-self-center ml-3 mb-1"
  title="%s">(%s)</a>|}
    href title (transl conf "modify")

let notes_aliases conf =
  let fname =
    match List.assoc_opt "notes_alias_file" conf.base_env with
    | Some f -> f
    | None -> Filename.concat (Util.bpath conf.bname) "notes.alias"
  in
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
  | Some ic ->
      let rec loop list =
        match try Some (input_line ic) with End_of_file -> None with
        | Some s ->
            let list =
              (* S: is it replacable by `String.split_on_char ' '` s? *)
              try
                let i = String.index s ' ' in
                ( String.sub s 0 i,
                  String.sub s (i + 1) (String.length s - i - 1) )
                :: list
              with Not_found -> list
            in
            loop list
        | None ->
            close_in ic;
            list
      in
      loop []
  | None -> []

let map_notes aliases f = try List.assoc f aliases with Not_found -> f
let fname_of_path (dirs, file) = List.fold_right Filename.concat dirs file

let str_start_with str i x =
  let rec loop i j =
    if j = String.length x then true
    else if i = String.length str then false
    else if str.[i] = x.[j] then loop (i + 1) (j + 1)
    else false
  in
  loop i 0

let escape (s : string) = (Util.escape_html s : Adef.escaped_string :> string)
let encode (s : string) = (Mutil.encode s : Adef.encoded_string :> string)

type wiki_info = {
  wi_mode : string;
  wi_file_path : string -> string;
  wi_person_exists : string * string * int -> bool;
  wi_mark_if_not_public : string * string * int -> bool;
  wi_always_show_link : bool;
}

let find_first_char_from_list str i chars =
  let len = String.length str in
  let tbl : (char, unit) Hashtbl.t = Hashtbl.create 17 in
  List.iter (fun c -> Hashtbl.replace tbl c ()) chars;
  let rec find_index j =
    if j >= len then None
    else if Hashtbl.mem tbl str.[j] then Some j
    else find_index (j + 1)
  in
  find_index i

type quot_lev = Italic | Bold | BoldItalic | Zero

let italic_delimiter_at s i quot_lev =
  let len = String.length s in
  i <= len - 2
  && s.[i] = '\''
  && s.[i + 1] = '\''
  && (quot_lev = Zero || quot_lev = Italic)

let bold_delimiter_at s i quot_lev =
  let len = String.length s in
  i <= len - 3
  && s.[i] = '\''
  && s.[i + 1] = '\''
  && s.[i + 2] = '\''
  && (quot_lev = Zero || quot_lev = Bold)

let bold_italic_delimiter_at s i quot_lev =
  let len = String.length s in
  i <= len - 5
  && s.[i] = '\''
  && s.[i + 1] = '\''
  && s.[i + 2] = '\''
  && s.[i + 3] = '\''
  && s.[i + 4] = '\''
  && (quot_lev = Zero || quot_lev = BoldItalic)

(* bold italic detects ''bold'', '''italic''' and '''''bold_italic'''''
    and {highlight} sequences in a string, and replaces it with the
    appropriate html code (<i>, <b>, ...) *)
let bold_italic_syntax s =
  let buff = Buffer.create 80 in
  let chars = [ '{'; '%'; '\'' ] in
  let slen = String.length s in
  let rec loop quot_lev i =
    (if i = slen || List.exists (str_start_with s i) [ "</li>"; "</p>" ] then
       match quot_lev with
       | Italic -> Buffer.add_string buff "</i>"
       | Bold -> Buffer.add_string buff "</b>"
       | BoldItalic -> Buffer.add_string buff "</b></i>"
       | Zero -> ());
    if i = slen then () (* % allows escaping [, {, ' *)
    else if
      s.[i] = '%'
      && i < slen - 1
      && (s.[i + 1] = '['
         || s.[i + 1] = '{'
         || s.[i + 1] = '}' (* is this needed?, then why not ']' ?*)
         || s.[i + 1] = '\'')
    then (
      Buffer.add_char buff s.[i + 1];
      loop quot_lev (i + 2) (* single % are kept *))
    else if s.[i] = '%' then (
      Buffer.add_char buff '%';
      loop quot_lev (i + 1) (* highlight *))
    else if s.[i] = '{' then (
      let buff2 = Buffer.create 80 in
      let b, j =
        let rec loop j =
          if j = slen then ("", i + 1)
          else if j < slen - 1 && s.[j] = '%' then (
            Buffer.add_char buff2 s.[j + 1];
            loop (j + 2))
          else if s.[j] = '}' then (Buffer.contents buff2, j + 1)
          else (
            Buffer.add_char buff2 s.[j];
            loop (j + 1))
        in
        loop (i + 1)
      in
      let t =
        if b <> "" then
          Printf.sprintf "<span class=\"highlight\">%s</span>" (escape b)
        else ""
      in
      Buffer.add_string buff t;
      loop quot_lev j)
    else if bold_italic_delimiter_at s i quot_lev then (
      let t, ql =
        if quot_lev = Zero then ("<b><i>", BoldItalic) else ("</i></b>", Zero)
      in
      Buffer.add_string buff t;
      loop ql (i + 5))
    else if bold_delimiter_at s i quot_lev then (
      let t, ql = if quot_lev = Zero then ("<b>", Bold) else ("</b>", Zero) in
      Buffer.add_string buff t;
      loop ql (i + 3))
    else if italic_delimiter_at s i quot_lev then (
      let t, ql = if quot_lev = Zero then ("<i>", Italic) else ("</i>", Zero) in
      Buffer.add_string buff t;
      loop ql (i + 2))
    else if s.[i] = '\'' then (
      Buffer.add_char buff '\'';
      loop quot_lev (i + 1))
    else
      let k = find_first_char_from_list s i chars in
      match k with
      | None -> Buffer.add_string buff (String.sub s i (String.length s - i))
      | Some k ->
          Buffer.add_string buff (String.sub s i (k - i));
          loop quot_lev k
  in
  loop Zero 0;
  Buffer.contents buff

let syntax_links conf wi s =
  let buff = Buffer.create 80 in
  let cancel_links = Util.p_getenv conf.env "cgl" = Some "on" in
  let slen = String.length s in
  let rec loop quot_lev pos i =
    (if i = slen || List.exists (str_start_with s i) [ "</li>"; "</p>" ] then
       match quot_lev with
       | Italic -> Buffer.add_string buff "</i>"
       | Bold -> Buffer.add_string buff "</b>"
       | BoldItalic -> Buffer.add_string buff "</b></i>"
       | Zero -> ());
    if i = slen then ()
    else if
      s.[i] = '%'
      && i < slen - 1
      && (s.[i + 1] = '['
         || s.[i + 1] = ']'
         || s.[i + 1] = '{'
         || s.[i + 1] = '}'
         || s.[i + 1] = '\'')
    then (
      Buffer.add_char buff s.[i + 1];
      loop quot_lev pos (i + 2))
    else if s.[i] = '%' then (
      Buffer.add_char buff '%';
      loop quot_lev pos (i + 1))
    else if s.[i] = '{' then (
      let buff2 = Buffer.create 80 in
      let b, j =
        let rec loop j =
          if j = slen then ("", i + 1)
          else if j < slen - 1 && s.[j] = '%' then (
            Buffer.add_char buff2 s.[j + 1];
            loop (j + 2))
          else if s.[j] = '}' then (Buffer.contents buff2, j + 1)
          else (
            Buffer.add_char buff2 s.[j];
            loop (j + 1))
        in
        loop (i + 1)
      in
      let t =
        if String.length b <> 0 then
          Printf.sprintf "<span class=\"highlight\">%s</span>" (escape b)
        else ""
      in
      Buffer.add_string buff t;
      loop quot_lev pos j)
    else if bold_italic_delimiter_at s i quot_lev then (
      let t, ql =
        if quot_lev = Zero then ("<i><b>", BoldItalic) else ("</b></i>", Zero)
      in
      Buffer.add_string buff t;
      loop ql pos (i + 5))
    else if bold_delimiter_at s i quot_lev then (
      let t, ql = if quot_lev = Zero then ("<b>", Bold) else ("</b>", Zero) in
      Buffer.add_string buff t;
      loop ql pos (i + 3))
    else if italic_delimiter_at s i quot_lev then (
      let t, ql = if quot_lev = Zero then ("<i>", Italic) else ("</i>", Zero) in
      Buffer.add_string buff t;
      loop ql pos (i + 2))
    else if s.[i] = '\'' then (
      Buffer.add_char buff '\'';
      loop quot_lev pos (i + 1))
    else
      match NotesLinks.misc_notes_link s i with
      | NotesLinks.WLpage (j, fpath1, fname1, anchor, text) ->
          let text = bold_italic_syntax text in
          let fpath, fname =
            let aliases = notes_aliases conf in
            let fname = map_notes aliases fname1 in
            match NotesLinks.check_file_name fname with
            | Some fpath -> (fpath, fname)
            | None -> (fpath1, fname1)
          in
          let c =
            let f = wi.wi_file_path (fname_of_path fpath) in
            if Sys.file_exists f then "" else " style=\"color:red\""
          in
          let anchor = if anchor = "" then "" else "#" ^ encode anchor in
          let t =
            if cancel_links then text
            else
              Printf.sprintf {|<a href="%sm=%s&f=%s%s"%s>%s</a>|}
                (commd conf : Adef.escaped_string :> string)
                (encode wi.wi_mode) (encode fname) anchor c text
          in
          Buffer.add_string buff t;
          loop quot_lev pos j
      | NotesLinks.WLperson (j, (fn, sn, oc), name, _) ->
          let name =
            if wi.wi_person_exists (fn, sn, oc) || conf.friend || conf.wizard
            then Option.value ~default:"??" name
            else Util.private_txt conf ""
          in
          let name = bold_italic_syntax name in
          let color = " style=\"color:red\"" in
          let color1 =
            if wi.wi_mark_if_not_public (fn, sn, oc) then "style=\"color:red\""
            else ""
          in
          let t =
            if cancel_links || name = Util.private_txt conf "" then name
            else if wi.wi_person_exists (fn, sn, oc) then
              Printf.sprintf "<a id=\"p_%d\" href=\"%sp=%s;n=%s%s\" %s>%s</a>"
                pos
                (commd conf :> string)
                (Mutil.encode fn :> string)
                (Mutil.encode sn :> string)
                (if oc = 0 then "" else ";oc=" ^ string_of_int oc)
                color1 name
            else if wi.wi_always_show_link then
              Printf.sprintf "<a id=\"p_%d\" href=\"%sp=%s;n=%s%s\"%s>%s</a>"
                pos
                (commd conf :> string)
                (Mutil.encode fn :> string)
                (Mutil.encode sn :> string)
                (if oc = 0 then "" else ";oc=" ^ string_of_int oc)
                color name
            else
              Printf.sprintf "<a href=\"%s\" %s>%s</a>"
                (commd conf :> string)
                color
                (if conf.hide_names then Util.private_txt conf "" else name)
          in
          Buffer.add_string buff t;
          loop quot_lev (pos + 1) j
      | NotesLinks.WLwizard (j, wiz, name) ->
          let name = bold_italic_syntax name in
          let t =
            let s = if name <> "" then name else wiz in
            if cancel_links then s
            else
              Printf.sprintf "<a href=\"%sm=WIZNOTES&f=%s\">%s</a>"
                (commd conf :> string)
                (encode wiz) s
          in
          Buffer.add_string buff t;
          loop quot_lev (pos + 1) j
      | NotesLinks.WLnone (j, none_s) ->
          Buffer.add_string buff none_s;
          loop quot_lev pos j
  in
  loop Zero 1 0;
  Buffer.contents buff

let toc_list = [ "__NOTOC__"; "__TOC__"; "__SHORT_TOC__" ]

module Buff = Buff.Make ()
(* TODO replace Buff by Buffer. Needs more understanding of the functions *)

let lines_list_of_string s =
  let rec loop no_toc lines len i =
    if i = String.length s then
      (List.rev (if len = 0 then lines else Buff.get len :: lines), no_toc)
    else if s.[i] = '\n' then
      let line = Buff.get len in
      let no_toc = List.mem line toc_list || no_toc in
      loop no_toc (line :: lines) 0 (i + 1)
    else loop no_toc lines (Buff.store len s.[i]) (i + 1)
  in
  loop false [] 0 0

let adjust_ul_level rev_lines old_lev new_lev =
  if old_lev < new_lev then tab (old_lev + 1) "<ul>" :: rev_lines
  else
    let rev_lines = (List.hd rev_lines ^ "</li>") :: List.tl rev_lines in
    let rec loop rev_lines lev =
      if lev = new_lev then rev_lines
      else loop (tab lev "</ul></li>" :: rev_lines) (lev - 1)
    in
    loop rev_lines old_lev

let message_txt conf i = transl_nth conf "visualize/show/hide/summary" i

let sections_nums_of_tlsw_lines lines =
  let _, _, _, rev_sections_nums =
    List.fold_left
      (fun (prev_lev, indent_stack, cnt, sections_nums) s ->
        let len = String.length s in
        if len > 2 && s.[0] = '=' && s.[len - 1] = '=' then
          let slev = section_level s len in
          let lev, stack =
            let rec loop lev stack =
              match stack with
              | (prev_num, prev_slev) :: rest_stack ->
                  if slev < prev_slev then
                    match rest_stack with
                    | (_, prev_prev_slev) :: _ ->
                        if slev > prev_prev_slev then
                          let stack = (prev_num, slev) :: rest_stack in
                          loop lev stack
                        else loop (lev - 1) rest_stack
                    | [] ->
                        let stack = (prev_num + 1, slev) :: rest_stack in
                        (lev - 1, stack)
                  else if slev = prev_slev then
                    let stack = (prev_num + 1, slev) :: rest_stack in
                    (lev - 1, stack)
                  else
                    let stack = (1, slev) :: stack in
                    (lev, stack)
              | [] ->
                  let stack = (1, slev) :: stack in
                  (lev, stack)
            in
            loop prev_lev indent_stack
          in
          let section_num =
            let nums = List.map fst stack in
            String.concat "." (List.rev_map string_of_int nums)
          in
          (lev + 1, stack, cnt + 1, (lev, section_num) :: sections_nums)
        else (prev_lev, indent_stack, cnt, sections_nums))
      (0, [], first_cnt, []) lines
  in
  List.rev rev_sections_nums

let remove_links s =
  let rec loop len i =
    if i = String.length s then Buff.get len
    else
      let len, i =
        match NotesLinks.misc_notes_link s i with
        | NotesLinks.WLpage (j, _, _, _, text) -> (Buff.mstore len text, j)
        | NotesLinks.WLperson (j, (fn, sn, _), name, text) ->
            let text =
              match text with
              | None | Some "" -> Option.value ~default:(fn ^ " " ^ sn) name
              | Some text -> text
            in
            (Buff.mstore len text, j)
        | NotesLinks.WLwizard (j, _, text) -> (Buff.mstore len text, j)
        | NotesLinks.WLnone (j, none_s) -> (Buff.mstore len none_s, j)
      in
      loop len i
  in
  loop 0 0

let summary_of_tlsw_lines conf short lines =
  let sections_nums = sections_nums_of_tlsw_lines lines in
  let rev_summary, lev, cnt, _ =
    List.fold_left
      (fun (summary, prev_lev, cnt, sections_nums) s ->
        let s = remove_links s in
        let len = String.length s in
        if len > 2 && s.[0] = '=' && s.[len - 1] = '=' then
          let slev = section_level s len in
          let lev, section_num, sections_nums =
            match sections_nums with
            | (lev, sn) :: sns -> (lev, sn, sns)
            | [] -> (0, "", [])
          in
          let summary =
            let s =
              Printf.sprintf "<a href=\"#a_%d\">%s%s</a>" cnt
                (if short then "" else section_num ^ " – ")
                (String.trim (String.sub s slev (len - (2 * slev))))
            in
            if short then if summary = [] then [ s ] else s :: "&" :: summary
            else
              let line = tab (lev + 1) "<li>" ^ s in
              line :: adjust_ul_level summary (prev_lev - 1) lev
          in
          (summary, lev + 1, cnt + 1, sections_nums)
        else (summary, prev_lev, cnt, sections_nums))
      ([], 0, first_cnt, sections_nums)
      lines
  in
  if cnt <= first_cnt + 2 then ([], [])
  else
    let rev_summary =
      if short then rev_summary
      else "</ul>" :: adjust_ul_level rev_summary (lev - 1) 0
    in
    let lines =
      Format.sprintf
        {|<div id="summary">
  <div class="d-flex align-items-center">
    <h2>0 – %s</h2>
    <a href="#" class="toc-toggle ml-2">(%s)</a>
  </div>
<div id="toc-content">|}
        (Utf8.capitalize_fst (message_txt conf 3))
        (transl_nth conf "visualize/show/hide/summary" 2)
      :: List.rev_append rev_summary [ "</div></div>" ]
    in
    (lines, sections_nums)

let modify_link conf cnt _empty = function
  | Some (_, mode, sfn) when conf.wizard ->
      make_edit_button conf ~mode sfn ~cnt:(Some cnt) ()
  | _ -> ""

let rec tlsw_list tag1 tag2 lev list sl =
  let btag2 = "<" ^ tag2 ^ ">" in
  let etag2 = "</" ^ tag2 ^ ">" in
  let list = tab lev ("<" ^ tag1 ^ ">") :: list in
  let list =
    let rec loop list = function
      | s1 :: (s2 :: _ as sl) ->
          if String.length s2 > 0 && List.mem s2.[0] [ '*'; '#'; ':'; ';' ] then
            let list = (tab lev btag2 ^ s1) :: list in
            let list, sl = do_sub_list s2.[0] lev list sl in
            loop (tab lev etag2 :: list) sl
          else
            let s1, ss1 = sub_sub_list lev tag2 s1 in
            loop ((tab lev btag2 ^ s1 ^ etag2 ^ ss1) :: list) sl
      | [ s1 ] ->
          let s1, ss1 = sub_sub_list lev tag2 s1 in
          (tab lev btag2 ^ s1 ^ etag2 ^ ss1) :: list
      | [] -> list
    in
    loop list sl
  in
  tab lev ("</" ^ tag1 ^ ">") :: list

and sub_sub_list lev tag2 s1 =
  if tag2 = "dt" && String.contains s1 ':' then
    let i = String.index s1 ':' in
    let s = String.sub s1 0 i in
    let ss =
      "\n"
      ^ tab (lev + 1) "<dd>"
      ^ String.sub s1 (i + 1) (String.length s1 - i - 1)
      ^ "</dd>"
    in
    (s, ss)
  else (s1, "")

and do_sub_list prompt lev list sl =
  let tag1, tag2 =
    match prompt with
    | '*' -> ("ul", "li")
    | '#' -> ("ol", "li")
    | ':' -> ("dl", "dd")
    | ';' -> ("dl", "dt")
    | _ -> assert false
  in
  let list2, sl =
    let rec loop list = function
      | s :: sl ->
          if String.length s > 0 && s.[0] = prompt then
            let s = String.sub s 1 (String.length s - 1) in
            loop (s :: list) sl
          else (list, s :: sl)
      | [] -> (list, [])
    in
    loop [] sl
  in
  let list = tlsw_list tag1 tag2 (lev + 1) list (List.rev list2) in
  match sl with
  | s :: _ ->
      if String.length s > 0 && List.mem s.[0] [ '*'; '#'; ':'; ';' ] then
        do_sub_list s.[0] lev list sl
      else (list, sl)
  | [] -> (list, sl)

let rec hotl conf wlo cnt edit_opt sections_nums list = function
  | "__NOTOC__" :: sl -> hotl conf wlo cnt edit_opt sections_nums list sl
  | "__TOC__" :: sl ->
      let list =
        match wlo with
        | Some lines ->
            let summary, _ = summary_of_tlsw_lines conf false lines in
            List.rev_append summary list
        | None -> list
      in
      hotl conf wlo cnt edit_opt sections_nums list sl
  | "__SHORT_TOC__" :: sl ->
      let list =
        match wlo with
        | Some lines ->
            let summary, _ = summary_of_tlsw_lines conf true lines in
            List.rev_append summary list
        | None -> list
      in
      hotl conf wlo cnt edit_opt sections_nums list sl
  | "" :: sl ->
      let parag =
        let rec loop1 parag = function
          | "" :: sl -> Some (parag, sl, true)
          | s :: sl ->
              if
                List.mem s.[0] [ '*'; '#'; ':'; ';'; '=' ]
                || List.mem s toc_list
              then if parag = [] then None else Some (parag, s :: sl, true)
              else if s.[0] = ' ' && parag = [] then loop2 [ s ] sl
              else loop1 (s :: parag) sl
          | [] -> Some (parag, [], true)
        and loop2 parag = function
          | "" :: sl -> Some (parag, sl, false)
          | s :: sl ->
              if s.[0] = ' ' then loop2 (s :: parag) sl
              else loop1 parag (s :: sl)
          | [] -> Some (parag, [], true)
        in
        loop1 [] sl
      in
      let list, sl =
        match parag with
        | Some ([], _, _) | None -> (list, sl)
        | Some (parag, sl, false) when List.length parag >= 2 ->
            ("</pre>" :: (parag @ ("<pre>" :: list)), "" :: sl)
        | Some (parag, sl, _) -> ("</p>" :: (parag @ ("<p>" :: list)), "" :: sl)
      in
      hotl conf wlo cnt edit_opt sections_nums list sl
  | s :: sl -> (
      let len = String.length s in
      let tago =
        if len > 0 then
          match s.[0] with
          | '*' -> Some ("ul", "li")
          | '#' -> Some ("ol", "li")
          | ':' -> Some ("dl", "dd")
          | ';' -> Some ("dl", "dt")
          | _ -> None
        else None
      in
      match tago with
      | Some (tag1, tag2) ->
          let sl, rest = select_list_lines conf s.[0] [] (s :: sl) in
          let list = tlsw_list tag1 tag2 0 list sl in
          hotl conf wlo cnt edit_opt sections_nums list ("" :: rest)
      | None ->
          if len > 2 && s.[0] = '=' && s.[len - 1] = '=' then
            let slev = section_level s len in
            let section_num, sections_nums =
              match sections_nums with
              | (_, a) :: l -> (a ^ " – ", l)
              | [] -> ("", [])
            in
            let s =
              Format.asprintf
                "<div class=\"d-flex\"><h%d %s %s>%s%s</h%d>%s</div>" slev
                (if slev <= 3 then "class=\"subtitle\"" else "")
                (if wlo <> None then Printf.sprintf {|id="a_%d"|} cnt else "")
                section_num
                (String.sub s slev (len - (2 * slev)))
                slev
                (modify_link conf cnt false edit_opt)
            in
            hotl conf wlo (cnt + 1) edit_opt sections_nums list (s :: sl)
          else hotl conf wlo cnt edit_opt sections_nums (s :: list) sl)
  | [] -> List.rev list

and select_list_lines conf prompt list = function
  | s :: sl ->
      let len = String.length s in
      if len > 0 && s.[0] = '=' then (List.rev list, s :: sl)
      else if len > 0 && s.[0] = prompt then
        let s = String.sub s 1 (len - 1) in
        let s, sl =
          let rec loop s1 = function
            | "" :: s :: sl
              when String.length s > 1 && s.[0] = prompt && s.[1] = prompt ->
                let br = "<br" ^ ">" in
                loop (s1 ^ br ^ br) (s :: sl)
            | s :: sl ->
                if String.length s > 0 && s.[0] = '=' then (s1, s :: sl)
                else if String.length s > 0 && s.[0] <> prompt then
                  loop (s1 ^ "\n" ^ s) sl
                else (s1, s :: sl)
            | [] -> (s1, [])
          in
          loop s sl
        in
        select_list_lines conf prompt (s :: list) sl
      else (List.rev list, s :: sl)
  | [] -> (List.rev list, [])

let html_of_tlsw conf s =
  let lines, _ = lines_list_of_string s in
  let sections_nums =
    match sections_nums_of_tlsw_lines lines with [ _ ] -> [] | l -> l
  in
  hotl conf (Some lines) first_cnt None sections_nums [] ("" :: lines)

let html_with_summary_of_tlsw conf wi edit_opt s =
  let lines, no_toc = lines_list_of_string s in
  let summary, sections_nums =
    if no_toc then ([], []) else summary_of_tlsw_lines conf false lines
  in
  let rev_lines_before_summary, lines =
    let rec loop lines_bef = function
      | s :: sl ->
          if String.length s > 1 && s.[0] = '=' then (lines_bef, s :: sl)
          else loop (s :: lines_bef) sl
      | [] -> (lines_bef, [])
    in
    loop [] lines
  in
  let lines_before_summary =
    hotl conf (Some lines) first_cnt None [] []
      (List.rev rev_lines_before_summary)
  in
  let lines_after_summary =
    hotl conf (Some lines) first_cnt edit_opt sections_nums [] lines
  in
  let s =
    syntax_links conf wi
      (String.concat "\n"
         (lines_before_summary @ summary @ lines_after_summary))
  in
  if lines_before_summary <> [] || lines = [] then
    modify_link conf 0 (s = "") edit_opt ^ s
  else s

(* v = 0 -> keeps the last lines until a title occurs, discards the rest *)
(* v = 1 -> *)
let rev_extract_sub_part (s : string) (v : int) : string list =
  let lines, _ = lines_list_of_string s in
  let rec loop (lines : string list) (* The accumulator of lines *) (lev : int)
      (* The section level *) (cnt : int) : string list -> string list =
    (* A counter of titles *) function
    | s :: sl ->
        let len = String.length s in
        if len > 2 && s.[0] = '=' && s.[len - 1] = '=' then
          (* This line is a title *)
          if v = first_cnt - 1 then lines
            (* S: previous condition is a strange way to write `if v = 0` *)
          else
            let nlev = section_level s len in
            if cnt = v (* *) then loop (s :: lines) nlev (cnt + 1) sl
            else if cnt > v then
              if nlev > lev then loop (s :: lines) lev (cnt + 1) sl else lines
            else loop lines lev (cnt + 1) sl
        else if
          (* This line is not a title *)
          cnt <= v
        then loop lines lev cnt sl (* Line is in an ignored section *)
        else loop (s :: lines) lev cnt sl (* Keeping the line *)
    | [] -> lines
  in
  loop [] 0 first_cnt lines

let extract_sub_part s v = List.rev (rev_extract_sub_part s v)

let print_sub_part_links conf edit_mode sfn cnt0 is_empty =
  Output.print_sstring conf "<div class=\"btn-group mb-3\">";
  if cnt0 >= first_cnt then (
    Output.print_sstring conf {|<a href="|};
    Output.print_sstring conf (commd conf :> string);
    Output.print_sstring conf {|m=|};
    Output.print_string conf edit_mode;
    Output.print_string conf sfn;
    Output.print_sstring conf {|&v=|};
    Output.print_sstring conf (string_of_int @@ (cnt0 - 1));
    Output.print_sstring conf {|" class="btn btn-sm btn-outline-primary">|};
    Output.print_sstring conf {|<i class="fa fa-arrow-left fa-fw"></i> |};
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_nth conf "modify all note" 1));
    Output.print_sstring conf "</a>");

  Output.print_sstring conf {|<a href="|};
  Output.print_string conf (commd conf);
  Output.print_sstring conf {|m=|};
  Output.print_string conf edit_mode;
  Output.print_string conf sfn;
  Output.print_sstring conf {|" class="btn btn-sm btn-outline-primary">|};
  Output.print_sstring conf {|<i class="fa fa-pencil fa-fw"></i> |};
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl_nth conf "modify all note" 0));
  Output.print_sstring conf "</a>";

  Output.print_sstring conf {|<a href="|};
  Output.print_string conf (commd conf);
  Output.print_sstring conf {|m=|};
  Output.print_sstring conf "NOTES";
  Output.print_string conf sfn;
  Output.print_sstring conf {|" class="btn btn-sm btn-outline-primary">|};
  Output.print_sstring conf {|<i class="fa fa-image fa-fw"></i> |};
  Output.print_sstring conf
    (transl_nth conf "visualize/show/hide/summary" 1 |> Utf8.capitalize_fst);
  Output.print_sstring conf "</a>";

  if not is_empty then (
    Output.print_sstring conf {|<a href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf "m=";
    Output.print_string conf edit_mode;
    Output.print_string conf sfn;
    Output.print_sstring conf "&v=";
    Output.print_sstring conf (string_of_int @@ (cnt0 + 1));
    Output.print_sstring conf {|" class="btn btn-sm btn-outline-primary">|};
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_nth conf "modify all note" 2));
    Output.print_sstring conf {| <i class="fa fa-arrow-right fa-fw"></i>|};
    Output.print_sstring conf "</a>");
  Output.print_sstring conf "</div>"

let print_sub_part_text conf wi edit_opt cnt0 lines =
  let lines =
    List.map
      (function
        | "__TOC__" | "__SHORT_TOC__" ->
            Printf.sprintf "<p>...%s...</p>" (message_txt conf 3)
        | "__NOTOC__" -> ""
        | s -> s)
      lines
  in
  let lines = hotl conf None cnt0 edit_opt [] [] lines in
  let s = String.concat "\n" lines in
  let s = syntax_links conf wi s in
  let s =
    if cnt0 < first_cnt then modify_link conf 0 (s = "") edit_opt ^ s else s
  in
  Output.print_string conf (Util.safe_html s)

let print_sub_part conf wi can_edit edit_mode sub_fname cnt0 lines =
  let edit_opt = Some (can_edit, edit_mode, sub_fname) in
  let sfn =
    if sub_fname = "" then Adef.encoded "" else "&f=" ^<^ Mutil.encode sub_fname
  in
  print_sub_part_links conf (Mutil.encode edit_mode) sfn cnt0 (lines = []);
  print_sub_part_text conf wi edit_opt cnt0 lines

let print_mod_view_page conf can_edit mode fname title env s =
  let s = List.fold_left (fun s (k, v) -> s ^ k ^ "=" ^ v ^ "\n") "" env ^ s in
  let mode_pref = Mutil.encode (if can_edit then "MOD_" else "VIEW_") in
  let has_v, v =
    match p_getint conf.env "v" with Some v -> (true, v) | None -> (false, 0)
  in
  let sub_part =
    if not has_v then s else String.concat "\n" (extract_sub_part s v)
  in
  let is_empty = sub_part = "" in
  let sfn =
    if fname = "" then Adef.encoded "" else "&f=" ^<^ Mutil.encode fname
  in
  Hutil.header_without_title conf;
  Output.print_sstring conf "<div class=\"d-flex mb-3\">";
  Output.print_sstring conf "<h1 class=\"mb-0\">";
  title false;
  Output.print_sstring conf "</h1>";
  if can_edit then (
    Output.print_sstring conf {|<a href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf {|m=|};
    Output.print_string conf mode;
    if has_v then Output.printf conf "&v=%d" v;
    Output.print_string conf sfn;
    Output.print_sstring conf
      {|" class="btn btn-sm btn-outline-primary align-self-center ml-3 mt-1">|};
    Output.print_sstring conf (Utf8.capitalize_fst (message_txt conf 0));
    Output.print_sstring conf "</a>");
  Output.print_sstring conf "</div>";
  if can_edit && has_v then
    print_sub_part_links conf (mode_pref ^^^ mode) sfn v is_empty;
  Output.print_sstring conf {|<form method="POST" action="|};
  Output.print_sstring conf conf.command;
  Output.print_sstring conf {|">|};
  Util.hidden_env conf;
  if can_edit then Util.hidden_input conf "m" ("MOD_" ^<^ mode ^>^ "_OK");
  if has_v then Util.hidden_input conf "v" (Adef.encoded @@ string_of_int v);
  if fname <> "" then Util.hidden_input conf "f" (Mutil.encode fname);
  if can_edit then
    Util.hidden_input conf "digest" (Mutil.digest s |> Mutil.encode);
  Output.print_sstring conf
    {|<div class="d-flex flex-column"><div class="pt-1">|};
  let env =
    Templ.Env.(add "name" (Templ.Vstring (Adef.encoded "notes")) empty)
  in
  Templ.output_simple conf env "toolbar";
  Output.print_sstring conf {|</div><div class="row editor-container">|};
  Output.print_sstring conf
    {|<div class="d-flex flex-column col-9"><textarea name="notes" id="notes_comments"
  class="form-control insert-character-target"|};
  Output.print_sstring conf
    (if can_edit then ">" else " readonly=\"readonly\">");
  Output.print_string conf (Util.escape_html sub_part);
  Output.print_sstring conf "</textarea>";
  if can_edit then (
    Output.print_sstring conf
      {|<button type="submit" class="btn btn-outline-primary btn-lg mx-auto py-3 px-5 my-3">|};
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0));
    Output.print_sstring conf "</button>");
  Output.print_sstring conf "</div><div class=\"col mx-2 p-2\"";
  Templ.output_simple conf env "toolbar";
  Output.print_sstring conf "</div></div>";
  Output.print_sstring conf "</div></form>";
  Hutil.trailer conf

let insert_sub_part s v sub_part =
  let lines, _ = lines_list_of_string s in
  let lines, sl =
    let rec loop sub_part_added lines lev cnt = function
      | s :: sl ->
          let len = String.length s in
          if len > 2 && s.[0] = '=' && s.[len - 1] = '=' then
            if v = first_cnt - 1 then
              ((if sub_part = "" then [] else [ ""; sub_part ]), s :: sl)
            else
              let nlev = section_level s len in
              if cnt = v then
                let lines =
                  if sub_part = "" then lines else "" :: sub_part :: lines
                in
                loop true lines nlev (cnt + 1) sl
              else if cnt > v then
                if nlev > lev then loop sub_part_added lines lev (cnt + 1) sl
                else (lines, s :: sl)
              else loop sub_part_added (s :: lines) lev (cnt + 1) sl
          else if cnt <= v then loop sub_part_added (s :: lines) lev cnt sl
          else loop sub_part_added lines lev cnt sl
      | [] ->
          let lines =
            if sub_part_added then lines
            else if sub_part = "" then lines
            else "" :: sub_part :: lines
          in
          (lines, [])
    in
    loop false [] 0 first_cnt lines
  in
  String.concat "\n" (List.rev_append lines sl)

(* TODO: simplify with Str *)
let rec find_env s i =
  match
    try Some (String.index_from s i '=', String.index_from s i '\n')
    with Not_found -> None
  with
  | Some (j, k) ->
      if j > i && j < k then
        let is_key =
          let rec loop i =
            if i = j then true
            else match s.[i] with 'A' .. 'Z' -> loop (i + 1) | _ -> false
          in
          loop i
        in
        if is_key then
          let key = String.sub s i (j - i) in
          let v = String.sub s (j + 1) (k - j - 1) in
          let env, i = find_env s (k + 1) in
          ((key, v) :: env, i)
        else ([], i)
      else ([], i)
  | None -> ([], i)

let split_title_and_text s =
  let env, i = find_env s 0 in
  let s = if i = 0 then s else String.sub s i (String.length s - i) in
  if (try List.assoc "TITLE" env with Not_found -> "") = "" then
    let tit, txt =
      try
        let i = String.index s '\n' in
        let tit = String.sub s 0 i in
        let txt = String.sub s (i + 1) (String.length s - i - 1) in
        (tit, txt)
      with Not_found -> (s, "")
    in
    let tit, txt =
      if
        (String.length tit > 0 && tit.[0] = '=')
        || String.contains tit '<' || String.contains tit '['
      then ("", s)
      else (tit, txt)
    in
    let env = if tit <> "" then ("TITLE", tit) :: env else env in
    (env, txt)
  else (env, s)

let print_ok conf wi edit_mode fname title_is_1st s =
  let title _ =
    Output.print_sstring conf
      (Utf8.capitalize_fst (Util.transl conf "notes modified"))
  in
  Hutil.header conf title;
  Output.print_sstring conf {|<div style="text-align:center"> --- |};
  title ();
  Output.print_sstring conf {| --- </div>|};
  let get_v = Util.p_getint conf.env "v" in
  let v = match get_v with Some v -> v | None -> 0 in
  let title, s =
    if v = 0 && title_is_1st then
      let env, s = split_title_and_text s in
      ((try List.assoc "TITLE" env with Not_found -> ""), s)
    else ("", s)
  in
  let lines, _ = lines_list_of_string s in
  let lines =
    if v = 0 && title <> "" then ("<h1>" ^ title ^ "</h1>") :: lines else lines
  in
  print_sub_part conf wi conf.wizard edit_mode fname v lines;
  Hutil.trailer conf

let print_mod_ok conf wi edit_mode fname read_string commit string_filter
    title_is_1st =
  let new_fname = Util.p_getenv conf.env "new_f" in
  let fname =
    match new_fname with
    | Some f -> fname (Some f)
    | None -> fname (Util.p_getenv conf.env "f")
  in
  match edit_mode fname with
  | Some edit_mode ->
      let old_string =
        let e, s = read_string fname in
        List.fold_left (fun s (k, v) -> s ^ k ^ "=" ^ v ^ "\n") "" e ^ s
      in
      let sub_part =
        match Util.p_getenv conf.env "notes" with
        | Some v -> Mutil.strip_all_trailing_spaces v
        | None -> failwith "notes unbound"
      in
      let digest =
        match Util.p_getenv conf.env "digest" with Some s -> s | None -> ""
      in
      if digest <> Mutil.digest old_string then Update.error_digest conf
      else
        let s =
          match Util.p_getint conf.env "v" with
          | Some v -> insert_sub_part old_string v sub_part
          | None -> sub_part
        in
        if s <> old_string then commit fname s;
        let sub_part = string_filter sub_part in
        print_ok conf wi edit_mode fname title_is_1st sub_part
  | None -> Hutil.incorrect_request conf
