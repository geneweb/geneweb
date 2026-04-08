module C = Combinator
module Loc = Geneweb_loc

let ws = C.(char ' ' <|> char '\t')
let eol = C.(string "\n" <|> string "\r\n")
let space = C.string " "
let empty_line = C.(many ws *> eol)
let std_toc_tk = C.string "__TOC__"
let short_toc_tk = C.string "__SHORT_TOC__"
let no_toc_tk = C.string "__NOTOC__"
let rule_tk = C.string "----"
let indent_tk = C.string ":"
let ul_tk = C.string "*"
let ol_tk = C.string "#"
let h1_del = C.string "="
let h2_del = C.string "=="
let h3_del = C.string "==="
let h4_del = C.string "===="
let h5_del = C.string "====="
let h6_del = C.string "======"
let person_link_open = C.string "[["
let person_link_close = C.string "]]"
let note_link_open = C.string "[[["
let note_link_close = C.string "]]]"
let wizard_link_open = C.string "[[w:"
let wizard_link_close = C.string "]]"
let image_link_open = C.string "[[image:"
let image_link_close = C.string "]]"
let slash = C.string "/"

(********** Parsing span *********)

let italic_del = C.string "''"
let bold_del = C.string "'''"
let bold_italic_del = C.string "'''''"
let underline_del = C.string "__"
let strike_del = C.string "~~"
let highlight_open = C.string "{"
let highlight_close = C.string "}"

(* Parse an input of the form `start s close` where `s` is any string
   that does not contain `s`. Output a text block with the kind [k]. *)
let span_text start close k =
  C.(
    let* s, loc = located (start *> until close <* close) in
    if String.length s = 0 then fail "empty span"
    else ret (Ast.mk_span ~loc k s))

let italic = span_text italic_del italic_del Italic
let bold = span_text bold_del bold_del Bold
let bold_italic = span_text bold_italic_del bold_italic_del BoldItalic
let strike = span_text strike_del strike_del Strike
let underline = span_text underline_del underline_del Underline
let highlight = span_text highlight_open highlight_close Highlight

(********* Parsing paths *********)

let path =
  C.(
    take_while1 (fun c ->
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' | ':' -> true
        | _ -> false))

(********* Parsing links *********)

let person_link =
  C.(
    let firstname = until (slash <|> person_link_close) in
    let surname = until (slash <|> person_link_close) in
    let oc = C.int in
    let link_person_text = until person_link_close in
    let content =
      let* fn = firstname in
      let* sn = slash *> surname in
      let* oc = option (slash *> oc) in
      let* text = option (slash *> link_person_text) in
      ret (fn, sn, oc, text)
    in
    let* (fn, sn, oc, text), loc =
      located (person_link_open *> content <* person_link_close)
    in
    ret (Ast.mk_person_link ~loc ~fn ~sn ?oc text))

let note_link =
  C.(
    let content =
      let* p = path in
      let* text_opt = option (slash *> until note_link_close) in
      ret (p, text_opt)
    in
    let* (p, text), loc =
      located (note_link_open *> content <* note_link_close)
    in
    ret (Ast.mk_note_link ~loc ~path:p ~wizard:false text))

let wizard_link =
  C.(
    let content =
      let* p = path in
      let* text = option (slash *> until wizard_link_close) in
      ret (p, text)
    in
    let* (p, text), loc =
      located (wizard_link_open *> content <* wizard_link_close)
    in
    ret (Ast.mk_note_link ~loc ~path:p ~wizard:true text))

let image_link =
  C.(
    let content =
      let* path = until (slash <|> image_link_close) in
      let* alt_opt = option (slash *> until (slash <|> image_link_close)) in
      let* width = option (slash *> until image_link_close) in
      ret (path, width, alt_opt)
    in
    let* (path, width, alt_opt), loc =
      located (image_link_open *> content <* image_link_close)
    in
    ret (Ast.mk_image_link ~loc ~path ?width alt_opt))

let link =
  C.(
    case2 [
      wizard_link_open, wizard_link;
      image_link_open, image_link;
      note_link_open, note_link;
      person_link_open, person_link;
    ])
    [@ocamlformat "disable"]

(********* Parsing normal text *********)

let special_nl =
  C.choice [
    empty_line;
    rule_tk;
    indent_tk;
    h6_del;
    h5_del;
    h4_del;
    h2_del;
    h1_del;
    std_toc_tk;
    short_toc_tk;
    no_toc_tk;
    ul_tk;
    ol_tk;
  ]
  [@ocamlformat "disable"]

let special =
  C.(
  (eol *> special_nl)
  <|> italic_del
  <|> strike_del
  <|> underline_del
  <|> highlight_open
  <|> person_link_open)
  [@ocamlformat "disable"]

let plain =
  C.(
    let* s, loc = located @@ until special in
    if String.length s = 0 then fail "end of text block"
    else ret (Ast.mk_span ~loc Plain s))

(********* Parsing text *********)

let span_coercion p =
  C.(
    let* r = p in
    ret (r :> Ast.span))

let span ~recover =
  let recover =
    if recover then Some (fun loc -> Ast.mk_span ~loc Highlight "error")
    else None
  in
  C.(
    case ?recover [
      bold_italic_del, bold_italic;
      bold_del, bold;
      italic_del, italic;
      strike_del, strike;
      underline_del, underline;
      highlight_open, highlight;
      image_link_open, span_coercion image_link;
      wizard_link_open, span_coercion wizard_link;
      note_link_open, span_coercion note_link;
      person_link_open, span_coercion person_link;
    ] plain)
    [@ocamlformat "disable"]

let text ~recover =
  C.(
    let* l, loc = located @@ many1 (span ~recover) <* option eol in
    ret (Ast.mk_text ~loc l))

(********* Parsing header *********)

let header del sz =
  C.(
    let* s, loc = located (del *> until del <* del <* many ws <* eol) in
    if String.length s = 0 then fail "empty header"
    else ret (Ast.mk_header ~loc sz s))

let h1 = header h1_del Ast.One
let h2 = header h2_del Ast.Two
let h3 = header h3_del Ast.Three
let h4 = header h4_del Ast.Four
let h5 = header h5_del Ast.Five
let h6 = header h6_del Ast.Six

(********* Parser toc *********)

let toc tk k =
  C.(
    let* (), loc = located (tk *> skip ws <* eol) in
    ret (Ast.mk_toc ~loc k))

let std_toc = toc std_toc_tk Std
let short_toc = toc short_toc_tk Short
let no_toc = toc no_toc_tk No

(********* Parser pre *********)

let pre_open = C.(empty_line *> space)
let pre_close = C.(eol *> empty_line)
let pre_tk = C.(empty_line *> space *> until eol *> eol *> space)

let pre =
  C.(
    let* s, loc = located (pre_open *> until pre_close <* pre_close) in
    ret (Ast.mk_pre ~loc s))

(********* Parser list *********)

let rec ul_at_lvl ~recover lvl =
  C.(
    let* c = count ul_tk in
    if c != lvl then fail "expected a unordered list of level %d" lvl
    else
      let* text_opt = option (text ~recover) in
      let* nested, loc =
        located @@ option
        @@ case2
             [
               (ul_tk, ul_at_lvl ~recover (lvl + 1));
               (ol_tk, ol_at_lvl ~recover 1);
             ]
      in
      let* tl = option @@ ul_at_lvl ~recover lvl in
      let tl = match tl with None -> [] | Some (_, tl) -> tl in
      match nested with
      | None ->
          let hd = Ast.mk_node ~loc text_opt Ast.Unordered [] in
          ret (Ast.Unordered, hd :: tl)
      | Some (nested_kind, nested) ->
          let hd = Ast.mk_node ~loc text_opt nested_kind nested in
          ret (Ast.Unordered, hd :: tl))

and ol_at_lvl ~recover lvl =
  C.(
    let* c = count ol_tk in
    if c != lvl then fail "expected an ordered list of level %d" lvl
    else
      let* text_opt = option (text ~recover) in
      let* nested, loc =
        located @@ option
        @@ case2
             [
               (ol_tk, ol_at_lvl ~recover (lvl + 1));
               (ul_tk, ul_at_lvl ~recover 1);
             ]
      in
      let* tl = option @@ ol_at_lvl ~recover lvl in
      let tl = match tl with None -> [] | Some (_, tl) -> tl in
      match nested with
      | None ->
          let hd = Ast.mk_node ~loc text_opt Ast.Unordered [] in
          ret (Ast.Ordered, hd :: tl)
      | Some (nested_kind, nested) ->
          let hd = Ast.mk_node ~loc text_opt nested_kind nested in
          ret (Ast.Ordered, hd :: tl))

let ul ~recover =
  C.(
    let* (_, l), loc = located @@ ul_at_lvl ~recover 1 in
    ret (Ast.mk_node ~loc None Unordered l))

let ol ~recover =
  C.(
    let* (_, l), loc = located @@ ol_at_lvl ~recover 1 in
    ret (Ast.mk_node ~loc None Ordered l))

(********* Parser rule *********)

let rule =
  C.(
    let* _, loc = located (rule_tk <* skip (char '-' <|> ws) <* eol) in
    ret (Ast.mk_rule ~loc ()))

(********* Parser indent *********)

let indent ~recover =
  C.(
    let content =
      let* c = count indent_tk in
      let* t = text ~recover in
      ret (c, t)
    in
    let* (c, t), loc = located content in
    ret (Ast.mk_indent ~loc c t))

(********* Parser block *********)

let newline =
  C.(
    let* _, loc = located empty_line in
    ret (Ast.mk_newline ~loc ()))

let coercion p =
  C.(
    let* r = p in
    ret (r :> Ast.block))

let block ~recover =
  let r =
    if recover then
      Some
        (fun loc -> (Ast.(mk_text ~loc [ mk_span Plain "error" ]) :> Ast.block))
    else None
  in
  C.(
    case ?recover:r [
      pre_tk, pre;
      empty_line, newline;
      rule_tk, rule;
      indent_tk, (indent ~recover);
      h6_del, h6;
      h5_del, h5;
      h4_del, h4;
      h3_del, h3;
      h2_del, h2;
      h1_del, h1;
      std_toc_tk, std_toc;
      short_toc_tk, short_toc;
      no_toc_tk, no_toc;
      ul_tk, coercion @@ ul ~recover;
      ol_tk, coercion @@ ol ~recover;
    ] (coercion @@ text ~recover))
    [@ocamlformat "disable"]

let parse t ~on_err s =
  let st = Input.of_string s in
  let rec loop st () =
    let start = Input.offset st in
    match C.run t st with
    | Ok tk, st' -> Seq.Cons (tk, loop st')
    | Error err, st' ->
        if Input.eof st then Seq.Nil
        else
          let stop = Input.offset st' in
          let loc = Loc.mk (Input.to_source st') start stop in
          on_err ~loc (err ());
          Seq.Nil
  in
  loop st

type error_handler = loc:Geneweb_loc.t -> string -> unit

let is_valid_path s =
  match fst @@ C.run path @@ Input.of_string s with
  | Ok _ -> true
  | Error _ -> false

let parse_links = parse link
let parse ~recover = parse (block ~recover)
