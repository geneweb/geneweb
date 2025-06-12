module C = Combinator

let ws = C.(char ' ' <|> char '\t' <|> char '\r')
let nl = C.string "\n"
let std_toc_tk = C.string "__TOC__"
let short_toc_tk = C.string "__SHORT_TOC__"
let no_toc_tk = C.string "__NOTOC__"
let pre_tk = C.string "\n "
let ul_tk = C.string "*"
let ol_tk = C.string "#"
let h1_del = C.string "="
let h2_del = C.string "=="
let h3_del = C.string "==="
let h4_del = C.string "===="
let h5_del = C.string "====="
let h6_del = C.string "======"
let link_person_open = C.string "[["
let link_person_close = C.string "]]"
let link_note_open = C.string "[[["
let link_note_close = C.string "]]]"
let dash = C.string "/"
let any = C.string ""

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
    ret (Ast.mk_span ~loc k s))

let italic = span_text italic_del italic_del Italic
let bold = span_text bold_del bold_del Bold
let bold_italic = span_text bold_italic_del bold_italic_del BoldItalic
let strike = span_text strike_del strike_del Strike
let underline = span_text underline_del underline_del Underline
let highlight = span_text highlight_open highlight_close Highlight

(********* Parsing links *********)

let firstname = C.(until (dash <|> link_person_close))
let surname = C.(until (dash <|> link_person_close))
let occ = C.int
let link_person_text = C.(until link_person_close)

let link_person =
  C.(
    let content =
      let* fn = firstname in
      let* sn = dash *> surname in
      let* occ_opt = option (dash *> occ) in
      let* text_opt = option (dash *> link_person_text) in
      ret (fn, sn, occ_opt, text_opt)
    in
    let* (fn, sn, occ_opt, text_opt), loc =
      located (link_person_open *> content <* link_person_close)
    in
    let text = Option.value ~default:fn text_opt in
    ret (Ast.mk_span ~loc (Person (fn, sn, occ_opt)) text))

let file = C.(until link_note_close)
let link_note_text = C.(until link_note_close)

let link_note =
  C.(
    let content =
      let* fl = file in
      let* text_opt = option (dash *> link_note_text) in
      ret (fl, text_opt)
    in
    let* (fl, text_opt), loc =
      located (link_note_open *> content <* link_note_close)
    in
    let text = Option.value ~default:fl text_opt in
    ret (Ast.mk_span ~loc (Note fl) text))

(********* Parsing normal text *********)

let special_nl =
  C.choice [
    pre_tk;
    nl;
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
  (nl *> special_nl)
  <|> italic_del
  <|> strike_del
  <|> underline_del
  <|> highlight_open
  <|> link_person_open)
  [@ocamlformat "disable"]

let plain =
  C.(
    let* s, loc = located @@ until special in
    if String.length s = 0 then fail else ret (Ast.mk_span ~loc Plain s))

(********* Parsing text *********)

let span =
  C.(
    case [
      bold_italic_del, bold_italic;
      bold_del, bold;
      italic_del, italic;
      strike_del, strike;
      underline_del, underline;
      highlight_open, highlight;
      link_note_open, link_note;
      link_person_open, link_person;
      any, plain
    ])
    [@ocamlformat "disable"]

let text =
  C.(
    let* l, loc = located @@ many1 span in
    ret (Ast.mk_text ~loc l))

(********* Parsing header *********)

let header del sz =
  C.(
    let* s, loc = located (del *> until del <* del <* many ws <* nl) in
    ret (Ast.mk_header ~loc sz s))

let h1 = header h1_del Ast.One
let h2 = header h2_del Ast.Two
let h3 = header h3_del Ast.Three
let h4 = header h4_del Ast.Four
let h5 = header h5_del Ast.Five
let h6 = header h6_del Ast.Six

(********* Parser toc *********)

let toc tk k =
  C.(
    let* (), loc = located (tk *> skip ws <* nl) in
    ret (Ast.mk_toc ~loc k))

let std_toc = toc std_toc_tk Std
let short_toc = toc short_toc_tk Short
let no_toc = toc no_toc_tk No

(********* Parser pre *********)

let pre =
  C.(
    let pre_end = nl *> many ws *> nl in
    let* s, loc = located (pre_tk *> until pre_end <* pre_end) in
    ret (Ast.mk_pre ~loc s))

(********* Parser list *********)

let ul_at_lvl =
  C.(
    fix @@ fun lvl self ->
    let* c = count ul_tk in
    if c != lvl then fail
    else
      let* text_opt = option text <* nl in
      let* nested = option @@ self (lvl + 1) in
      let hd =
        let nested = match nested with None -> [] | Some l -> l in
        Ast.mk_node text_opt Unordered nested
      in
      let* tl = option (self lvl) in
      match tl with None -> ret [ hd ] | Some tl -> ret (hd :: tl))

let ul =
  C.(
    let* l, loc = located @@ ul_at_lvl 1 in
    ret (Ast.mk_node ~loc None Unordered l))

(********* Parser block *********)

let newline =
  C.(
    let* _, loc = located nl in
    ret (Ast.mk_newline ~loc ()))

let toplevel_ul =
  C.(
    let* ul = ul in
    ret (ul :> Ast.t))

let toplevel_text =
  C.(
    let* text = text in
    ret (text :> Ast.t))

let block =
  C.(
    case [
      pre_tk, pre;
      nl, newline;
      h6_del, h6;
      h5_del, h5;
      h4_del, h4;
      h3_del, h3;
      h2_del, h2;
      h1_del, h1;
      std_toc_tk, std_toc;
      short_toc_tk, short_toc;
      no_toc_tk, no_toc;
      ul_tk, toplevel_ul;
      any, toplevel_text
    ])
    [@ocamlformat "disable"]

let parse s =
  let st = Input.of_string s in
  let rec loop acc st =
    match C.run block st with
    | Some (tk, st') -> loop (tk :: acc) st'
    | None -> List.rev acc
  in
  loop [] st
