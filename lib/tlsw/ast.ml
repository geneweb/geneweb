module Html = Tyxml.Html

type 'a located = { desc : 'a; loc : Loc.t }

type link_tag =
  | PersonLink of string * string * int option
  | NoteLink of string
  | WizardLink of string

type link_desc = [ `Link of link_tag * string option ]
type link = link_desc located
type tag = Plain | Italic | Bold | BoldItalic | Strike | Underline | Highlight
type span_desc = [ `Span of tag * string | link_desc ]
type span = span_desc located
type text_desc = [ `Text of span list ]
type text = text_desc located
type kind = Ordered | Unordered

type node_desc = [ `Node of text option * kind * node list ]
and node = node_desc located

type size = One | Two | Three | Four | Five | Six
type toc = Std | Short | No

type block =
  [ `Header of size * string
  | `Toc of toc
  | `Newline
  | `Rule
  | `Indent of int * text
  | `Pre of string
  | node_desc
  | text_desc ]

and t = block located

let equal t1 t2 = t1 = t2

let pp_link_tag ppf t =
  match t with
  | PersonLink (fn, sn, occ) ->
      Fmt.pf ppf "PersonLink (%S, %S, %a)" fn sn Fmt.(option int) occ
  | NoteLink fl -> Fmt.pf ppf "NoteLink (%S)" fl
  | WizardLink fl -> Fmt.pf ppf "WizardLink (%S)" fl

let pp_tag ppf t =
  match t with
  | Plain -> Fmt.pf ppf "Plain"
  | Italic -> Fmt.pf ppf "Italic"
  | Bold -> Fmt.pf ppf "Bold"
  | BoldItalic -> Fmt.pf ppf "BoldItalic"
  | Strike -> Fmt.pf ppf "Strike"
  | Underline -> Fmt.pf ppf "Underline"
  | Highlight -> Fmt.pf ppf "Highlight"

let pp_size ppf sz =
  match sz with
  | One -> Fmt.pf ppf "One"
  | Two -> Fmt.pf ppf "Two"
  | Three -> Fmt.pf ppf "Three"
  | Four -> Fmt.pf ppf "Four"
  | Five -> Fmt.pf ppf "Five"
  | Six -> Fmt.pf ppf "Six"

let pp_toc ppf toc =
  match toc with
  | Std -> Fmt.pf ppf "Std"
  | Short -> Fmt.pf ppf "Short"
  | No -> Fmt.pf ppf "No"

let pp_escaped ppf s = Fmt.pf ppf "%S" s

let pp_link_desc ppf (`Link (tag, s)) =
  Fmt.pf ppf "Link (%a, %a)" pp_link_tag tag Fmt.(option pp_escaped) s

let pp_link ppf { desc; _ } = pp_link_desc ppf desc

let pp_span ppf { desc; _ } =
  match desc with
  | #link_desc as t -> pp_link_desc ppf t
  | `Span (tag, s) -> Fmt.pf ppf "Span (%a, %S)" pp_tag tag s

let pp_text_desc ppf (`Text l) = Fmt.(list ~sep:comma pp_span) ppf l
let pp_text ppf { desc; _ } = pp_text_desc ppf desc

let pp_kind ppf k =
  match k with
  | Ordered -> Fmt.pf ppf "Ordered"
  | Unordered -> Fmt.pf ppf "Unordered"

let rec pp_node_desc ppf (`Node (t, k, l)) =
  Fmt.pf ppf "Node (%a, %a, %a)"
    Fmt.(option pp_text)
    t pp_kind k
    Fmt.(list ~sep:comma pp_node)
    l

and pp_node ppf { desc; _ } = pp_node_desc ppf desc

let pp_block ppf t =
  match t with
  | `Header (sz, t) -> Fmt.pf ppf "Header (%a, %S)" pp_size sz t
  | `Toc toc -> Fmt.pf ppf "Toc (%a)" pp_toc toc
  | `Newline -> Fmt.pf ppf "Newline"
  | `Rule -> Fmt.pf ppf "Rule"
  | `Indent (c, t) -> Fmt.pf ppf "Indent (%d, %a)" c pp_text t
  | `Pre s -> Fmt.pf ppf "Pre (%S)" s
  | #node_desc as t -> pp_node_desc ppf t
  | #text_desc as t -> pp_text_desc ppf t

let pp ppf { desc; _ } = pp_block ppf desc
let[@inline always] mk ?(loc = Loc.dummy) desc = { desc; loc }
let[@inline always] mk_link ?loc x y = mk ?loc @@ `Link (x, y)
let[@inline always] mk_span ?loc x y = mk ?loc @@ `Span (x, y)
let[@inline always] mk_node ?loc x y z = mk ?loc @@ `Node (x, y, z)
let[@inline always] mk_header ?loc x y = mk ?loc @@ `Header (x, y)
let[@inline always] mk_toc ?loc x = mk ?loc @@ `Toc x
let[@inline always] mk_text ?loc x = mk ?loc @@ `Text x
let[@inline always] mk_indent ?loc x y = mk ?loc @@ `Indent (x, y)
let[@inline always] mk_pre ?loc x = mk ?loc @@ `Pre x
let[@inline always] mk_newline ?loc () = mk ?loc @@ `Newline
let[@inline always] mk_rule ?loc () = mk ?loc @@ `Rule

let link_to_html (tag, s) =
  match tag with
  | PersonLink (fn, sn, occ) ->
      let s =
        Fmt.str "(%s/%s/%a) %a" fn sn Fmt.(option int) occ Fmt.(option string) s
      in
      Html.(span ~a:[ a_style "color: blue" ] [ txt s ])
  | NoteLink fl ->
      let s = Fmt.str "(%s)%a" fl Fmt.(option string) s in
      Html.(span ~a:[ a_style "color: red" ] [ txt s ])
  | WizardLink fl ->
      let s = Fmt.str "(%s)%a" fl Fmt.(option string) s in
      Html.(span ~a:[ a_style "color: red" ] [ txt s ])

let span_to_html { desc; _ } =
  match desc with
  | `Link l -> link_to_html l
  | `Span (tag, s) -> (
      match tag with
      | Plain -> Html.txt s
      | Italic -> Html.(i [ txt s ])
      | Bold -> Html.(b [ txt s ])
      | BoldItalic -> Html.(b [ i [ txt s ] ])
      | Strike ->
          Html.(span ~a:[ a_style "text-decoration: line-through" ] [ txt s ])
      | Underline ->
          Html.(span ~a:[ a_style "text-decoration: underline" ] [ txt s ])
      | Highlight -> Html.(mark [ txt s ]))

let text_desc_to_html (`Text l) = List.map span_to_html l
let text_to_html { desc; _ } = text_desc_to_html desc

let rec node_desc_to_html (`Node (t, k, l)) =
  let l = node_list_to_html k l in
  match t with
  | Some t -> Html.(li (text_to_html t @ [ l ]))
  | None -> Html.(li [ l ])

and node_list_to_html k l =
  let l = List.map (fun { desc; _ } -> node_desc_to_html desc) l in
  match l with
  | [] -> Html.(txt "")
  | _ -> ( match k with Ordered -> Html.(ol l) | Unordered -> Html.(ul l))

let to_html { desc; _ } =
  match desc with
  | `Header (One, s) -> Html.(h1 [ txt s ])
  | `Header (Two, s) -> Html.(h2 [ txt s ])
  | `Header (Three, s) -> Html.(h3 [ txt s ])
  | `Header (Four, s) -> Html.(h4 [ txt s ])
  | `Header (Five, s) -> Html.(h5 [ txt s ])
  | `Header (Six, s) -> Html.(h6 [ txt s ])
  | `Toc Std -> Html.(div [ txt "__TOC__" ])
  | `Toc Short -> Html.(div [ txt "__SHORT_TOC__" ])
  | `Toc No -> Html.(div [ txt "__NOTOC__" ])
  | `Indent (_, t) -> Html.(div @@ text_to_html t)
  | `Pre s -> Html.(pre [ txt s ])
  | `Node (None, k, l) -> node_list_to_html k l
  | `Node (Some _, _, _) ->
      (* Top level lists never contain text. *)
      assert false
  | `Newline -> Html.br ()
  | `Rule -> Html.hr ()
  | #text_desc as t -> Html.(p @@ text_desc_to_html t)
