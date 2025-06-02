module Html = Tyxml.Html

type 'a located = { desc : 'a; loc : Loc.t }

type tag =
  | Plain
  | Italic
  | Bold
  | BoldItalic
  | Strike
  | Underline
  | Highlight
  | Person of string * string * int option
  | Note of string

type span = (tag * string) located
type text = span list
type size = One | Two | Three | Four | Five | Six
type toc = Std | Short | No

type kind = Ordered | Unordered

type node_desc =
  | Node of text option * kind * node list

and node = node_desc located

type block =
  | Header of size * string
  | Toc of toc
  | Text of text
  | Indent of int * text
  | Pre of string
  | List of node_desc
  | Newline

and t = block located

let equal t1 t2 = t1 = t2

let pp_tag ppf t =
  match t with
  | Plain -> Fmt.pf ppf "Plain"
  | Italic -> Fmt.pf ppf "Italic"
  | Bold -> Fmt.pf ppf "Bold"
  | BoldItalic -> Fmt.pf ppf "BoldItalic"
  | Strike -> Fmt.pf ppf "Strike"
  | Underline -> Fmt.pf ppf "Underline"
  | Highlight -> Fmt.pf ppf "Highlight"
  | Person (fn, sn, occ) ->
      Fmt.pf ppf "Person (%s, %s, %a)" fn sn Fmt.(option int) occ
  | Note fl -> Fmt.pf ppf "Note (%s)" fl

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

let pp_span ppf { desc = t, s; _ } = Fmt.pf ppf "Span (%a, %s)" pp_tag t s
let pp_text = Fmt.(list ~sep:comma pp_span)

let pp_kind ppf k =
  match k with
  | Ordered -> Fmt.pf ppf "Ordered"
  | Unordered -> Fmt.pf ppf "Unordered"

let rec pp_node ppf (Node (t, k, l)) =
  Fmt.pf ppf "Node (%a, %a, %a)" Fmt.(option pp_text) t pp_kind k
    Fmt.(list ~sep:comma pp_node_located) l

and pp_node_located ppf { desc; _ } = pp_node ppf desc

let pp_block ppf t =
  match t with
  | Header (sz, t) -> Fmt.pf ppf "Header (%a, %s)" pp_size sz t
  | Toc toc -> Fmt.pf ppf "Toc (%a)" pp_toc toc
  | Text t -> Fmt.pf ppf "Text (%a)" pp_text t
  | Indent (c, t) -> Fmt.pf ppf "Indent (%d, %a)" c pp_text t
  | Pre s -> Fmt.pf ppf "Pre (%s)" s
  | List n -> pp_node ppf n
  | Newline -> Fmt.pf ppf "Newline"

let pp ppf { desc; _ } = pp_block ppf desc
let[@inline always] mk ?(loc = Loc.dummy) desc = { desc; loc }
let[@inline always] mk_span ?loc x y = mk ?loc (x, y)
let[@inline always] mk_node ?loc x y z = mk ?loc @@ Node (x, y, z)
let[@inline always] mk_header ?loc x y = mk ?loc @@ Header (x, y)
let[@inline always] mk_toc ?loc x = mk ?loc @@ Toc x
let[@inline always] mk_text ?loc x = mk ?loc @@ Text x
let[@inline always] mk_indent ?loc x y = mk ?loc @@ Indent (x, y)
let[@inline always] mk_pre ?loc x = mk ?loc @@ Pre x
let[@inline always] mk_list ?loc x = mk ?loc @@ List x
let[@inline always] mk_newline ?loc () = mk ?loc @@ Newline

let span_to_html { desc = tag, s; _ } =
  match tag with
  | Plain -> Html.txt s
  | Italic -> Html.(i [ txt s ])
  | Bold -> Html.(b [ txt s ])
  | BoldItalic -> Html.(b [ i [ txt s ] ])
  | Strike ->
      Html.(span ~a:[ a_style "text-decoration: line-through" ] [ txt s ])
  | Underline ->
      Html.(span ~a:[ a_style "text-decoration: underline" ] [ txt s ])
  | Highlight -> Html.(mark [ txt s ])
  | _ -> assert false

let text_to_html l = List.map span_to_html l

let rec node_to_html (Node (t, k, l)) =
  let l = node_list_to_html k l in
  match t with
  | Some t -> Html.(li (text_to_html t @ [l]))
  | None -> Html.(li [l])

and node_list_to_html k l =
  let l = List.map (fun { desc; _ } -> node_to_html desc) l in
  match k with
  | Ordered -> Html.(ol l)
  | Unordered -> Html.(ul l)

let to_html { desc; _ } =
  match desc with
  | Header (One, s) -> Html.(h1 [ txt s ])
  | Header (Two, s) -> Html.(h2 [ txt s ])
  | Header (Three, s) -> Html.(h3 [ txt s ])
  | Header (Four, s) -> Html.(h4 [ txt s ])
  | Header (Five, s) -> Html.(h5 [ txt s ])
  | Header (Six, s) -> Html.(h6 [ txt s ])
  | Toc Std -> Html.(div [ txt "__TOC__" ])
  | Toc Short -> Html.(div [ txt "__SHORT_TOC__" ])
  | Toc No -> Html.(div [ txt "__NOTOC__" ])
  | Text t -> Html.(p @@ text_to_html t)
  | Indent (_, t) -> Html.(div @@ text_to_html t)
  | Pre s -> Html.(pre [ txt s ])
  | List (Node (None, k, l)) -> node_list_to_html k l
  | List (Node (Some _, _, _)) ->
      (* Top level lists never contain text. *)
      assert false
  | Newline -> Html.br ()
