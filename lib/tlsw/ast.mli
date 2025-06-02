type +'a located = private { desc : 'a; loc : Loc.t }

(** {2 Span language} *)

type link_tag =
  | PersonLink of string * string * int option
  | NoteLink of string
  | WizardLink of string

type link_desc = [ `Link of link_tag * string option ]
type link = link_desc located

val mk_link : ?loc:Loc.t -> link_tag -> string option -> link
val pp_link : link Fmt.t

type tag = Plain | Italic | Bold | BoldItalic | Strike | Underline | Highlight
type span_desc = [ `Span of tag * string | link_desc ]
type span = span_desc located

(** {2 Text language} *)

type text_desc = [ `Text of span list ]
type text = text_desc located

val mk_span : ?loc:Loc.t -> tag -> string -> span
val mk_text : ?loc:Loc.t -> span list -> text

(** {2 List language} *)

type kind = Ordered | Unordered

type node_desc = [ `Node of text option * kind * node list ]
and node = node_desc located

val mk_node : ?loc:Loc.t -> text option -> kind -> node list -> node

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

val equal : t -> t -> bool

(** {2 Constructors for blocks} *)

val mk_header : ?loc:Loc.t -> size -> string -> t
val mk_toc : ?loc:Loc.t -> toc -> t
val mk_indent : ?loc:Loc.t -> int -> text -> t
val mk_pre : ?loc:Loc.t -> string -> t
val mk_newline : ?loc:Loc.t -> unit -> t
val mk_rule : ?loc:Loc.t -> unit -> t

(** {2 Printers} *)

val to_html : t -> Html_types.flow5 Tyxml.Html.elt
val pp : t Fmt.t
