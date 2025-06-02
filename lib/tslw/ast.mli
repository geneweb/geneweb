type 'a located = private { desc : 'a; loc : Loc.t }

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

(** {2 Type for ordered and unordered lists} *)

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

val equal : t -> t -> bool

val mk_span : ?loc:Loc.t -> tag -> string -> span
val mk_node : ?loc:Loc.t -> text option -> kind -> node list -> node

(** {2 Constructors for blocks} *)

val mk_header : ?loc:Loc.t -> size -> string -> t
val mk_toc : ?loc:Loc.t -> toc -> t
val mk_text : ?loc:Loc.t -> text -> t
val mk_indent : ?loc:Loc.t -> int -> text -> t
val mk_pre : ?loc:Loc.t -> string -> t
val mk_newline : ?loc:Loc.t -> unit -> t
val mk_list : ?loc:Loc.t -> node_desc -> t

(** {2 Printers} *)

val to_html : t -> Html_types.flow5 Tyxml.Html.elt
val pp : t Fmt.t
