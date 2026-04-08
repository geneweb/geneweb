type +'a located = { desc : 'a; loc : Geneweb_loc.t }

(** {2 Span language} *)

type link_tag =
  | Person of { fn : string; sn : string; oc : int option }
  | Note of { path : string; anchor : string option; wizard : bool }
  | Image of { path : string; width : string option }

type link_desc = [ `Link of link_tag * string option ]
type link = link_desc located

val mk_link : ?loc:Geneweb_loc.t -> link_tag -> string option -> link
val pp_link : link Fmt.t

type tag =
  | Plain
  | Italic
  | Bold
  | BoldItalic
  | Strike
  | Underline
  | Highlight
  | Error

type span_desc = [ `Span of tag * string | link_desc ]
type span = span_desc located

(** {2 Text language} *)

type text_desc = [ `Text of span list ]
type text = text_desc located

(** {2 Constructors for spans} *)

val mk_span : ?loc:Geneweb_loc.t -> tag -> string -> span

val mk_person_link :
  ?loc:Geneweb_loc.t ->
  fn:string ->
  sn:string ->
  ?oc:int ->
  string option ->
  link

val mk_note_link :
  ?loc:Geneweb_loc.t ->
  path:string ->
  ?anchor:string ->
  wizard:bool ->
  string option ->
  link

val mk_image_link :
  ?loc:Geneweb_loc.t -> path:string -> ?width:string -> string option -> link

val mk_text : ?loc:Geneweb_loc.t -> span list -> text

(** {2 List language} *)

type kind = Ordered | Unordered

type node_desc = [ `Node of text option * kind * node list ]
and node = node_desc located

val mk_node : ?loc:Geneweb_loc.t -> text option -> kind -> node list -> node

type size = One | Two | Three | Four | Five | Six
type toc = Std | Short | No

type block_desc =
  [ `Header of size * string
  | `Toc of toc
  | `Newline
  | `Rule
  | `Indent of int * text
  | `Pre of string
  | node_desc
  | text_desc ]

and block = block_desc located

val equal : block -> block -> bool

(** {2 Constructors for blocks} *)

val mk_header : ?loc:Geneweb_loc.t -> size -> string -> block
val mk_toc : ?loc:Geneweb_loc.t -> toc -> block
val mk_indent : ?loc:Geneweb_loc.t -> int -> text -> block
val mk_pre : ?loc:Geneweb_loc.t -> string -> block
val mk_newline : ?loc:Geneweb_loc.t -> unit -> block
val mk_rule : ?loc:Geneweb_loc.t -> unit -> block

(** {2 Printers} *)

val to_html : block -> Html_types.flow5 Tyxml.Html.elt
val pp : block Fmt.t
