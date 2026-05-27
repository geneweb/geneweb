(** DAG to HTML table conversion.

    Transforms a directed acyclic graph into an HTML table structure suitable
    for rendering genealogical trees. The pipeline runs in two stages:
    {!table_of_dag} lays out the DAG on a 2D grid, then {!html_table_struct}
    converts that grid into the matrix of logical HTML cells consumed by
    [DagDisplay] and other downstream renderers (ascendant and descendant
    trees). *)

type 'a dag = { mutable dag : 'a node array }
(** Container for the DAG nodes. The [dag] field is mutable for in-place
    operations such as vertical inversion. *)

and 'a node = { mutable pare : idag list; valu : 'a; mutable chil : idag list }
(** A DAG node. [pare] holds the immediate parents and [chil] the immediate
    children. By default, parents are rendered above the node and children
    below; if {!table_of_dag} is called with [invert = true], the two roles are
    swapped. [valu] is the caller payload (typically [Def.choice] of [iper] for
    real persons and synthetic id for connectors). *)

and idag
(** Opaque index of a node inside a {!dag} array. *)

external int_of_idag : idag -> int = "%identity"
external idag_of_int : int -> idag = "%identity"

type 'a table = { mutable table : 'a data array array }
(** 2D layout grid produced by {!table_of_dag}. Rows are indexed top to bottom,
    columns left to right. *)

and 'a data = { mutable elem : 'a elem; mutable span : span_id }
(** One cell of the layout grid. [elem] is the logical content; [span]
    identifies the horizontal group the cell belongs to: contiguous cells of the
    same row sharing a [span_id] form a single logical span (used by the
    grouping passes and the HTML colspan computation). *)

and 'a elem =
  | Elem of 'a  (** Real node carrying a payload. *)
  | Ghost of ghost_id
      (** Vertical continuation of a logical node across several rows; all
          [Ghost] cells of the same continuation share the same [ghost_id]. *)
  | Nothing  (** Empty cell (padding or trimmed area). *)

and span_id
(** Opaque horizontal grouping identifier. *)

and ghost_id
(** Opaque identifier shared by all {!Ghost} cells continuing the same logical
    node. *)

type align =
  | LeftA
  | CenterA
  | RightA  (** Horizontal alignment of a rendered HTML cell. *)

type 'a table_data =
  | TDitem of Geneweb_db.Driver.iper * 'a * Adef.safe_string
      (** Person cell: iper of the person, payload (typically formatted HTML),
          optional decoration string. *)
  | TDtext of Geneweb_db.Driver.iper * Adef.safe_string
      (** Text-only cell associated with an iper (used by the descendant
          renderer for caption rows). *)
  | TDhr of align
      (** Horizontal-rule fragment used between rows to draw sibling brackets;
          the [align] indicates which segment of the bracket this fragment
          carries (left tip, center span, right tip). *)
  | TDbar of Adef.escaped_string option
      (** Vertical bar; the optional payload is a URL target attached to the
          first occurrence of the bar in a multi-row run. *)
  | TDnothing  (** Padding cell. *)

type 'a html_table_line = (int * align * 'a table_data) array
(** One row of the output matrix: a sequence of [(colspan, align, data)]
    triples. *)

type 'a html_table = 'a html_table_line array
(** Full output matrix returned by {!html_table_struct}. *)

val html_table_struct :
  ('a node -> Geneweb_db.Driver.iper) ->
  ('a node -> 'b) ->
  ('a node -> Adef.escaped_string) ->
  ('a node -> bool) ->
  'a dag ->
  idag table ->
  (int * align * 'b table_data) array array
(** [html_table_struct indi_ip indi_txt vbar_txt phony d t] converts the layout
    grid [t] (built from DAG [d]) into the output matrix of
    [(colspan, align, table_data)] triples.

    Callbacks describing each real node:
    - [indi_ip] returns the iper carried by the node;
    - [indi_txt] returns the cell payload;
    - [vbar_txt] returns the URL target of the vertical bar drawn above the
      node;
    - [phony] tells whether the node is synthetic (e.g. a spouse-pair connector)
      and must be rendered as a plain bar rather than a person cell.

    Each logical row of [t] is expanded into up to three matrix rows: an
    optional vertical-bar row above, the cell row itself, and an optional
    horizontal-rule row below grouping siblings. Each logical column expands
    into three HTML columns (left padding, content, right padding). *)

val table_of_dag :
  ('a node -> bool) -> bool -> bool -> bool -> 'a dag -> idag table
(** [table_of_dag phony no_optim invert no_group d] lays out the DAG [d] onto a
    2D grid.

    Pipeline:
    + optionally invert the DAG (swap [pare]/[chil]);
    + tablify: starting from the ancestors row, generate one new row per
      generation and apply the grouping passes (equilibrate, group
      elems/ghosts/children, group span by common children, gap filling) unless
      skipped by [no_optim] or [no_group];
    + invert the grid back if the input was inverted;
    + apply the fall heuristics that drop ghosts vertically and laterally to
      compact the layout ([fall], [fall2_right], [fall2_left],
      [shorten_too_long]);
    + trim empty top and bottom rows.

    Parameters:
    - [phony]: identifies synthetic nodes; they are still positioned but treated
      as transparent in the grouping passes;
    - [no_optim]: skip the [equilibrate] and [treat_gaps] passes (faster, looser
      layout);
    - [invert]: render descendants on top instead of ancestors;
    - [no_group]: skip all grouping passes on rows that contain no phony
      children (used by callers that already produce a pre-grouped tree).

    On an empty DAG the resulting [table] field is the empty array. *)
