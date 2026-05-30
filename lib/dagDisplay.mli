(** DAG (Directed Acyclic Graph) display for genealogical trees. *)

val image_txt :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string
(** [image_txt conf base p] returns the HTML fragment displaying [p]'s portrait
    below the DAG cell text, or the empty string when no portrait is available
    or when the URL parameter [im] is disabled (default [true]: image is shown).

    Resolves the portrait via {!Image.get_portrait_with_size} and dispatches on
    its source kind:
    - [`Path]: local image, scaled to fit within 100x75 px, wrapped in a link to
      [m=IM] unless [cgl=on];
    - [`Url] with explicit size: external URL, sized as provided;
    - [`Url] without size: external URL, height defaulted to 75 px to keep
      neighbouring cells vertically aligned. *)

type item =
  | Item of Geneweb_db.Driver.person * Adef.safe_string
      (** DAG node item: person with optional annotation. *)

val make_tree_hts :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.person -> item) ->
  (Geneweb_db.Driver.iper -> Adef.escaped_string) ->
  bool ->
  Geneweb_db.Driver.iper list ->
  (Geneweb_db.Driver.iper
  * (Geneweb_db.Driver.iper * Geneweb_db.Driver.ifam option))
  list ->
  (Geneweb_db.Driver.iper, 'a) Def.choice Dag2html.dag ->
  (int * Dag2html.align * Adef.safe_string Dag2html.table_data) array array
(** [make_tree_hts conf base elem_txt vbar_txt invert set spl dag] lays [dag]
    out as a 2D HTML cell matrix later consumed by {!print_dag_page}.

    - [elem_txt]: builds the display text of a real person node (typically a
      titled link plus dates, wrapped as an {!item}).
    - [vbar_txt]: builds the link target of the vertical bar drawn above each
      person (usually a navigation URL to that person's page).
    - [invert]: when [true], descendants are placed at the top and ancestors at
      the bottom (used by descendant trees); otherwise the reverse.
    - [set]: persons participating in the DAG; used to filter out spouses whose
      iper is already a node.
    - [spl]: external spouse pairs [(ip, (sp_ip, ifam_opt))] rendered next to
      childless leaves (relation paths with terminal spouses).

    Also reads the URL parameters [nogroup] (disable sibling grouping),
    [sp]/[spouse] (toggle spouse display). Returns [[||]] when the layout is
    empty. *)

val print_dag_page :
  Config.config ->
  Geneweb_db.Driver.base ->
  Adef.safe_string ->
  (int * Dag2html.align * Adef.safe_string Dag2html.table_data) array array ->
  Adef.escaped_string ->
  unit
(** [print_dag_page conf base page_title hts next_txt] renders the DAG as HTML
    using the dag.txt template.
    - [page_title]: title displayed in the page header
    - [hts]: HTML table structure from {!make_tree_hts}
    - [next_txt]: link text for pagination (empty if none) *)

val make_and_print_dag :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.person -> item) ->
  (Geneweb_db.Driver.iper -> Adef.escaped_string) ->
  bool ->
  Geneweb_db.Driver.iper list ->
  (Geneweb_db.Driver.iper
  * (Geneweb_db.Driver.iper * Geneweb_db.Driver.ifam option))
  list ->
  Adef.safe_string ->
  Adef.escaped_string ->
  unit
(** [make_and_print_dag conf base elem_txt vbar_txt invert set spl page_title
     next_txt] builds a DAG from the person set and renders it as HTML.
    - [elem_txt]: function to generate display text for each person
    - [vbar_txt]: function to generate vertical bar link for each person
    - [invert]: if true, display descendants at top, ancestors at bottom
    - [set]: list of person identifiers to include in the DAG
    - [spl]: spouse pairs to connect in the graph
    - [page_title]: title displayed in the page header
    - [next_txt]: link text for pagination (empty if none) *)

val print : Config.config -> Geneweb_db.Driver.base -> unit
(** [print conf base] is the entry point for the [m=DAG] request.

    The person set is collected by {!Dag.get_dag_elems} from the indexed URL
    parameters ([iN] or [pN]/[nN]/[ocN]) and their Sosa branches ([sN]), then
    laid out and rendered by {!make_and_print_dag}. The [invert=on] parameter
    flips the tree orientation; the page title is the localised ["tree"]
    translation. *)
