(** DAG (Directed Acyclic Graph) display for genealogical trees. *)

val image_txt :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string
(** Generate HTML for person's portrait image. *)

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
(** Build HTML table structure from DAG.
    [make_tree_hts conf base elem_txt vbar_txt invert set spl dag] *)

val print_dag_page :
  Config.config ->
  Geneweb_db.Driver.base ->
  Adef.safe_string ->
  (int * Dag2html.align * Adef.safe_string Dag2html.table_data) array array ->
  Adef.escaped_string ->
  unit
(** Render DAG as HTML page using dag.txt template. *)

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
(** Build DAG from person set and render as HTML page. *)

val print : Config.config -> Geneweb_db.Driver.base -> unit
(** Entry point for m=DAG request. *)
