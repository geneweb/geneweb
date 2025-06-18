(* TODOOCP *)
val image_txt :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string

type item = Item of Geneweb_db.Driver.person * Adef.safe_string

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
(** [make_tree_hts conf base elem_txt vbar_txt invert set spl d] *)

val print_slices_menu_or_dag_page :
  Config.config ->
  Geneweb_db.Driver.base ->
  Adef.safe_string ->
  (int * Dag2html.align * Adef.safe_string Dag2html.table_data) array array ->
  Adef.escaped_string ->
  unit
(** [print_slices_menu_or_dag_page conf page_title hts next_txt] *)

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
     next_txt] *)

val print : Config.config -> Geneweb_db.Driver.base -> unit
