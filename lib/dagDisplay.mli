(* TODOOCP *)
val image_txt : Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string

type item = Item of Gwdb.person * Adef.safe_string

val make_tree_hts :
  Config.config ->
  Gwdb.base ->
  (Gwdb.person -> item) ->
  (Gwdb.iper -> Adef.escaped_string) ->
  bool ->
  Gwdb.iper list ->
  (Gwdb.iper * (Gwdb.iper * Gwdb.ifam option)) list ->
  (Gwdb.iper, 'a) Def.choice Dag2html.dag ->
  (int * Dag2html.align * Adef.safe_string Dag2html.table_data) array array
(** [make_tree_hts conf base elem_txt vbar_txt invert set spl d]  *)

val print_slices_menu_or_dag_page :
  Config.config ->
  Gwdb.base ->
  Adef.safe_string ->
  (int * Dag2html.align * Adef.safe_string Dag2html.table_data) array array ->
  Adef.escaped_string ->
  unit
(** [print_slices_menu_or_dag_page conf page_title hts next_txt] *)

val make_and_print_dag :
  Config.config ->
  Gwdb.base ->
  (Gwdb.person -> item) ->
  (Gwdb.iper -> Adef.escaped_string) ->
  bool ->
  Gwdb.iper list ->
  (Gwdb.iper * (Gwdb.iper * Gwdb.ifam option)) list ->
  Adef.safe_string ->
  Adef.escaped_string ->
  unit
(** [make_and_print_dag conf base elem_txt vbar_txt invert set spl page_title next_txt] *)

val print : Config.config -> Gwdb.base -> unit
