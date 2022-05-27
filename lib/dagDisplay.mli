(* TODOOCP *)
val image_txt : Config.config -> Gwdb.base -> Gwdb.person -> string
type item = Item of Gwdb.person * string
val make_tree_hts :
  Config.config ->
  Gwdb.base ->
  (Gwdb.person -> item) ->
  (Gwdb.iper -> string) ->
  bool ->
  Gwdb.iper list ->
  (Gwdb.iper * (Gwdb.iper * Gwdb.ifam option)) list ->
  (Gwdb.iper, 'a) Def.choice Dag2html.dag ->
  (int * Dag2html.align *
   (string, string) Dag2html.table_data)
  array array
type dag_item = string
val print_slices_menu_or_dag_page :
  Config.config ->
  Gwdb.base ->
  string ->
  (int * Dag2html.align *
   (dag_item, string) Dag2html.table_data)
  array array -> string -> unit
val make_and_print_dag :
  Config.config ->
  Gwdb.base ->
  (Gwdb.person -> item) ->
  (Gwdb.iper -> string) ->
  bool ->
  Gwdb.iper list ->
  (Gwdb.iper * (Gwdb.iper * Gwdb.ifam option)) list ->
  string -> string -> unit
val print : Config.config -> Gwdb.base -> unit