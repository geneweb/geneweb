type 'a vother =
  | Vdef of (string * TemplAst.ast option) list * TemplAst.ast list
  | Vval of 'a TemplAst.expr_val
  | Vbind of string * Adef.encoded_string

type 'a env = (string * 'a) list

type ('a, 'b) interp_fun = {
  eval_var :
    'a env -> 'b -> TemplAst.loc -> string list -> 'b TemplAst.expr_val;
  eval_transl : 'a env -> bool -> string -> string -> string;
  eval_predefined_apply :
    'a env -> string -> 'b TemplAst.expr_val list -> string;
  get_vother : 'a -> 'b vother option;
  set_vother : 'b vother -> 'a;
  print_foreach :
    ('a env -> 'b -> TemplAst.ast -> unit) ->
    ('a env -> 'b -> TemplAst.ast -> string) ->
    'a env ->
    'b ->
    TemplAst.loc ->
    string ->
    string list ->
    TemplAst.ast list list ->
    TemplAst.ast list ->
    unit;
}

val apply_format : Config.config -> int option -> string -> string -> string
val eval_transl_lexicon : Config.config -> bool -> string -> string -> string
val eval_transl : Config.config -> bool -> string -> string -> string
val input_templ : Config.config -> string -> TemplAst.ast list option
val include_hed_trl : Config.config -> string -> unit

val copy_from_templ :
  Config.config -> Adef.encoded_string env -> in_channel -> unit

val interp_ast :
  Config.config ->
  ('a, 'b) interp_fun ->
  'a env ->
  'b ->
  TemplAst.ast list ->
  unit

val print_copyright : Config.config -> unit
