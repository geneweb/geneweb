module Env : sig
  type 'a t

  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find : string -> 'a t -> 'a
end

type 'a vother =
  | Vdef of (string * TemplAst.ast option) list * TemplAst.ast list
  | Vval of 'a TemplAst.expr_val
  | Vbind of string * Adef.encoded_string

type ('a, 'b) interp_fun = {
  eval_var :
    'a Env.t -> 'b -> TemplAst.loc -> string list -> 'b TemplAst.expr_val;
  eval_transl : 'a Env.t -> bool -> string -> string -> string;
  eval_predefined_apply :
    'a Env.t -> string -> 'b TemplAst.expr_val list -> string;
  get_vother : 'a -> 'b vother option;
  set_vother : 'b vother -> 'a;
  print_foreach :
    ('a Env.t -> 'b -> TemplAst.ast -> unit) ->
    ('a Env.t -> 'b -> TemplAst.ast -> string) ->
    'a Env.t ->
    'b ->
    TemplAst.loc ->
    string ->
    string list ->
    TemplAst.ast list list ->
    TemplAst.ast list ->
    unit;
}

val input_templ : Config.config -> string -> TemplAst.ast list option

val interp_ast :
  Config.config ->
  ('a, 'b) interp_fun ->
  'a Env.t ->
  'b ->
  TemplAst.ast list ->
  unit

val eval_transl : Config.config -> bool -> string -> string -> string
val eval_transl_lexicon : Config.config -> bool -> string -> string -> string

val copy_from_templ :
  Config.config -> Adef.encoded_string Env.t -> in_channel -> unit

val include_hed_trl : Config.config -> string -> unit

val include_template :
  Config.config -> Adef.encoded_string Env.t -> string -> (unit -> unit) -> unit

val apply_format : Config.config -> int option -> string -> string -> string

val print_copyright : Config.config -> unit
(** Evaluates and prints content of {i cpr} template.
    If template wasn't found prints basic copyrigth HTML structure. *)
