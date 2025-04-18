type 'a vother =
  | Vdef of (string * TemplAst.ast option) list * TemplAst.ast list
  | Vval of 'a TemplAst.expr_val
  | Vbind of string * Adef.encoded_string

module Env : sig
  type 'a t

  val empty : 'a t
  val find : string -> 'a t -> 'a
  val add : string -> 'a -> 'a t -> 'a t
end

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

val apply_format : Config.config -> int option -> string -> string -> string
val eval_transl_lexicon : Config.config -> bool -> string -> string -> string
val eval_transl : Config.config -> bool -> string -> string -> string
val input_templ : Config.config -> string -> TemplAst.ast list option
val include_hed_trl : Config.config -> string -> unit

val open_etc_file : Config.config -> string -> (in_channel * string) option
(** [open_etc_file conf fname] search for template {i etc/fname.txt}
    inside the base directory or inside one of assets directories.
    Returns input channel and the path to given template. *)

val include_begin : Config.config -> Adef.safe_string -> unit
val include_end : Config.config -> Adef.safe_string -> unit

val include_template :
  Config.config -> Adef.encoded_string Env.t -> string -> (unit -> unit) -> unit
(** [include_template conf env fname failure]
    Search [fname] in templates path and interpret it with global environnement [env] provided.
    Interpretation of template write directly its results in the socket.
    If the file can not be found, [failure] is called.
*)

val copy_from_templ :
  Config.config -> Adef.encoded_string Env.t -> in_channel -> unit

val interp_ast :
  Config.config ->
  ('a, 'b) interp_fun ->
  'a Env.t ->
  'b ->
  TemplAst.ast list ->
  unit

val print_copyright : Config.config -> unit
