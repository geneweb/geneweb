module Ast = Geneweb_templ.Ast
module Loc = Geneweb_templ.Loc

module Env : sig
  type 'a t

  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find : string -> 'a t -> 'a
end

type 'a expr_val =
  | VVbool of bool
  | VVstring of string
  | VVother of (string list -> 'a expr_val)

type 'a vother =
  | Vdef of (string * Ast.t option) list * Ast.t list
  | Vval of 'a expr_val
  | Vbind of string * Adef.encoded_string

type ('a, 'b) interp_fun = {
  eval_var : 'a Env.t -> 'b -> Loc.t -> string list -> 'b expr_val;
  eval_transl : 'a Env.t -> bool -> string -> string -> string;
  eval_predefined_apply : 'a Env.t -> string -> 'b expr_val list -> string;
  get_vother : 'a -> 'b vother option;
  set_vother : 'b vother -> 'a;
  print_foreach :
    ('a Env.t -> 'b -> Ast.t -> unit) ->
    ('a Env.t -> 'b -> Ast.t -> string) ->
    'a Env.t ->
    'b ->
    Loc.t ->
    string ->
    string list ->
    Ast.t list list ->
    Ast.t list ->
    unit;
}

val apply_format : Config.config -> int option -> string -> string -> string
val eval_transl : Config.config -> bool -> string -> string -> string
val eval_transl_lexicon : Config.config -> bool -> string -> string -> string
val eval_date_var : Config.config -> int -> string list -> 'a expr_val

val output :
  Config.config -> ('a, 'b) interp_fun -> 'a Env.t -> 'b -> string -> unit
(** [output conf ifun env v fl] outputs on the client socket the template [fl]
    using the functions [ifun] and the environment [env]. *)

type simple_env = Vstring of Adef.encoded_string | Vother of unit vother

val output_simple : Config.config -> simple_env Env.t -> string -> unit
(** [output_simple conf env fl] outputs on the client socket the template [fl]
    using only builtin evaluator and the environment [env]. *)
