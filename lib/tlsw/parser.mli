(** TLSW: Text Language Stolen to Wikipedia *)

type error_handler = loc:Loc.t -> string -> unit

val parse : ?strict:bool -> on_err:error_handler -> string -> Ast.t list
(** [parse ~on_err s] parses the input [s] for the TLSW syntax. *)

val parse_links : on_err:error_handler -> string -> Ast.link list
(** [parse_links ~on_err s] *)
