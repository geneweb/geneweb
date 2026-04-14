type backend = {
  ppf : Format.formatter;
  close : unit -> unit;
  flush : (unit -> unit) -> unit;
}

val make_backend : Unix.file_descr -> backend
(** [make_backend fd] creates a backend from the file descriptor [fd]. *)

val timestamp : Logs.Tag.set

val infer_renderer : Unix.file_descr -> Fmt.style_renderer
(** [infer_renderer fd] infers the capabilities of the file descriptor [fd] to
    prints special characters. *)

val setup :
  predictable_mode:bool -> (Unix.file_descr -> backend) -> Cmd.log -> unit
(** [setup ~predictable_mode mk l] setups a reporter for [l] using the
    constructor [mk] to build new backend. *)
