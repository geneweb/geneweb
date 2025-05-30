val parse_source : Loc.source -> Ast.t list
(** [parse_source src] parses [src] without expanding included templates. *)

val parse :
  on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  resolve_include:(Loc.t -> string -> string) ->
  Loc.source ->
  Ast.t list
(** [parse ~on_exn ~find src] parses [src] and expands included templates.
    The [resolve_include] function resolves paths for included files.
    The [on_exn] callback handles exceptions raised during included template
    parsing, providing both the exception and its backtrace. *)
