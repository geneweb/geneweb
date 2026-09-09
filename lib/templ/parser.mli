val parse :
  on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  resolve_include:(Loc.t -> string -> string) ->
  Loc.source ->
  Ast.t list
(** [parse ?cached ~on_exn ~find src] parses [src] and expands included
    templates.
    - [resolve_include] function resolves paths for included files.
    - [on_exn] callback handles exceptions raised during included template
      parsing, providing both the exception and its backtrace. *)
