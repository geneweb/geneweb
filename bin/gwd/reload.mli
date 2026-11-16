(** Hot reload of in-memory caches (lexicons, asset/template lookups).

    gwd runs a pool of long-lived worker processes: a reload triggered by one
    worker cannot touch the memory of the others. The request is therefore
    signalled through a stamp file, [<bases>/gwd.reload]; every worker compares
    its modification time with the generation it last saw at the start of each
    request ({!check}) and flushes its own caches when it changed. The file can
    also be touched from outside (deployment script: [touch bases/gwd.reload]).
*)

val init : unit -> unit
(** Record the current generation. Call once in the master process, before the
    caches are filled and before the workers are forked. *)

val on_reload : (unit -> unit) -> unit
(** Register a cache-flushing function, run by {!check} when a reload is seen.
*)

val generation : unit -> string
(** Current generation, to be mixed into on-disk cache keys/magics so that a
    reload also invalidates the marshalled lexicon files. *)

val check : unit -> bool
(** Flush this process's caches if a reload was requested since the last call.
    Costs one [stat] per call. Returns [true] if caches were flushed. *)

val request : unit -> unit
(** Ask every worker to reload (touch the stamp file), then flush the caches of
    the current process immediately. *)
