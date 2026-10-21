val no_lock_flag : bool ref
(** Flag that indicates if the lock should be used. *)

val pp_exception : Format.formatter -> exn * Printexc.raw_backtrace -> unit

val control :
  on_exn:(exn -> Printexc.raw_backtrace -> 'a) ->
  wait:bool ->
  lock_file:string ->
  (unit -> 'a) ->
  'a
(** [control ~on_exn ~wait ~lock_file k] tries to acquire a lock on [lock_file]
    and invokes [k] on succeed. If [wait] is [true], the function blocks until
    it acquires the lock. On failure, [on_exn] callback is invoked with the
    exception and its backtrace.

    If flag [no_lock_flag] is up, this function invokes immediatly [k] without
    lock. *)
