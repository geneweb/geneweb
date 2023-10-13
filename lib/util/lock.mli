val no_lock_flag : bool ref
(** Flag that indicates if the lock should be used. *)

val print_error_and_exit : unit -> unit
(** Print lock error message and terminate program. *)

val print_try_again : unit -> unit
(** Print message about locked database. *)

val control : onerror:(unit -> 'a) -> string -> bool -> (unit -> 'a) -> 'a
(** [control ~onerror lname wait f] opens file [lname], puts a write lock on it and then calls [f].
    If [wait] is true then if it tries to access locked file it will be blocked until these lock is removed. Otherwise
    it will fail, and function [onerror] will be called. If flag [no_lock_flag] is set then returns [f ()] immediatly. *)

val control_retry : onerror:(unit -> 'a) -> string -> (unit -> 'a) -> 'a
(** Tries to call [control] without blocking. If it fail (lock is put) then call again [control]
    and waits untill lock is removed. If it fails with another reason calls [onerror]. *)
