val no_lock_flag : bool ref

val print_error_and_exit : unit -> unit

val print_try_again : unit -> unit

val control
  : onerror:(unit -> 'a) -> string -> bool -> (unit -> 'a) -> 'a

val control_retry
  : onerror:(unit -> 'a) -> string -> (unit -> 'a) -> 'a

