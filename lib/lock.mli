(* $Id: lock.mli,v 5.0 2005-12-13 11:51:27 ddr Exp $ *)

val no_lock_flag : bool ref
val control : string -> bool -> (unit -> 'a) -> 'a option
