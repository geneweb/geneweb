(* $Id: lock.mli,v 4.0 2001-03-16 19:34:48 ddr Exp $ *)

value no_lock_flag : ref bool;
value control : string -> bool -> (unit -> 'a) -> option 'a;
