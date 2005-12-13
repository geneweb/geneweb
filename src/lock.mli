(* $Id: lock.mli,v 5.0 2005-12-13 11:51:27 ddr Exp $ *)

value no_lock_flag : ref bool;
value control : string -> bool -> (unit -> 'a) -> option 'a;
