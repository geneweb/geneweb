(* $Id: lock.mli,v 3.1 2000-05-14 12:41:28 ddr Exp $ *)

value no_lock_flag : ref bool;
value control : string -> bool -> (unit -> 'a) -> option 'a;
