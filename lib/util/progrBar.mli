(* $Id: progrBar.mli,v 5.3 2007-02-01 10:28:55 ddr Exp $ *)

val start : unit -> unit
val run : int -> int -> unit
val finish : unit -> unit

val suspend : unit -> unit
val restart : int -> int -> unit

val empty : char ref
val full : char ref
