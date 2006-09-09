(* $Id: progrBar.mli,v 5.1 2006-09-09 18:27:44 ddr Exp $ *)

value full : ref char;
value start : unit -> unit;
value run : int -> int -> unit;
value finish : unit -> unit;
