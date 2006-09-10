(* $Id: progrBar.mli,v 5.2 2006-09-10 08:22:14 ddr Exp $ *)

value start : unit -> unit;
value run : int -> int -> unit;
value finish : unit -> unit;

value empty : ref char;
value full : ref char;
