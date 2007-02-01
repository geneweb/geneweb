(* $Id: progrBar.mli,v 5.3 2007-02-01 10:28:55 ddr Exp $ *)

value start : unit -> unit;
value run : int -> int -> unit;
value finish : unit -> unit;

value suspend : unit -> unit;
value restart : int -> int -> unit;

value empty : ref char;
value full : ref char;
