(* $Id: iobase.mli,v 1.1.1.1 1998-09-01 14:32:05 ddr Exp $ *)

open Def;

value magic_gwb : string;

value input : string -> base;
value output : string -> base -> unit;

value lock_file : string -> string;
