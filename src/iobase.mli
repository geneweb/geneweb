(* $Id: iobase.mli,v 1.2 1998-11-30 20:26:31 ddr Exp $ *)

open Def;

value magic_gwb : string;

value input : string -> base;
value output : string -> base -> unit;

value lock_file : string -> string;

value name_key : string -> string;
