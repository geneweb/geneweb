(* $Id: iobase.mli,v 4.0 2001-03-16 19:34:47 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;

value input : string -> base;
value output : string -> base -> unit;
value gen_output : bool -> string -> base -> unit;

value lock_file : string -> string;

value name_key : string -> string;

value save_mem : ref bool;
