(* $Id: iobase.mli,v 3.3 2001-01-06 09:55:57 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;

value input : string -> base;
value output : string -> base -> unit;
value gen_output : bool -> string -> base -> unit;

value lock_file : string -> string;

value name_key : string -> string;

value save_mem : ref bool;
