(* $Id: iobase.mli,v 3.0 1999-10-29 10:31:20 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;

value input : string -> base;
value output : string -> base -> unit;

value simple_output : string -> base -> unit;
value gen_output : bool -> string -> base -> unit;

value lock_file : string -> string;

value name_key : string -> string;

value save_mem : ref bool;
