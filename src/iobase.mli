(* $Id: iobase.mli,v 3.2 2000-01-28 14:11:35 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;

value input : string -> base;
value output : string -> base -> unit;
value gen_output : bool -> string -> base -> unit;

value lock_file : string -> string;

value name_key : string -> string;

value save_mem : ref bool;
