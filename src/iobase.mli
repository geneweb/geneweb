(* $Id: iobase.mli,v 4.1 2001-04-12 08:49:54 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;

value input : string -> base;
value output : string -> base -> unit;
value gen_output : bool -> string -> base -> unit;

value lock_file : string -> string;

value name_key : string -> string;

value save_mem : ref bool;
value verbose : ref bool;
