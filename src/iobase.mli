(* $Id: iobase.mli,v 4.2 2004-12-14 09:30:13 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;

value input : string -> base;
value output : string -> base -> unit;
value gen_output : bool -> string -> base -> unit;

value lock_file : string -> string;

value name_key : string -> string;

value save_mem : ref bool;
value verbose : ref bool;
