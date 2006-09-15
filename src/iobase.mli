(* $Id: iobase.mli,v 5.2 2006-09-15 11:45:37 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Gwdb;

value input : string -> base;
value output : string -> base -> unit;
value gen_output : bool -> string -> base -> unit;
value input_particles : string -> list string;

value lock_file : string -> string;

value name_key : string -> string;

value save_mem : ref bool;
value verbose : ref bool;
