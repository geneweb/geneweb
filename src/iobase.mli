(* $Id: iobase.mli,v 5.3 2006-09-22 12:40:35 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Gwdb;

value input : string -> base;
value output : string -> base -> unit;
value gen_output : bool -> string -> base -> unit;

value name_key : string -> string;

value save_mem : ref bool;
value verbose : ref bool;
