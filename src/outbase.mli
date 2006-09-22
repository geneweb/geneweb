(* $Id: outbase.mli,v 5.1 2006-09-22 19:26:59 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Gwdb;

value output : string -> base -> unit;
value gen_output : bool -> string -> base -> unit;
value save_mem : ref bool;
