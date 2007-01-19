(* $Id: outbase.mli,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk;

value output : string -> dsk_base -> unit;
value gen_output : bool -> string -> dsk_base -> unit;
value save_mem : ref bool;
