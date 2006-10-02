(* $Id: outbase.mli,v 5.2 2006-10-02 14:39:01 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Dbdisk;

value output : string -> dsk_base -> unit;
value gen_output : bool -> string -> dsk_base -> unit;
value save_mem : ref bool;
