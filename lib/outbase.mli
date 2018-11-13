(* $Id: outbase.mli,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk

val output : string -> dsk_base -> unit
val gen_output : bool -> string -> dsk_base -> unit
val save_mem : bool ref
