(* $Id: database.mli,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

val opendb : string -> Dbdisk.dsk_base

(* Ajout pour l'API *)
type synchro_patch =
  { mutable synch_list : (string * int list * int list) list }
val input_synchro : string -> synchro_patch