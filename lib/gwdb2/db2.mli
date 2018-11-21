(* $Id: db2.mli,v 5.4 2012-01-20 19:02:51 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

val first_item_pos : int -> int

type key2
val key2_of_key : Adef.istr * Adef.istr * int -> key2
