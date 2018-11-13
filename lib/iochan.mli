(* $Id: iochan.mli,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

type t

val openfile : string -> bool -> t
val input_binary_int : t -> int
val input_value_no_header : t -> 'a
val output_binary_int : t -> int -> unit
val output_value_no_header : t -> 'a -> unit
val seek : t -> int -> unit
val seek_end : t -> int
val close : t -> unit
