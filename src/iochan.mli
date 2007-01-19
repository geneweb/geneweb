(* $Id: iochan.mli,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

type t = 'abstract;

value openfile : string -> bool -> t;
value input_binary_int : t -> int;
value input_value_no_header : t -> 'a;
value output_binary_int : t -> int -> unit;
value output_value_no_header : t -> 'a -> unit;
value seek : t -> int -> unit;
value seek_end : t -> int;
value close : t -> unit;
