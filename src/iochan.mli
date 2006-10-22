(* $Id: iochan.mli,v 5.1 2006-10-22 08:38:16 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

type t = 'abstract;

value openfile : string -> bool -> t;
value input_binary_int : t -> int;
value input_value_no_header : t -> 'a;
value output_binary_int : t -> int -> unit;
value output_value_no_header : t -> 'a -> unit;
value seek : t -> int -> unit;
value seek_last : t -> int;
value close : t -> unit;
