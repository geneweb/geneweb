(* camlp4r *)
(* $Id: db2out.mli,v 5.1 2007-02-19 10:35:57 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

value phony_min_size : int;

value str_pos : out_channel -> Hashtbl.t 'a int -> ref int -> 'a -> int;
value output_value_array_string :
  out_channel -> ((string -> int) -> unit) -> unit;
