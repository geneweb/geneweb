(* camlp4r *)
(* $Id: db2out.mli,v 5.3 2007-02-20 19:30:25 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

value phony_min_size : int;

value output_item_return_pos :
  out_channel -> Hashtbl.t 'a int -> ref int -> 'a -> int;
value output_value_array :
  out_channel -> 'a -> (('a -> int) -> unit) -> unit;
