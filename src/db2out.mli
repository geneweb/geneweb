(* camlp5r *)
(* $Id: db2out.mli,v 5.8 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

value phony_min_size : int;

value output_item_return_pos :
  out_channel -> Hashtbl.t 'a int -> ref int -> bool -> 'a -> int;
value output_value_array :
  out_channel -> 'a -> bool -> (('a -> int) -> unit) -> unit;

value output_hashtbl : string -> string -> Hashtbl.t _ _ -> unit;

value make_indexes : string -> int -> list string -> unit;
