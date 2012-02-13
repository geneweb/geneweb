(* camlp5r *)
(* $Id: db2out.mli,v 5.12 2012-01-21 10:30:52 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

value phony_min_size : int;

value output_item_compress_return_pos :
  out_channel -> Hashtbl.t 'a int -> ref int -> 'a -> int;

value output_value_array_no_compress :
  string -> string -> int -> 'a -> (out_channel -> ('a -> int) -> unit) ->
    unit;

value output_value_array_compress :
  string -> string -> int -> 'a -> (out_channel -> ('a -> int) -> unit) ->
    unit;

value output_hashtbl : string -> string -> Hashtbl.t _ _ -> unit;

value make_indexes : string -> int -> list string -> unit;

value check_input_value : string -> string -> int -> unit;
