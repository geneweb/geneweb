(* $Id: db2out.mli,v 5.12 2012-01-21 10:30:52 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

val phony_min_size : int

val output_item_compress_return_pos :
  out_channel -> ('a, int) Hashtbl.t -> int ref -> 'a -> int

val output_value_array_no_compress :
  string -> string -> int -> 'a -> (out_channel -> ('a -> int) -> unit) ->
    unit

val output_value_array_compress :
  string -> string -> int -> 'a -> (out_channel -> ('a -> int) -> unit) ->
    unit

val output_hashtbl : string -> string -> (_, _) Hashtbl.t -> unit

val make_indexes : string -> int -> string list -> unit

val check_input_value : string -> string -> int -> unit
