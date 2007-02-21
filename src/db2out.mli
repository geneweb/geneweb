(* camlp4r *)
(* $Id: db2out.mli,v 5.5 2007-02-21 20:44:04 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

value phony_min_size : int;

value output_item_return_pos :
  out_channel -> Hashtbl.t 'a int -> ref int -> 'a -> int;
value output_value_array :
  out_channel -> 'a -> (('a -> int) -> unit) -> unit;

value output_hashtbl : string -> string -> Hashtbl.t _ _ -> unit;

value make_string_of_crush_index : string -> unit;
value make_person_of_string_index : string -> unit;
value make_name_index : string -> int -> unit;
value make_index : string -> list string -> string -> unit;
