(* $Id: iovalue.mli,v 5.5 2012-01-27 08:53:53 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

value input : in_channel -> 'a;
value output : out_channel -> 'a -> unit;

value digest : 'a -> string;

value sizeof_long : int;
value sign_extend : int -> int;

(* making a header for input_value like output_value does *)

type header_pos = 'abstract;

value create_output_value_header : out_channel -> header_pos;
value patch_output_value_header : out_channel -> header_pos -> int;

(* generic functions *)

type in_funs 'a =
  { input_byte : 'a -> int;
    input_binary_int : 'a -> int;
    input : 'a -> bytes -> int -> int -> unit }
;
value gen_input : in_funs 'a -> 'a -> 'b;

type out_funs 'a =
  { output_byte : 'a -> int -> unit;
    output_binary_int : 'a -> int -> unit;
    output : 'a -> string -> int -> int -> unit }
;
value gen_output : out_funs 'a -> 'a -> 'b -> unit;

value output_block_header : out_channel -> int -> int -> unit;
value size_32 : ref int;
value size_64 : ref int;

value output_array_access : out_channel -> (int -> 'a) -> int -> int -> int;
