(* $Id: iovalue.mli,v 5.1 2006-01-01 05:35:07 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

value input : in_channel -> 'a;
value output : out_channel -> 'a -> unit;

value size : 'a -> int;
value digest : 'a -> Digest.t;

value sizeof_long : int;

(* generic functions *)

type in_funs 'a =
  { input_byte : 'a -> int;
    input_binary_int : 'a -> int;
    input : 'a -> string -> int -> int -> unit }
;
value gen_input : in_funs 'a -> 'a -> 'b;
value in_channel_funs : in_funs in_channel;

type out_funs 'a =
  { output_byte : 'a -> int -> unit;
    output_binary_int : 'a -> int -> unit;
    output : 'a -> string -> int -> int -> unit }
;
value gen_output : out_funs 'a -> 'a -> 'b -> unit;
value out_channel_funs : out_funs out_channel;

value output_block_header : out_channel -> int -> int -> unit;
value size_32 : ref int;
value size_64 : ref int;
