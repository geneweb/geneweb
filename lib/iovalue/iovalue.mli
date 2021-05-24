(* $Id: iovalue.mli,v 5.5 2012-01-27 08:53:53 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

val input : in_channel -> 'a
val output : out_channel -> 'a -> unit

val digest : 'a -> string

val sizeof_long : int
val sign_extend : int -> int

(* generic functions *)

type 'a in_funs =
  { input_byte : 'a -> int;
    input_binary_int : 'a -> int;
    input : 'a -> bytes -> int -> int -> unit }
val gen_input : 'a in_funs -> 'a -> 'b

type 'a out_funs =
  { output_byte : 'a -> int -> unit;
    output_binary_int : 'a -> int -> unit;
    output : 'a -> string -> int -> int -> unit }
val gen_output : 'a out_funs -> 'a -> 'b -> unit

val output_block_header : out_channel -> int -> int -> unit

val output_array_access : out_channel -> (int -> 'a) -> int -> int -> int
