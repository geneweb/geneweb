(* $Id: iovalue.mli,v 1.1.1.1 1998-09-01 14:32:03 ddr Exp $ *)

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

type sizes =
  { size_32 : mutable int;
    size_64 : mutable int }
;
type out_funs 'a =
  { output_byte : 'a -> int -> unit;
    output_binary_int : 'a -> int -> unit;
    output : 'a -> string -> int -> int -> unit }
;
value gen_output : sizes -> out_funs 'a -> 'a -> 'b -> unit;
value out_channel_funs : out_funs out_channel;
