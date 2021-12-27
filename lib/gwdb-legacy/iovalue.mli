(* $Id: iovalue.mli,v 5.5 2012-01-27 08:53:53 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(** Size of long integer value inside the Geneweb's binary files *)
val sizeof_long : int

(** Input a value from the giving channel. Identical to [Marshal.from_channel]. *)
val input : in_channel -> 'a

(** Output a value to the giving channel. Identical to [Marshal.to_channel] with [No_sharing] flag. *)
val output : out_channel -> 'a -> unit

(** [output_array_acces oc getf arr_get arr_len pos] prints to the channel
    [oc] position for each element (that could be obtained with [arr_get]) 
    in the binary file where marshalled array is stored. Array should be
    of length [arr_len] and should start at the position [pos] inside the 
    binary file. Returns a position just after the end of array. *)
val output_array_access : out_channel -> (int -> 'a) -> int -> int -> int
