(* $Id: iovalue.mli,v 5.5 2012-01-27 08:53:53 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

val sizeof_long : int
(** Size of long integer value inside the Geneweb's binary files *)

val input : in_channel -> 'a
(** Input a value from the giving channel. Identical to [Marshal.from_channel].
*)

val block_size : in_channel -> int option
(** [block_size ic] reads the header of the value starting at the current
    position of [ic], as written by [output_value] or {!output_array_access}'s
    writer, and returns its number of fields if it is a block. *)

val marshaled_block_size : 'a -> int option
(** Number of fields of [v] once marshaled, if it is a block; [None] also if the
    marshaling header is of an unknown kind. *)

val output_array_access :
  out_channel -> (int -> 'a) -> int -> Position.t -> Position.t
(** [output_array_acces oc getf arr_get arr_len pos] prints to the channel
    [oc] position for each element (that could be obtained with [arr_get]) in
    the binary file where marshalled array is stored. Array should be of length
    [arr_len] and should start at the position [pos] inside the binary file.
    Returns a position just after the end of array. *)
