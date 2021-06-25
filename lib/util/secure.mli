(* Copyright (c) 1998-2007 INRIA *)

val assets : unit -> string list
val bd : unit -> string

val add_assets : string -> unit
val set_base_dir : string -> unit

(** Check if a filename is safe to read or not
    (i.e. will not read in a location it is not supposed to read).
*)
val check : string -> bool

val open_in : string -> in_channel
val open_in_bin : string -> in_channel
val open_out : string -> out_channel
val open_out_bin : string -> out_channel
val open_out_gen : open_flag list -> int -> string -> out_channel
