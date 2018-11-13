(* $Id: secure.mli,v 5.2 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

val lang_path : unit -> string list
val base_dir : unit -> string
val add_lang_path : string -> unit
val set_base_dir : string -> unit

val open_in : string -> in_channel
val open_in_bin : string -> in_channel
val open_out : string -> out_channel
val open_out_bin : string -> out_channel
val open_out_gen : open_flag list -> int -> string -> out_channel
