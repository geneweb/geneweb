(* Copyright (c) 1998-2007 INRIA *)

(** Returns list of allowed to acces assets *)
val assets : unit -> string list

(** Returns base directory to which acces is allowed *)
val bd : unit -> string

(** Add new asset to the [assets] list *)
val add_assets : string -> unit

(** Set base directory *)
val set_base_dir : string -> unit

(** Check if a filename is safe to read or not
    (i.e. will not read in a location it is not supposed to read).
*)
val check : string -> bool

(** Secured version of [open_in] *)
val open_in : string -> in_channel

(** Secured version of [open_in_bin] *)
val open_in_bin : string -> in_channel

(** Secured version of [open_out] *)
val open_out : string -> out_channel

(** Secured version of [open_out_bin] *)
val open_out_bin : string -> out_channel

(** Secured version of [open_out_gen] *)
val open_out_gen : open_flag list -> int -> string -> out_channel
