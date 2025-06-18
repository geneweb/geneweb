(* Copyright (c) 1998-2007 INRIA *)

val assets : unit -> string list
(** Returns list of allowed to acces assets *)

val base_dir : unit -> string
(** Returns directory where databases are installed to which acces is allowed *)

val add_assets : string -> unit
(** Add new asset to the [assets] list *)

val set_base_dir : string -> unit
(** Set base directory *)

val check : string -> bool
(** Check if a filename is safe to read:
    - it must not contain the '\000' character
    - it must either be relative to the local directory OR included in one of
      the allowed directories (base_dir or assets)
    - the relative part does not contain the '..' directory *)

val open_in : string -> in_channel
(** Secured version of [open_in] *)

val with_open_in_text : string -> (in_channel -> 'a) -> 'a
(** Secured version of [In_channel.with_open_text] *)

val open_in_bin : string -> in_channel
(** Secured version of [open_in_bin] *)

val with_open_in_bin : string -> (in_channel -> 'a) -> 'a
(** Secured version of [In_channel.with_open_bin] *)

val open_out : string -> out_channel
(** Secured version of [open_out] *)

val with_open_out_text : string -> (out_channel -> 'a) -> 'a
(** Secured version of [Out_channel.open_text] *)

val open_out_bin : string -> out_channel
(** Secured version of [open_out_bin] *)

val with_open_out_bin : string -> (out_channel -> 'a) -> 'a
(** Secured version of [Out_channel.with_open_bin] *)

val open_out_gen : open_flag list -> int -> string -> out_channel
(** Secured version of [open_out_gen] *)

val with_open_out_gen :
  open_flag list -> int -> string -> (out_channel -> 'a) -> 'a
(** Secured version of [Out_channel.with_open_gen] *)
