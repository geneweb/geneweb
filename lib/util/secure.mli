(* Copyright (c) 1998-2007 INRIA *)

val assets : unit -> Geneweb_fs.Fpath.t list
(** Returns list of allowed to acces assets *)

val default_base_dir : Geneweb_dirs.one Geneweb_dirs.var
(** Default value for the base directory. *)

val base_dir : unit -> Geneweb_fs.Fpath.t
(** Returns directory where databases are installed to which acces is allowed *)

val add_assets : Geneweb_fs.Fpath.t -> unit
(** Add new asset to the [assets] list *)

val set_base_dir : Geneweb_fs.Fpath.t -> unit
(** Set base directory *)

val check : Geneweb_fs.Fpath.t -> bool
(** Check if a filename is safe to read:
    - it must not contain the '\000' character
    - it must either be relative to the local directory OR included in one of
      the allowed directories (base_dir or assets)
    - the relative part does not contain the '..' directory *)

val open_in : Geneweb_fs.Fpath.t -> in_channel
(** Secured version of [open_in] *)

val with_open_in_text : Geneweb_fs.Fpath.t -> (in_channel -> 'a) -> 'a
(** Secured version of [In_channel.with_open_text] *)

val open_in_bin : Geneweb_fs.Fpath.t -> in_channel
(** Secured version of [open_in_bin] *)

val with_open_in_bin : Geneweb_fs.Fpath.t -> (in_channel -> 'a) -> 'a
(** Secured version of [In_channel.with_open_bin] *)

val open_out : Geneweb_fs.Fpath.t -> out_channel
(** Secured version of [open_out] *)

val with_open_out_text : Geneweb_fs.Fpath.t -> (out_channel -> 'a) -> 'a
(** Secured version of [Out_channel.open_text] *)

val open_out_bin : Geneweb_fs.Fpath.t -> out_channel
(** Secured version of [open_out_bin] *)

val with_open_out_bin : Geneweb_fs.Fpath.t -> (out_channel -> 'a) -> 'a
(** Secured version of [Out_channel.with_open_bin] *)

val open_out_gen : open_flag list -> int -> Geneweb_fs.Fpath.t -> out_channel
(** Secured version of [open_out_gen] *)

val with_open_out_gen :
  open_flag list -> int -> Geneweb_fs.Fpath.t -> (out_channel -> 'a) -> 'a
(** Secured version of [Out_channel.with_open_gen] *)
