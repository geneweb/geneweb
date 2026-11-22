(** Files and directories of a database, in one of its two layouts: reorg, where
    everything is in [<bases_dir>/<bname>.gwb], or legacy, where the
    configuration, sources and images are in directories of [<bases_dir>] shared
    by all the bases. The paths are computed from {!Secure.bases_dir} when asked
    for. *)

type t
(** The layout of a database. *)

type mode =
  | Reorg
  | Legacy
  | Detect  (** reorg if {!is_reorg_base}, legacy otherwise *)

val bpath : string -> string
(** [bpath bname] is the directory of the database [bname],
    [<bases_dir>/<bname>.gwb], in both layouts.
    @raise Invalid_argument if [bname] is not a valid name ({!Mutil.good_name}).
*)

val is_reorg_base : string -> bool
(** [is_reorg_base bname] is [true] iff [<bname>.gwb/config/<bname>.gwf] exists.
*)

val of_bname : mode:mode -> string -> t
(** [of_bname ~mode bname] is the layout of the database [bname].
    @raise Invalid_argument if [bname] is not a valid name ({!Mutil.good_name}).
*)

val is_reorg : t -> bool
(** [true] for the reorg layout. *)

val gwf : t -> string
(** Configuration file: [<bname>.gwb/config/<bname>.gwf] or [<bname>.gwf]. *)

val config : t -> string
(** Directory of the configuration file and of the authorization files:
    [<bname>.gwb/config] or the bases directory. *)

val cnt : t -> string
(** Directory of the counters: [<bname>.gwb/config/cnt] or [cnt]. *)

val etc : t -> string
(** Templates of the base: [<bname>.gwb/etc] or [etc/<bname>]. *)

val src : t -> string
(** Source files of the base: [<bname>.gwb/src] or [src/<bname>]. *)

val lang : t -> string
(** Lexicon of the base: [<bname>.gwb/lang] or [lang/<bname>]. *)

val portraits : t -> string
(** Portraits: [<bname>.gwb/documents/portraits] or [images/<bname>]. *)

val images : t -> string
(** Images of the carrousel: [<bname>.gwb/documents/images] or
    [src/<bname>/images]. *)

val albums : t -> string
(** Albums: [<bname>.gwb/documents/albums] or [src/<bname>/albums]. *)
