(** The directories of the media of a base: images, albums and the images of the
    persons (see {!Layout.images} and {!Layout.albums}). *)

type file = { name : string; size : int }

type t = { files : file list; dirs : string list }
(** The entries of a directory, sorted by name: its regular files and the names
    of its subdirectories. *)

val saved : string
(** ["saved"], the subdirectory that keeps the files replaced or deleted. *)

val read : string -> t
(** [read dir] lists [dir] without the hidden entries (leading [.] or [~]) nor
    the subdirectory {!saved}. Empty if [dir] cannot be read. *)
