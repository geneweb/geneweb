(** Abstract type for file system paths. *)

type t
(** Abstract type representing a file system path. *)

val empty : t
(** The empty path. *)

val is_empty : t -> bool

val of_string : string -> t
(** [of_string s] creates a path from the string [s]. *)

val to_string : t -> string
(** [to_string p] returns the string representation of path [p]. *)

val concat : t -> t -> t
(** [concat dir file] returns a path that designates [file] inside [dir]. *)

val of_segments : string list -> t
(** [of_segments segments] creates a path by concatenating all [segments].
    @raise Invalid_argument if [segments] is empty. *)

val dirname : t -> t
(** [dirname p] returns the directory containing [p]. *)

val basename : t -> string
(** [basename p] returns the base name of path [p]. *)

val check_suffix : t -> string -> bool

val chop_extension : t -> t
(** [chop_extension p] removes the extension from [p].
    @raise Invalid_argument if [p] has no extension. *)

val remove_extension : t -> t
(** [remove_extension p] removes the extension from [p]. Returns [p] unchanged
    if it has no extension. *)

val replace_extension : t -> string -> t
(** [replace_extension p ext] replaces the extension of [p] with [ext]. [ext]
    should include the leading dot (e.g. ".txt"). *)

val extension : t -> string
(** [extension p] returns the extension of [p] (including the dot). *)

val equal : t -> t -> bool
(** [equal p1 p2] returns [true] iff [p1] and [p2] are equal paths. *)

val compare : t -> t -> int
(** [compare p1 p2] compares paths [p1] and [p2]. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt p] prints [p] to the formatter [fmt]. *)

val ( ~$ ) : string -> t
(** [~$s] is [of_string s]. *)

val ( // ) : t -> t -> t
(** [dir // file] is [concat dir file]. *)
