(** This module provides a portable representation of file positions for the
    GeneWeb binary format.

    Historically, GeneWeb was developed on 32-bit systems and used signed 32-bit
    integers to store positions and offsets on disk. This choice limited the
    maximum size of the `base` file to 2 GiB on 64-bit systems.

    Currently, positions are represented as 32-bit unsigned integers in
    big-endian format. This allows for a maximum file size of approximately 4
    GiB on 64-bit systems. *)

type t
(** The type for file positions. *)

exception Overflow
(** Raised when a position exceeds the storage capacity of type [t]. *)

val incr : t -> int -> t
(** [incr p i] adds [i] units to [p].

    @raise Invalid_argument if [i] is negative.
    @raise Overflow if the result cannot be represented by type [t]. *)

val compare : t -> t -> int
(** [compare p1 p2] compares positions [p1] and [p2]. *)

val pp : t Fmt.t
(** [pp ppf p] formats the position [p] on the formatter [ppf]. *)

val input : in_channel -> t
(** [input ic] reads 4 bytes in big-endian from the input channel [ic] and
    returns the corresponding position. *)

val output : out_channel -> t -> unit
(** [output oc p] writes the position [p] to the output channel [oc]. *)

val pos_in : in_channel -> t
(** [pos_in ic] returns the current reading position of [ic]. This function is
    subject to the same limitations as [In_channel.pos].

    @raise Overflow if the current position cannot be represented by type [t].
*)

val seek_in : in_channel -> t -> unit
(** [seek_in ic p] sets the current reading position of [ic] to [p]. This
    function is subject to the same limitations as [In_channel.seek]. *)

val pos_out : out_channel -> t
(** [pos_out oc] returns the current writing position of [oc]. This function is
    subject to the same limitations as [Out_channel.pos].

    @raise Overflow if the current position cannot be represented by type [t].
*)

val seek_out : out_channel -> t -> unit
(** [seek_out oc p] sets the current writing position of [oc] to [p]. This
    function is subject to the same limitations as [Out_channel.seek]. *)
