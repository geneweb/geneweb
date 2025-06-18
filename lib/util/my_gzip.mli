type in_channel
(** Abstract type representing a channel opened for reading from compressed
    file. *)

val open_in : string -> in_channel
(** [open_in path] open a compressed file [path]. *)

val with_open : string -> (in_channel -> 'a) -> 'a
(** [with_open path f] open the compressed file [path] and call [f] on the input
    channel. The input channel is properly closed even if [f] raises exceptions.
*)

val close_in : in_channel -> unit
(** [close_in ic] closes the input channel. *)

val close_in_noerr : in_channel -> unit
(** [close_in_noerr ic] closes the input channel without raising exception. *)

val input_line : in_channel -> string
(** [input_line ic] reads the next line on the channel [ic]. The last line will
    be produce even if it does not end with a line feed.

    @raise End_of_file if it reaches the end of the file. *)
