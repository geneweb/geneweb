type fd
(** Type of serializable file descriptors. *)

val file_descr_to_fd : Unix.file_descr -> fd
(** [file_descr_to_fd x] converts a Unix file descriptor into a serializable
    file descriptor. On UNIX systems, this operation is noop. *)

val file_descr_of_fd : fd -> Unix.file_descr
(** [file_descr_of_fd x] converts a serializable file descriptor into Unix file
    descriptor. On UNIX systems, this operation is noop. *)
