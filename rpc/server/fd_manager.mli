type t
(** Type of the file descriptor manager. *)

val make : ?max_connection:int -> ?idle_timeout:float -> unit -> t
(** [make ()] creates a file descriptor manager.

    If [max_connection], respectively [idle_timeout], is omitted, there will be
    no limit. *)

val add : t -> Lwt_unix.file_descr -> bool Lwt.t
(** [add t fd] adds the file descriptor [fd] to the manager [t]. Return [false]
    if [max_connection] limit is reached, in which case the file descriptor is
    closed. *)

val close : t -> Lwt_unix.file_descr -> unit Lwt.t
(** [close t s fd] closes the file descriptor [fd]. The function does not fail
    if [fd] is already closed. *)

val close_idle : t -> unit Lwt.t
(** [close_idle t] closes all the idle file descriptors. *)

val ping : t -> Lwt_unix.file_descr -> unit
(** [ping t fd] signales that [fd] is still active. *)

val loop : ?sleep:float -> t -> unit
(** [loop t] starts a loop that periodically closes idle file descriptors at
    intervals of [slee] seconds. *)
