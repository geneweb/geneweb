(** Marker of a long write task holding the lock of a base.

    A bulk task (MOD_DATA_OK, CHK_DATA_OK, PLACE_TERMS_OK, ...) writes it once
    it holds the lock and removes it when it ends. A request that needs the lock
    and finds it taken reads it to tell the wizard that a long task is running,
    instead of waiting silently. *)

type t = {
  command : string;  (** the command of the task, e.g. ["MOD_DATA_OK"] *)
  user : string;  (** the wizard who started it *)
  started : float;  (** start time, [Unix.time] *)
  pid : int;  (** process running it *)
}

val read : string -> t option
(** [read bfile] is the marker of the base [bfile], if a bulk task is running. A
    marker whose process is gone, or older than 12 hours, is ignored. *)

val with_marker : string -> command:string -> user:string -> (unit -> 'a) -> 'a
(** [with_marker bfile ~command ~user k] runs [k] with the marker of [bfile]
    set. To be called while holding the lock of the base. *)
