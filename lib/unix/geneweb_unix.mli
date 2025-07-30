val seteuid : int -> unit
(** [seteuid euid] shall set the effective user ID of the process.

    @raise Unix.Unix_error if it cannot change the ID.
    @raise Invalid_arg on non-Unix platform. *)

val setegid : int -> unit
(** [setegid egid] shall set the effective group IDs of the process.

    @raise Unix.Unix_error if it cannot change the ID.
    @raise Invalid_arg on non-Unix platform. *)

val setreuid : int -> int -> unit
(** [setreuid ruid euid] shall set the real and effective user IDs of the
    process. If [ruid] is [-1], the real user ID shall not be changed; if [euid]
    is [-1], the effective user ID shall not be changed.

    @raise Unix.Unix_error if it cannot change the IDs.
    @raise Invalid_arg on non-Unix platform. *)

val setregid : int -> int -> unit
(** [setregid rgid egid] shall set the real and effective group IDs of the
    process. If [rgid] is [-1], the real group ID shall not be changed; if
    [egid] is [-1], the effective group ID shall not be changed.

    @raise Unix.Unix_error if it cannot change the IDS.
    @raise Invalid_arg on non-Unix platform. *)

(* As the SA_RESTART flag is no support in Unix OCaml library, the following
   functions provide variants of system calls that automatically restart after
   being interrupted by signals. *)

val waitpid_noeintr : Unix.wait_flag list -> int -> int * Unix.process_status
(** Equivalent to [Unix.waitpid], but this variant restart upon interruption by
    signals. *)

val accept_noeintr :
  ?cloexec:bool -> Unix.file_descr -> Unix.file_descr * Unix.sockaddr
(** Equivalent to [Unix.accept], but this variant restart upon interruption by
    signals. *)
