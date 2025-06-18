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

val listen_noeintr : Unix.file_descr -> int -> unit
(** Equivalent to [Unix.listen], but this variant restart upon interruption by
    signals. *)
