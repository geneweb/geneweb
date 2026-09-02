exception Error of int
(** Exception raised by functions of the module with the WSA error code. *)

module Protocol_info : sig
  type t
  (** Type of a protocol information structure. *)

  val duplicate_socket : Unix.file_descr -> int -> t
  (** [duplicate_socket s pid] duplicates the socket [s] for the process
      identified by the pseudo handle [pid]. *)

  val to_socket : t -> Unix.file_descr
  (** [to_socket pi] converts a protocol information into a socket. This
      function must be run in the process with the [pid] given to
      [duplicate_socket]. *)
end
