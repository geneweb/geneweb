type t
(** Structure that store credentials of the process. *)

val init : unit -> t
(** Initialize a structure of type [t] with the current credentials of the
    current process. *)

val restore : t -> unit
(** [restore t] restores the privileged credentials. *)

val drop : t -> unit
(** [drop_all t] drops permanently all the privileges. *)
