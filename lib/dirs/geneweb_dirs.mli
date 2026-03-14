type one
type many
type 'a var

val name : 'a var -> string
(** [name v] returns the standard name of the variable. *)

val path : one var -> string
(** [path v] returns the path contained in the variable. *)

val concat : one var -> string -> one var
(** [concat v s] returns a new variable whose the path is suffixed by [s]. *)

val ( // ) : one var -> string -> one var
(** Alias of [concat]. *)

val paths : many var -> string list
(** [paths v] returns the list of paths contained in the variable. *)

type t

val make : ?getenv:(string -> string option) -> unit -> t
val data_home : t -> one var
val cache_home : t -> one var
