(* val src : Logs.src *)
(** Source of debugging logs of this module. *)

type t
(** Type of a worker pool for workers. *)

val start : int -> (int -> unit) -> unit
(** [start n k] creates a worker pool of [n] workers and executes the function
    [k] in each of them.

    @raise Invalid_argument
      if [n] is smaller than [1] or [Sys.Unix] is not [true]. *)
