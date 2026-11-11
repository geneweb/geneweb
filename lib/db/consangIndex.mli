(** Index of the consanguinities of a base: the file {i consang.dat} of its
    directory, a magic number followed by one record of 8 bytes per person, in
    the order of their identifiers.

    A record holds the inbreeding coefficient of the person as a single
    precision float, so that the smallest values are kept, and the number of
    implexes of its ancestry. It is written in place: no patch of the base, and
    a lock of its own, {i consang.lck}, taken by the writers only. A record
    missing or left at zero stands for a person the index knows nothing about.
*)

type consang =
  | Unset  (** No record: the coefficient stored in the base applies. *)
  | Outdated
      (** The ancestry changed since the coefficient was computed, the one
          stored in the base included. *)
  | Deferred
      (** A reader of the base gave up computing the coefficient: it is left to
          [consang]. *)
  | Known of float

type entry = {
  consang : consang;
  implexes : int option;
      (** Ancestors reached through several children, when counted. *)
}

val unset : entry
val outdated : entry

type t
(** Index of a base as it was opened. *)

val make : unchanged:(unit -> bool) -> string -> t
(** [make ~unchanged bdir] is the index of the base directory [bdir]. No file is
    opened. [unchanged ()] tells whether the base is still as it was read: a
    value computed from an earlier state is not stored. *)

val get : t -> int -> entry
(** [get t i] is the record of the person [i], {!unset} when the file is
    missing. The file is opened by the first call and kept until {!close}. *)

val close : t -> unit
(** [close t] closes the file opened by {!get}. *)

val remove : t -> unit
(** [remove t] deletes the file of the index. *)

val update : t -> ((int -> entry) -> (int -> entry -> unit) -> 'a) -> 'a
(** [update t k] calls [k] with the reader and the writer of the records, under
    the lock of the index, waited for. The file is created when missing. *)

val cache : t -> ((int -> entry) -> (int -> entry -> unit) -> unit) -> unit
(** [cache t k] is [update t k] for values computed by a reader of the base:
    nothing is done when the lock is taken, when the base changed since {!make}
    or when the file cannot be written. *)
