(* $Id: progrBar.mli,v 5.3 2007-02-01 10:28:55 ddr Exp $ *)

val empty : char ref
(** Character that represents not passed part of progression bar *)

val full : char ref
(** Character that represents passed part of progression bar *)

val start : unit -> unit
(** Prints empty bar with carriage return. *)

val run : int -> int -> unit
(** [run i len] modifies progression bar that is now filled proportionally to
    [i] by comparison with [len]. *)

val finish : unit -> unit
(** Stop printing progression bar and prints a new line. *)

val suspend : unit -> unit
(** Stop printing progression bar and prints a new line. *)

val restart : int -> int -> unit
(** [restart i len] restart progression bar. It's equivalent to call successively
    [run] from 0 to [i]. *)
