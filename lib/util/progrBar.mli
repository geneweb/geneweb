(* $Id: progrBar.mli,v 5.3 2007-02-01 10:28:55 ddr Exp $ *)

type t
(** Type of a progress bar. *)

val progress : t -> int -> int -> unit
(** [progress t n max] changes the completion of the progress bar to be
    [n / max] of its length.

    This function can be used in moderately hot loops. *)

val with_bar :
  ?width:int ->
  ?empty:char ->
  ?full:char ->
  ?disabled:bool ->
  Format.formatter ->
  (t -> 'a) ->
  'a
(** [with_bar ppf f] wraps the call to [f] with a progress bar printed on the
    formatter [ppf]. The function [step] can be call to change the progression
    of the bar in [f].

    To work properly, one should not print anything on [ppf] in [f].

    If [ppf] is the standard output or the error output, one should not print
    anything of both of them. *)

(* XXX: The below bar is deprecated and should not be used in new code. *)

val empty : char ref
(** Character that represents not passed part of progression bar *)

val full : char ref
(** Character that represents passed part of progression bar *)

val start : unit -> unit
(** Prints empty bar with carriage return. *)

val run : int -> int -> unit
(** [run i len] modifies progression bar that is now filled proportionally to
    [i] by comparison with [len]. *)

(* XXX: This function cannot be used in hot loops without impacting
        performance. *)

val finish : unit -> unit
(** Stop printing progression bar and prints a new line. *)

val suspend : unit -> unit
(** Stop printing progression bar and prints a new line. *)

val restart : int -> int -> unit
(** [restart i len] restart progression bar. It's equivalent to call
    successively [run] from 0 to [i]. *)
