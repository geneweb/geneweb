(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Out_channel : sig
  type t = out_channel
  (** The type of output channel. *)

  val isatty : t -> bool
  (** [isatty oc] is [true] if [oc] refers to a terminal or console window,
      [false] otherwise.

      @since 5.1 *)
end

module List : sig
  val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
  (** [equal eq [a1; ...; an] [b1; ..; bm]] holds when the two input lists have
      the same length, and for each pair of elements [ai], [bi] at the same
      position we have [eq ai bi].

      Note: the [eq] function may be called even if the lists have different
      length. If you know your equality function is costly, you may want to
      check {!compare_lengths} first.

      @since OCaml 4.12 *)
end

module Seq : sig
  val empty : 'a Seq.t
  (** [empty] is the empty sequence. It has no elements. Its length is 0. *)

  val take : int -> 'a Seq.t -> 'a Seq.t
  (** [take n xs] is the sequence of the first [n] elements of [xs].

      If [xs] has fewer than [n] elements, then [take n xs] is equivalent to
      [xs].

      [n] must be nonnegative.

      @raise Invalid_argument if [n] is negative.

      @since 4.14 *)

  val concat : 'a Seq.t Seq.t -> 'a Seq.t
  (** If [xss] is a sequence of sequences, then [concat xss] is its
      concatenation.

      If [xss] is the sequence [xs0; xs1; ...] then [concat xss] is the sequence
      [xs0 @ xs1 @ ...].

      @since 4.13 *)

  val equal : ('a -> 'b -> bool) -> 'a Seq.t -> 'b Seq.t -> bool
  (** Provided the function [eq] defines an equality on elements,
      [equal eq xs ys] determines whether the sequences [xs] and [ys] are
      pointwise equal.

      May not terminate if both of the sequences [xs] and [ys] are infinite.

      @since 4.14 *)
end

module String : sig
  val starts_with : prefix:string -> string -> bool
  (** [starts_with ][~prefix s] is [true] if and only if [s] starts with
      [prefix].

      @since 4.13 *)
end

module Stream : sig
  type 'a t
  (** A single-element look-ahead stream. Minimal replacement for the [Stream]
      module that used to come from [camlp-streams]. *)

  exception Failure
  (** Raised by {!next} on an empty stream, and used by hand-written parsers to
      signal a (backtrackable) parse failure. *)

  exception Error of string
  (** Used by hand-written parsers to signal a committed parse error. *)

  val of_string : string -> char t
  (** [of_string s] is a stream of the characters of [s]. *)

  val of_channel : in_channel -> char t
  (** [of_channel ic] is a stream of the characters read from [ic]. *)

  val from : (int -> 'a option) -> 'a t
  (** [from f] is the stream built by calling [f] with the successive element
      indices (0, 1, 2, ...); [f] returns [None] to signal the end. *)

  val peek : 'a t -> 'a option
  (** [peek t] returns the next element without consuming it, or [None]. *)

  val junk : 'a t -> unit
  (** [junk t] discards the next element (no-op on an empty stream). *)

  val next : 'a t -> 'a
  (** [next t] returns and consumes the next element.
      @raise Failure if the stream is empty. *)

  val count : 'a t -> int
  (** [count t] is the number of elements consumed so far. *)
end
