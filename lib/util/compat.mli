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

module In_channel : sig
  type t = in_channel
  (** The type of input channel. *)

  val with_open_bin : string -> (t -> 'a) -> 'a
  (** [with_open_bin fn f] opens a channel [ic] on file [fn] and returns [f ic].
      After [f] returns, either with a value or by raising an exception, [ic]
      is guaranteed to be closed.

      @since 4.14 *)

  val with_open_text : string -> (t -> 'a) -> 'a
  (** Like {!with_open_bin}, but the channel is opened in text mode (see
      {!open_text}).

      @since 4.14 *)

  val with_open_gen : open_flag list -> int -> string -> (t -> 'a) -> 'a
  (** Like {!with_open_bin}, but can specify the opening mode and file
      permission, in case the file must be created (see {!open_gen}).

      @since 4.14 *)
end

module Out_channel : sig
  type t = out_channel
  (** The type of output channel. *)

  val with_open_bin : string -> (t -> 'a) -> 'a
  (** [with_open_bin fn f] opens a channel [oc] on file [fn] and returns [f oc].
      After [f] returns, either with a value or by raising an exception, [oc]
      is guaranteed to be closed.

      @since 4.14 *)

  val with_open_text : string -> (t -> 'a) -> 'a
  (** Like {!with_open_bin}, but the channel is opened in text mode (see
      {!open_text}).

      @since 4.14 *)

  val with_open_gen : open_flag list -> int -> string -> (t -> 'a) -> 'a
  (** Like {!with_open_bin}, but can specify the opening mode and file
      permission, in case the file must be created (see {!open_gen}).

      @since 4.14 *)
end

module Seq : sig
  val take : int -> 'a Seq.t -> 'a Seq.t
  (** [take n xs] is the sequence of the first [n] elements of [xs].

      If [xs] has fewer than [n] elements,
      then [take n xs] is equivalent to [xs].

      [n] must be nonnegative.

      @raise Invalid_argument if [n] is negative.

      @since 4.14 *)
end
