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
      After [f] returns, either with a value or by raising an exception, [ic] is
      guaranteed to be closed.

      @since 4.14 *)

  val with_open_text : string -> (t -> 'a) -> 'a
  (** Like {!with_open_bin}, but the channel is opened in text mode (see
      {!open_text}).

      @since 4.14 *)

  val with_open_gen : open_flag list -> int -> string -> (t -> 'a) -> 'a
  (** Like {!with_open_bin}, but can specify the opening mode and file
      permission, in case the file must be created (see {!open_gen}).

      @since 4.14 *)

  val input : t -> bytes -> int -> int -> int
  (** [input ic buf pos len] reads up to [len] characters from the given channel
      [ic], storing them in byte sequence [buf], starting at character number
      [pos]. It returns the actual number of characters read, between 0 and
      [len] (inclusive). A return value of 0 means that the end of file was
      reached.

      Use {!really_input} to read exactly [len] characters.

      @raise Invalid_argument
        if [pos] and [len] do not designate a valid range of [buf]. *)
end

module Out_channel : sig
  type t = out_channel
  (** The type of output channel. *)

  val with_open_bin : string -> (t -> 'a) -> 'a
  (** [with_open_bin fn f] opens a channel [oc] on file [fn] and returns [f oc].
      After [f] returns, either with a value or by raising an exception, [oc] is
      guaranteed to be closed.

      @since 4.14 *)

  val with_open_text : string -> (t -> 'a) -> 'a
  (** Like {!with_open_bin}, but the channel is opened in text mode (see
      {!open_text}).

      @since 4.14 *)

  val with_open_gen : open_flag list -> int -> string -> (t -> 'a) -> 'a
  (** Like {!with_open_bin}, but can specify the opening mode and file
      permission, in case the file must be created (see {!open_gen}).

      @since 4.14 *)

  val flush : t -> unit
  (** Flush the buffer associated with the given output channel, performing all
      pending writes on that channel. Interactive programs must be careful about
      flushing standard output and standard error at the right time. *)

  val output : t -> bytes -> int -> int -> unit
  (** [output oc buf pos len] writes [len] characters from byte sequence [buf],
      starting at offset [pos], to the given output channel [oc].

      @raise Invalid_argument
        if [pos] and [len] do not designate a valid range of [buf]. *)
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
