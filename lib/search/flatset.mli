(** This module implements flat sets, which arel sets represented by
    sorted arrays.

    The main goal of this implementation is to provide sets with an
    iterator interface that supports efficient seek operations. *)
module type S = sig
  type elt
  (** Type of elements of the collection. *)

  type cmp

  module Comparator : Iterator.Comparator with type t = elt and type wit = cmp

  type t
  (** Type of flat set. *)

  val of_seq : elt Seq.t -> t
  (** Build an flat set from a sequence of elements. The sequence is
      entirely forced while building the set. *)

  val to_seq : t -> elt Seq.t
  (** [to_seq t] returns the sequence of elements of [t] in ascending
      ordering. *)

  val mem : elt -> t -> bool
  (** [mem e t] checks if the element [e] is present in the set [t]. *)

  val cardinal : t -> int
  (** [cardinal t] returns the cardinal of the set. *)

  val iterator : t -> (elt, cmp) Iterator.t
  (** [iterator s] returns a iterator starting at the first element of [s]. *)
end

module type OrderedType = sig
  type t

  val dummy : t
  val compare : t -> t -> int
  val pp : t Fmt.t
end

module Make (O : OrderedType) : S with type elt = O.t
