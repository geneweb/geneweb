(** This module implements flat sets, which arel sets represented by
    sorted arrays.

    The main goal of this implementation is to provide sets with an
    iterator interface that supports efficient seek operations. *)
module type S = sig
  type elt
  (** Type of elements of the collection. *)

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

  module Iterator : sig
    type t
    (** Type of an iterator. *)

    exception End
    (** Exception raised when attempting to access elements beyond the
        end of an iterator. *)

    val curr : t -> elt
    (** [curr it] returns the element currently pointed by the iterator [it].
        @raise End if the iterator [it] has reached the end of the
               collection. *)

    val next : t -> unit
    (** [next it] advances the iterator [it] to the next element. No effect
        if the iterator [it] has already reached the end of the collection. *)

    val seek : t -> elt -> unit
    (** [seek e] advances the iterator [it] to the smallest element in
        the collection that is greater or equal to [e]. If already positioned
        at this element, the iterator remains unchanged.

        For a sequence of calls [seek it ei] where [e1 <= ... <= en], the
        amortized complexity is expected to be O(1 + log(N/n)) where N is the
        cardinal of the collection. *)

    val union : t list -> t
    (** [union l] computes the union iterator of the iterators [l].
        The resulting iterator produces elements of the union of [l] in
        ascending order.

        @raise Invalid_argument if the list [l] is empty. *)

    val join : t list -> t
    (** [join l] computes the join iterator of the iterators [l]. The resulting
        iterator produces elements of the intersection of [l] in ascending
        order.

        @raise Invalid_argument if the list [l] is empty. *)

    val equal : t -> t -> bool
    (** [equal it1 it2] checks if the two iterators [it1] and [it2] are equal.
        This function consumes both [it1] and [it2]. *)

    val to_seq : t -> elt Seq.t
    (** [to_seq it] converts the iterator [it] into a sequence. Forcing the
        resulting sequence consumes [it]. *)
  end

  val iterator : t -> Iterator.t
  (** [iterator s] returns a iterator starting at the first element of [s]. *)
end

module type OrderedType = sig
  type t

  val dummy : t
  val compare : t -> t -> int
  val pp : t Fmt.t
end

module Make (O : OrderedType) : S with type elt = O.t
