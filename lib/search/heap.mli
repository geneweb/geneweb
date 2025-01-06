(** This module implements a simple imperative priority heap. *)

module type S = sig
  type elt
  (** Type of the element in the heap. *)

  type t
  (** Type of the heap. *)

  exception Empty
  (** Exception raised if we attempt to delete the minimal element of an empty
      heap. *)

  val create : int -> t
  (** [create sz] makes a heap of size [sz]. *)

  val insert : t -> elt -> unit
  (** [insert t p v] inserts the element [v] in the heap [t].

      @raise Invalid_argument if the heap is full. *)

  val delete_min : t -> elt
  (** [delete_min t] deletes and returns an element of the heap of minimal
      priority.

      Notice that the structure does not garantee the stability of elements of
      the same priority.

      @raise Empty if the heap is empty. *)

  val min : t -> elt
  (** [min t] returns a minimum element of the heap [t].

      @raise Empty if the heap is empty. *)
end

module type OrderedType = sig
  type t

  val dummy : t
  val compare : t -> t -> int
end

module Make (O : OrderedType) : S with type elt = O.t
