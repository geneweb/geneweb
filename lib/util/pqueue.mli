(* Copyright (c) 1998-2007 INRIA *)

(** Module [Pqueue]: priority queues. *)

(** This module implements priority queues, given a total ordering function
    over the elements inserted. All operations are purely applicative
    (no side effects).
    The implementation uses binomial queues from Chris Okasaki.
    "add", "take" and "union" are in o(log n) in the worst case. *)

(** The input signature of the functor [Pqueue.Make].
    [t] is the type of the inserted elements.
    [leq] is a total ordering function over the elements.
    This is a two-argument function [f] returning [True] if the
    first argument is less or equal to the second one. *)
module type OrderedType = sig type t val leq : t -> t -> bool end

(** Output signature for priority queue *)
module type S = sig

  (** Type of elementes contained in priority queues. *)
  type elt

  (** Type of priority queues. *)
  type t

  (** The empty queue. *)
  val empty : t

  (** [is_empty q] checks the emptiness of [q]. *)
  val is_empty : t -> bool

  (** [add x h] adds a new element [x] in heap [h]. *)
  val add : elt -> t -> t

  (** [take x] removes the minimum element of [x] and returns it;
      raises [Not_found] when [x] is empty. *)
  val take : t -> elt * t

  (** [union q1 q2] returns heap constructed by union of [q1] [q2] *)
  val union : t -> t -> t

end

(** Functor that creates instance of priority queue from given element type. *)
module Make (Ord : OrderedType) : S with type elt = Ord.t
