type ('a, 'c) t = private {
  curr : unit -> 'a;
  next : unit -> unit;
  seek : 'a -> unit;
}
(** Type of an iterator. *)

exception End
(** Exception raised when attempting to access elements beyond the
        end of an iterator. *)

module type Comparator = sig
  type t
  type wit

  val dummy : t
  val compare : t -> t -> int
end

type ('a, 'c) comparator =
  (module Comparator with type t = 'a and type wit = 'c)

val make :
  ('a, 'c) comparator ->
  curr:(unit -> 'a) ->
  next:(unit -> unit) ->
  seek:('a -> unit) ->
  ('a, 'c) t

val curr : ('a, 'c) t -> 'a
(** [curr it] returns the element currently pointed by the iterator [it].
    @raise End if the iterator [it] has reached the end of the
           collection. *)

val next : ('a, 'c) t -> unit
(** [next it] advances the iterator [it] to the next element. No effect
    if the iterator [it] has already reached the end of the collection. *)

val seek : ('a, 'c) t -> 'a -> unit
(** [seek e] advances the iterator [it] to the smallest element in
    the collection that is greater or equal to [e]. If already positioned
    at this element, the iterator remains unchanged.

    For a sequence of calls [seek it ei] where [e1 <= ... <= en], the
    amortized complexity is expected to be O(1 + log(N/n)) where N is the
    cardinal of the collection. *)

val union : ('a, 'c) comparator -> ('a, 'c) t list -> ('a, 'c) t
(** [union l] computes the union iterator of the iterators [l].
    The resulting iterator produces elements of the union of [l] in
    ascending order. *)

val join : ('a, 'c) comparator -> ('a, 'c) t list -> ('a, 'c) t
(** [join l] computes the join iterator of the iterators [l]. The resulting
    iterator produces elements of the intersection of [l] in ascending
    order.

    @raise Invalid_argument if the list [l] is empty. *)

val equal : ('a, 'c) comparator -> ('a, 'c) t -> ('a, 'c) t -> bool
(** [equal it1 it2] checks if the two iterators [it1] and [it2] are equal.
    This function consumes both [it1] and [it2]. *)

val to_seq : ('a, 'c) t -> 'a Seq.t
(** [to_seq it] converts the iterator [it] into a sequence. Forcing the
    resulting sequence consumes [it]. *)
