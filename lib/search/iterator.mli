type ('k, 'v, 'c) t
(** Type of an iterator. *)

exception End
(** Exception raised when attempting to access elements beyond the end of an
    iterator. *)

module type Comparator = sig
  type t
  type wit

  val dummy : t
  val compare : t -> t -> int
end

type ('k, 'c) comparator =
  (module Comparator with type t = 'k and type wit = 'c)

val make :
  ('k, 'c) comparator ->
  curr:(unit -> 'k * 'v) ->
  next:(unit -> unit) ->
  seek:('k -> unit) ->
  ('k, 'v, 'c) t

val curr : ('k, 'v, 'c) t -> 'k * 'v
(** [curr it] returns the element currently pointed by the iterator [it].
    @raise End if the iterator [it] has reached the end of the collection. *)

val next : ('k, 'v, 'c) t -> unit
(** [next it] advances the iterator [it] to the next element. No effect if the
    iterator [it] has already reached the end of the collection. *)

val seek : ('k, 'v, 'c) t -> 'k -> unit
(** [seek e] advances the iterator [it] to the smallest element in the
    collection that is greater or equal to [e]. If already positioned at this
    element, the iterator remains unchanged.

    For a sequence of calls [seek it ei] where [e1 <= ... <= en], the amortized
    complexity is expected to be O(1 + log(N/n)) where N is the cardinal of the
    collection. *)

val union : ('k, 'c) comparator -> ('k, 'v, 'c) t list -> ('k, 'v, 'c) t
(** [union l] computes the union iterator of the iterators [l]. The resulting
    iterator produces elements of the union of [l] in ascending order. *)

val join : ('k, 'c) comparator -> ('k, 'v, 'c) t list -> ('k, 'v, 'c) t
(** [join l] computes the join iterator of the iterators [l]. The resulting
    iterator produces elements of the intersection of [l] in ascending order.

    @raise Invalid_argument if the list [l] is empty. *)

val equal : ('k, 'c) comparator -> ('k, 'v, 'c) t -> ('k, 'v, 'c) t -> bool
(** [equal it1 it2] checks if the two iterators [it1] and [it2] are equal. This
    function consumes both [it1] and [it2]. *)

val to_seq : ('k, 'v, 'c) t -> ('k * 'v) Seq.t
(** [to_seq it] converts the iterator [it] into a sequence. Forcing the
    resulting sequence consumes [it]. *)
