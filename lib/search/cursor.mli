type ('k, 'v, 'c) t
(** Type of an cursor. *)

exception End
(** Exception raised when attempting to access elements beyond the end of an
    cursor. *)

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
(** [curr it] returns the element currently pointed by the cursor [it].
    @raise End if the cursor [it] has reached the end of the collection. *)

val next : ('k, 'v, 'c) t -> unit
(** [next it] advances the cursor [it] to the next element. No effect if the
    cursor [it] has already reached the end of the collection. *)

val seek : ('k, 'v, 'c) t -> 'k -> unit
(** [seek e] advances the cursor [it] to the smallest element in the collection
    that is greater or equal to [e]. If already positioned at this element, the
    cursor remains unchanged.

    For a sequence of calls [seek it ei] where [e1 <= ... <= en], the amortized
    complexity is expected to be O(1 + log(N/n)) where N is the cardinal of the
    collection. *)

val union : ('k, 'c) comparator -> ('k, 'v, 'c) t list -> ('k, 'v, 'c) t
(** [union l] computes the union cursor of the cursors [l]. The resulting cursor
    produces elements of the union of [l] in ascending order. *)

val join : ('k, 'c) comparator -> ('k, 'v, 'c) t list -> ('k, 'v, 'c) t
(** [join l] computes the join cursor of the cursors [l]. The resulting cursor
    produces elements of the intersection of [l] in ascending order.

    @raise Invalid_argument if the list [l] is empty. *)

val equal : ('k, 'c) comparator -> ('k, 'v, 'c) t -> ('k, 'v, 'c) t -> bool
(** [equal it1 it2] checks if the two cursors [it1] and [it2] are equal. This
    function consumes both [it1] and [it2]. *)

val to_seq : ('k, 'v, 'c) t -> ('k * 'v) Seq.t
(** [to_seq it] converts the cursor [it] into a sequence. Forcing the resulting
    sequence consumes [it]. *)
