type ('k, 'v, 'c) t
(** Type of a cursor. *)

exception End
(** Exception raised when attempting to access elements beyond the end of an
    cursor. *)

val make :
  ('k, 'c) Comparator.t ->
  curr:(unit -> 'k * 'v) ->
  next:(unit -> unit) ->
  seek:('k -> unit) ->
  ('k, 'v, 'c) t

val curr : ('k, 'v, 'c) t -> 'k * 'v
(** [curr c] returns the element currently pointed by the cursor [c].
    @raise End if the cursor [c] has reached the end of the collection. *)

val next : ('k, 'v, 'c) t -> unit
(** [next c] advances the cursor [c] to the next element. No effect if the
    cursor [c] has already reached the end of the collection. *)

val seek : ('k, 'v, 'c) t -> 'k -> unit
(** [seek e] advances the cursor [c] to the smallest element in the collection
    that is greater or equal to [e]. If already positioned at this element, the
    cursor remains unchanged.

    For a sequence of calls [seek c ei] where [e1 <= ... <= en], the amortized
    complexity is expected to be O(1 + log(N/n)) where N is the cardinal of the
    collection. *)

val union : ('k, 'c) Comparator.t -> ('k, 'v, 'c) t list -> ('k, 'v, 'c) t
(** [union l] computes the union cursor of the cursors [l]. The resulting cursor
    produces elements of the union of [l] in ascending order. *)

val join : ('k, 'c) Comparator.t -> ('k, 'v, 'c) t list -> ('k, 'v, 'c) t
(** [join l] computes the join cursor of the cursors [l]. The resulting cursor
    produces elements of the intersection of [l] in ascending order.

    @raise Invalid_argument if the list [l] is empty. *)

val equal : ('k, 'c) Comparator.t -> ('k, 'v, 'c) t -> ('k, 'v, 'c) t -> bool
(** [equal c1 c2] checks if the two cursors [c1] and [c2] are equal. This
    function consumes both [c1] and [c2]. *)

val to_seq : ('k, 'v, 'c) t -> ('k * 'v) Seq.t
(** [to_seq c] converts the cursor [c] into a sequence. Forcing the resulting
    sequence consumes [c]. *)
