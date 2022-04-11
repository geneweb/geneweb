
(** Collections of elemetns *)
module Collection : sig

  (** Collections are sets of elements you want to traverse. *)
  type 'a t =
    { length : int
    ; get : int -> 'a option
    }

  (** Return the number of elements of a colletion *)
  val length : 'a t -> int

  (** [map fn c]
      Return a collection corresponding to [c]
      where [fn] would have been applied to each of its elements.
   *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** [iter fn c]
      Apply [fn] would have been applied to each elements of [c].
   *)
  val iter : ('a -> unit) -> 'a t -> unit

  (** [iter fn c]
      Apply [fn i] would have been applied to each elements of [c]
      where [i] is the index (starting with 0) of the element.
   *)
  val iteri : (int -> 'a -> unit) -> 'a t -> unit

  (** [fold fn acc c]
      Combine each element of [c] into a single value using [fn].
      [fn] first argument is the result computed so far as we traverse the
      collection, and second element is the current element being combined.
      [acc] is the starting combined value.
      Start at [from]-nth and finish with [until]-nth element (included).
   *)
  val fold : ?from:int -> ?until:int -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  (** [fold_until continue fn acc c]
      Same as [fold fn acc c], but computation stops as soon as [continue]
      is not satisfied by combined value anymore.
   *)
  val fold_until : ('a -> bool) -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  (** [iterator c]
      Return a function returning [Some next_element] when it is called,
      or [None] if you reached the end of the collection.
   *)
  val iterator : 'a t -> (unit -> 'a option)

end

(** Markers for elements inside [Collection.t] *)
module Marker : sig

  (** Markers are way to annotate (add extra information to) elements of a {!val:Collection.t}. *)
  type ('k, 'v) t =
    { get : 'k -> 'v
    ; set : 'k -> 'v -> unit
    }

  (** [get marker key]
      Return the annotation associated to [key].
   *)
  val get : ('k, 'v) t -> 'k -> 'v

  (** [set marker key value]
      Set [value] as annotation associated to [key].
   *)
  val set : ('k, 'v) t -> 'k -> 'v -> unit

  val make : ('a -> int) -> 'a Collection.t -> 'v -> ('a, 'v) t
end

val dummy_collection : 'a -> 'a Collection.t
val dummy_marker : 'a -> 'b -> ('a, 'b) Marker.t
