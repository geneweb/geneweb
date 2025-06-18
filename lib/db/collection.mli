type 'a t
(** A collection is a set of elements you want to traverse. *)

val make : len:int -> (int -> 'a option) -> 'a t
(** [make ~len get] creates a collection for the accessor [get]. *)

val empty : unit -> 'a t
(** [empty ()] creates an empty collection. *)

val length : 'a t -> int
(** Return the number of elements of a colletion *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map fn c] returns a collection corresponding to [c] where [fn] would have
    been applied to each of its elements. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter fn c] applies [fn] would have been applied to each elements of [c]. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iter fn c] applies [fn i] would have been applied to each elements of [c]
    where [i] is the index (starting with 0) of the element. *)

val fold : ?from:int -> ?until:int -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold fn acc c] Combine each element of [c] into a single value using [fn].
    [fn] first argument is the result computed so far as we traverse the
    collection, and second element is the current element being combined. [acc]
    is the starting combined value. Start at [from]-nth and finish with
    [until]-nth element (included). *)

val fold_until : ('a -> bool) -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Same as [fold fn acc c], but computation stops as soon as [continue] is not
    satisfied by combined value anymore. *)

val iterator : 'a t -> unit -> 'a option
(** [iterator c] returns a function returning [Some next_element] when it is
    called, or [None] if you reached the end of the collection. *)

type 'a collection = 'a t

(** Markers for elements inside a collection. *)
module Marker : sig
  type ('k, 'v) t
  (** Markers are way to annotate (add extra information to) elements of a
      {!val:t}. *)

  val make : ('k -> int) -> 'k collection -> 'a -> ('k, 'a) t

  val dummy : 'a -> 'b -> ('a, 'b) t
  (** [dummy k v] create a dummy collection with no element. [k] and [v] are
      only used for typing. Useful for placeholders or for typing purpose. *)

  val get : ('k, 'v) t -> 'k -> 'v
  (** [get marker key] Return the annotation associated to [key]. *)

  val set : ('k, 'v) t -> 'k -> 'v -> unit
  (** [set marker key value] Set [value] as annotation associated to [key]. *)
end
