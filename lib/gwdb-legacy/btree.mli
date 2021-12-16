
(** Input signature of the functor [Btree.Make]. *)
module type OrderedType = sig type t val compare : t -> t -> int end

(** Output signature of the functor [Btree.Make]. *)
module type S =
  sig
    
    (** Same as {!Stdlib.Map.S.key} *)
    type key

    (** Same as {!Stdlib.Map.S.t} *)
    type +'a t

    (** Same as {!Stdlib.Map.S.mem} *)
    val mem : key -> 'a t -> bool

    (** Same as {!Stdlib.Map.S.add} *)
    val add : key -> 'a -> 'a t -> 'a t

    (** Same as {!Stdlib.Map.S.find} *)
    val find : key -> 'a t -> 'a

    (** [key_after f_compare m] browse map [m] to find the key [k] which
        gives [f_compare k = 0]. Raise [Not_found] if such key doesn't exists. *)
    val key_after : (key -> int) -> 'a t -> key

    (** [next k bt] returns the smallest key that is bigger then [k] inside [bt]. *)
    val next : key -> 'a t -> key
  end

(** Functor building an implementation of the map structure given a 
    totally ordered type. *)
module Make : functor (Ord : OrderedType) -> S with type key = Ord.t