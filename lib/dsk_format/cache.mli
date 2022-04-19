
(* type of caches *)
type ('a, 'b) t

(** [create n] initializes an empty cache, n is the initial size of the cache.
**)
val create : ?random:bool -> int -> ('a, 'b) t

(** [cached ~cache ~f] returns a memoized version of [f] using [cache].
**)
val cached : cache:('a, 'b) t -> f:('a -> 'b) -> 'a -> 'b

(** [invalidate ~cache ~key] returns a cache with a removed entry associated with [key] in [cache]
 **)
val invalidate : cache:('a, 'b) t -> key:'a -> ('a, 'b) t

(** [add ~cache ~key ~value] returns a cache with added pair key value.
 **)  
val add : cache:('a, 'b) t -> key:'a -> value:'b -> ('a, 'b) t

val find_opt : cache:('a, 'b) t -> key:'a -> 'b option
