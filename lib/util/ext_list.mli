val take : 'a list -> int -> 'a list
(** [take l n] returns the first [n] elements of [l].  *)

val drop : 'a list -> int -> 'a list
(** [drop l n] returns [l] without its first [n] elements.  *)

val sublist : 'a list -> int -> int -> 'a list
(** [sublist l pos len] returns the sublist of [l] of length [len]
    starting at the position [pos].  *)
