val cons_opt : 'a option -> 'a list -> 'a list
(** [cons_opt None xs] is [xs] and [cons_opt (Some x) xs] is [x :: xs]. *)

val take : 'a list -> int -> 'a list
(** [take l n] returns the first [n] elements of [l]. *)

val drop : 'a list -> int -> 'a list
(** [drop l n] returns [l] without its first [n] elements. *)

val sublist : 'a list -> int -> int -> 'a list
(** [sublist l pos len] returns the sublist of [l] of length [len] starting at
    the position [pos]. *)

val cmp : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val is_subset : 'a list -> 'a list -> bool
val elements_cmp : 'a list -> 'a list -> bool

val iter_first : (bool -> 'a -> unit) -> 'a list -> unit
(** [iter_first f l] iter over first element with [f true] and over others with
    [f false]. *)

val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
(** [compare cmp l1 l2] Comparison function for lists, using [cmp] to compare
    each elements *)

val find_map : ('a -> 'b option) -> 'a list -> 'b option
(** [find_map fn list] OCaml Stdlib's [List.find_map] (introduced in 4.10.0)
    backported into GeneWeb *)

val last : 'a list -> 'a
(** [last list] Return the last element of the list. Raises [Failure] if the
    list is empty. *)

val slice : int -> int -> 'a list -> 'a list
(** [slice from_ to_ list] Extracts elements from [a]-nth (starts with zero,
    inclusive) to [b]-nth (exclusive). If [list] is not long enough, result will
    be shorter than requested, but the function will not fail. *)

val replace : 'a -> 'a -> 'a list -> 'a list
(** [replace old_v new_v list] Return the same list as [list] were the first
    occurence of [old_v] has been replaced by [new_v]. If [old_v] is unbound,
    the list is returned unchanged. *)

val except : 'a -> 'a list -> 'a list
(** [except x list] Return a list containing all the elements from [list] except
    the first occurence of [x]. *)

val index : 'a -> 'a list -> int
(** [index element list] Finds the index of [element] in list. Raises
    [Not_found] if it does not exists. *)

val ref_append : 'a list ref -> 'a -> unit
(** [ref_append tl hd] Add [hd] at the beginning of [tl] ref. *)

val map_sort_uniq : ('a -> 'b) -> 'a list -> 'b list
(** [map_sort_uniq f l] apply [f] to every element and return sorted with Merge
    Sort algorithm list where every element is unique. *)

val rev_map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list
(** [rev_map_append f l1 l2] apply [f] to every element in [l1], reverse it and
    concat with [l2]. *)

val rev_iter : ('a -> unit) -> 'a list -> unit
(** [rev_iter fn list] is like [List.iter fn (List.rev list)]. Not
    tail-recursive. *)

val groupby :
  key:('a -> 'k) -> value:('a -> 'v) -> 'a list -> ('k * 'v list) list
(** [groupby ~key ~value list] Group the elements returning the same key
    together. Ordering of elements is unspecified. *)

module Infix : sig
  val ( @?: ) : 'a option -> 'a list -> 'a list
  (** [ x @?: xs] is [cons_opt x xs]. *)

  val ( @:: ) : 'a -> 'a list -> 'a list
  (** [x @:: xs] is [x :: xs], but can be combined with [( @?: )] without extra
      parentheses. *)
end
