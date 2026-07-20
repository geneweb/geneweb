val to_list_map : ('a -> 'b) -> 'a array -> 'b list
(** [to_list_map fn a] is almost like [Array.to_list a |> List.map fn]
    but is more efficient.

    The list is constructed backward,
    so if [fn] have side effects it may not behave as excepted.
 *)

val except : 'a -> 'a array -> 'a array
(** [except value array]
    Return a new array containing all the elements
    from [array] except the first occurence of [value]
 *)
