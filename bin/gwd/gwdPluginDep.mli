type 'a sort_result = Sorted of 'a list | ErrorCycle of 'a list

val sort : ('a * 'a list) list -> 'a sort_result
(** Given a list of elements (in this case, plugins) and their dependencies,
    tries to compute a valid order `l` and return `Sorted l` . If there is a
    cycle, returns `ErrorCycle l'` where `l'` is a dependency cycle. Uses Kahn's
    algorithm for cycle detection. *)
