type 'a result = Sorted of 'a list | Cycle of 'a list

val topological_sort : ('a * 'a list) list -> 'a result
(** [topological_sort l] attempts to sort the nodes of the graph represented by
    the adjacency list [l] into a topological order.

    The input [l] is an association list where each key is a node and its value
    is the list of nodes reachable via an outgoing edge.

    If successful, the function returns [Sorted r] where [r] is a topologically
    sorted list of all nodes. Otherwise, it returns [Cycle c] where [c]
    represents a cycle path in the graph. *)
