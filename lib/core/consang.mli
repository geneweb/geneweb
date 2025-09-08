(* Copyright (c) 1998-2007 INRIA *)

(* TODOOCP: doc *)

type anc_stat
(** Relation with ancestor status *)

type relationship = {
  mutable weight1 : float;
  mutable weight2 : float;
  mutable relationship : float;
  mutable lens1 : (int * int * Gwdb.iper list) list;
  mutable lens2 : (int * int * Gwdb.iper list) list;
  mutable inserted : int;
  mutable elim_ancestors : bool;
  mutable anc_stat1 : anc_stat;
  mutable anc_stat2 : anc_stat;
}
(** Consanguinity information attached to person (relationship between parents)
*)

type relationship_info = {
  (* Information about topological rank for each person *)
  tstab : (Gwdb.iper, int) Gwdb.Marker.t;
  reltab : (Gwdb.iper, relationship) Gwdb.Marker.t;
  mutable queue : Gwdb.iper list array;
}
(** Computation consanguinity state for every person in the base *)

exception TopologicalSortError of Gwdb.person
(** Error that could occure while topological sorting, and raised when person is
    ancestor of himself. *)

val topological_sort :
  Gwdb.base ->
  (Gwdb.base -> Gwdb.iper -> Gwdb.person) ->
  (Gwdb.iper, int) Gwdb.Marker.t
(** Returns result of topological sort of persons. Result is represented as
    marker that associates to every person in the base his topologic rank (let's
    suppose [r]). Global rule is : if person p1 is ancestor of p2 then r(p1) >
    r(p2). For example, all leaf persons (without children) have rank 0, their
    parents (if no another child that has child themself) - rank 1, parents of
    their parents - rank 2, etc. Raises [TopologicalSortError] if person is
    directly or undirectly is ancestor of himself (cycle). *)

val make_relationship_info :
  Gwdb.base -> (Gwdb.iper, int) Gwdb.Marker.t -> relationship_info
(** Initialise relationship info. *)

(* Returns relationship rate between two person and common ancestors (is exists). *)
val relationship_and_links :
  Gwdb.base ->
  relationship_info ->
  bool ->
  Gwdb.iper ->
  Gwdb.iper ->
  float * Gwdb.iper list

val check_noloop : Gwdb.base -> (Gwdb.person Def.error -> unit) -> unit
(** [check_noloop base onerror] scans database person's oriented graph (vertex
    is a person and edge is parenthood from child to parent). If cycle is found
    (person is directly or undirectly is ancestor of himself) calls [onerror]
    with [OwnAncestor] error. Array of ascendants should be load in the memory.
*)

val check_noloop_for_person_list :
  Gwdb.base -> (Gwdb.person Def.error -> unit) -> Gwdb.iper list -> unit
(** Same as [check_noloop] but scans only specified list of persons and their
    ancestors instead of entire database. *)
