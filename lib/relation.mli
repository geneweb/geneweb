(* Copyright (c) 1998-2007 INRIA *)

open Config

(** {1 Path encoding} *)

type famlink =
  | Self
  | Parent
  | Sibling
  | HalfSibling
  | Mate
  | Child
      (** Edge label on the shortest path returned by
          {!get_shortest_path_relation}. Each step of the path is annotated with
          how the current vertex relates to the previous one. *)

type anc_f = {
  p : Geneweb_db.Driver.iper;
  f : Geneweb_db.Driver.ifam list;
  c : int;
}
(** Ancestor cell of a {!path_f}:
    - [p] : iper of the common ancestor;
    - [f] : family chain through which the ancestor is reached, innermost family
      first;
    - [c] : path multiplicity through this ancestor. *)

type path_f = { l1 : int; l2 : int; anc : anc_f list }
(** One relationship path between two persons:
    - [l1] : ascending degree from person 1 up to the common ancestor;
    - [l2] : ascending degree from person 2 up to the common ancestor (or
      descending degree from the common ancestor when person 2 sits below person
      1);
    - [anc] : common ancestors reached at this [(l1, l2)] cell. *)

(** {1 Shortest-path BFS} *)

val get_shortest_path_relation :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  Geneweb_db.Driver.iper ->
  Geneweb_db.Driver.ifam list ->
  ((Geneweb_db.Driver.iper * famlink) list * Geneweb_db.Driver.ifam) option
(** [get_shortest_path_relation conf base ip1 ip2 excl] runs a bidirectional
    breadth-first search across parent / mate / child / sibling edges and
    returns the shortest annotated path between [ip1] and [ip2], paired with the
    ifam at which the two halves of the BFS met. Families listed in [excl] are
    skipped, which lets callers enumerate alternative paths through the [efN]
    URL parameters. Returns [None] when the two persons are unrelated. *)

(** {1 Path inspection} *)

val get_piece_of_branch :
  config ->
  Geneweb_db.Driver.base ->
  ((( Geneweb_db.Driver.iper,
      Consang.relationship )
    Geneweb_db.Collection.Marker.t
   * (Geneweb_db.Driver.person * int) list)
  * int)
  * (Consang.relationship -> (int * int * Geneweb_db.Driver.iper list) list) ->
  int * int ->
  Geneweb_db.Driver.iper list
(** [get_piece_of_branch conf base (((reltab, list), x), proj) (len1, len2)]
    walks the [proj] sub-branches of the ancestor [List.hd list] (whose Consang
    relationship is in [reltab]) at degree [x], collecting the iper of every
    person strictly above degree [len2] and at most [len1]. Used by
    {!Geneweb.RelationDisplay} to pick witness persons that disambiguate
    gendered kinship labels (e.g. choosing between "uncle" and "aunt"). The
    nested-tuple shape mirrors the call site verbatim and is intended to be
    flattened in a future refactor. *)

(** {1 Relationship computation} *)

val compute_simple_relationship :
  config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, int) Geneweb_db.Collection.Marker.t ->
  Geneweb_db.Driver.iper ->
  Geneweb_db.Driver.iper ->
  (path_f list
  * Geneweb_sosa.t
  * float
  * (Geneweb_db.Driver.iper * Geneweb_db.Driver.ifam) list)
  option
(** [compute_simple_relationship conf base tstab ip1 ip2] computes the
    relationship between [ip1] and [ip2] using the topological sort [tstab]
    (typically built via {!Util.create_topological_sort}). Returns:
    - [None] when no relationship exists;
    - [Some (paths, total, coefficient, anc_ifams)] otherwise, where [paths]
      groups {!path_f} by [(l1, l2)] cell, [total] is the cumulated path
      multiplicity as a Sosa number, [coefficient] is the consanguinity
      coefficient, and [anc_ifams] is the raw [(ancestor_iper, ifam)] list from
      {!Consang.relationship_and_links}.

    Used by {!Geneweb.RelationMatrix} which works directly on cells.
    Higher-level callers should prefer {!compute_relationship} which also
    handles by-marriage relations and merges branches.
    @raise Consang.TopologicalSortError
      when the database contains a self-ancestor cycle. *)

val compute_relationship :
  config ->
  Geneweb_db.Driver.base ->
  bool ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.person ->
  ((Geneweb_db.Driver.person option
   * Geneweb_db.Driver.person option
   * (int * int * (Geneweb_db.Driver.person * int) list)
   * ( Geneweb_db.Driver.iper,
       Consang.relationship )
     Geneweb_db.Collection.Marker.t)
   list
  * Geneweb_sosa.t
  * float)
  option
(** [compute_relationship conf base by_marr p1 p2] computes every shortest
    relationship between [p1] and [p2], optionally including by-marriage
    relations when [by_marr] is true. Returns:
    - [None] when [p1 = p2] or no relationship exists;
    - [Some (solutions, total, coefficient)] otherwise, where [solutions] is a
      list of [(spouse_of_p1, spouse_of_p2, (l1, l2, ancestors), reltab)] tuples
      sorted by total degree [l1 + l2] then by [l1]; [total] is the cumulated
      path multiplicity as a Sosa number; [coefficient] is the consanguinity
      coefficient between the original pair (zero when the relationship is
      purely by-marriage).

    Each [reltab] is the Consang marker that backs the corresponding solution;
    consumers pass it to {!get_piece_of_branch} to disambiguate kinship labels.
    @raise Consang.TopologicalSortError
      when the database contains a self-ancestor cycle. *)
