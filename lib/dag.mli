(** DAG construction from URL parameters for genealogical graph display.

    This module builds directed acyclic graphs (DAGs) from URL parameters
    specifying persons and their Sosa numbers. The resulting DAG can then be
    rendered as an HTML table via {!Dag2html} and {!DagDisplay}. *)

module Iperset : Set.S with type elt = Geneweb_db.Driver.iper
module Ipermap : Map.S with type key = Geneweb_db.Driver.iper

val get_dag_elems :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper list
(** [get_dag_elems conf base] extracts person identifiers from URL parameters.

    Reads indexed parameter pairs from the URL:
    - [i1], [i2], ... or [p1/n1/oc1], [p2/n2/oc2], ... : person identifiers
    - [s1], [s2], ... : Sosa numbers relative to each person

    For each (person, sosa) pair, collects all ancestors along the Sosa branch.
    Returns the union of all collected persons without duplicates. *)

type ('a, 'b) sum = ('a, 'b) Def.choice

val make_dag :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper list ->
  (Geneweb_db.Driver.iper, int) Def.choice Dag2html.dag
(** [make_dag conf base ipers] builds a DAG from a list of person identifiers.

    The DAG structure:
    - Each person in [ipers] becomes a node with [valu = Left iper]
    - Parent/child links are restricted to persons within [ipers]
    - Couples without common children in [ipers] get a synthetic "spouse node"
      with [valu = Right n] to maintain graph connectivity

    Uses {!Ipermap} internally for efficient iper-to-idag lookup. *)
