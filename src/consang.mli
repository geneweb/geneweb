(* $Id: consang.mli,v 1.2 1999-02-02 10:24:03 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;

type relationship =
  { weight1 : mutable float;
    weight2 : mutable float;
    relationship : mutable float;
    lens1 : mutable list (int * int);
    lens2 : mutable list (int * int);
    elim_ancestors : mutable bool;
    mark : mutable int }
and relationship_table = { id : array int; info : array relationship }
;

exception TopologicalSortError;
value topological_sort : base -> array int;
value tsort_leq : array int -> int -> int -> bool;

value make_relationship_table : base -> array int -> relationship_table;

value relationship_and_links :
  base -> relationship_table -> bool -> iper -> iper -> (float * list int);

value compute_all_consang : base -> bool -> unit;
