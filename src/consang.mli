(* $Id: consang.mli,v 2.5 1999-06-30 19:55:32 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;

type anc_stat = 'a;

type relationship =
  { weight1 : mutable float;
    weight2 : mutable float;
    relationship : mutable float;
    lens1 : mutable list (int * int);
    lens2 : mutable list (int * int);
    elim_ancestors : mutable bool;
    anc_stat1 : mutable anc_stat;
    anc_stat2 : mutable anc_stat;
    rank : int;
    mark : mutable int }
and relationship_table = { id : array int; info : array relationship }
;

exception TopologicalSortError;
value topological_sort : base -> array int;

value make_relationship_table : base -> array int -> relationship_table;

value relationship_and_links :
  base -> relationship_table -> bool -> iper -> iper -> (float * list int);
