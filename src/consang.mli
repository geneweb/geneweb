(* $Id: consang.mli,v 3.5 2000-11-23 18:40:10 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;

type anc_stat = 'a;

type relationship =
  { weight1 : mutable float;
    weight2 : mutable float;
    relationship : mutable float;
    lens1 : mutable list (int * int);
    lens2 : mutable list (int * int);
    inserted : mutable int;
    elim_ancestors : mutable bool;
    anc_stat1 : mutable anc_stat;
    anc_stat2 : mutable anc_stat }
;

type relationship_info =
  { tstab : array int;
    reltab : array relationship;
    queue : mutable array (list int) }
;

exception TopologicalSortError of person;
value topological_sort : base -> array int;

value make_relationship_info : base -> array int -> relationship_info;

value relationship_and_links :
  base -> relationship_info -> bool -> iper -> iper -> (float * list int);
