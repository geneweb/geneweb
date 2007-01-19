(* $Id: consang.mli,v 5.5 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def;
open Gwdb;

type anc_stat = 'a;

type relationship =
  { weight1 : mutable float;
    weight2 : mutable float;
    relationship : mutable float;
    lens1 : mutable list (int * int * list iper);
    lens2 : mutable list (int * int * list iper);
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
value topological_sort : base -> (base -> iper -> person) -> array int;

value make_relationship_info : base -> array int -> relationship_info;

value relationship_and_links :
  base -> relationship_info -> bool -> iper -> iper -> (float * list int);

value check_noloop : base -> (error person -> unit) -> unit;
value check_noloop_for_person_list :
  base -> (error person -> unit) -> list iper -> unit
;
