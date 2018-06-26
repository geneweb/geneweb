(* $Id: consang.mli,v 5.5 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb

type anc_stat

type relationship =
  { mutable weight1 : float;
    mutable weight2 : float;
    mutable relationship : float;
    mutable lens1 : (int * int * iper list) list;
    mutable lens2 : (int * int * iper list) list;
    mutable inserted : int;
    mutable elim_ancestors : bool;
    mutable anc_stat1 : anc_stat;
    mutable anc_stat2 : anc_stat }

type relationship_info =
  { tstab : int array;
    reltab : relationship array;
    mutable queue : int list array }

exception TopologicalSortError of person
val topological_sort : base -> (base -> iper -> person) -> int array

val make_relationship_info : base -> int array -> relationship_info

val relationship_and_links :
  base -> relationship_info -> bool -> iper -> iper -> float * int list

val check_noloop : base -> (person error -> unit) -> unit
val check_noloop_for_person_list :
  base -> (person error -> unit) -> iper list -> unit
