(* Copyright (c) 1998-2007 INRIA *)

(* TODO: Def.iper -> Gwdb.iper *)

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
  { tstab : (Def.iper, int) Gwdb.Marker.t
  ; reltab : (Def.iper, relationship) Gwdb.Marker.t
  ; mutable queue : Def.iper list array
  }

exception TopologicalSortError of person

val topological_sort
  : Gwdb.base
  -> (Gwdb.base -> Def.iper -> Gwdb.person)
  -> (Def.iper, int) Gwdb.Marker.t

val make_relationship_info : base -> (Def.iper, int) Gwdb.Marker.t -> relationship_info

val relationship_and_links :
  base -> relationship_info -> bool -> iper -> iper -> float * Def.iper list

val check_noloop : base -> (person error -> unit) -> unit
val check_noloop_for_person_list :
  base -> (person error -> unit) -> iper list -> unit
