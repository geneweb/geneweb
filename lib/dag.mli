(* TODOCP *)
module Pset : sig
  type t = Gwdb.iper list
  type elt = Gwdb.iper

  val add : 'a -> 'a list -> 'a list
  val empty : 'a list
  val mem : 'a -> 'a list -> bool
end

val get_dag_elems : Config.config -> Gwdb.base -> Gwdb.iper list

val make_dag :
  Config.config ->
  Gwdb.base ->
  Gwdb.iper list ->
  (Gwdb.iper, int) Def.choice Dag2html.dag
