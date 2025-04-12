(* TODOCP *)
module Pset : sig
  type t = Geneweb_db.Driver.iper list
  type elt = Geneweb_db.Driver.iper

  val add : 'a -> 'a list -> 'a list
  val empty : 'a list
  val elements : 'a list -> 'a list
  val mem : 'a -> 'a list -> bool
end

val get_dag_elems :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper list

type ('a, 'b) sum = ('a, 'b) Def.choice

val make_dag :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper list ->
  (Geneweb_db.Driver.iper, int) Def.choice Dag2html.dag
