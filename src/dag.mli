(* $Id: dag.mli,v 4.7 2005-08-10 19:01:57 ddr Exp $ *)

open Config;
open Def;
open Dag2html;

module Pset :
  sig
    type elt = iper and t = 'a;
    value empty : t;
    value add : elt -> t -> t;
  end
;

type sum 'a 'b = [ Left of 'a | Right of 'b ];

value make_dag : config -> base -> Pset.t -> dag (sum iper 'b);

value image_txt : config -> base -> person -> string;

value html_table_of_dag :
  (node (sum 'a 'b) -> 'c) -> (node (sum 'a 'b) -> 'c) -> bool -> bool ->
    dag (sum 'a 'b) -> array (array (int * align * table_data 'c))
;

value make_tree_hts :
  config -> base ->
    (person -> string) -> (iper -> string) -> bool ->
    Pset.t -> list (iper * (iper * option ifam)) ->
    dag (sum iper 'a) -> html_table string;

value print_dag_page :
  config -> bool -> string -> html_table string -> (unit -> unit) -> unit;

value print_dag :
  config -> base -> bool -> Pset.t ->
    list (iper * (iper * option ifam)) -> unit;
value print : config -> base -> unit;
