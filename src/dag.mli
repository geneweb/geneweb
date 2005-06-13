(* $Id: dag.mli,v 4.4 2005-06-13 12:27:26 ddr Exp $ *)

open Config;
open Def;
open Dag2html;

module Pset :
  sig
    type elt = iper and t = 'a;
    value empty : t;
    value add : elt -> t -> t;
    value elements : t -> list elt;
  end
;

type sum 'a 'b = [ Left of 'a | Right of 'b ];

value make_dag : config -> base -> list iper -> dag (sum iper 'b);

value image_txt : config -> base -> person -> string;

value print_html_table : config -> html_table string -> unit;

value html_table_of_dag :
  (node 'a -> 'b) -> (node 'a -> 'b) -> (node 'a -> bool) -> bool -> bool ->
    dag 'a -> array (array (int * align * table_data 'b))
;

value make_tree_hts :
  config -> base ->
    (person -> string) -> (iper -> string) -> bool -> bool -> bool ->
    Pset.t -> list (iper * (iper * option ifam)) ->
    dag (sum  iper 'a) -> html_table string;

value gen_print_dag :
  config -> base -> bool -> bool -> Pset.t ->
    list (iper * (iper * option ifam)) ->
    dag (sum iper 'a) -> unit;
value print_dag :
  config -> base -> Pset.t -> list (iper * (iper * option ifam)) ->
    dag (sum iper 'a) -> unit;
value print : config -> base -> unit;

value print_slices_menu : config -> base -> option (html_table string) -> unit;
