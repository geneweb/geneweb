(* $Id: dag.mli,v 5.1 2006-09-15 11:45:37 ddr Exp $ *)

open Config;
open Dag2html;
open Def;
open Gwdb;

module Pset :
  sig
    type elt = iper and t = 'a;
    value empty : t;
    value add : elt -> t -> t;
  end
;

type item = [ Item of person and string ];

(**)
type sum 'a 'b = [ Left of 'a | Right of 'b ];
(**)

value image_txt : config -> base -> person -> string;

(**)
value make_tree_hts :
  config -> base ->
    (person -> item) -> (iper -> string) -> bool ->
    Pset.t -> list (iper * (iper * option ifam)) ->
    dag (sum iper 'a) -> html_table string string;
(**)

value print_slices_menu_or_dag_page :
  config -> base -> string -> html_table string string -> string -> unit;

value make_and_print_dag :
  config -> base -> (person -> item) -> (iper -> string) -> bool -> Pset.t ->
    list (iper * (iper * option ifam)) -> string -> string -> unit;
value print : config -> base -> unit;
