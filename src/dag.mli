(* $Id: dag.mli,v 3.1 2001-01-06 09:14:16 ddr Exp $ *)

open Config;
open Def;

module Pset :
  sig
    type elt = iper and t = 'a;
    value empty : t;
    value add : elt -> t -> t;
    value elements : t -> list elt;
  end
;

type sum 'a 'b = [ Left of 'a | Right of 'b ];

value make_dag : config -> base -> list iper -> Dag2html.dag (sum iper 'b);

value image_txt : config -> base -> person -> string;

value print_only_dag :
  config -> base ->
    (config -> base -> person -> string) -> bool -> bool ->
    Pset.t -> list (iper * (iper * option ifam)) ->
    Dag2html.dag (sum iper 'a) -> unit;
value gen_print_dag :
  config -> base -> bool -> bool -> Pset.t ->
    list (iper * (iper * option ifam)) ->
    Dag2html.dag (sum iper 'a) -> unit;
value print_dag :
  config -> base -> Pset.t -> list (iper * (iper * option ifam)) ->
    Dag2html.dag (sum iper 'a) -> unit;
value print : config -> base -> unit;
