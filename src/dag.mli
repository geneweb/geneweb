(* $Id: dag.mli,v 5.1 2006-09-15 11:45:37 ddr Exp $ *)

open Config
open Dag2html
open Def
open Gwdb

module Pset :
  sig
    type elt = iper
    and t
    val empty : t
    val add : elt -> t -> t
  end

type item =
    Item of person * string

(**)
type ('a, 'b) sum =
    Left of 'a
  | Right of 'b
(**)

val image_txt : config -> base -> person -> string

(**)
val make_tree_hts :
  config -> base -> (person -> item) -> (iper -> string) -> bool -> Pset.t ->
    (iper * (iper * ifam option)) list -> (iper, 'a) sum dag ->
    (string, string) html_table
(**)

val print_slices_menu_or_dag_page :
  config -> base -> string -> (string, string) html_table -> string -> unit

val make_and_print_dag :
  config -> base -> (person -> item) -> (iper -> string) -> bool -> Pset.t ->
    (iper * (iper * ifam option)) list -> string -> string -> unit
val print : config -> base -> unit
