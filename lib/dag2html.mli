(* $Id: dag2html.mli,v 5.0 2005-12-13 11:51:26 ddr Exp $ *)

type 'a dag = { mutable dag : 'a node array }
and 'a node =
  { mutable pare : idag list; valu : 'a; mutable chil : idag list }
and idag

external int_of_idag : idag -> int = "%identity"
external idag_of_int : int -> idag = "%identity"

type 'a table = { mutable table : 'a data array array }
and 'a data = { mutable elem : 'a elem; mutable span : span_id }
and 'a elem =
    Elem of 'a
  | Ghost of ghost_id
  | Nothing
and span_id
and ghost_id

type align = LeftA | CenterA | RightA
type ('a, 'b) table_data =
    TDitem of 'a
  | TDtext of string
  | TDhr of align
  | TDbar of 'b option
  | TDnothing
type ('a, 'b) html_table_line = (int * align * ('a, 'b) table_data) array
type ('a, 'b) html_table = ('a, 'b) html_table_line array

val html_table_struct :
  ('a node -> 'b) -> ('a node -> 'c) -> ('a node -> bool) -> 'a dag ->
    idag table -> (int * align * ('b, 'c) table_data) array array

val table_of_dag :
  ('a node -> bool) -> bool -> bool -> bool -> 'a dag -> idag table
