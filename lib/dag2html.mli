(* $Id: dag2html.mli,v 5.0 2005-12-13 11:51:26 ddr Exp $ *)

(* TODOCP *)
type 'a dag = { mutable dag : 'a node array }
and 'a node = { mutable pare : idag list; valu : 'a; mutable chil : idag list }
and idag

external int_of_idag : idag -> int = "%identity"
external idag_of_int : int -> idag = "%identity"

type 'a table = { mutable table : 'a data array array }
and 'a data = { mutable elem : 'a elem; mutable span : span_id }
and 'a elem = Elem of 'a | Ghost of ghost_id | Nothing
and span_id
and ghost_id

type align = LeftA | CenterA | RightA

type 'a table_data =
  | TDitem of Geneweb_db.Driver.iper * 'a * Adef.safe_string
  | TDtext of Geneweb_db.Driver.iper * Adef.safe_string
  | TDhr of align
  | TDbar of Adef.escaped_string option
  | TDnothing

type 'a html_table_line = (int * align * 'a table_data) array
type 'a html_table = 'a html_table_line array

val html_table_struct :
  ('a node -> Geneweb_db.Driver.iper) ->
  ('a node -> 'b) ->
  ('a node -> Adef.escaped_string) ->
  ('a node -> bool) ->
  'a dag ->
  idag table ->
  (int * align * 'b table_data) array array

val table_of_dag :
  ('a node -> bool) -> bool -> bool -> bool -> 'a dag -> idag table
