(* $Id: dag2html.mli,v 3.4 2001-01-05 23:25:43 ddr Exp $ *)

type dag 'a = { dag : mutable array (node 'a) }
and node 'a =
  { pare : mutable list idag; valu : 'a; chil : mutable list idag }
and idag = 'a
;

external int_of_idag : idag -> int = "%identity";
external idag_of_int : int -> idag = "%identity";

type table 'a = 'b;

value table_of_dag : bool -> bool -> dag 'a -> table idag;

type align = [ LeftA | CenterA | RightA ];
type table_data = [ TDstring of string | TDhr of align ];

value print_html_table :
  (string -> unit) -> (node 'a -> unit) -> (node 'a -> bool) -> int
    -> dag 'a -> table idag -> unit;
value html_table_struct :
  (node 'a -> string) -> (node 'a -> bool) -> dag 'a -> table idag ->
    array (array (int * align * table_data));
