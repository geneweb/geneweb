(* $Id: dag2html.mli,v 3.7 2001-01-30 14:17:12 ddr Exp $ *)

type dag 'a = { dag : mutable array (node 'a) }
and node 'a =
  { pare : mutable list idag; valu : 'a; chil : mutable list idag }
and idag = 'a
;

external int_of_idag : idag -> int = "%identity";
external idag_of_int : int -> idag = "%identity";

type align = [ LeftA | CenterA | RightA ];
type table_data = [ TDstring of string | TDhr of align ];
type html_table = array (array (int * align * table_data));

value html_table_of_dag :
  (node 'a -> string) -> (node 'a -> bool) -> bool -> bool -> dag 'a ->
     html_table;

    
