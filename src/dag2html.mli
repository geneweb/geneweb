(* $Id: dag2html.mli,v 3.1 1999-12-21 13:14:08 ddr Exp $ *)

type dag 'a = { dag : mutable array (node 'a) }
and node 'a =
  { pare : mutable list idag; valu : 'a; chil : mutable list idag }
and idag = 'a
;

external int_of_idag : idag -> int = "%identity";
external idag_of_int : int -> idag = "%identity";

type table 'a = 'b;

value print_html_table :
  (string -> unit) -> (node 'a -> unit) -> int -> dag 'a -> table idag ->
    unit;

value table_of_dag : bool -> dag 'a -> table idag;
value invert_dag : dag 'a -> dag 'a;
value invert_table : table 'a -> table 'a;
