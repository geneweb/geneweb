(* $Id: dag2html.mli,v 3.3 2000-01-01 07:55:40 ddr Exp $ *)

type dag 'a = { dag : mutable array (node 'a) }
and node 'a =
  { pare : mutable list idag; valu : 'a; chil : mutable list idag }
and idag = 'a
;

external int_of_idag : idag -> int = "%identity";
external idag_of_int : int -> idag = "%identity";

type table 'a = 'b;

value print_html_table :
  (string -> unit) -> (node 'a -> unit) -> (node 'a -> bool) -> int
    -> dag 'a -> table idag -> unit;

value table_of_dag : bool -> bool -> dag 'a -> table idag;
