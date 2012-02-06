(* $Id: db2.mli,v 5.3 2012-01-18 20:49:10 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

value first_item_pos : int -> int;
value empty_string_pos : int -> int;
value quest_string_pos : int -> int;

type key2 = 'abstract;
value key2_of_key : (Adef.istr * Adef.istr * int) -> key2;
