(* $Id: db2.mli,v 5.1 2007-02-22 03:50:29 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

value first_item_pos : int;
value empty_string_pos : int;
value quest_string_pos : int;

type key2 = 'abstract;
value key2_of_key : (Adef.istr * Adef.istr * int) -> key2;
