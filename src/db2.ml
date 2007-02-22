(* $Id: db2.ml,v 5.4 2007-02-22 03:50:29 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

value first_item_pos = 25;
value empty_string_pos = first_item_pos;
value quest_string_pos = first_item_pos + 1;

type key2 =
  [ Key of Adef.istr and Adef.istr and int
  | Key0 of Adef.istr and Adef.istr (* to save memory space *) ]
;

value key2_of_key (fn, sn, oc) = if oc = 0 then Key0 fn sn else Key fn sn oc;
