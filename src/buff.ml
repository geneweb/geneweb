(* $Id: buff.ml,v 4.1 2001-04-18 12:36:34 ddr Exp $ *)

value buff = ref (String.create 80);

value store len x =
  do {
    if len >= String.length buff.val then
      buff.val := buff.val ^ String.create (String.length buff.val)
    else ();
    buff.val.[len] := x;
    succ len
  }
;

value mstore len s =
  add_rec len 0 where rec add_rec len i =
    if i == String.length s then len else add_rec (store len s.[i]) (succ i)
;

value get len = String.sub buff.val 0 len;
