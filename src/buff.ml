(* $Id: buff.ml,v 3.0 1999-10-29 10:31:00 ddr Exp $ *)

value buff = ref (String.create 80);

value store len x =
  do if len >= String.length buff.val then
       buff.val := buff.val ^ String.create (String.length buff.val)
     else ();
     buff.val.[len] := x;
  return succ len
;

value get len = String.sub buff.val 0 len;
