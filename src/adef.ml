(* $Id: adef.ml,v 1.2 1998-11-27 20:09:37 ddr Exp $ *)

type iper = int;
type ifam = int;
type istr = int;
type fix = int;

value float_of_fix x = float x /. 1000000.0;
value fix_of_float x = truncate (x *. 1000000.0 +. 0.5);
external fix : int -> fix = "%identity";
external fix_repr : fix -> int = "%identity";

external int_of_iper : iper -> int = "%identity";
external iper_of_int : int -> iper = "%identity";
external int_of_ifam : ifam -> int = "%identity";
external ifam_of_int : int -> ifam = "%identity";
external int_of_istr : istr -> int = "%identity";
external istr_of_int : int -> istr = "%identity";

type precision =
  [ Sure | About | Maybe | Before | After | OrYear of int | YearInt of int ]
;
type date =
  { year : int;
    month : int;
    day : int;
    prec : precision }
;

type cdate = date;
type codate = option date;

value cdate_of_date d = d;
value date_of_cdate c = c;

value codate_of_od od = od;
value od_of_codate oc = oc;

value codate_None = codate_of_od None;
