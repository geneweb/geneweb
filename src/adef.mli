(* $Id: adef.mli,v 1.1 1998-09-01 14:32:03 ddr Exp $ *)

type iper = 'a;
type ifam = 'a;
type istr = 'a;
type fix = 'a;
type cdate = 'a;
type codate = 'a;

type precision = [ Sure | About | Maybe | Before | After | OrYear of int ];
type date =
  [ Djma of int and int and int
  | Dma of int and int
  | Da of precision and int ]
;

value float_of_fix : fix -> float;
value fix_of_float : float -> fix;
external fix : int -> fix = "%identity";
external fix_repr : fix -> int = "%identity";

value date_of_cdate : cdate -> date;
value cdate_of_date : date -> cdate;

value codate_None : codate;
value od_of_codate : codate -> option date;
value codate_of_od : option date -> codate;

external int_of_iper : iper -> int = "%identity";
external iper_of_int : int -> iper = "%identity";
external int_of_ifam : ifam -> int = "%identity";
external ifam_of_int : int -> ifam = "%identity";
external int_of_istr : istr -> int = "%identity";
external istr_of_int : int -> istr = "%identity";
