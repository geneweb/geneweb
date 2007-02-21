(* $Id: adef.mli,v 5.6 2007-02-21 18:14:01 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type iper = 'a;
type ifam = 'a;
type istr = 'a;
type fix = 'a;
type cdate = 'a;
type codate = 'a;
type gen_couple 'person = 'a;

type date =
  [ Dgreg of dmy and calendar
  | Dtext of string ]
and calendar =
  [ Dgregorian
  | Djulian
  | Dfrench
  | Dhebrew ]
and dmy =
  { day : int;
    month : int;
    year : int;
    prec : precision;
    delta : int }
and precision =
  [ Sure | About | Maybe | Before | After | OrYear of int | YearInt of int ]
;

value float_of_fix : fix -> float;
value fix_of_float : float -> fix;
external fix : int -> fix = "%identity";
external fix_repr : fix -> int = "%identity";

value no_consang : fix;

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

exception Request_failure of string;

value father : gen_couple 'a -> 'a;
value mother : gen_couple 'a -> 'a;
value couple : 'a -> 'a -> gen_couple 'a;
value parent : array 'a -> gen_couple 'a;
value parent_array : gen_couple 'a -> array 'a;

value multi_couple : 'a -> 'a -> gen_couple 'a;
value multi_parent : array 'a -> gen_couple 'a;
