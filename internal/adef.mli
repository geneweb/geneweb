(* $Id: adef.mli,v 5.6 2007-02-21 18:14:01 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type iper
type ifam
type istr
type fix
type cdate
type codate
type 'person gen_couple

type date =
    Dgreg of dmy * calendar
  | Dtext of string
and calendar = Dgregorian | Djulian | Dfrench | Dhebrew
and dmy =
  { day : int; month : int; year : int; prec : precision; delta : int }
and dmy2 = { day2 : int; month2 : int; year2 : int; delta2 : int }
and precision =
    Sure
  | About
  | Maybe
  | Before
  | After
  | OrYear of dmy2
  | YearInt of dmy2

val float_of_fix : fix -> float
val fix_of_float : float -> fix
external fix : int -> fix = "%identity"
external fix_repr : fix -> int = "%identity"

val no_consang : fix

val date_of_cdate : cdate -> date
val cdate_of_date : date -> cdate

val codate_None : codate
val od_of_codate : codate -> date option
val codate_of_od : date option -> codate

external int_of_iper : iper -> int = "%identity"
external iper_of_int : int -> iper = "%identity"
external int_of_ifam : ifam -> int = "%identity"
external ifam_of_int : int -> ifam = "%identity"
external int_of_istr : istr -> int = "%identity"
external istr_of_int : int -> istr = "%identity"

exception Request_failure of string

val father : 'a gen_couple -> 'a
val mother : 'a gen_couple -> 'a
val couple : 'a -> 'a -> 'a gen_couple
val parent : 'a array -> 'a gen_couple
val parent_array : 'a gen_couple -> 'a array

val multi_couple : 'a -> 'a -> 'a gen_couple
val multi_parent : 'a array -> 'a gen_couple
