(* $Id: adef.mli,v 5.6 2007-02-21 18:14:01 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(** Consanguinity rate *)
type fix

(** Returns float coefficient of consanguinity rate *)
val float_of_fix : fix -> float

(** Returns consanguinity rate from its float coefficient *)
val fix_of_float : float -> fix

(** [fix] from int *)
external fix : int -> fix = "%identity"

(** [fix] to int *)
external fix_repr : fix -> int = "%identity"

(** No consanguinity *)
val no_consang : fix

(** Date data type that can be either concrete date associated to a calendar or a textual form of the date. *)
type date =
    Dgreg of dmy * calendar
  | Dtext of string

(** Supported calendars *)
and calendar = Dgregorian | Djulian | Dfrench | Dhebrew

(** Concrete date with precision. *)
and dmy =
  { day : int; month : int; year : int; prec : precision; delta : int }

(** Concrete date without precision. *)
and dmy2 = { day2 : int; month2 : int; year2 : int; delta2 : int }

(** Precision attached to the concrete date. *)
and precision =
    Sure
  | About
  | Maybe
  | Before
  | After
  | OrYear of dmy2
  | YearInt of dmy2

(** Compressed date *)
type cdate =
  | Cgregorian of int
  | Cjulian of int
  | Cfrench of int
  | Chebrew of int
  | Ctext of string
  | Cdate of date
  | Cnone


(** Polymorphic type to represent a family's couple.
    Couple consists of the father and of the mother. *)
type 'person gen_couple

(** Get father from couple *)
val father : 'a gen_couple -> 'a

(** Get mother from couple *)
val mother : 'a gen_couple -> 'a

(** [couple f m] creates a couple from father [f] and mother [m] *)
val couple : 'a -> 'a -> 'a gen_couple

(** Create [gen_couple] from array. First element of array should be father, second - mother  *)
val parent : 'a array -> 'a gen_couple

(** Returns array from [gen_couple]. First element of array is father, second - mother  *)
val parent_array : 'a gen_couple -> 'a array

(** @deprecated Use [couple] instead *)
val multi_couple : 'a -> 'a -> 'a gen_couple

(** @deprecated Use [parent] instead *)
val multi_parent : 'a array -> 'a gen_couple

type +'a astring = private string
type safe_string = [`encoded|`escaped|`safe] astring
type escaped_string = [`encoded|`escaped] astring
type encoded_string = [`encoded] astring
val ( ^^^ ) : 'a astring -> 'a astring -> 'a astring
val ( ^>^ ) : 'a astring -> string -> 'a astring
val ( ^<^ ) : string -> 'a astring -> 'a astring
val ( <^> ) : 'a astring -> 'a astring -> bool
val safe : string -> safe_string
val escaped : string -> escaped_string
val encoded : string -> encoded_string
val as_string : 'a astring -> string
val safe_fn : (string -> string) -> 'a astring -> 'a astring
