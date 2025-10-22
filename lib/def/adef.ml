(* $Id: adef.ml,v 5.6 2007-02-21 18:14:01 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type fix = int

let float_of_fix x = float x /. 1000000.0
let fix_of_float x = truncate ((x *. 1000000.0) +. 0.5)

external fix : int -> fix = "%identity"
external fix_repr : fix -> int = "%identity"

let no_consang = fix (-1)

type date = Dgreg of dmy * calendar | Dtext of string
and calendar = Dgregorian | Djulian | Dfrench | Dhebrew | Dislamic
and dmy = { day : int; month : int; year : int; prec : precision; delta : int }
and dmy2 = { day2 : int; month2 : int; year2 : int; delta2 : int }

and precision =
  | Sure
  | About
  | Maybe
  | Before
  | After
  | OrYear of dmy2
  | YearInt of dmy2

type cdate =
  | Cgregorian of int
  | Cjulian of int
  | Cfrench of int
  | Chebrew of int
  | Ctext of string
  | Cdate of date
  | Cnone
  | Cislamic of int

type 'person gen_couple = { father : 'person; mother : 'person }

let father cpl = cpl.father
let mother cpl = cpl.mother
let couple father mother = { father; mother }
let parent parent = { father = parent.(0); mother = parent.(1) }
let parent_array cpl = [| cpl.father; cpl.mother |]

type 'a astring = string
type safe_string = [ `encoded | `escaped | `safe ] astring
type escaped_string = [ `encoded | `escaped ] astring
type encoded_string = [ `encoded ] astring

let ( ^^^ ) : 'a astring -> 'a astring -> 'a astring =
 fun (a : 'a astring) (b : 'a astring) : 'a astring -> a ^ b

let ( ^>^ ) : 'a astring -> string -> 'a astring =
 fun (a : 'a astring) (b : string) : 'a astring -> a ^ b

let ( ^<^ ) : string -> 'a astring -> 'a astring =
 fun (a : string) (b : 'a astring) : 'a astring -> a ^ b

let ( <^> ) : 'a astring -> 'a astring -> bool = ( <> )

external safe : string -> safe_string = "%identity"
external escaped : string -> escaped_string = "%identity"
external encoded : string -> encoded_string = "%identity"
external as_string : 'a astring -> string = "%identity"

let safe_fn = ( @@ )
