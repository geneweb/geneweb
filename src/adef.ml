(* $Id: adef.ml,v 4.0 2001-03-16 19:34:22 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

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

type cdate =
  [ Cgregorian of int
  | Cjulian of int
  | Cfrench of int
  | Chebrew of int
  | Ctext of string
  | Cdate of date
  | Cnone ]
;

type codate = cdate;

value compress d =
  let simple =
    match d.prec with
    [ Sure | About | Maybe | Before | After ->
        d.day >= 0 && d.month >= 0 && d.year > 0 && d.year < 2500 &&
        d.delta = 0
    | _ -> False ]
  in
  if simple then
    let p =
      match d.prec with
      [ About -> 1
      | Maybe -> 2
      | Before -> 3
      | After -> 4
      | _ -> 0 ]
    in
    Some (((p * 32 + d.day) * 13 + d.month) * 2500 + d.year)
  else None
;

value cdate_of_date d =
  match d with
  [ Dgreg g cal ->
      match compress g with
      [ Some i ->
          match cal with
          [ Dgregorian -> Cgregorian i
          | Djulian -> Cjulian i
          | Dfrench -> Cfrench i
          | Dhebrew -> Chebrew i ]
      | None -> Cdate d ]
  | Dtext t -> Ctext t ]
;

value uncompress x =
  let (year, x) = (x mod 2500, x / 2500) in
  let (month, x) = (x mod 13, x / 13) in
  let (day, x) = (x mod 32, x / 32) in
  let prec =
    match x with
    [ 1 -> About
    | 2 -> Maybe
    | 3 -> Before
    | 4 -> After
    | _ -> Sure ]
  in
  {day = day; month = month; year = year; prec = prec; delta = 0}
;

value date_of_cdate =
  fun
  [ Cgregorian i -> Dgreg (uncompress i) Dgregorian
  | Cjulian i -> Dgreg (uncompress i) Djulian
  | Cfrench i -> Dgreg (uncompress i) Dfrench
  | Chebrew i -> Dgreg (uncompress i) Dhebrew
  | Cdate d -> d
  | Ctext t -> Dtext t
  | Cnone -> failwith "date_of_cdate" ]
;

value codate_of_od =
  fun
  [ Some d -> cdate_of_date d
  | None -> Cnone ]
;

value od_of_codate od =
  match od with
  [ Cnone -> None
  | _ -> Some (date_of_cdate od) ]
;

value codate_None = codate_of_od None;
