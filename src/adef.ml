(* $Id: adef.ml,v 2.1 1999-03-08 11:18:16 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

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
  { day : int;
    month : int;
    year : int;
    prec : precision }
;

type cdate = [ CdateNone | Cdate1 of int | Cdate2 of date ];
type codate = cdate;

value cdate_of_date d =
  let simple =
    match d.prec with
    [ Sure | About | Maybe | Before | After -> d.year > 0 && d.year < 2500
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
    Cdate1 (((p * 32 + d.day) * 13 + d.month) * 2500 + d.year)
  else Cdate2 d
;

value date_of_cdate c =
  match c with
  [ Cdate1 x ->
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
      {day = day; month = month; year = year; prec = prec}
  | Cdate2 d -> d
  | CdateNone -> failwith "date_of_cdate" ]
;

value codate_of_od =
  fun
  [ Some d -> cdate_of_date d
  | None -> CdateNone ]
;

value od_of_codate od =
  match od with
  [ CdateNone -> None
  | _ -> Some (date_of_cdate od) ]
;

value codate_None = codate_of_od None;
