(* $Id: adef.ml,v 1.1.1.1 1998-09-01 14:32:03 ddr Exp $ *)

type iper = int;
type ifam = int;
type istr = int;
type fix = int;
type cdate = int;
type codate = int;

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

type precision = [ Sure | About | Maybe | Before | After | OrYear of int ];
type date =
  [ Djma of int and int and int
  | Dma of int and int
  | Da of precision and int ]
;

value cyoy y = if y <= 0 then 2500 - y else y;

value cdate_of_date =
  fun
  [ Djma d m y -> ((cyoy y * 12 + m - 1) * 31 + d - 1) * 8
  | Dma m y -> (cyoy y * 12 + m - 1) * 8 + 1
  | Da Sure y -> cyoy y * 8 + 2
  | Da About y -> cyoy y * 8 + 3
  | Da Maybe y -> cyoy y * 8 + 4
  | Da Before y -> cyoy y * 8 + 5
  | Da After y -> cyoy y * 8 + 6
  | Da (OrYear y2) y -> ((cyoy y2 * 5000) + cyoy y) * 8 + 7 ]
;

value yocy y = if y >= 2500 then 2500 - y else y;

value date_of_cdate c =
  match (c mod 8, c / 8) with
  [ (0, c) ->
      let d = c mod 31 + 1 in
      let c = c / 31 in
      let m = c mod 12 + 1 in
      Djma d m (yocy (c / 12))
  | (1, c) ->
      let m = c mod 12 + 1 in
      Dma m (yocy (c / 12))
  | (2, c) -> Da Sure (yocy c)
  | (3, c) -> Da About (yocy c)
  | (4, c) -> Da Maybe (yocy c)
  | (5, c) -> Da Before (yocy c)
  | (6, c) -> Da After (yocy c)
  | (_, c) -> Da (OrYear (yocy (c / 5000))) (yocy (c mod 5000)) ]
;

value codate_of_od =
  fun
  [ None -> 0
  | Some d -> cdate_of_date d * 2 ]
;

value od_of_codate c =
  if c == 0 then None
  else Some (date_of_cdate (c / 2))
;

value codate_None = codate_of_od None;
