(* $Id: history.mli,v 2.1 1999-09-24 05:59:31 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;

value record : config -> base -> (string * string * int) -> string -> unit;
value print : config -> base -> unit;
