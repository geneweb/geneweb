(* $Id: history.mli,v 3.0 1999-10-29 10:31:19 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;

value file_name : config -> string;
value record : config -> base -> (string * string * int) -> string -> unit;
value print : config -> base -> unit;
