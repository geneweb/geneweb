(* $Id: history.mli,v 4.0 2001-03-16 19:34:45 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Config;
open Def;

value file_name : config -> string;
value record : config -> base -> (string * string * int) -> string -> unit;
value print : config -> base -> unit;
