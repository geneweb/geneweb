(* $Id: history.mli,v 3.1 2000-01-10 02:14:39 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Config;
open Def;

value file_name : config -> string;
value record : config -> base -> (string * string * int) -> string -> unit;
value print : config -> base -> unit;
