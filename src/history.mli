(* $Id: history.mli,v 2.2 1999-09-24 11:55:31 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;

value file_name : config -> string;
value record : config -> base -> (string * string * int) -> string -> unit;
value print : config -> base -> unit;
