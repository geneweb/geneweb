(* $Id: history.mli,v 4.1 2004-12-14 09:30:12 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;

value file_name : config -> string;
value record : config -> base -> (string * string * int) -> string -> unit;
value print : config -> base -> unit;
