(* $Id: history.mli,v 4.5 2005-06-07 20:31:43 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;

value file_name : config -> string;
value record : config -> base -> (string * string * int) -> string -> unit;
value record_notes : config -> base -> (option int * string) -> string -> unit;
value print : config -> base -> unit;
