(* $Id: history.mli,v 5.1 2006-01-01 05:35:07 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;
open Def;

value file_name : config -> string;
value record :
  config -> base -> (string * string * int * iper) -> string -> unit
;
value record_notes : config -> base -> (option int * string) -> string -> unit;
value print : config -> base -> unit;
