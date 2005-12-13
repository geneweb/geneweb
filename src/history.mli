(* $Id: history.mli,v 5.0 2005-12-13 11:51:27 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;

value file_name : config -> string;
value record :
  config -> base -> (string * string * int * iper) -> string -> unit
;
value record_notes : config -> base -> (option int * string) -> string -> unit;
value print : config -> base -> unit;
