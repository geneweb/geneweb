(* $Id: history.mli,v 4.6 2005-09-10 10:31:29 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;

value file_name : config -> string;
value record :
  config -> base -> (string * string * int * iper) -> string -> unit
;
value record_notes : config -> base -> (option int * string) -> string -> unit;
value print : config -> base -> unit;
