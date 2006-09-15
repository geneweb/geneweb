(* $Id: history.mli,v 5.2 2006-09-15 11:45:37 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;
open Def;
open Gwdb;

value file_name : config -> string;
value record :
  config -> base -> (string * string * int * iper) -> string -> unit
;
value record_notes : config -> base -> (option int * string) -> string -> unit;
value print : config -> base -> unit;
