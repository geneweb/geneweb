(* $Id: history.mli,v 5.4 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;

value file_name : config -> string;
value record :
  config -> base -> (string * string * int * iper) -> string -> unit
;
value record_notes : config -> base -> (option int * string) -> string -> unit;
value print : config -> base -> unit;
value print_search : config -> base -> unit;

value record_key : config -> base -> string -> string -> unit;

value notify_places : config -> base -> string -> unit;
  
