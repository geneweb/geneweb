(* $Id: history.mli,v 4.2 2005-06-02 16:43:02 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;

value file_name : config -> string;
value record :
  config -> base -> option (string * string * int) -> string -> unit;
value print : config -> base -> unit;
