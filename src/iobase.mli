(* $Id: iobase.mli,v 2.2 1999-08-30 23:55:49 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;

value input : string -> base;
value output : string -> base -> unit;

value lock_file : string -> string;

value name_key : string -> string;
