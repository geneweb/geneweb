(* $Id: iobase.mli,v 2.3 1999-09-17 06:29:56 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;

value input : string -> base;
value output : string -> base -> unit;

value simple_output : string -> base -> unit;

value lock_file : string -> string;

value name_key : string -> string;
