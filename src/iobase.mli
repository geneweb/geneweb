(* $Id: iobase.mli,v 2.1 1999-03-08 11:18:48 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;

value magic_gwb : string;

value input : string -> base;
value output : string -> base -> unit;

value lock_file : string -> string;

value name_key : string -> string;
