(* $Id: iobase.mli,v 1.3 1999-02-02 10:24:15 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;

value magic_gwb : string;

value input : string -> base;
value output : string -> base -> unit;

value lock_file : string -> string;

value name_key : string -> string;
