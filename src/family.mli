(* $Id: family.mli,v 4.1 2001-03-23 11:46:06 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Config;
open Def;

value family :
  config -> base -> (string * unit -> out_channel * out_channel -> unit) ->
     unit;
