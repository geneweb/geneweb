(* $Id: family.mli,v 4.2 2003-12-04 20:30:56 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Config;

value treat_request_on_base :
  config -> (string * unit -> out_channel * out_channel -> unit) -> unit;
