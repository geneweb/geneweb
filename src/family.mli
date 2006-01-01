(* $Id: family.mli,v 5.1 2006-01-01 05:35:07 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;

value treat_request_on_base :
  config -> (string * unit -> out_channel * out_channel -> unit) -> unit;
