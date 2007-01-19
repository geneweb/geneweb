(* $Id: request.mli,v 5.2 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;

value treat_request_on_base :
  config -> (string * unit -> out_channel * out_channel -> unit) -> unit;
