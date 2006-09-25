(* $Id: request.mli,v 5.1 2006-09-25 09:21:30 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;

value treat_request_on_base :
  config -> (string * unit -> out_channel * out_channel -> unit) -> unit;
