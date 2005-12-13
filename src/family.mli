(* $Id: family.mli,v 5.0 2005-12-13 11:51:27 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;

value treat_request_on_base :
  config -> (string * unit -> out_channel * out_channel -> unit) -> unit;
