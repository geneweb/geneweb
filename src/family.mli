(* $Id: family.mli,v 4.3 2004-12-14 09:30:12 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;

value treat_request_on_base :
  config -> (string * unit -> out_channel * out_channel -> unit) -> unit;
