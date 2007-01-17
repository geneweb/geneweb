(* $Id: hutil.mli,v 5.1 2007-01-17 14:27:24 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config;
open Gwdb;

value gen_trailer : bool -> config -> unit;
value trailer : config -> unit;

value incorrect_request : config -> unit;

value interp :
  config -> base -> string -> Templ.interp_fun 'a 'b -> Templ.env 'a -> 'b ->
    unit;
