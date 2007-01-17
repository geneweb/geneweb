(* $Id: hutil.mli,v 5.2 2007-01-17 14:40:34 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config;
open Gwdb;

value header : config -> (bool -> unit) -> unit;
value trailer : config -> unit;

value header_no_page_title : config -> (bool -> unit) -> unit;
value rheader : config -> (bool -> unit) -> unit;
value gen_trailer : bool -> config -> unit;

value incorrect_request : config -> unit;

value interp :
  config -> base -> string -> Templ.interp_fun 'a 'b -> Templ.env 'a -> 'b ->
    unit;
