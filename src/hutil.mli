(* $Id: hutil.mli,v 5.4 2007-01-17 15:07:26 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config;
open Gwdb;

value header : config -> (bool -> unit) -> unit;
value print_link_to_welcome : config -> bool -> unit;
value trailer : config -> unit;

value header_without_page_title : config -> (bool -> unit) -> unit;
value header_without_http : config -> (bool -> unit) -> unit;
value header_no_page_title : config -> (bool -> unit) -> unit;
value rheader : config -> (bool -> unit) -> unit;
value link_to_referer : config -> string;
value gen_print_link_to_welcome : (unit -> unit) -> config -> bool -> unit;
value gen_trailer : bool -> config -> unit;

value incorrect_request : config -> unit;

value interp :
  config -> base -> string -> Templ.interp_fun 'a 'b -> Templ.env 'a -> 'b ->
    unit;

value interp_no_header :
  config -> base -> string -> Templ.interp_fun 'a 'b -> Templ.env 'a -> 'b ->
    unit;
