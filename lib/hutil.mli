(* $Id: hutil.mli,v 5.4 2007-01-17 15:07:26 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config

val header : config -> (bool -> unit) -> unit
val header_fluid : config -> (bool -> unit) -> unit
val header_link_welcome : config -> (bool -> unit) -> unit
val print_link_to_welcome : config -> bool -> unit
val trailer : config -> unit

val header_without_page_title : config -> (bool -> unit) -> unit
val header_without_http : config -> (bool -> unit) -> unit
val header_no_page_title : config -> (bool -> unit) -> unit
val rheader : config -> (bool -> unit) -> unit
val link_to_referer : config -> string
val gen_print_link_to_welcome : (unit -> unit) -> config -> bool -> unit
val gen_trailer : bool -> config -> unit

val incorrect_request : config -> unit

val interp :
  config -> string -> ('a, 'b) Templ.interp_fun -> 'a Templ.env -> 'b -> unit

val interp_no_header :
  config -> string -> ('a, 'b) Templ.interp_fun -> 'a Templ.env -> 'b -> unit
