(* $Id: searchName.mli,v 5.2 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Gwdb;

value search_partial_key : config -> base -> string -> list person;
value print : config -> base -> (config -> base -> string -> list person -> unit) -> (config -> string -> unit) -> unit;
value search_by_sosa : config -> base -> string -> list person;
value search_by_key : config -> base -> string -> list person;
value search_approx_key : config -> base -> string -> list person;
