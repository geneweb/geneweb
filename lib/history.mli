(* $Id: history.mli,v 5.4 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

val file_name : config -> string

val record : config -> base -> (iper, iper, ifam, string) base_changed -> string -> unit
val notify : config -> base -> string -> unit

val print : config -> base -> unit
val print_search : config -> base -> unit


(* Ajout pour l'API *)
val line_fields : string -> (string * string * string * string option) option
