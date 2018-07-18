(* $Id: history.mli,v 5.4 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

val file_name : config -> string

val record : config -> base -> (iper, string) base_changed -> string -> unit
val notify : config -> base -> string -> unit

val print : config -> base -> unit
val print_search : config -> base -> unit


(* Ajout pour l'API *)
exception Begin_of_file
val line_fields : string -> (string * string * string * string option) option
val rev_input_line : in_channel -> int -> bytes ref * int ref -> string * int
