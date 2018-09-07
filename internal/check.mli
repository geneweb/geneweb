(* $Id: check.mli,v 5.7 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

(* checking database ; independent from its implementation on disk *)

open Gwdb

val print_base_error : out_channel -> base -> CheckItem.base_error -> unit
val print_base_warning : out_channel -> base -> CheckItem.base_warning -> unit

val check_base :
  base -> (CheckItem.base_error -> unit) -> (CheckItem.base_warning -> unit) -> (int -> bool) ->
    (Def.iper * person * Def.sex option * relation list option -> unit) ->
    bool -> unit
