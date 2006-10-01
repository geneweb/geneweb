(* $Id: check.mli,v 5.1 2006-10-01 11:30:07 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Gwdb;

value print_base_error :
  out_channel -> base -> Def.error person -> unit;
value print_base_warning :
  out_channel -> base ->
    Def.warning person descend (Def.gen_title Adef.istr) -> unit;

value check_base :
  base -> (Gutil.base_error -> unit) -> (Gutil.base_warning -> unit) ->
    (int -> bool) -> ((Def.iper * person) -> unit) -> bool -> unit;
