(* $Id: check.mli,v 5.6 2006-12-27 14:57:46 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

(* checking database ; independant from its implementation on disk *)

open Gwdb;
open CheckItem;

value print_base_error : out_channel -> base -> base_error -> unit;
value print_base_warning : out_channel -> base -> base_warning -> unit;

value check_base :
  base -> (base_error -> unit) -> (base_warning -> unit) ->
    (int -> bool) ->
    ((Def.iper * person * option Def.sex * option (list relation)) -> unit) ->
    bool -> unit;
