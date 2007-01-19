(* $Id: check.mli,v 5.7 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

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
