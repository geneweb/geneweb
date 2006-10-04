(* $Id: check.mli,v 5.4 2006-10-04 14:17:54 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

(* checking database ; independant from its implementation on disk *)

open Gwdb;
open CheckItem;

value print_base_error : out_channel -> base -> base_error -> unit;
value print_base_warning : out_channel -> base -> base_warning -> unit;

value check_base :
  base -> (base_error -> unit) -> (base_warning -> unit) ->
    (int -> bool) -> ((Def.iper * person) -> unit) -> bool -> unit;
