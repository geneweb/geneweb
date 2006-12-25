(* $Id: check.mli,v 5.5 2006-12-25 22:56:03 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

(* checking database ; independant from its implementation on disk *)

open Gwdb;
open CheckItem;

value print_base_error : out_channel -> base -> base_error -> unit;
value print_base_warning : out_channel -> base -> base_warning -> unit;

value check_base :
  base -> (base_error -> unit) -> (base_warning -> unit) ->
    (int -> bool) -> ((Def.iper * Def.gen_person Def.iper istr) -> unit) ->
    bool -> unit;
