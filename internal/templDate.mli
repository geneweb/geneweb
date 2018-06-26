(* $Id: templDate.mli,v 5.2 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config
open TemplAst

val eval_date_var : config -> int -> string list -> 'a expr_val
