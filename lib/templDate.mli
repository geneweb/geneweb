(* $Id: templDate.mli,v 5.2 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

val eval_date_var : Config.config -> int -> string list -> 'a TemplAst.expr_val
