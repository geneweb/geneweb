(* camlp4r *)
(* $Id: templDate.mli,v 5.1 2007-02-12 11:47:59 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config;
open TemplAst;

value eval_date_var : config -> int -> list string -> expr_val 'a;
