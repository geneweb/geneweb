(* $Id: consangAll.mli,v 5.5 2007/02/16 10:38:36 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Gwdb

val compute : ?verbosity:int -> base -> int -> bool -> Adef.fix array option
