(* $Id: consangAll.mli,v 5.2 2006-10-23 20:06:31 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Gwdb;

value compute : base -> bool -> bool -> option (array Adef.fix);
