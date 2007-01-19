(* $Id: consangAll.mli,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Gwdb;

value compute : base -> bool -> bool -> option (array Adef.fix);
