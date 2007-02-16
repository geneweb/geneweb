(* $Id: consangAll.mli,v 5.5 2007-02-16 10:38:36 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Gwdb;

value compute : base -> bool -> bool -> option (array Adef.fix);
